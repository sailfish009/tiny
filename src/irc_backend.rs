use config;
use mio::Poll;
use mio::Token;
use stream::{Stream, StreamErr};
use tui::handle::*;
use tui::tabbed::TabStyle;
use tui::Timestamp;
use utils;
use wire::{Cmd, Msg, Pfx};
use wire;

pub struct IrcBackend<'poll> {
    poll: &'poll Poll,

    serv_addr: String,
    serv_port: u16,
    tls: bool,
    hostname: String,
    realname: String,
    nicks: Vec<String>,

    /// Always in range of `nicks`
    current_nick_idx: usize,

    /// Channels to auto-join. Every channel we join will be added here to be able to re-join
    /// automatically on reconnect and channels we leave will be removed.
    auto_join: Vec<String>,

    /// Auto commands to run on successfully connecting to the server.
    auto_cmds: Vec<String>,

    /// Away reason if away mode is on. `None` otherwise.
    away_status: Option<String>,

    /// servername to be used in PING messages. Read from 002 RPL_YOURHOST.
    /// `None` until 002.
    servername: Option<String>,

    /// Our usermask given by the server. Currently only parsed after a JOIN,
    /// reply 396.
    ///
    /// Note that RPL_USERHOST (302) does not take cloaks into account, so we
    /// don't parse USERHOST responses to set this field.
    usermask: Option<String>,

    status: ConnStatus<'poll>,

    /// Incoming message buffer
    in_buf: Vec<u8>,
}

/// How many ticks to wait before assuming disconnect in introduce state.
const INTRO_TICKS: u8 = 30;
/// How many ticks to wait before sending a ping to the server.
const PING_TICKS: u8 = 60;
/// How many ticks to wait after sending a ping to the server to consider a
/// disconnect.
const PONG_TICKS: u8 = 60;
/// How many ticks to wait after a disconnect or a socket error.
const RECONNECT_TICKS: u8 = 30;

enum ConnStatus<'poll> {
    /// Need to introduce self
    Introduce {
        ticks_passed: u8,
        stream: Stream<'poll>,
    },
    PingPong {
        /// Ticks passed since last time we've heard from the server. Reset on
        /// each message. After `PING_TICKS` ticks we send a PING message and
        /// move to `WaitPong` state.
        ticks_passed: u8,
        stream: Stream<'poll>,
    },
    WaitPong {
        /// Ticks passed since we sent a PING to the server. After a message
        /// move to `PingPong` state. On timeout we reset the connection.
        ticks_passed: u8,
        stream: Stream<'poll>,
    },
    Disconnected {
        ticks_passed: u8,
    },
}

macro_rules! update_status {
    ($self:ident, $v:ident, $code:expr) => {{
        // temporarily putting `Disconnected` to `self.status`
        let $v = ::std::mem::replace(&mut $self.status, ConnStatus::Disconnected { ticks_passed: 0 });
        let new_status = $code;
        $self.status = new_status;
    }}
}

impl<'poll> ConnStatus<'poll> {
    fn get_stream(&self) -> Option<&Stream<'poll>> {
        use self::ConnStatus::*;
        match *self {
            Introduce { ref stream, .. }
            | PingPong { ref stream, .. }
            | WaitPong { ref stream, .. } =>
                Some(stream),
            Disconnected { .. } =>
                None,
        }
    }

    fn get_stream_mut(&mut self) -> Option<&mut Stream<'poll>> {
        use self::ConnStatus::*;
        match *self {
            Introduce { ref mut stream, .. }
            | PingPong { ref mut stream, .. }
            | WaitPong { ref mut stream, .. } =>
                Some(stream),
            Disconnected { .. } =>
                None,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Public interface
////////////////////////////////////////////////////////////////////////////////

impl<'poll> IrcBackend<'poll> {

    pub fn init(server: config::Server, poll: &'poll Poll) -> Result<IrcBackend<'poll>, StreamErr> {
        let stream =
            Stream::new(poll, &server.addr, server.port, server.tls).map_err(StreamErr::from)?;
        Ok(IrcBackend {
            poll: poll,
            serv_addr: server.addr,
            serv_port: server.port,
            tls: server.tls,
            hostname: server.hostname,
            realname: server.realname,
            nicks: server.nicks,
            current_nick_idx: 0,
            auto_join: server.join,
            auto_cmds: server.auto_cmds,
            away_status: None,
            servername: None,
            usermask: None,
            status: ConnStatus::Introduce {
                ticks_passed: 0,
                stream: stream,
            },
            in_buf: vec![],
        })
    }

    /// Reconnect using new server options.
    pub fn reconnect(&mut self, new_serv: Option<(&str, u16)>) -> Result<(), StreamErr> {
        // drop existing connection first
        let old_stream = ::std::mem::replace(
            &mut self.status,
            ConnStatus::Disconnected { ticks_passed: 0 },
        );
        drop(old_stream);

        if let Some((new_name, new_port)) = new_serv {
            self.serv_addr = new_name.to_owned();
            self.serv_port = new_port;
        }
        match Stream::new(self.poll, &self.serv_addr, self.serv_port, self.tls) {
            Err(err) =>
                Err(StreamErr::from(err)),
            Ok(stream) => {
                self.status = ConnStatus::Introduce {
                    ticks_passed: 0,
                    stream: stream,
                };
                self.current_nick_idx = 0;
                Ok(())
            }
        }
    }

    /// Get mio token for the connection.
    pub fn get_conn_tok(&self) -> Option<Token> {
        self.status.get_stream().map(Stream::get_tok)
    }

    pub fn get_serv_name(&self) -> &str {
        &self.serv_addr
    }

    /// Handle a command sent to this backend.
    pub fn handle_cmd(&mut self, tui: &mut TuiHandle, src: InputSrc, str: &str, cmd: &str, args: &[&str]) {
        if cmd == "connect" && args.is_empty() {
            tui.add_client_msg(
                "Reconnecting...",
                &TuiTarget::AllServTabs,
            );
            match self.reconnect(None) {
                Ok(()) =>
                    {}
                Err(err) => {
                    tui.add_err_msg(
                        &reconnect_err_msg(&err),
                        Timestamp::now(),
                        &TuiTarget::AllServTabs,
                    );
                }
            }
        }

        else if cmd == "join" {
            self.join(args);
        }

        else if cmd == "me" {
            let mut word_indices = utils::split_whitespace_indices(str);
            word_indices.next(); // "/me"
            if let Some(msg_begins) = word_indices.next() {
                let msg = &str[msg_begins..];
                self.send_msg_(tui, src, msg, true);
            } else {
                tui.add_client_err_msg("/me usage: /me message", &TuiTarget::CurrentTab);
            }
        }

        else if cmd == "away" {
            let mut word_indices = utils::split_whitespace_indices(str);
            word_indices.next(); // "/away"
            let msg = {
                if let Some(msg_begins) = word_indices.next() {
                    Some(&str[msg_begins..])
                } else {
                    None
                }
            };
            self.away(msg);
        }

        else if cmd == "nick" && args.len() == 1 {
            let new_nick = args[0];
            self.set_nick(new_nick);
            tui.set_nick(new_nick.to_owned());
        }

        else if cmd == "names" && args.is_empty() {
            if let InputSrc::Chan {
                ref chan_name,
            } = src
            {
                let nicks_vec = tui
                    .get_nicks(chan_name)
                    .map(|nicks| nicks.to_strings(""));
                if let Some(nicks_vec) = nicks_vec {
                    let target = TuiTarget::Chan { chan_name };
                    tui.add_client_msg(
                        &format!("{} users: {}", nicks_vec.len(), nicks_vec.join(", ")),
                        &target,
                    );
                }
            } else {
                tui.add_client_err_msg(
                    "/names only supported in chan tabs",
                    &TuiTarget::CurrentTab,
                );
            }
        }

        else if cmd == "ignore" && args.is_empty() {
            match src {
                InputSrc::Server => {
                    tui.toggle_ignore(&TuiTarget::AllServTabs);
                }
                InputSrc::Chan {
                    chan_name,
                } => {
                    tui.toggle_ignore(&TuiTarget::Chan {
                        chan_name: &chan_name,
                    });
                }
                InputSrc::User { nick } => {
                    tui.toggle_ignore(&TuiTarget::User {
                        nick: &nick,
                    });
                }
            }
        }
    }

    /// Handle a command sent to this backend.
    pub fn handle_cmd_(&mut self, tui: &mut TuiHandle, src: InputSrc, str: &str) {
        let words: Vec<&str> = str.split_whitespace().into_iter().collect();
        match words.split_first() {
            Some((cmd, args)) =>
                self.handle_cmd(tui, src, str, cmd, args),
            None =>
                // empty command, ignore
                ()
        }
    }

    pub fn send_msg(&mut self, tui: &mut TuiHandle, from: InputSrc, msg: &str) {
        self.send_msg_(tui, from, msg, false);
    }

    pub fn send_msg_(&mut self, tui: &mut TuiHandle, from: InputSrc, msg: &str, ctcp_action: bool) {
        let ts = Timestamp::now();
        // `tui_target`: Where to show the message on TUI
        // `msg_target`: Actual PRIVMSG target to send to the server
        let (tui_target, msg_target) = {
            match from {
                InputSrc::Server => {
                    // we don't split raw messages to 512-bytes long chunks
                    self.raw_msg(msg);
                    tui.add_privmsg(self.get_nick(), msg, ts, &TuiTarget::Server, false);
                    return;
                }

                InputSrc::Chan {
                    ref chan_name,
                } =>
                    (
                        TuiTarget::Chan {
                            chan_name: chan_name,
                        },
                        chan_name,
                    ),

                InputSrc::User {
                    ref nick,
                } => {
                    let msg_target = if nick.eq_ignore_ascii_case("nickserv")
                        || nick.eq_ignore_ascii_case("chanserv")
                    {
                        TuiTarget::Server
                    } else {
                        TuiTarget::User {
                            nick: nick,
                        }
                    };
                    (msg_target, nick)
                }
            }
        };

        let extra_len = msg_target.len() as i32 + if ctcp_action {
            9 // "\0x1ACTION \0x1".len()
        } else {
            0
        };
        let send_fn = if ctcp_action {
            IrcBackend::ctcp_action
        } else {
            IrcBackend::privmsg
        };
        for msg in self.split_privmsg(extra_len, msg) {
            send_fn(self, msg_target, msg);
            tui.add_privmsg(self.get_nick(), msg, ts, &tui_target, ctcp_action);
        }
    }

    /// Call when the backend's socket is ready for writing.
    pub fn write_ready(&mut self, tui: &mut TuiHandle) {
        // borrowchk workaround
        let write_ret = {
            match self.status.get_stream_mut() {
                Some(stream) => {
                    match stream.write_ready() {
                        Err(err) =>
                            if !err.is_would_block() {
                                Err(err)
                            } else {
                                Ok(())
                            }
                        Ok(()) =>
                            Ok(())
                    }
                },
                None =>
                    Ok(())
            }
        };

        if let Err(err) = write_ret {
            self.conn_err(tui, err);
        }
    }

    /// Call when the backend's socket is ready for reading.
    pub fn read_ready(&mut self, tui: &mut TuiHandle) {
        let mut read_buf: [u8; 512] = [0; 512];

        // borrowchk workaround
        let read_ret = {
            match self.status.get_stream_mut() {
                Some(stream) =>
                    match stream.read_ready(&mut read_buf) {
                        Err(err) =>
                            if !err.is_would_block() {
                                Some(Err(err))
                            } else {
                                None
                            }
                        Ok(bytes_read) =>
                            Some(Ok(bytes_read)),
                    },
                None =>
                    None,
            }
        };

        match read_ret {
            None =>
                {}
            Some(Err(err)) => {
                self.conn_err(tui, err);
            }
            Some(Ok(bytes_read)) => {
                self.reset_ticks();
                self.in_buf.extend(&read_buf[0..bytes_read]);
                self.handle_msgs(tui);
            }
        }
    }

    /// Call on HUP event.
    pub fn hup(&mut self, tui: &mut TuiHandle) {
        self.status = ConnStatus::Disconnected { ticks_passed: 0 };
        tui.add_err_msg(
            &format!(
                "Connection error (HUP). \
                 Will try to reconnect in {} seconds.",
                RECONNECT_TICKS
            ),
            Timestamp::now(),
            &TuiTarget::AllServTabs,
        );
    }

    /// Call on clock tick.
    pub fn tick(&mut self, tui: &mut TuiHandle) {
        update_status!(
            self,
            status,
            match status {
                ConnStatus::Introduce {
                    stream,
                    ticks_passed,
                } => {
                    let ticks = ticks_passed + 1;
                    if ticks == INTRO_TICKS {
                        self.ping_timeout(tui);
                        ConnStatus::Disconnected { ticks_passed: 0 }
                    } else {
                        ConnStatus::Introduce {
                            stream,
                            ticks_passed: ticks,
                        }
                    }
                }
                ConnStatus::PingPong {
                    mut stream,
                    ticks_passed,
                } => {
                    let ticks = ticks_passed + 1;
                    if ticks == PING_TICKS {
                        match self.servername {
                            None => {
                                // debug_out.write_line(format_args!(
                                //     "{}: Can't send PING, servername unknown",
                                //     self.serv_addr
                                // ));
                            }
                            Some(ref host_) => {
                                // debug_out.write_line(format_args!(
                                //     "{}: Ping timeout, sending PING",
                                //     self.serv_addr
                                // ));
                                wire::ping(&mut stream, host_).unwrap();
                            }
                        }
                        ConnStatus::WaitPong {
                            stream,
                            ticks_passed: 0,
                        }
                    } else {
                        ConnStatus::PingPong {
                            stream,
                            ticks_passed: ticks,
                        }
                    }
                }
                ConnStatus::WaitPong {
                    stream,
                    ticks_passed,
                } => {
                    let ticks = ticks_passed + 1;
                    if ticks == PONG_TICKS {
                        self.ping_timeout(tui);
                        ConnStatus::Disconnected { ticks_passed: 0 }
                    } else {
                        ConnStatus::WaitPong {
                            stream,
                            ticks_passed: ticks,
                        }
                    }
                }
                ConnStatus::Disconnected { ticks_passed } => {
                    let ticks = ticks_passed + 1;
                    if ticks_passed + 1 == RECONNECT_TICKS {
                        tui.add_client_msg(
                            "Connecting...",
                            &TuiTarget::AllServTabs,
                        );
                        self.current_nick_idx = 0;
                        match Stream::new(self.poll, &self.serv_addr, self.serv_port, self.tls) {
                            Err(err) => {
                                tui.add_err_msg(
                                    &reconnect_err_msg(&err),
                                    Timestamp::now(),
                                    &TuiTarget::AllServTabs,
                                );
                                ConnStatus::Disconnected { ticks_passed: 0 }
                            }
                            Ok(stream) =>
                                ConnStatus::Introduce {
                                    ticks_passed: 0,
                                    stream: stream,
                                }
                        }
                    } else {
                        ConnStatus::Disconnected {
                            ticks_passed: ticks,
                        }
                    }
                }
            }
        );
    }
}

////////////////////////////////////////////////////////////////////////////////
// Internals
////////////////////////////////////////////////////////////////////////////////

impl<'poll> IrcBackend<'poll> {

    fn get_nick(&self) -> &str {
        &self.nicks[self.current_nick_idx]
    }

    fn send_nick(&mut self) {
        let nick = &self.nicks[self.current_nick_idx];
        self.status.get_stream_mut().map(|stream| {
            wire::nick(stream, nick).unwrap();
        });
    }

    fn next_nick(&mut self) {
        if self.current_nick_idx + 1 == self.nicks.len() {
            let mut new_nick = self.nicks.last().unwrap().to_string();
            new_nick.push('_');
            self.nicks.push(new_nick);
        }
        self.current_nick_idx += 1;
    }

    fn away(&mut self, msg: Option<&str>) {
        self.away_status = msg.map(|s| s.to_string());
        self.status.get_stream_mut().map(|stream| {
            wire::away(stream, msg).unwrap();
        });
    }

    fn set_nick(&mut self, nick: &str) {
        if let Some(nick_idx) = self.nicks.iter().position(|n| n == nick) {
            self.current_nick_idx = nick_idx;
        } else {
            self.nicks.push(nick.to_owned());
            self.current_nick_idx = self.nicks.len() - 1;
        }
        self.send_nick();
    }

    fn join(&mut self, chans: &[&str]) {
        self.status.get_stream_mut().map(|stream| {
            wire::join(stream, &chans).unwrap();
        });
        // the channel will be added to auto-join list on successful join (i.e.
        // after RPL_TOPIC)
    }

    fn part(&mut self, serv_name: &str, chan: &str) {
        self.status.get_stream_mut().map(|stream| {
            wire::part(stream, chan).unwrap();
        });
        self.auto_join.drain_filter(|chan_| chan_ == chan);
    }

    fn reset_ticks(&mut self) {
        update_status!(
            self,
            status,
            match status {
                ConnStatus::Introduce { stream, .. } =>
                    ConnStatus::Introduce { ticks_passed: 0, stream },
                ConnStatus::PingPong { stream, .. } =>
                    ConnStatus::PingPong { ticks_passed: 0, stream },
                ConnStatus::WaitPong { stream, .. } =>
                    // no bug: we heard something from the server, whether it was a pong or not
                    // doesn't matter that much, connectivity is fine.
                    ConnStatus::PingPong { ticks_passed: 0, stream },
                ConnStatus::Disconnected { .. } =>
                    status,
            }
        );
    }

    fn conn_err(&mut self, tui: &mut TuiHandle, err: StreamErr) {
        tui.add_err_msg(
            &reconnect_err_msg(&err),
            Timestamp::now(),
            &TuiTarget::AllServTabs,
        );
        self.status = ConnStatus::Disconnected { ticks_passed: 0 };
    }

    fn ping_timeout(&mut self, tui: &mut TuiHandle) {
        let target = TuiTarget::AllServTabs;
        tui.add_err_msg(
            &format!(
                "Disconnected. Will try to reconnect in {} seconds.",
                RECONNECT_TICKS
            ),
            Timestamp::now(),
            &target
        );
        tui.clear_nicks(&target);
    }

    /// `extra_len`: Size (in bytes) for a prefix/suffix etc. that'll be added to each line.
    /// Strings returned by the iterator will have enough room for that.
    fn split_privmsg<'a>(&self, extra_len: i32, msg: &'a str) -> utils::SplitIterator<'a> {
        // Max msg len calculation adapted from hexchat
        // (src/common/outbound.c:split_up_text)
        let mut max: i32 = 512; // RFC 2812
        max -= 3; // :, !, @
        max -= 13; // " PRIVMSG ", " ", :, \r, \n
        max -= self.get_nick().len() as i32;
        max -= extra_len;
        match self.usermask {
            None => {
                max -= 9; // max username
                max -= 64; // max possible hostname (63) + '@'
                           // NOTE(osa): I think hexchat has an error here, it
                           // uses 65
            }
            Some(ref usermask) => {
                max -= usermask.len() as i32;
            }
        }

        assert!(max > 0);

        utils::split_iterator(msg, max as usize)
    }

    fn privmsg(&mut self, target: &str, msg: &str) {
        self.status.get_stream_mut().map(|stream| {
            wire::privmsg(stream, target, msg).unwrap();
        });
    }

    fn ctcp_action(&mut self, target: &str, msg: &str) {
        self.status.get_stream_mut().map(|stream| {
            wire::ctcp_action(stream, target, msg).unwrap();
        });
    }

    fn raw_msg(&mut self, msg: &str) {
        self.status.get_stream_mut().map(|stream| {
            use std::io::Write;
            write!(stream, "{}\r\n", msg).unwrap();
        });
    }

    fn handle_msgs(&mut self, tui: &mut TuiHandle) {
        while let Some(msg) = Msg::read(
            &mut self.in_buf,
            None,
        ) {
            self.handle_msg(tui, msg);
        }
    }

    fn handle_msg(&mut self, tui: &mut TuiHandle, msg: Msg) {
        if let Msg {
            cmd: Cmd::PING { ref server },
            ..
        } = msg
        {
            self.status.get_stream_mut().map(|stream| {
                wire::pong(stream, server).unwrap();
            });
        }

        update_status!(
            self,
            status,
            match status {
                ConnStatus::Introduce { mut stream, .. } => {
                    wire::user(&mut stream, &self.hostname, &self.realname).unwrap();
                    wire::nick(&mut stream, &self.nicks[self.current_nick_idx]).unwrap();
                    tui.set_nick(self.get_nick().to_owned());
                    ConnStatus::PingPong {
                        ticks_passed: 0,
                        stream: stream,
                    }
                }
                _ =>
                    status,
            }
        );

        if let Msg {
            cmd: Cmd::JOIN { .. },
            pfx: Some(Pfx::User { ref nick, ref user }),
        } = msg
        {
            if nick == self.get_nick() {
                let usermask = format!("{}!{}", nick, user);
                // logger
                //     .get_debug_logs()
                //     .write_line(format_args!("usermask set: {}", usermask));
                self.usermask = Some(usermask);
            }
        }

        if let Msg {
            cmd: Cmd::Reply {
                num: 396,
                ref params,
            },
            ..
        } = msg
        {
            // :hobana.freenode.net 396 osa1 haskell/developer/osa1
            // :is now your hidden host (set by services.)
            if params.len() == 3 {
                let usermask = format!("{}!~{}@{}", self.get_nick(), self.hostname, params[1]);
                // logger
                //     .get_debug_logs()
                //     .write_line(format_args!("usermask set: {}", usermask));
                self.usermask = Some(usermask);
            }
        }

        if let Msg {
            cmd: Cmd::Reply {
                num: 302,
                ref params,
            },
            ..
        } = msg
        {
            // 302 RPL_USERHOST
            // :ircd.stealth.net 302 yournick :syrk=+syrk@millennium.stealth.net
            //
            // We know there will be only one nick because /userhost cmd sends
            // one parameter (our nick)
            //
            // Example args: ["osa1", "osa1=+omer@moz-s8a.9ac.93.91.IP "]

            let param = &params[1];
            match wire::find_byte(param.as_bytes(), b'=') {
                None => {
                    // logger
                    //     .get_debug_logs()
                    //     .write_line(format_args!("can't parse RPL_USERHOST: {}", params[1]));
                }
                Some(mut i) => {
                    if param.as_bytes().get(i + 1) == Some(&b'+')
                        || param.as_bytes().get(i + 1) == Some(&b'-')
                    {
                        i += 1;
                    }
                    let usermask = (&param[i..]).trim();
                    self.usermask = Some(usermask.to_owned());
                    // logger
                    //     .get_debug_logs()
                    //     .write_line(format_args!("usermask set: {}", usermask));
                }
            }
        }

        if let Msg {
            cmd: Cmd::Reply { num: 001, .. },
            ..
        } = msg
        {
            // 001 RPL_WELCOME is how we understand that the registration was successful
            tui.add_msg(
                "Connected.",
                Timestamp::now(),
                &TuiTarget::AllServTabs,
            );

            let auto_cmds = self.auto_cmds.clone();
            for auto_cmd in auto_cmds {
                self.handle_cmd_(tui, InputSrc::Server, &auto_cmd);
            }
        }

        if let Msg {
            cmd: Cmd::Reply {
                num: 002,
                ref params,
            },
            ..
        } = msg
        {
            // 002    RPL_YOURHOST
            //        "Your host is <servername>, running version <ver>"

            // An example <servername>: cherryh.freenode.net[149.56.134.238/8001]

            match parse_servername(params) {
                None => {
                    // logger.get_debug_logs().write_line(format_args!(
                    //     "{} Can't parse hostname from params: {:?}",
                    //     self.serv_addr,
                    //     params
                    // ));
                }
                Some(servername) => {
                    // logger
                    //     .get_debug_logs()
                    //     .write_line(format_args!("{} host: {}", self.serv_addr, servername));
                    self.servername = Some(servername);
                }
            }
        }

        if let Msg {
            cmd: Cmd::Reply { num: 433, .. },
            ..
        } = msg
        {
            // ERR_NICKNAMEINUSE
            self.next_nick();
            self.send_nick();
            tui.set_nick(self.get_nick().to_owned());
        }

        // Not in any of the RFCs. Also known as ERR_BANONCHAN on the internets.
        // Sent by freenode when nick change failed. See issue #29.
        if let Msg {
            cmd: Cmd::Reply {
                num: 435,
                ref params,
            },
            ..
        } = msg
        {
            if params.len() == 4 {
                // args: [old_nick, new_nick, chan, msg]
                let old_nick = &params[0];
                // make current nick 'old_nick'
                for (nick_idx, nick) in self.nicks.iter().enumerate() {
                    if nick == old_nick {
                        self.current_nick_idx = nick_idx;
                        tui.set_nick(self.get_nick().to_owned());
                        break;
                    }
                }
            }
        }

        if let Msg {
            cmd: Cmd::Reply { num: 376, .. },
            ..
        } = msg
        {
            if let Some(mut stream) = self.status.get_stream_mut() {
                // RPL_ENDOFMOTD. Join auto-join channels.
                wire::join(
                    &mut stream,
                    self.auto_join
                        .iter()
                        .map(String::as_str)
                        .collect::<Vec<&str>>()
                        .as_slice(),
                ).unwrap();

                // Set away mode
                if let Some(ref reason) = self.away_status {
                    wire::away(stream, Some(reason)).unwrap();
                }
            }
        }

        if let Msg {
            cmd: Cmd::Reply {
                num: 332,
                ref params,
            },
            ..
        } = msg
        {
            if params.len() == 2 || params.len() == 3 {
                // RPL_TOPIC. We've successfully joined a channel, add the channel to
                // self.auto_join to be able to auto-join next time we connect
                let chan = &params[params.len() - 2];
                if !self.auto_join.contains(chan) {
                    self.auto_join.push(chan.to_owned());
                }
            }
        }

        ////////////////////////////////////////////////////////////////////////

        let ts = Timestamp::now();
        let pfx = msg.pfx;

        match msg.cmd {
            Cmd::PRIVMSG { target, msg } | Cmd::NOTICE { target, msg } => {
                let pfx = match pfx {
                    Some(pfx) =>
                        pfx,
                    None => {
                        // self.logger
                        //     .get_debug_logs()
                        //     .write_line(format_args!("PRIVMSG or NOTICE without prefix \
                        //                              target: {:?} msg: {:?}", target, msg));
                        return;
                    }
                };

                let origin = match pfx {
                    Pfx::Server(_) =>
                        &self.serv_addr,
                    Pfx::User { ref nick, .. } =>
                        nick,
                };

                let (msg, is_ctcp_action) = wire::check_ctcp_action_msg(&msg);

                match target {
                    wire::MsgTarget::Chan(chan) => {
                        // self.logger
                        //     .get_chan_logs(conn.get_serv_name(), &chan)
                        //     .write_line(format_args!("PRIVMSG: {}", msg));
                        let msg_target = TuiTarget::Chan {
                            chan_name: &chan,
                        };
                        // highlight the message if it mentions us
                        if msg.find(self.get_nick()).is_some() {
                            tui.add_privmsg_highlight(
                                origin,
                                msg,
                                ts,
                                &msg_target,
                                is_ctcp_action,
                            );
                            tui.set_tab_style(TabStyle::Highlight, &msg_target);
                            let mentions_target = TuiTarget::MentionsTab;
                            tui.add_msg(
                                &format!(
                                    "{} in {}:{}: {}",
                                    origin,
                                    &self.serv_addr,
                                    chan,
                                    msg
                                ),
                                ts,
                                &mentions_target,
                            );
                            tui.set_tab_style(TabStyle::Highlight, &mentions_target);
                        } else {
                            tui.add_privmsg(origin, msg, ts, &msg_target, is_ctcp_action);
                            tui.set_tab_style(TabStyle::NewMsg, &msg_target);
                        }
                    }
                    wire::MsgTarget::User(target) => {
                        let serv_name = &self.serv_addr;
                        let msg_target = match pfx {
                            Pfx::Server(_) =>
                                TuiTarget::Server,
                            Pfx::User { ref nick, .. } if nick.eq_ignore_ascii_case("nickserv") ||
                                                          nick.eq_ignore_ascii_case("chanserv") =>
                                TuiTarget::Server,
                            Pfx::User { ref nick, .. } =>
                                TuiTarget::User { nick },
                        };
                        tui.add_privmsg(origin, msg, ts, &msg_target, is_ctcp_action);
                        if target == self.get_nick() {
                            tui.set_tab_style(TabStyle::Highlight, &msg_target);
                        } else {
                            // not sure if this case can happen
                            tui.set_tab_style(TabStyle::NewMsg, &msg_target);
                        }
                    }
                }
            }

            Cmd::JOIN { chan } =>
                match pfx {
                    Some(Pfx::User { nick, .. }) => {
                        // self.logger
                        //     .get_chan_logs(serv_name, &chan)
                        //     .write_line(format_args!("JOIN: {}", nick));
                        if nick == self.get_nick() {
                            tui.new_chan_tab(&chan);
                        } else {
                            tui.add_nick(
                                drop_nick_prefix(&nick),
                                Some(ts),
                                &TuiTarget::Chan {
                                    chan_name: &chan,
                                },
                            );
                        }
                    }
                    pfx => {
                        // self.logger
                        //     .get_debug_logs()
                        //     .write_line(format_args!("Weird JOIN message pfx {:?}", pfx));
                    }
                },

            Cmd::PART { chan, .. } =>
                match pfx {
                    Some(Pfx::User { nick, .. }) =>
                        if nick != self.get_nick() {
                            // self.logger
                            //     .get_chan_logs(serv_name, &chan)
                            //     .write_line(format_args!("PART: {}", nick));
                            tui.remove_nick(
                                &nick,
                                Some(ts),
                                &TuiTarget::Chan {
                                    chan_name: &chan,
                                },
                            );
                        },
                    pfx => {
                        // self.logger
                        //     .get_debug_logs()
                        //     .write_line(format_args!("Weird PART message pfx {:?}", pfx));
                    }
                },

            Cmd::QUIT { .. } =>
                match pfx {
                    Some(Pfx::User { ref nick, .. }) => {
                        tui.remove_nick(
                            nick,
                            Some(ts),
                            &TuiTarget::AllUserTabs { nick },
                        );
                    },
                    pfx => {
                        // self.logger
                        //     .get_debug_logs()
                        //     .write_line(format_args!("Weird QUIT message pfx {:?}", pfx));
                    }
                },

            Cmd::NICK { nick } =>
                match pfx {
                    Some(Pfx::User {
                        nick: ref old_nick, ..
                    }) => {
                        tui.rename_nick(
                            old_nick,
                            &nick,
                            ts,
                            &TuiTarget::AllUserTabs {
                                nick: old_nick,
                            },
                        );
                    },
                    pfx => {
                        // self.logger
                        //     .get_debug_logs()
                        //     .write_line(format_args!("Weird NICK message pfx {:?}", pfx));
                    }
                },

            Cmd::PING { .. } | Cmd::PONG { .. } =>
                // ignore
                {}

            Cmd::ERROR { ref msg } => {
                tui.add_err_msg(
                    msg,
                    ts,
                    &TuiTarget::AllServTabs,
                );
            }

            Cmd::TOPIC { ref chan, ref topic } => {
                tui.show_topic(
                    topic,
                    ts,
                    &TuiTarget::Chan {
                        chan_name: chan,
                    },
                );
            }

            Cmd::Reply { num: n, params } => {
                if n <= 003 /* RPL_WELCOME, RPL_YOURHOST, RPL_CREATED */
                        || n == 251 /* RPL_LUSERCLIENT */
                        || n == 255 /* RPL_LUSERME */
                        || n == 372 /* RPL_MOTD */
                        || n == 375 /* RPL_MOTDSTART */
                        || n == 376
                /* RPL_ENDOFMOTD */
                {
                    debug_assert_eq!(params.len(), 2);
                    let msg = &params[1];
                    tui.add_msg(
                        msg,
                        ts,
                        &TuiTarget::Server,
                    );
                } else if n == 4 // RPL_MYINFO
                        || n == 5 // RPL_BOUNCE
                        || (n >= 252 && n <= 254)
                /* RPL_LUSEROP, RPL_LUSERUNKNOWN, */
                /* RPL_LUSERCHANNELS */
                {
                    let msg = params.into_iter().collect::<Vec<String>>().join(" ");
                    tui.add_msg(
                        &msg,
                        ts,
                        &TuiTarget::Server,
                    );
                } else if n == 265 || n == 266 || n == 250 {
                    let msg = &params[params.len() - 1];
                    tui.add_msg(
                        msg,
                        ts,
                        &TuiTarget::Server,
                    );
                }
                // RPL_TOPIC
                else if n == 332 {
                    // FIXME: RFC 2812 says this will have 2 arguments, but freenode
                    // sends 3 arguments (extra one being our nick).
                    assert!(params.len() == 3 || params.len() == 2);
                    let chan = &params[params.len() - 2];
                    let topic = &params[params.len() - 1];
                    tui.show_topic(
                        topic,
                        ts,
                        &TuiTarget::Chan {
                            chan_name: chan,
                        },
                    );
                }
                // RPL_NAMREPLY: List of users in a channel
                else if n == 353 {
                    let chan = &params[2];
                    let chan_target = TuiTarget::Chan {
                        chan_name: chan,
                    };

                    for nick in params[3].split_whitespace() {
                        tui.add_nick(drop_nick_prefix(nick), None, &chan_target);
                    }
                }
                // RPL_ENDOFNAMES: End of NAMES list
                else if n == 366 {
                }
                // RPL_UNAWAY or RPL_NOWAWAY
                else if n == 305 || n == 306 {
                    let msg = &params[1];
                    tui.add_client_msg(
                        msg,
                        &TuiTarget::AllServTabs,
                    );
                }
                // ERR_NOSUCHNICK
                else if n == 401 {
                    let nick = &params[1];
                    let msg = &params[2];
                    tui.add_client_msg(
                        msg,
                        &TuiTarget::User {
                            nick: nick,
                        },
                    );
                // RPL_AWAY
                } else if n == 301 {
                    let nick = &params[1];
                    let msg = &params[2];
                    tui.add_client_msg(
                        &format!("{} is away: {}", nick, msg),
                        &TuiTarget::User { nick });
                } else {
                    match pfx {
                        Some(Pfx::Server(msg_serv_name)) => {
                            let msg_target = TuiTarget::Server;
                            tui.add_privmsg(
                                &msg_serv_name,
                                &params.join(" "),
                                ts,
                                &msg_target,
                                false,
                            );
                            tui.set_tab_style(TabStyle::NewMsg, &msg_target);
                        }
                        pfx => {
                            // add everything else to debug file
                            // self.logger.get_debug_logs().write_line(format_args!(
                            //     "Ignoring numeric reply msg:\nPfx: {:?}, num: {:?}, args: {:?}",
                            //     pfx,
                            //     n,
                            //     params
                            // ));
                        }
                    }
                }
            }

            Cmd::Other { cmd, params } => {
                match pfx {
                    Some(Pfx::Server(msg_serv_name)) => {
                        let msg_target = TuiTarget::Server;
                        tui.add_privmsg(
                            &msg_serv_name,
                            &params.join(" "),
                            ts,
                            &msg_target,
                            false,
                        );
                        tui.set_tab_style(TabStyle::NewMsg, &msg_target);
                    }
                    pfx => {
                        // self.logger.get_debug_logs().write_line(format_args!(
                        //     "Ignoring msg:\nPfx: {:?}, msg: {} :{}",
                        //     pfx,
                        //     cmd,
                        //     params.join(" "),
                        // ));
                    }
                }
            }
        }
    }
}

/// Try to parse servername in a 002 RPL_YOURHOST reply
fn parse_servername(params: &[String]) -> Option<String> {
    let msg = try_opt!(params.get(1).or_else(|| params.get(0)));
    let slice1 = &msg[13..];
    let servername_ends = try_opt!(
        wire::find_byte(slice1.as_bytes(), b'[')
            .or_else(|| wire::find_byte(slice1.as_bytes(), b','))
    );
    Some((&slice1[..servername_ends]).to_owned())
}

fn reconnect_err_msg(err: &StreamErr) -> String {
    format!(
        "Connection error: {}. \
         Will try to reconnect in {} seconds.",
        err.description(),
        RECONNECT_TICKS
    )
}

/// Nicks may have prefixes, indicating it is a operator, founder, or
/// something else.
/// Channel Membership Prefixes:
/// http://modern.ircdocs.horse/#channel-membership-prefixes
///
/// Returns the nick without prefix
fn drop_nick_prefix(nick: &str) -> &str {
    static PREFIXES: [char; 5] = ['~', '&', '@', '%', '+'];

    if PREFIXES.contains(&nick.chars().nth(0).unwrap()) {
        &nick[1..]
    } else {
        nick
    }
}

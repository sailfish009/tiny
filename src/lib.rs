#![cfg_attr(test, feature(test))]
#![feature(alloc_system)]
#![feature(allocator_api)]
#![feature(ascii_ctype)]
#![feature(const_fn)]
#![feature(drain_filter)]
#![feature(entry_and_modify)]
#![feature(global_allocator)]
#![feature(inclusive_range_syntax)]
#![feature(offset_to)]

extern crate alloc_system;

#[global_allocator]
static ALLOC: alloc_system::System = alloc_system::System;

#[cfg(test)]
extern crate quickcheck;

extern crate libc;
extern crate mio;
extern crate native_tls;
extern crate net2;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;
extern crate time;

extern crate term_input;
extern crate termbox_simple;

extern crate take_mut;

#[macro_use]
mod utils;

mod conn;
mod irc_backend;
mod logger;
mod stream;
mod wire;
pub mod config;
pub mod trie;
pub mod tui;

use mio::Events;
use mio::Poll;
use mio::PollOpt;
use mio::Ready;
use mio::Token;
use mio::unix::EventedFd;
use mio::unix::UnixReady;
use std::error::Error;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use conn::{Conn, ConnErr, ConnEv};
use irc_backend::IrcBackend;
use logger::Logger;
use term_input::{Event, Input};
use tui::tabbed::MsgSource;
use tui::tabbed::TabStyle;
use tui::{MsgTarget, TUIRet, Timestamp, TUI};
use tui::handle::{TuiHandle, InputSrc};
use wire::{Cmd, Msg, Pfx};

////////////////////////////////////////////////////////////////////////////////////////////////////

pub fn run() {
    let config_path = config::get_config_path();
    if !config_path.is_file() {
        config::generate_default_config();
    } else {
        match config::parse_config(config_path) {
            Err(yaml_err) => {
                println!("Can't parse config file:");
                println!("{}", yaml_err);
                ::std::process::exit(1);
            }
            Ok(config::Config {
                servers,
                defaults,
                colors,
                log_dir,
            }) => {
                let args = ::std::env::args().into_iter().collect::<Vec<String>>();
                let servers = if args.len() >= 2 {
                    // connect only to servers that match at least one of
                    // the given patterns
                    let pats = &args[1..];
                    servers
                        .into_iter()
                        .filter(|s| {
                            for pat in pats {
                                if s.addr.contains(pat) {
                                    return true;
                                }
                            }
                            false
                        })
                        .collect()
                } else {
                    servers
                };
                Tiny::run(servers, defaults, log_dir, colors)
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

struct Tiny<'poll> {
    conns: Vec<IrcBackend<'poll>>,
    defaults: config::Defaults,
    servers: Vec<config::Server>,
    tui: TUI,
    input_ev_handler: Input,
    logger: Logger,
}

const STDIN_TOKEN: Token = Token(libc::STDIN_FILENO as usize);

impl<'poll> Tiny<'poll> {
    pub fn run(
        servers: Vec<config::Server>,
        defaults: config::Defaults,
        log_dir: String,
        colors: config::Colors,
    ) {
        let poll = Poll::new().unwrap();

        poll.register(
            &EventedFd(&libc::STDIN_FILENO),
            STDIN_TOKEN,
            Ready::readable(),
            PollOpt::level(),
        ).unwrap();

        let mut conns = Vec::with_capacity(servers.len());

        let mut tui = TUI::new(colors);

        // init "mentions" tab
        tui.new_server_tab("mentions");
        tui.add_client_msg(
            "Any mentions to you will be listed here.",
            &MsgTarget::Server {
                serv_name: "mentions",
            },
        );

        tui.draw();

        for server in servers.iter().cloned() {
            let serv_name = server.addr.clone();
            match IrcBackend::init(server, &poll) {
                Ok(conn) => {
                    conns.push(conn);
                }
                Err(err) => {
                    // tui.add_err_msg(&connect_err_msg(&err), Timestamp::now(), &serv_name);
                }
            }
        }

        let mut tiny = Tiny {
            conns: conns,
            defaults: defaults,
            servers: servers,
            tui: tui,
            input_ev_handler: Input::new(),
            logger: Logger::new(PathBuf::from(log_dir)),
        };

        tiny.tui.draw();

        let mut last_tick = Instant::now();
        let mut events = Events::with_capacity(10);
        'mainloop: loop {
            // FIXME this will sometimes miss the tick deadline
            match poll.poll(&mut events, Some(Duration::from_secs(1))) {
                Err(_) => {
                    // usually SIGWINCH, which is caught by term_input
                    if tiny.handle_stdin(&poll) {
                        break 'mainloop;
                    }
                }
                Ok(_) => {
                    for event in events.iter() {
                        let token = event.token();
                        if token == STDIN_TOKEN {
                            if tiny.handle_stdin(&poll) {
                                break 'mainloop;
                            }
                        } else {
                            match find_token_conn_idx(&tiny.conns, token) {
                                None => {
                                    tiny.logger.get_debug_logs().write_line(format_args!(
                                        "BUG: Can't find Token in conns: {:?}",
                                        event.token()
                                    ));
                                }
                                Some(conn_idx) => {
                                    tiny.handle_socket(&poll, event.readiness(), conn_idx);
                                }
                            }
                        }
                    }

                    if last_tick.elapsed() >= Duration::from_secs(1) {
                        for conn_idx in 0..tiny.conns.len() {
                            let conn = &mut tiny.conns[conn_idx];
                            let mut tui_handle = TuiHandle::init(
                                conn.get_serv_name().to_owned(),
                                &mut tiny.tui.ui,
                            );
                            conn.tick(&mut tui_handle);
                        }
                        last_tick = Instant::now();
                    }
                }
            }

            tiny.tui.draw();
        }
    }

    fn handle_stdin(&mut self, poll: &'poll Poll) -> bool {
        let mut ev_buffer = Vec::with_capacity(10);
        let mut abort = false;
        self.input_ev_handler.read_input_events(&mut ev_buffer);
        for ev in ev_buffer.drain(..) {
            match self.tui.handle_input_event(ev) {
                TUIRet::Abort => {
                    abort = true;
                }
                TUIRet::Input { msg, from } => {
                    self.logger.get_debug_logs().write_line(format_args!(
                        "Input source: {:#?}, msg: {}",
                        from,
                        msg.iter().cloned().collect::<String>()
                    ));

                    let conn = match find_conn(&mut self.conns, from.serv_name()) {
                        None => {
                            continue;
                        },
                        Some(conn) =>
                            conn,
                    };
                    let mut handle = TuiHandle::init(conn.get_serv_name().to_owned(), &mut self.tui.ui);
                    let src = match from {
                        MsgSource::Serv { .. } =>
                            InputSrc::Server,
                        MsgSource::Chan { chan_name, .. } =>
                            InputSrc::Chan { chan_name },
                        MsgSource::User { nick, .. } =>
                            InputSrc::User { nick }
                    };

                    // We know msg has at least one character as the TUI won't accept it otherwise.
                    if msg[0] == '/' {
                        let msg_str: String = (&msg[1..]).into_iter().cloned().collect();
                        // self.handle_cmd(poll, from, &msg_str);
                    } else {
                        conn.send_msg(&mut handle, src, &msg.into_iter().collect::<String>());
                    }
                }
                TUIRet::KeyHandled =>
                    {}
                TUIRet::EventIgnored(Event::FocusGained)
                | TUIRet::EventIgnored(Event::FocusLost) =>
                    {}
                ev => {
                    self.logger
                        .get_debug_logs()
                        .write_line(format_args!("Ignoring event: {:?}", ev));
                }
            }
        }
        abort
    }

    fn handle_cmd(&mut self, poll: &'poll Poll, src: MsgSource, msg: &str) {
        let words: Vec<&str> = msg.split_whitespace().into_iter().collect();
        let (cmd, args) = match words.split_first() {
            Some(parts) =>
                parts,
            None => {
                return;
            }
        };

        if *cmd == "connect" && args.len() == 1 {
            self.connect(poll, words[1]);
        }

        else if *cmd == "reload" {
            match config::parse_config(config::get_config_path()) {
                Ok(config::Config { colors, .. }) =>
                    self.tui.set_colors(colors),
                Err(err) => {
                    self.tui
                        .add_client_err_msg("Can't parse config file:", &MsgTarget::CurrentTab);
                    for line in err.description().lines() {
                        self.tui.add_client_err_msg(line, &MsgTarget::CurrentTab);
                    }
                }
            }

        } else if *cmd == "msg" && args.len() >= 2 {
            // need to find index of the third word
            let mut word_indices = utils::split_whitespace_indices(msg);
            word_indices.next(); // "/msg"
            word_indices.next(); // target
            if let Some(msg_begins) = word_indices.next() {
                let target = args[0];
                let msg = &msg[msg_begins..];
                let conn_idx = find_conn_idx(&self.conns, src.serv_name());
                let source = if self.conns[conn_idx].get_serv_name() == target {
                    InputSrc::Server
                } else {
                    InputSrc::User { nick: target.to_owned() }
                };
                /*
                let source = if self.conns.iter().any(|conn| conn.get_serv_name() == target) {
                    MsgSource::Serv {
                        serv_name: target.to_owned(),
                    }
                } else {
                    let serv = src.serv_name();
                    MsgSource::User {
                        serv_name: serv.to_owned(),
                        nick: target.to_owned(),
                    }
                };
                self.send_msg(source, msg, false);
                */
            } else {
                self.tui
                    .add_client_err_msg("/msg usage: /msg target message", &MsgTarget::CurrentTab);
            }
        }
        /*
        } else if words[0] == "close" {
            match src {
                MsgSource::Serv { ref serv_name } if serv_name == "mentions" => {
                    // ignore
                }
                MsgSource::Serv { serv_name } => {
                    self.tui.close_server_tab(&serv_name);
                    let conn_idx = find_conn_idx(&self.conns, &serv_name).unwrap();
                    self.conns.remove(conn_idx);
                }
                MsgSource::Chan {
                    serv_name,
                    chan_name,
                } => {
                    self.tui.close_chan_tab(&serv_name, &chan_name);
                    self.part(&serv_name, &chan_name);
                }
                MsgSource::User { serv_name, nick } => {
                    self.tui.close_user_tab(&serv_name, &nick);
                }
            }
        } else if words[0] == "clear" {
            self.tui.clear(&src.to_target());
        } else if words[0] == "switch" && words.len() == 2 {
            self.tui.switch(words[1]);
        } else {
            self.tui.add_client_err_msg(
                &format!("Unsupported command: \"/{}\"", msg),
                &MsgTarget::CurrentTab,
            );
        }
            */
    }

    fn connect(&mut self, poll: &'poll Poll, serv_addr: &str) {
        fn split_port(s: &str) -> Option<(&str, &str)> {
            s.find(':').map(|split| (&s[0..split], &s[split + 1..]))
        }

        // parse host name and port
        let (serv_name, serv_port) = {
            match split_port(serv_addr) {
                None => {
                    self.tui
                        .add_client_err_msg("connect: Need a <host>:<port>", &MsgTarget::CurrentTab);
                    return;
                }
                Some((serv_name, serv_port)) =>
                    match serv_port.parse::<u16>() {
                        Err(err) => {
                            self.tui.add_client_err_msg(
                                &format!("connect: Can't parse port {}: {}", serv_port, err),
                                &MsgTarget::CurrentTab,
                            );
                            return;
                        }
                        Ok(serv_port) =>
                            (serv_name, serv_port),
                    },
            }
        };

        // if we already connected to this server reconnect using new port
        if let Some(conn) = find_conn(&mut self.conns, serv_name) {
            self.tui.add_client_msg(
                "Connecting...",
                &MsgTarget::AllServTabs {
                    serv_name: serv_name,
                },
            );
            match conn.reconnect(Some((serv_name, serv_port))) {
                Ok(()) =>
                    {}
                Err(err) => {
                    self.tui.add_err_msg(
                        &reconnect_err_msg(&err),
                        Timestamp::now(),
                        &MsgTarget::AllServTabs {
                            serv_name: conn.get_serv_name(),
                        },
                    );
                }
            }
            return;
        }

        // otherwise create a new connection
        // can't move the rest to an else branch because of borrowchk

        // otherwise create a new Conn, tab etc.
        self.tui.new_server_tab(serv_name);
        let msg_target = MsgTarget::Server {
            serv_name: serv_name,
        };
        self.tui.add_client_msg("Connecting...", &msg_target);

        let conn_ret = IrcBackend::init(
            config::Server {
                addr: serv_name.to_owned(),
                port: serv_port,
                tls: self.defaults.tls,
                hostname: self.defaults.hostname.clone(),
                realname: self.defaults.realname.clone(),
                nicks: self.defaults.nicks.clone(),
                auto_cmds: self.defaults.auto_cmds.clone(),
                join: self.defaults.join.clone(),
            },
            poll,
        );

        match conn_ret {
            Ok(conn) => {
                self.conns.push(conn);
            }
            Err(err) => {
                self.tui
                    .add_err_msg(&connect_err_msg(&err), Timestamp::now(), &msg_target);
            }
        }
    }

    fn handle_socket(&mut self, poll: &'poll Poll, readiness: Ready, conn_idx: usize) {
        let conn = &mut self.conns[conn_idx];
        let mut tui_handle = TuiHandle::init(
            conn.get_serv_name().to_owned(),
            &mut self.tui.ui,
        );
        if readiness.is_readable() {
            conn.read_ready(&mut tui_handle);
        }
        if readiness.is_writable() {
            conn.write_ready(&mut tui_handle);
        }
        if readiness.contains(UnixReady::hup()) {
            conn.hup(&mut tui_handle);
        }
    }
}

fn find_token_conn_idx(conns: &[IrcBackend], token: Token) -> Option<usize> {
    for (conn_idx, conn) in conns.iter().enumerate() {
        if conn.get_conn_tok() == Some(token) {
            return Some(conn_idx);
        }
    }
    None
}

fn find_conn<'a, 'poll>(
    conns: &'a mut [IrcBackend<'poll>],
    serv_name: &str,
) -> Option<&'a mut IrcBackend<'poll>> {
    match find_conn_idx(conns, serv_name) {
        None =>
            None,
        Some(idx) =>
            Some(unsafe { conns.get_unchecked_mut(idx) }),
    }
}

fn find_conn_idx(conns: &[IrcBackend], serv_name: &str) -> Option<usize> {
    for (conn_idx, conn) in conns.iter().enumerate() {
        if conn.get_serv_name() == serv_name {
            return Some(conn_idx);
        }
    }
    None
}

fn connect_err_msg(err: &ConnErr) -> String {
    format!("Connection error: {}", err.description())
}

fn reconnect_err_msg(err: &ConnErr) -> String {
    format!(
        "Connection error: {}. \
         Will try to reconnect in {} seconds.",
        err.description(),
        conn::RECONNECT_TICKS
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

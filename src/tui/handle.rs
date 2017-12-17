//! This module defines the interface between the TUI and backends.

use trie::Trie;
use tui::messaging::Timestamp;
use tui::tabbed::Tabbed;
use tui::tabbed::TabStyle;

/// A handle for backends to manipulate their TUI tabs.
pub struct TuiHandle<'tui> {
    server: String,
    ui: &'tui mut Tabbed,
}

impl<'tui> TuiHandle<'tui> {
    pub fn init(server: String, ui: &'tui mut Tabbed) -> TuiHandle<'tui> {
        TuiHandle { server, ui }
    }
}

/// Where to apply a change in the TUI.
pub enum TuiTarget<'a> {
    /// Apply to the server tab
    Server,

    /// Apply to the channel tab
    Chan {
        chan_name: &'a str,
    },

    /// Apply to the user tab
    User {
        nick: &'a str,
    },

    /// Apply to all tabs
    AllServTabs,

    /// Apply to all tabs that the nick is in
    AllUserTabs {
        nick: &'a str,
    },

    /// Apply to currently selected tab
    CurrentTab,

    /// Apply to mentions tab
    MentionsTab,
}

/// Where the input is originated from.
pub enum InputSrc {
    /// From the server tab
    Server,

    /// From a channel tab
    Chan {
        chan_name: String,
    },

    /// From a user tab
    User {
        nick: String,
    }
}

impl InputSrc {
    pub fn to_target(&self) -> TuiTarget {
        match *self {
            InputSrc::Server =>
                TuiTarget::Server,
            InputSrc::Chan { ref chan_name } =>
                TuiTarget::Chan { chan_name },
            InputSrc::User { ref nick } =>
                TuiTarget::User { nick }
        }
    }
}

impl<'tui> TuiHandle<'tui> {
    /// An error message coming from tiny, probably because of a command error
    /// etc. Those are not timestamped and not logged.
    pub fn add_client_err_msg(&mut self, msg: &str, target: &TuiTarget) {
        self.ui.add_client_err_msg(msg, &self.server, target);
    }

    /// A message from client, usually just to indidate progress, e.g.
    /// "Connecting...". Not timestamed and not logged.
    pub fn add_client_msg(&mut self, msg: &str, target: &TuiTarget) {
        self.ui.add_client_msg(msg, &self.server, target);
    }

    /// privmsg is a message coming from a server or client. Shown with sender's
    /// nick/name and receive time and logged.
    pub fn add_privmsg(
        &mut self,
        sender: &str,
        msg: &str,
        ts: Timestamp,
        target: &TuiTarget,
        ctcp_action: bool,
    ) {
        self.ui.add_privmsg(sender, msg, ts, &self.server, target, ctcp_action);
    }

    /// Similar to `add_privmsg`, except the whole message is highlighted.
    pub fn add_privmsg_highlight(
        &mut self,
        sender: &str,
        msg: &str,
        ts: Timestamp,
        target: &TuiTarget,
        ctcp_action: bool,
    ) {
        self.ui
            .add_privmsg_highlight(sender, msg, ts, &self.server, target, ctcp_action);
    }

    /// A message without any explicit sender info. Useful for e.g. in server
    /// and debug log tabs. Timestamped and logged.
    pub fn add_msg(&mut self, msg: &str, ts: Timestamp, target: &TuiTarget) {
        self.ui.add_msg(msg, ts, &self.server, target);
    }

    /// Error messages related with the protocol - e.g. can't join a channel,
    /// nickname is in use etc. Timestamped and logged.
    pub fn add_err_msg(&mut self, msg: &str, ts: Timestamp, target: &TuiTarget) {
        self.ui.add_err_msg(msg, ts, &self.server, target);
    }

    pub fn show_topic(&mut self, msg: &str, ts: Timestamp, target: &TuiTarget) {
        self.ui.show_topic(msg, ts, &self.server, target);
    }

    pub fn clear_nicks(&mut self, target: &TuiTarget) {
        self.ui.clear_nicks(&self.server, target);
    }

    pub fn add_nick(&mut self, nick: &str, ts: Option<Timestamp>, target: &TuiTarget) {
        self.ui.add_nick(nick, ts, &self.server, target);
    }

    pub fn toggle_ignore(&mut self, target: &TuiTarget) {
        self.ui.toggle_ignore(&self.server, target);
    }

    pub fn remove_nick(&mut self, nick: &str, ts: Option<Timestamp>, target: &TuiTarget) {
        self.ui.remove_nick(nick, ts, &self.server, target);
    }

    pub fn rename_nick(
        &mut self,
        old_nick: &str,
        new_nick: &str,
        ts: Timestamp,
        target: &TuiTarget,
    ) {
        self.ui.rename_nick(old_nick, new_nick, ts, &self.server, target);
    }

    pub fn clear(&mut self, target: &TuiTarget) {
        self.ui.clear(&self.server, target);
    }

    pub fn set_tab_style(&mut self, style: TabStyle, target: &TuiTarget) {
        self.ui.set_tab_style(style, &self.server, target);
    }

    pub fn set_nick(&mut self, nick: String) {
        self.ui.set_nick(nick, &self.server, &TuiTarget::AllServTabs);
    }

    pub fn new_chan_tab(&mut self, chan_name: &str) {
        self.ui.new_chan_tab(&self.server, chan_name);
    }

    pub fn get_nicks(&self, chan_name: &str) -> Option<&Trie> {
        self.ui.get_nicks(&self.server, chan_name)
    }
}

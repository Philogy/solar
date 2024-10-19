//! Source positions and related helper functions.
//!
//! Important concepts in this module include:
//!
//! - the *span*, represented by [`Span`] and related types;
//! - source code as represented by a [`SourceMap`]; and
//! - interned strings, represented by [`Symbol`]s, with some common symbols available statically in
//!   the [`sym`] module.
//!
//! ## Note
//!
//! This API is completely unstable and subject to change.

#![doc(
    html_logo_url = "https://raw.githubusercontent.com/paradigmxyz/solar/main/assets/logo.jpg",
    html_favicon_url = "https://avatars0.githubusercontent.com/u/97369466?s=256"
)]
#![cfg_attr(docsrs, feature(doc_cfg, doc_auto_cfg))]

#[macro_use]
extern crate tracing;

pub mod diagnostics;
use diagnostics::ErrorGuaranteed;

mod globals;
pub use globals::SessionGlobals;

mod pos;
pub use pos::{BytePos, CharPos, Pos, RelativeBytePos};

mod session;
pub use session::Session;

pub mod source_map;
pub use source_map::SourceMap;

mod span;
pub use span::Span;

mod symbol;
pub use symbol::{kw, sym, Ident, Symbol};

pub use anstream::ColorChoice;
pub use solar_config as config;

/// Compiler result type.
pub type Result<T = (), E = ErrorGuaranteed> = std::result::Result<T, E>;

/// Creates a new compiler session on the current thread if it doesn't exist already and then
/// executes the given closure.
pub fn enter<R>(f: impl FnOnce() -> R) -> R {
    SessionGlobals::with_or_default(|_| f())
}

/// Pluralize a word based on a count.
#[macro_export]
#[rustfmt::skip]
macro_rules! pluralize {
    // Pluralize based on count (e.g., apples)
    ($x:expr) => {
        if $x == 1 { "" } else { "s" }
    };
    ("has", $x:expr) => {
        if $x == 1 { "has" } else { "have" }
    };
    ("is", $x:expr) => {
        if $x == 1 { "is" } else { "are" }
    };
    ("was", $x:expr) => {
        if $x == 1 { "was" } else { "were" }
    };
    ("this", $x:expr) => {
        if $x == 1 { "this" } else { "these" }
    };
}

/// The current version of the Solar compiler.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

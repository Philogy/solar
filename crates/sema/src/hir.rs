use std::marker::PhantomData;
use sulk_data_structures::{index::IndexVec, newtype_index};
use sulk_interface::{Ident, Span};

pub use sulk_ast::ast::ContractKind;

/// The high-level intermediate representation (HIR).
///
/// This struct contains all the information about the ent
pub struct Hir<'hir> {
    /// All contracts.
    pub(crate) contracts: IndexVec<ContractId, Contract<'hir>>,
    /// All functions.
    pub(crate) functions: IndexVec<FunctionId, Function<'hir>>,
    /// All structs.
    pub(crate) structs: IndexVec<StructId, Struct<'hir>>,
    /// All enums.
    pub(crate) enums: IndexVec<EnumId, Enum<'hir>>,
    /// All user-defined value types.
    pub(crate) udvts: IndexVec<UdvtId, Udvt<'hir>>,
    /// All events.
    pub(crate) events: IndexVec<EventId, Event<'hir>>,
    /// All custom errors.
    pub(crate) errors: IndexVec<ErrorId, Error<'hir>>,
    /// All constants and variables.
    pub(crate) vars: IndexVec<VarId, Var<'hir>>,
}

impl<'hir> Hir<'hir> {
    pub(crate) fn new() -> Self {
        Self {
            contracts: IndexVec::new(),
            functions: IndexVec::new(),
            structs: IndexVec::new(),
            enums: IndexVec::new(),
            udvts: IndexVec::new(),
            events: IndexVec::new(),
            errors: IndexVec::new(),
            vars: IndexVec::new(),
        }
    }

    #[inline]
    pub fn contract(&self, id: ContractId) -> &Contract<'hir> {
        &self.contracts[id]
    }

    #[inline]
    pub fn function(&self, id: FunctionId) -> &Function<'hir> {
        &self.functions[id]
    }

    #[inline]
    pub fn strukt(&self, id: StructId) -> &Struct<'hir> {
        &self.structs[id]
    }

    #[inline]
    pub fn enumm(&self, id: EnumId) -> &Enum<'hir> {
        &self.enums[id]
    }

    #[inline]
    pub fn udvt(&self, id: UdvtId) -> &Udvt<'hir> {
        &self.udvts[id]
    }

    #[inline]
    pub fn event(&self, id: EventId) -> &Event<'hir> {
        &self.events[id]
    }

    #[inline]
    pub fn error(&self, id: ErrorId) -> &Error<'hir> {
        &self.errors[id]
    }

    #[inline]
    pub fn var(&self, id: VarId) -> &Var<'hir> {
        &self.vars[id]
    }
}

newtype_index! {
    /// A [`Contract`] ID.
    pub struct ContractId;

    /// A [`Function`] ID.
    pub struct FunctionId;

    /// A [`Struct`] ID.
    pub struct StructId;

    /// An [`Enum`] ID.
    pub struct EnumId;

    /// An [`Udvt`] ID.
    pub struct UdvtId;

    /// An [`Event`] ID.
    pub struct EventId;

    /// An [`Error`] ID.
    pub struct ErrorId;

    /// A [`Var`] ID.
    pub struct VarId;
}

/// A contract, interface, or library.
#[derive(Debug)]
pub struct Contract<'hir> {
    /// The function name.
    pub name: Ident,
    /// The contract kind.
    pub kind: ContractKind,
    /// The contract bases.
    pub bases: &'hir [ContractId],
    /// The constructor function.
    pub ctor: Option<FunctionId>,
    /// The `fallback` function.
    pub fallback: Option<FunctionId>,
    /// The `receive` function.
    pub receive: Option<FunctionId>,
    /// The contract items.
    pub items: &'hir [ContractItemId],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ContractItemId {
    Function(FunctionId),
    Var(VarId),
    Struct(StructId),
    Enum(EnumId),
    Udvt(UdvtId),
    Error(ErrorId),
    Event(EventId),
}

/// A function.
#[derive(Debug)]
pub struct Function<'hir> {
    /// The function name.
    pub name: Ident,
    /// The function span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

/// A struct.
#[derive(Debug)]
pub struct Struct<'hir> {
    /// The struct name.
    pub name: Ident,
    /// The struct span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

/// An enum.
#[derive(Debug)]
pub struct Enum<'hir> {
    /// The enum name.
    pub name: Ident,
    /// The enum span.
    pub span: Span,
    /// The enum variants.
    pub variants: &'hir [Ident],
}

/// A user-defined value type.
#[derive(Debug)]
pub struct Udvt<'hir> {
    /// The UDVT name.
    pub name: Ident,
    /// The UDVT span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

/// An event.
#[derive(Debug)]
pub struct Event<'hir> {
    /// The event name.
    pub name: Ident,
    /// The event span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

/// A custom error.
#[derive(Debug)]
pub struct Error<'hir> {
    /// The error name.
    pub name: Ident,
    /// The error span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

/// A constant or variable declaration.
#[derive(Debug)]
pub struct Var<'hir> {
    /// The declaration name.
    pub name: Ident,
    /// The declaration span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

/// A statement.
#[derive(Debug)]
pub struct Stmt<'hir> {
    /// The statement span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

/// An expression.
#[derive(Debug)]
pub struct Expr<'hir> {
    /// The expression span.
    pub span: Span,
    pub _tmp: PhantomData<&'hir ()>,
}

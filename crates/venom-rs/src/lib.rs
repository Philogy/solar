use alloy_primitives::U256;
use bumpalo::collections::Vec as BVec;
use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub struct IRVariable(u32);

impl IRVariable {
    pub fn new(value: u32) -> Self {
        Self(value)
    }
}

impl fmt::Display for IRVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug)]
pub struct IRLiteral(U256);

impl From<U256> for IRLiteral {
    fn from(value: U256) -> Self {
        Self(value)
    }
}

impl fmt::Display for IRLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IRLabel(u32);

impl IRLabel {
    pub fn new(value: u32) -> Self {
        Self(value)
    }
}

impl fmt::Display for IRLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Debug)]
pub enum IROperand {
    Var(IRVariable),
    Literal(IRLiteral),
    Label(IRLabel),
}

impl From<IRVariable> for IROperand {
    fn from(value: IRVariable) -> Self {
        Self::Var(value)
    }
}

impl From<U256> for IROperand {
    fn from(value: U256) -> Self {
        Self::Literal(value.into())
    }
}

impl fmt::Display for IROperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IROperand::Var(var) => var.fmt(f),
            IROperand::Literal(lit) => lit.fmt(f),
            IROperand::Label(label) => label.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct IRCall<'bump> {
    opcode: &'static str,
    operands: &'bump mut [IROperand],
}

impl fmt::Display for IRCall<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.operands.is_empty() {
            write!(f, "{}", self.opcode)
        } else {
            write!(f, "{} {}", self.opcode, self.operands.iter().join(", "))
        }
    }
}

#[derive(Debug)]
pub enum IRExpr<'bump> {
    Var(IRVariable),
    Call(IRCall<'bump>),
    Literal(IRLiteral),
}

impl fmt::Display for IRExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(var) => var.fmt(f),
            Self::Call(call) => call.fmt(f),
            Self::Literal(literal) => literal.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct IRAssignment<'bump> {
    to: IRVariable,
    expr: IRExpr<'bump>,
}

impl fmt::Display for IRAssignment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.to, self.expr)
    }
}

#[derive(Debug)]
pub enum IRStatement<'bump> {
    Call(IRCall<'bump>),
    Assignment(IRAssignment<'bump>),
}

impl fmt::Display for IRStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Call(call) => call.fmt(f),
            Self::Assignment(assign) => assign.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct IRBasicBlock<'bump> {
    label: IRLabel,
    statements: BVec<'bump, IRStatement<'bump>>,
}

const NATURAL_INDENT: &'static str = "    ";

impl IRBasicBlock<'_> {
    fn to_indented_string(&self, nested: bool) -> String {
        let mut s = if nested {
            format!("{NATURAL_INDENT}{}:\n", self.label.0)
        } else {
            format!("{}:\n", self.label.0)
        };
        let indent = if nested {
            format!("{NATURAL_INDENT}{NATURAL_INDENT}")
        } else {
            NATURAL_INDENT.to_string()
        };
        self.statements.iter().for_each(|stmt| {
            let as_string = format!("{}{}\n", indent, stmt.to_string());
            s.push_str(as_string.as_str());
        });
        s
    }
}

impl fmt::Display for IRBasicBlock<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_indented_string(false))
    }
}

#[derive(Debug)]
pub struct IRFunction<'bump> {
    label: IRLabel,
    bbs: BVec<'bump, IRBasicBlock<'bump>>,
}

impl fmt::Display for IRFunction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn {} => {{\n{}\n}}",
            self.label.0,
            self.bbs.iter().map(|bb| bb.to_indented_string(true)).join("\n")
        )
    }
}

#[derive(Debug)]
pub struct IR<'bump> {
    functions: BVec<'bump, IRFunction<'bump>>,
    data_section: BVec<'bump, IRCall<'bump>>,
}

impl fmt::Display for IR<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}\n\ndata:\n{}",
            self.functions.iter().join("\n"),
            self.data_section.iter().join("\n")
        )
    }
}

#[test]
fn build_and_print_ir() {
    use alloy_primitives::uint;
    use bumpalo::Bump;

    let arena = Bump::new();

    let callvalue = IRStatement::Assignment(IRAssignment {
        to: IRVariable(2),
        expr: IRExpr::Call(IRCall { opcode: "callvalue", operands: &mut [] }),
    });
    let sstore = IRStatement::Call(IRCall {
        opcode: "sstore",
        operands: arena.alloc([uint!(3U256).into(), IRVariable(2).into()]),
    });
    let revert = IRStatement::Call(IRCall {
        opcode: "revert",
        operands: arena.alloc([uint!(0U256).into(), uint!(32U256).into()]),
    });

    let bb = IRBasicBlock {
        label: IRLabel(1),
        statements: {
            let mut stmts = BVec::new_in(&arena);
            stmts.extend([callvalue, sstore, revert]);
            stmts
        },
    };

    let basic_fn = IRFunction {
        label: IRLabel(1),
        bbs: {
            let mut bbs = BVec::with_capacity_in(1, &arena);
            bbs.push(bb);
            bbs
        },
    };

    let ir = IR {
        functions: {
            let mut funcs = BVec::new_in(&arena);
            funcs.push(basic_fn);
            funcs
        },
        data_section: BVec::new_in(&arena),
    };

    println!("{}", ir);
}

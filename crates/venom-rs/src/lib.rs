use alloy_primitives::U256;
use bumpalo::collections::Vec as BVec;
use itertools::Itertools;
use solar_data_structures::newtype_index;
use std::fmt;

newtype_index! {
    pub struct IRVariable;
    pub struct IRLabel;
}

impl fmt::Display for IRVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.get())
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
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for IRLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.get())
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
pub struct IRCall<'arena> {
    opcode: &'static str,
    operands: &'arena mut [IROperand],
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
pub enum IRExpr<'arena> {
    Call(IRCall<'arena>),
    Literal(IRLiteral),
}

impl fmt::Display for IRExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Call(call) => call.fmt(f),
            Self::Literal(literal) => literal.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct IRAssignment<'arena> {
    to: IRVariable,
    expr: IRExpr<'arena>,
}

impl fmt::Display for IRAssignment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.to, self.expr)
    }
}

#[derive(Debug)]
pub enum IRStatement<'arena> {
    Call(IRCall<'arena>),
    Assignment(IRAssignment<'arena>),
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
pub struct IRBasicBlock<'arena> {
    label: IRLabel,
    statements: BVec<'arena, IRStatement<'arena>>,
}

const NATURAL_INDENT: &'static str = "    ";

impl IRBasicBlock<'_> {
    fn to_indented_string(&self, nested: bool) -> String {
        let mut s = if nested {
            format!("{NATURAL_INDENT}{}:\n", self.label.get())
        } else {
            format!("{}:\n", self.label.get())
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
pub struct IRFunction<'arena> {
    label: IRLabel,
    bbs: BVec<'arena, IRBasicBlock<'arena>>,
}

impl fmt::Display for IRFunction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "function {} => \n{}\n",
            self.label.get(),
            self.bbs.iter().map(|bb| bb.to_indented_string(true)).join("\n")
        )
    }
}

#[derive(Debug)]
pub struct IR<'arena> {
    functions: BVec<'arena, IRFunction<'arena>>,
    data_section: BVec<'arena, IRCall<'arena>>,
}

impl fmt::Display for IR<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}\n\n[data]\n{}",
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
        to: IRVariable::new(2),
        expr: IRExpr::Call(IRCall { opcode: "callvalue", operands: &mut [] }),
    });
    let sstore = IRStatement::Call(IRCall {
        opcode: "sstore",
        operands: arena.alloc([uint!(3U256).into(), IRVariable::new(2).into()]),
    });
    let revert = IRStatement::Call(IRCall {
        opcode: "revert",
        operands: arena.alloc([uint!(0U256).into(), uint!(32U256).into()]),
    });

    let bb = IRBasicBlock {
        label: IRLabel::new(1),
        statements: {
            let mut stmts = BVec::new_in(&arena);
            stmts.extend([callvalue, sstore, revert]);
            stmts
        },
    };

    let basic_fn = IRFunction {
        label: IRLabel::new(1),
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

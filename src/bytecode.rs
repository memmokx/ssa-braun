use std::fmt::Debug;
use crate::ir;

#[derive(Clone, Copy)]
pub enum Value {
    Reg(u16),
    Imm(i32),
    Addr(u16),
}

impl Value {
    pub fn register(&self) -> ir::VariableId {
        match self {
            Value::Reg(r) => *r as ir::VariableId,
            _ => panic!("Expected register"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Reg(r) => write!(f, "Reg({})", r),
            Value::Imm(i) => write!(f, "Imm({})", i),
            Value::Addr(a) => write!(f, "Addr({})", a),
        }
    }
}

#[derive(Clone)]
pub struct InstAdd {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Clone)]
pub struct InstSub {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Clone)]
pub struct InstMul {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}


#[derive(Clone)]
pub struct InstLoad {
    pub dst: Value,
    pub src: Value,
}

#[derive(Clone)]
pub struct InstJmp {
    pub dst: Value,
}

#[derive(Clone)]
pub struct InstJmpEq {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Clone)]
pub struct InstLt {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Clone)]
pub enum Instruction {
    InstAdd(InstAdd),
    InstSub(InstSub),
    InstMul(InstMul),
    InstLoad(InstLoad),
    InstJmp(InstJmp),
    InstJmpEq(InstJmpEq),
    InstLt(InstLt),
    InstRet
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::InstAdd(InstAdd { dst, lhs, rhs }) => {
                write!(f, "ADD {:?}, {:?}, {:?}", dst, lhs, rhs)
            }
            Instruction::InstSub(InstSub { dst, lhs, rhs }) => {
                write!(f, "SUB {:?}, {:?}, {:?}", dst, lhs, rhs)
            }
            Instruction::InstMul(InstMul { dst, lhs, rhs }) => {
                write!(f, "MUL {:?}, {:?}, {:?}", dst, lhs, rhs)
            }
            Instruction::InstLoad(InstLoad { dst, src }) => {
                write!(f, "LOAD {:?}, {:?}", dst, src)
            }
            Instruction::InstJmp(InstJmp { dst }) => {
                write!(f, "JMP {:?}", dst)
            }
            Instruction::InstJmpEq(InstJmpEq { dst, lhs, rhs }) => {
                write!(f, "JMPEQ {:?}, {:?}, {:?}", dst, lhs, rhs)
            }
            Instruction::InstLt(InstLt { dst, lhs, rhs }) => {
                write!(f, "LT {:?}, {:?}, {:?}", dst, lhs, rhs)
            }
            Instruction::InstRet => {
                write!(f, "RET")
            }
        }
    }
}

#[macro_export]
macro_rules! inst_binary_op {
    ($dst:expr, $lhs:expr, $rhs:expr, $inst:ident) => {
        Instruction::$inst($inst {
            dst: $dst,
            lhs: $lhs,
            rhs: $rhs,
        })
    }
}

#[macro_export]
macro_rules! inst_load {
    ($dst:expr, $src:expr) => {
        Instruction::InstLoad(InstLoad {
            dst: $dst,
            src: $src,
        })
    }
}
#[macro_export]
macro_rules! inst_jmp {
    ($dst:expr) => {
        Instruction::InstJmp(InstJmp {
            dst: $dst,
        })
    }
}
use std::collections::HashMap;
use std::fmt::Debug;

const NONE_VAR: Variable = Variable { id: 999999999, index: 999999999999 };

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Variable {
    pub id: VariableId,
    pub index: ValueId,
}

impl Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%v{}_{}", self.id, self.index)
    }
}

#[derive(Clone, Copy)]
pub enum Value {
    Variable(Variable),
    Immediate(i32),
}

impl Value {
    #[inline]
    pub fn variable(&self) -> Variable {
        match self {
            Value::Variable(v) => *v,
            _ => panic!("Expected variable"),
        }
    }

    pub fn immediate(&self) -> i32 {
        match self {
            Value::Immediate(i) => *i,
            _ => panic!("Expected immediate"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Variable(v) => write!(f, "{:?}", v),
            Value::Immediate(i) => write!(f, "{}", i),
        }
    }
}

pub struct IrAdd {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

pub struct IrSub {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

pub struct IrMul {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

pub struct IrLoad {
    pub dst: Value,
    pub src: Value,
}

pub struct IrLt {
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

pub struct IrPhi {
    pub var: Variable,
    pub operands: Vec<(Value, BBId)>,
}

pub enum IrInstruction {
    Add(IrAdd),
    Sub(IrSub),
    Mul(IrMul),
    Load(IrLoad),
    Lt(IrLt),
    Phi(IrPhi),
}

impl IrInstruction {
    pub fn phi_mut(&mut self) -> &mut IrPhi {
        match self {
            IrInstruction::Phi(phi) => phi,
            _ => panic!("Expected phi"),
        }
    }

    pub fn phi(&self) -> &IrPhi {
        match self {
            IrInstruction::Phi(phi) => phi,
            _ => panic!("Expected phi"),
        }
    }

    pub fn is_phi(&self) -> bool {
        match self {
            IrInstruction::Phi(_) => true,
            _ => false,
        }
    }
}

impl Debug for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrInstruction::Add(IrAdd { dst, lhs, rhs }) => {
                write!(f, "{:?} = add {:?}, {:?}", dst, lhs, rhs)
            }
            IrInstruction::Sub(IrSub { dst, lhs, rhs }) => {
                write!(f, "{:?} = sub {:?}, {:?}", dst, lhs, rhs)
            }
            IrInstruction::Mul(IrMul { dst, lhs, rhs }) => {
                write!(f, "{:?} = mul {:?}, {:?}", dst, lhs, rhs)
            }
            IrInstruction::Load(IrLoad { dst, src }) => {
                write!(f, "{:?} = load {:?}", dst, src)
            }
            IrInstruction::Lt(IrLt { dst, lhs, rhs }) => {
                write!(f, "{:?} = lt {:?}, {:?}", dst, lhs, rhs)
            }
            IrInstruction::Phi(IrPhi { var, operands }) => {
                write!(f, "{:?} = phi {:?}", var, operands)
            }
        }
    }
}

#[derive(Copy, Clone)]
pub enum Terminator {
    Unreachable,
    Branch(BBId),
    JmpEq {
        lhs: Value,
        rhs: Value,
        truthy: BBId,
        falsy: BBId,
    },
    Ret,
}

impl Debug for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Unreachable => write!(f, "unreachable"),
            Terminator::Branch(bb) => write!(f, "br: block_{:?}", bb),
            Terminator::JmpEq { lhs, rhs, truthy, falsy } => {
                write!(f, "jeq {:?}, {:?}, true: block_{:?}, false: block_{:?}", lhs, rhs, truthy, falsy)
            }
            Terminator::Ret => write!(f, "ret"),
        }
    }
}

pub struct BasicBlock {
    pub id: BBId,
    pub instructions: Vec<IrInstruction>,
    pub terminator: Terminator,
    pub predecessors: Vec<BBId>,
    pub successors: Vec<BBId>,
    pub incomplete_phis: HashMap<VariableId, InstId>,
    sealed: bool,
    filled: bool,
}

impl BasicBlock {
    pub fn new(id: BBId) -> Self {
        BasicBlock {
            id,
            instructions: Vec::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
            terminator: Terminator::Unreachable,
            incomplete_phis: HashMap::new(),
            sealed: false,
            filled: false,
        }
    }

    pub fn push_predecessor(&mut self, id: BBId) {
        self.predecessors.push(id);
    }

    pub fn push_successor(&mut self, id: BBId) {
        self.successors.push(id);
    }

    pub fn push_successors(&mut self, ids: &[BBId]) {
        self.successors.extend_from_slice(ids);
    }

    pub fn successors_iter(&self) -> impl Iterator<Item=BBId> + '_ {
        self.successors.iter().copied()
    }

    pub fn predecessors_iter(&self) -> impl Iterator<Item=BBId> + '_ {
        self.predecessors.iter().copied()
    }

    pub fn seal(&mut self) {
        self.sealed = true;
    }

    pub fn is_sealed(&self) -> bool {
        self.sealed
    }

    pub fn fill(&mut self) {
        self.filled = true;
    }

    pub fn is_filled(&self) -> bool {
        self.filled
    }

    pub fn is_terminated(&self) -> bool {
        match self.terminator {
            Terminator::Unreachable => false,
            _ => true,
        }
    }
}

impl Debug for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "block_{:?} (sealed: {}, filled: {}):     ", self.id, self.sealed, self.filled)?;
        if self.predecessors.len() > 0 {
            writeln!(f, "# Preds: {:?}", self.predecessors)?;
        } else {
            writeln!(f, "# Preds: None")?;
        }
        for inst in &self.instructions {
            writeln!(f, "    {:?}", inst)?;
        }
        writeln!(f, "    {:?}", self.terminator)
        // writeln!(f, "successors: {:?}", self.successors)?;
        // writeln!(f, "predecessors: {:?}", self.predecessors)
    }
}

pub type BBId = usize;
pub type ValueId = usize;
pub type VariableId = u32;
pub type InstId = usize;

pub struct Builder {
    pub blocks: Vec<BasicBlock>,
    current_block: BBId,
    definitions: HashMap<(VariableId, BBId), Variable>,
    variables: HashMap<VariableId, Variable>,
}

impl Builder {
    pub fn new() -> Self {
        Builder {
            blocks: vec![BasicBlock::new(0)],
            current_block: 0,
            variables: HashMap::new(),
            definitions: HashMap::new(),
        }
    }

    pub fn block_sealed(&self, bb: BBId) -> bool {
        self.blocks[bb].is_sealed()
    }

    pub fn block_predecessors(&self, bb: BBId) -> usize {
        self.blocks[bb].predecessors.len()
    }

    pub fn write_variable(&mut self, var: VariableId, bb: BBId) -> Variable {
        if let Some(v) = self.variables.get_mut(&var) {
            v.index += 1;
        } else {
            let v = Variable { id: var, index: 0 };
            self.variables.insert(var, v);
        }
        let v = self.variables[&var];
        self.write_variable_internal(var, bb, v);
        v
    }

    pub fn read_variable(&mut self, var: VariableId, bb: BBId) -> Variable {
        if let Some(v) = self.definitions.get(&(var, bb)) {
            return *v;
        }
        self.read_variable_recursive(var, bb)
    }

    fn read_variable_recursive(&mut self, var: VariableId, bb: BBId) -> Variable {
        let mut variable;
        if !self.block_sealed(bb) {
            // Incomplete CFG
            variable = self.increment_variable(var);
            let phi = self.inst_phi(variable, bb);
            self.get_block_mut(bb).incomplete_phis.insert(var, phi);
        } else if self.block_predecessors(bb) == 1 {
            // Optimize the common case of one predecessor: No phi needed
            variable = self.read_variable(var, self.get_block(bb).predecessors[0]);
        } else {
            variable = self.increment_variable(var);
            let phi = self.inst_phi(variable, bb);
            self.write_variable_internal(var, bb, variable);
            variable = self.add_phi_operands(var, bb, phi);
        }
        self.write_variable_internal(var, bb, variable);
        variable
    }


    fn add_phi_operands(&mut self, var: VariableId, bb: BBId, phi_id: InstId) -> Variable {
        let predecessors = self.get_block(bb).predecessors.clone();

        for pred in predecessors.iter() {
            let pred_var = self.read_variable(var, *pred);
            let phi = self.get_block_mut(bb).instructions[phi_id].phi_mut();
            phi.operands.push((Value::Variable(pred_var), *pred));
        }

        self.remove_trivial_phi(phi_id, bb)
    }

    fn get_phi_users(&self, var: Variable) -> Vec<(BBId, InstId)> {
        let mut users = Vec::new();

        for block in self.blocks.iter() {
            for (i, inst) in block.instructions.iter().enumerate() {
                match inst {
                    IrInstruction::Phi(phi) => {
                        for (op, _) in phi.operands.iter() {
                            if op.variable() == var {
                                users.push((block.id, i));
                                continue;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        users
    }

    fn replace_phi_uses(&mut self, old: Variable, new: Variable) {
        for block in self.blocks.iter_mut() {
            for inst in block.instructions.iter_mut() {
                match inst {
                    IrInstruction::Phi(phi) => {
                        for (op, _) in phi.operands.iter_mut() {
                            if op.variable() == old {
                                *op = Value::Variable(new);
                            }
                        }
                    }
                    IrInstruction::Add(IrAdd { lhs, rhs, .. }) |
                    IrInstruction::Mul(IrMul { lhs, rhs, .. }) |
                    IrInstruction::Sub(IrSub { lhs, rhs, .. }) |
                    IrInstruction::Lt(IrLt { lhs, rhs, .. }) => {
                        if let Value::Variable(v) = lhs {
                            if *v == old {
                                *lhs = Value::Variable(new);
                            }
                        } else if let Value::Variable(v) = rhs {
                            if *v == old {
                                *rhs = Value::Variable(new);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn remove_trivial_phi(&mut self, phi_id: InstId, bb: BBId) -> Variable {
        let mut same = NONE_VAR;
        let phi_var = self.get_block(bb).instructions[phi_id].phi().var;

        for (op, _) in self.get_block(bb).instructions[phi_id].phi().operands.iter() {
            if op.variable() == same || op.variable() == phi_var {
                continue;
            }

            if same != NONE_VAR {
                return phi_var;
            }

            same = op.variable();
        }


        if same == NONE_VAR {
            panic!("All operands are NONE_VAR");
        }

        self.replace_phi_uses(phi_var, same);
        self.get_block_mut(bb).instructions.remove(phi_id);

        // Try to recursively remove all phi users, which might have become trivial
        let phi_users = self.get_phi_users(same);
        for (block, inst) in phi_users {
            self.remove_trivial_phi(inst, block);
        }

        same
    }

    fn write_variable_internal(&mut self, var: VariableId, bb: BBId, value: Variable) {
        self.definitions.insert((var, bb), value);
    }

    fn increment_variable(&mut self, var: VariableId) -> Variable {
        if let Some(v) = self.variables.get_mut(&var) {
            v.index += 1;
            return *v;
        }

        let v = Variable { id: var, index: 0 };
        self.variables.insert(var, v);
        v
    }

    pub fn get_block(&self, id: BBId) -> &BasicBlock {
        &self.blocks[id]
    }

    pub fn get_block_mut(&mut self, id: BBId) -> &mut BasicBlock {
        &mut self.blocks[id]
    }

    pub fn set_current_block(&mut self, id: BBId) {
        self.current_block = id;
    }

    pub fn new_block(&mut self) -> BBId {
        let id: BBId = self.blocks.len();
        self.blocks.push(BasicBlock::new(id));
        id
    }

    pub fn current_block_id(&self) -> BBId {
        self.current_block
    }

    pub fn try_seal(&mut self, block: BBId, bb_to_pred: &HashMap<BBId, usize>) {
        if !bb_to_pred.contains_key(&block) {
            return;
        }

        if bb_to_pred[&block] == self.get_block(block).predecessors.len() {
            loop {
                let incomplete_phis = {
                    let block = self.get_block_mut(block);
                    if block.incomplete_phis.is_empty() {
                        break;
                    }
                    std::mem::take(&mut block.incomplete_phis)
                };

                for (k, phi) in incomplete_phis {
                    self.add_phi_operands(k, block, phi);
                }
            }

            self.get_block_mut(block).seal();
        } else {
            eprintln!("WARN: Tried to seal block {:?} with {} predecessors, but it has {}",
                      block, bb_to_pred[&block], self.get_block(block).predecessors.len());
        }
    }

    pub fn terminate_current_block(&mut self, terminator: Terminator, bb_to_pred: &HashMap<BBId, usize>) {
        let current_bb = self.current_block_id();

        match terminator {
            Terminator::Branch(target) => {
                self.get_block_mut(current_bb).push_successor(target);
                self.get_block_mut(target).push_predecessor(current_bb);

                self.try_seal(target, bb_to_pred);
            }
            Terminator::JmpEq { truthy, falsy, .. } => {
                self.get_block_mut(current_bb).push_successors(&[truthy, falsy]);
                self.get_block_mut(truthy).push_predecessor(current_bb);
                self.get_block_mut(falsy).push_predecessor(current_bb);

                self.try_seal(truthy, bb_to_pred);
                self.try_seal(falsy, bb_to_pred);
            }
            _ => {}
        }

        self.get_block_mut(current_bb).terminator = terminator;
        self.get_block_mut(current_bb).fill();

        self.try_seal(current_bb, bb_to_pred);
    }

    pub fn push_instruction(&mut self, instruction: IrInstruction) {
        self.blocks[self.current_block].instructions.push(instruction);
    }

    pub fn inst_phi(&mut self, var: Variable, bb: BBId) -> InstId {
        let i = self.blocks[bb].instructions.len();

        self.blocks[bb].instructions.push(IrInstruction::Phi(IrPhi {
            var,
            operands: Vec::new(),
        }));

        i
    }

    pub fn inst_add(&mut self, dst: Value, lhs: Value, rhs: Value) {
        self.push_instruction(IrInstruction::Add(IrAdd {
            dst,
            lhs,
            rhs,
        }));
    }

    pub fn inst_sub(&mut self, dst: Value, lhs: Value, rhs: Value) {
        self.push_instruction(IrInstruction::Sub(IrSub {
            dst,
            lhs,
            rhs,
        }));
    }

    pub fn inst_mul(&mut self, dst: Value, lhs: Value, rhs: Value) {
        self.push_instruction(IrInstruction::Mul(IrMul {
            dst,
            lhs,
            rhs,
        }));
    }

    pub fn inst_load(&mut self, dst: Value, src: Value) {
        self.push_instruction(IrInstruction::Load(IrLoad {
            dst,
            src,
        }));
    }

    pub fn inst_lt(&mut self, dst: Value, lhs: Value, rhs: Value) {
        self.push_instruction(IrInstruction::Lt(IrLt {
            dst,
            lhs,
            rhs,
        }));
    }
}


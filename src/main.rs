mod bytecode;
mod ir;

use std::collections::{HashMap};
use bytecode::*;

const DUMMY_BLOCK: ir::BBId = 999999999999;

fn as_ir_value(builder: &mut ir::Builder, value: Value) -> ir::Value {
    match value {
        Value::Reg(r) => {
            let block = builder.current_block_id();
            let variable = builder.read_variable(r as ir::VariableId, block);
            ir::Value::Variable(variable)
        }
        Value::Imm(i) => ir::Value::Immediate(i),
        Value::Addr(_) => panic!("Invalid value type"),
    }
}

pub struct Lifter {
    pub code: Vec<Instruction>,
    pub leaders: HashMap<usize, (ir::BBId, usize)>,
    pub builder: ir::Builder,
    pub pc: usize,
}

impl Lifter {
    pub fn new(code: Vec<Instruction>) -> Self {
        Self {
            code,
            leaders: HashMap::new(),
            builder: ir::Builder::new(),
            pc: 0,
        }
    }

    pub fn next(&mut self) -> &Instruction {
        let inst = &self.code[self.pc];
        self.pc += 1;
        inst
    }

    fn increment_target(&mut self, target: usize) {
        if let Some((_, predecessors)) = self.leaders.get_mut(&target) {
            *predecessors += 1;
        } else {
            self.leaders.insert(target, (DUMMY_BLOCK, 1));
        }
    }

    /// This function finds the closest leader that is less than the target and increments its predecessors.
    fn maybe_split_block(&mut self, target: usize) {
        if self.leaders.contains_key(&target) {
            return;
        }

        let mut keys: Vec<&usize> = self.leaders.keys().into_iter().collect();
        keys.sort();

        for k in keys {
            if *k > target {
                continue;
            }

            self.increment_target(target);
            break;
        }
    }

    /// This pass iterates the whole bytecode instructions in order to find the leaders of each basic block.
    /// It is necessary because the paper assumes we know how many predecessors a block has. This is not the case
    /// in a bytecode representation where you're not sure about the whole control flow until you've traversed the entirety of the instructions.
    /// So we'll collect the leaders position and how many predecessors they have.
    fn collect_leaders(&mut self) {
        self.leaders.insert(0, (DUMMY_BLOCK, 0));

        let mut pc = 0;
        while pc < self.code.len() {
            match self.code[pc] {
                Instruction::InstJmp(InstJmp { dst }) | Instruction::InstJmpEq(InstJmpEq { dst, .. }) => {
                    let target = match dst {
                        Value::Addr(a) => a as usize,
                        _ => panic!("Invalid jump target")
                    };

                    // The condition here is just in case the target reference a previous instruction and
                    // that we may need to split the block in order to insert the target block.
                    // Only solution I could think in order to avoid doing a second pass.
                    if target < pc {
                        self.maybe_split_block(target);
                    }

                    self.increment_target(target);
                    if !self.leaders.contains_key(&(pc + 1)) {
                        self.leaders.insert(pc + 1, (DUMMY_BLOCK, 1));
                    }
                }
                Instruction::InstRet => {
                    if !self.leaders.contains_key(&(pc + 1)) {
                        self.leaders.insert(pc + 1, (DUMMY_BLOCK, 1));
                    }
                }
                _ => {
                    if let Some((_, predecessors)) = self.leaders.get_mut(&(pc + 1)) {
                        *predecessors += 1;
                    }
                }
            }

            pc += 1;
        }
    }

    fn get_target_block(&mut self, target: usize, block_to_pred: &mut HashMap<ir::BBId, usize>) -> ir::BBId {
        let leader_info = self.leaders.get_mut(&target).unwrap();
        if leader_info.0 == DUMMY_BLOCK {
            let block = self.builder.new_block();
            block_to_pred.insert(block, leader_info.1);
            leader_info.0 = block;
            block
        } else {
            leader_info.0
        }
    }

    pub fn lift(&mut self) {
        self.collect_leaders();

        // Really hacky way of doing this, but everytime we encounter a new block we'll store the number of predecessors it has.
        // Which was obtained by the `collect_leaders` pass.
        let mut block_to_predecessors: HashMap<ir::BBId, usize> = HashMap::new();

        if let Some((bb, _)) = self.leaders.get_mut(&0) {
            *bb = self.builder.current_block_id();
        }

        loop {
            if self.pc >= self.code.len() {
                break;
            }

            // If we encounter a new block switch to it
            if let Some((bb, pred)) = self.leaders.get_mut(&self.pc) {
                if *bb == DUMMY_BLOCK {
                    *bb = self.builder.new_block();
                }

                block_to_predecessors.insert(*bb, *pred);

                if *bb != self.builder.current_block_id() {
                    let current_bb = self.builder.current_block_id();
                    if !self.builder.get_block(current_bb).is_terminated() {
                        self.builder.terminate_current_block(ir::Terminator::Branch(*bb), &block_to_predecessors);
                    }
                }
                self.builder.set_current_block(*bb);
            }

            let next_pc = self.pc + 1;
            let instruction = self.next();

            match *instruction {
                Instruction::InstAdd(InstAdd { dst, lhs, rhs }) => {
                    let dst = dst.register();
                    let lhs = as_ir_value(&mut self.builder, lhs);
                    let rhs = as_ir_value(&mut self.builder, rhs);

                    let dst_variable = self.builder.write_variable(dst, self.builder.current_block_id());
                    self.builder.inst_add(ir::Value::Variable(dst_variable), lhs, rhs);
                }
                Instruction::InstSub(InstSub { dst, lhs, rhs }) => {
                    let dst = dst.register();
                    let lhs = as_ir_value(&mut self.builder, lhs);
                    let rhs = as_ir_value(&mut self.builder, rhs);

                    let dst_variable = self.builder.write_variable(dst, self.builder.current_block_id());
                    self.builder.inst_sub(ir::Value::Variable(dst_variable), lhs, rhs);
                }
                Instruction::InstMul(InstMul { dst, lhs, rhs }) => {
                    let dst = dst.register();
                    let lhs = as_ir_value(&mut self.builder, lhs);
                    let rhs = as_ir_value(&mut self.builder, rhs);

                    let dst_variable = self.builder.write_variable(dst, self.builder.current_block_id());
                    self.builder.inst_mul(ir::Value::Variable(dst_variable), lhs, rhs);
                }
                Instruction::InstLoad(InstLoad { dst, src }) => {
                    let dst = dst.register();
                    let src = as_ir_value(&mut self.builder, src);

                    let dst_variable = self.builder.write_variable(dst, self.builder.current_block_id());
                    self.builder.inst_load(ir::Value::Variable(dst_variable), src);
                }
                Instruction::InstLt(InstLt { dst, lhs, rhs }) => {
                    let dst = dst.register();
                    let lhs = as_ir_value(&mut self.builder, lhs);
                    let rhs = as_ir_value(&mut self.builder, rhs);

                    let dst_variable = self.builder.write_variable(dst, self.builder.current_block_id());
                    self.builder.inst_lt(ir::Value::Variable(dst_variable), lhs, rhs);
                }
                Instruction::InstJmp(InstJmp { dst }) => {
                    let target = match dst {
                        Value::Addr(a) => a as usize,
                        _ => panic!("Invalid jump target")
                    };

                    let target_bb = self.get_target_block(target, &mut block_to_predecessors);

                    let terminator = ir::Terminator::Branch(target_bb);
                    self.builder.terminate_current_block(terminator, &block_to_predecessors);
                }
                Instruction::InstJmpEq(InstJmpEq { dst, lhs, rhs }) => {
                    let target = match dst {
                        Value::Addr(a) => a as usize,
                        _ => panic!("Invalid jump target")
                    };

                    let truthy_bb = self.get_target_block(target, &mut block_to_predecessors);
                    let falsy_bb = self.get_target_block(next_pc, &mut block_to_predecessors);

                    let lhs = as_ir_value(&mut self.builder, lhs);
                    let rhs = as_ir_value(&mut self.builder, rhs);

                    let terminator = ir::Terminator::JmpEq { lhs, rhs, truthy: truthy_bb, falsy: falsy_bb };
                    self.builder.terminate_current_block(terminator, &block_to_predecessors);
                }
                Instruction::InstRet => {
                    self.builder.terminate_current_block(ir::Terminator::Ret, &block_to_predecessors);
                }
            }
        }
    }
}

pub fn main() {
    let a = Value::Reg(0);
    let b = Value::Reg(1);
    let c = Value::Reg(2);

    let code = vec![
        // a = 10
        inst_load!(a, Value::Imm(10)),
        // b = 0
        inst_load!(b, Value::Imm(0)),
        // c = 1
        inst_load!(c, Value::Imm(1)),

        // while (a < 100)
        /* 3 */      inst_binary_op!(Value::Reg(3), a, Value::Imm(100), InstLt),
        inst_binary_op!(Value::Addr(18), Value::Reg(3), Value::Imm(0), InstJmpEq),

        // c++
        inst_binary_op!(c, c, Value::Imm(1), InstAdd),

        // if (b > 0)
        inst_binary_op!(Value::Reg(4), Value::Imm(0), b, InstLt),
        inst_binary_op!(Value::Addr(11), Value::Reg(4), Value::Imm(0), InstJmpEq),

        // a *= b - 1
        inst_binary_op!(Value::Reg(5), b, Value::Imm(1), InstSub),
        inst_binary_op!(a, a, Value::Reg(5), InstMul),
        inst_jmp!(Value::Addr(12)),

        // a += c
        inst_binary_op!(a, a, c, InstAdd),

        // b += a - c
        /* 12 */    inst_binary_op!(Value::Reg(6), a, c, InstSub),
        inst_binary_op!(b, b, Value::Reg(6), InstAdd),

        // if (c > 25)
        inst_binary_op!(Value::Reg(7), Value::Imm(25), c, InstLt),
        inst_binary_op!(Value::Addr(17), Value::Reg(7), Value::Imm(0), InstJmpEq),

        // c *= 2
        inst_binary_op!(c, c, Value::Imm(2), InstMul),

        // goto: start
        /* 17 */    inst_jmp!(Value::Addr(3)),
        Instruction::InstRet,
    ];

    let mut lifter = Lifter::new(code);
    lifter.lift();

    for block in lifter.builder.blocks.iter() {
        println!("{:?}", block);
    }
}
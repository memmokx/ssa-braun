Implementation of the SSA construction algorithm by Braun et al. (https://c9x.me/compile/bib/braun13cc.pdf)

The paper describes an algorithm for constructing an SSA Intermediate Representation that could "allow direct translation from an abstract syntax tree or bytecode" to SSA form with "no prior analysis required".
This might be true for AST's. But since the paper assumes you know how many predecessors a basic block has, you're forced to do some analysis on the bytecode beforehand.


src/main.rs contains the lifter implementation.
src/ir.rs contains the intermediate representation definitions and Builder.
src/bytecode.rs contains the bytecode instructions definitions.


I implemented simple bytecode instructions, enough to convert the following C function to bytecode:

-------------------

int a = 10;
int b = 0;
int c = 1;

while (a < 100) {
    c++;
    if (b > 0) {
        a *= b - 1;
    } else {
        a += c;
    }
    b += a - c;
    if (c > 25)
        c *= 2;
}

return;

-------------------

Which results in the following IR:

-------------------

block_0 (sealed: true, filled: true):     # Preds: None
    %v0_0 = load 10
    %v1_0 = load 0
    %v2_0 = load 1
    br: block_1

block_1 (sealed: true, filled: true):     # Preds: [0, 7]
    %v0_1 = phi [(%v0_0, 0), (%v0_4, 7)]
    %v3_0 = lt %v0_1, 100
    %v2_1 = phi [(%v2_0, 0), (%v2_5, 7)]
    %v1_1 = phi [(%v1_0, 0), (%v1_3, 7)]
    jeq %v3_0, 0, true: block_2, false: block_3

block_2 (sealed: true, filled: true):     # Preds: [1]
    ret

block_3 (sealed: true, filled: true):     # Preds: [1]
    %v2_2 = add %v2_1, 1
    %v4_0 = lt 0, %v1_1
    jeq %v4_0, 0, true: block_4, false: block_5

block_4 (sealed: true, filled: true):     # Preds: [3]
    %v0_3 = add %v0_1, %v2_2
    br: block_6

block_5 (sealed: true, filled: true):     # Preds: [3]
    %v5_0 = sub %v1_1, 1
    %v0_2 = mul %v0_1, %v5_0
    br: block_6

block_6 (sealed: true, filled: true):     # Preds: [5, 4]
    %v0_4 = phi [(%v0_2, 5), (%v0_3, 4)]
    %v6_0 = sub %v0_4, %v2_2
    %v1_3 = add %v1_1, %v6_0
    %v7_0 = lt 25, %v2_2
    jeq %v7_0, 0, true: block_7, false: block_8

block_7 (sealed: true, filled: true):     # Preds: [6, 8]
    %v2_5 = phi [(%v2_2, 6), (%v2_4, 8)]
    br: block_1

block_8 (sealed: true, filled: true):     # Preds: [6]
    %v2_4 = mul %v2_2, 2
    br: block_7

-------------------

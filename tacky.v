`define WORD	   [15:0] // Standard word length
`define OPFIELD1 [15:11] // Typical opcode field
`define OPFIELD2 [7:3] // VLIW 2nd Op location
`define REGFIELD1	 [10:8] // Typical opcode field
`define REGFIELD2	 [2:0] // VLIW 2nd Op location
`define IMM8FIELD	 [7:0] // Immediate 8 bit value location
`define HALFWORD [7:0]
`define TYPEBIT 16 // Where the register type is defined (float = 1, int = 0)
`define REGFLOAT 1
`define REGINT 0
`define REGWORD [16:0]
`define REGSIZE [7:0]
`define MEMSIZE [65535:0]
`define PCDEST 4'b1000
`define NODEST 4'b1001
`define HALT   4'b1010

// Simple VLIW ops
`define OPadd	5'b00000
`define OPsub	5'b00001
`define OPmul	5'b00010
`define OPdiv	5'b00011
`define OPnot	5'b00100
`define OPxor	5'b00101
`define OPand	5'b00110
`define OPor	5'b00111
`define OPcvt	5'b01000
`define OPr2a	5'b01001
`define OPsh	5'b01010
`define OPslt	5'b01011
`define OPa2r	5'b01100
`define OPlf	5'b01101
`define OPli	5'b01110
`define OPst	5'b01111
`define OPjr	5'b10000
`define VLIWMIN 5'b00000
`define VLIWMAX 5'b10000

// Non-VLIW
`define OPjp8  5'b10001
`define OPpre  5'b10010
`define OPsys  5'b10011
`define OPcf8  5'b10100
`define OPci8  5'b10101
`define OPjnz8 5'b10110
`define OPjz8  5'b10111
`define NONVLIWMIN 5'b10001
`define NONVLIWMAX 5'b10111

`define NOP 5'b11111
`define SQUASH 16'hFFFF

// Float Field definitions
`define	INT	signed [15:0]	// integer size
`define FLOAT	[15:0]	// half-precision float size
`define FSIGN	[15]	// sign bit
`define FEXP	[14:7]	// exponent
`define FFRAC	[6:0]	// fractional part (leading 1 implied)

// Float Constants
`define	FZERO	16'b0	  // float 0
`define F32767  16'h46ff  // closest approx to 32767, actually 32640
`define F32768  16'hc700  // -32768

// Memory definitions
`define LINE [63:0]
`define LINES [16383:0]
`define MEMDELAY 4

module processor(halt, reset, clk);
	// Input / Output
	output reg halt;
	input reset, clk;
	reg `WORD instructions [4:0];
	reg `WORD mainmem `MEMSIZE; // Main memory block

	// ****************   Reset   ****************
	always @(reset) begin
		halt = 0;
		pc = 0;
		$readmemh1(mainmem);
		instructions[0] <= 0;
		instructions[1] <= `SQUASH;
		instructions[2] <= `SQUASH;
		instructions[3] <= `SQUASH;
		instructions[4] <= `SQUASH;
	end
	// **************** End Reset ****************



	// **********   Instruction Fetch   ********** instructions[0]
	// All this phase does is grab the instruction from memory and
	// increment the PC. PC overwrites (jump instructions) are handled
	// in the Reg. Write stage.
	reg `WORD pc = 0; // Instruction location
	reg [2:0] delay; 
	wire dependent = 0;
	wire `WORD curInst, nextInst;
	always @(posedge clk) begin
		// 1) Delay pipeline if there are dependencies
		if(delay) begin
			instructions[1] <= `NOP;
			delay <= delay - 1;
		// 2) If there are no dependencies then load instruction and increment PC
		end else begin
			instructions[1] <= mainmem[pc]; // Pass the instruction down the pipeline
			pc <= pc + 1; // Increment to the next instruction
			// a) Check for dependencies
			if(dependent) begin
				delay <= 4; // Delay the pipeline 4 cycles to fix dependency
			end
		end
	end
	// ********** End Instruction Fetch **********



	// **********     Register Read     ********** instructions[1]
	reg `REGWORD regfile `REGSIZE; // Registers
	reg [7:0] prefix;
	reg `WORD accVal1, accVal2; // Accumulator values stored in the regfile
	reg `WORD regVal1, regVal2; // Register values stored in the regfile
	reg accType1, accType2; // Accumulator type (float/int)
	reg regType1, regType2; // Register type (float/int)
	reg [4:0] op1, op2; // op1 and op2 for easier access in the next ALU stage
	reg [7:0] imm8; 
	always @(posedge clk) begin
		// 1) These loads will always need to happen
		instructions[2] <= instructions[1]; // Pass instruction along
		accVal1 <= regfile[0]; // Load accumulator value
		regVal1 <= regfile[instructions[1] `REGFIELD1]; // Load register value
		accType1 <= regfile[0][`TYPEBIT]; // Load accumulator type
		regType1 <= regfile[instructions[1] `REGFIELD1][`TYPEBIT]; // Load register type
		op1 <= instructions[1] `OPFIELD1; // Load op1 code
		// 2) VLIW instructions, load second half
		if(instructions[1] `OPFIELD1 >= `VLIWMIN && instructions[1] `OPFIELD1 <= `VLIWMAX) begin 
			accVal2 <= regfile[1]; // Load accumulator value
			regVal2 <= regfile[instructions[1] `REGFIELD2]; // Load register value
			accType2 <= regfile[1][`TYPEBIT]; // Load accumulator type
			regType2 <= regfile[instructions[1] `REGFIELD2][`TYPEBIT]; // Load register type
			op2	<= instructions[1] `OPFIELD2; // Load op2 code
		// 3) Non-VLIW Instructions load immediate 8-bit value
		end else if(instructions[1] `OPFIELD1 >= `NONVLIWMIN && instructions[1] `OPFIELD1 <= `NONVLIWMAX) begin
			imm8 <= instructions[1] `IMM8FIELD;
			// a) Check if we need to push to the prefix register
			if(instructions[1] `OPFIELD1 == `OPpre) begin
				prefix <= instructions[1] `IMM8FIELD; // Update the prefix
			end
		end
	end
	// **********   End Register Read   **********



	// **********       ALU/MEM 1       ********** instructions[2]
	reg `REGWORD partialResult1, partialResult2; // First stage ALU result
	reg [4:0] dest1, dest2; // Where this result needs to go in the Register Write stage
	reg isDiv1, isDiv2; // Whether or not this instruction is a float div
	reg `WORD divTemp1, divTemp2; // Partial result stores the recip, this stores a copy of the accumulator value
	// Float computation values (temporary variables)
	wire `WORD fAdd1, fAdd2;
	wire `WORD fSub1, fSub2;
	wire `WORD fMul1, fMul2;
	wire `WORD fSh1, fSh2;
	wire `WORD f2i1, f2i2;
	wire `WORD i2f1, i2f2;
	wire `WORD fRecip1, fRecip2;
	wire fSlt1, fSlt2;
	// Float computation modules
	fadd fadd1(fAdd1, accVal1, regVal1); 
	fadd fadd2(fAdd2, accVal2, regVal2);
	fadd fsub1(fSub1, accVal1, regVal1^16'h8000);
	fadd fsub2(fSub2, accVal2, regVal2^16'h8000);
	fmul fmul1(fMul1, accVal1, regVal1);
	fmul fmul2(fMul2, accVal2, regVal2);
	fshift fshift1(fSh1, accVal1, regVal1);
	fshift fshift2(fSh1, accVal1, regVal1);
	f2i ff2i1(f2i1, regVal1);
	f2i ff2i2(f2i2, regVal2);
	i2f fi2f1(i2f1, regVal1);
	i2f fi2f2(i2f2, regVal2);
	frecip frecip1(fRecip1, regVal1);
	frecip frecip2(fRecip2, regVal2);
	fslt fslt1(fSlt1, accVal1, regVal1); 
	fslt fslt2(fSlt2, accVal2, regVal2);
	always @(posedge clk) begin
		instructions[3] <= instructions[2]; // Pass instruction along
		// 1) VLIW instructions
		if(instructions[2] `OPFIELD1 >= `VLIWMIN && instructions[2] `OPFIELD1 <= `VLIWMAX) begin 
			// a) Set the destination for op1
			if(op1 == `OPa2r || op1 == `OPlf || op1 == `OPli) begin
				dest1 <= instructions[2] `REGFIELD1; // Non-accumulator register destination
			end else if(op1 == `OPjr) begin
				dest1 <= `PCDEST; // PC destination
			end else if(op1 == `OPst) begin
				dest1 <= `NODEST; // No destination
			end else begin
				dest1 <= 0; // Accumulator desination
			end
			// b) Set destination for op2
			if(op2 == `OPa2r || op2 == `OPlf || op2 == `OPli) begin
				dest2 <= instructions[2] `REGFIELD2; // Non-accumulator register destination
			end else if(op2 == `OPjr) begin
				dest2 <= `PCDEST; // PC destination
			end else if(op2 == `OPst) begin
				dest2 <= `NODEST; // No destination
			end else begin
				dest2 <= 1; // Accumulator destination
			end
			// c) Check for float divide
			isDiv1 <= (op1 == `OPdiv && accType1) ? 1 : 0;
			isDiv2 <= (op2 == `OPdiv && accType2) ? 1 : 0;
			divTemp1 <= (op1 == `OPdiv && accType1) ? accVal1 : 0;
			divTemp2 <= (op2 == `OPdiv && accType2) ? accVal2 : 0;
			// d) Execute VLIW-1 operations
			case(op1)
				`OPnot: begin partialResult1 <= {accType1, ~regVal1}; end
				`OPxor: begin partialResult1 <= {accType1, accVal1^regVal1}; end
				`OPand: begin partialResult1 <= {accType1, accVal1&regVal1}; end
				`OPor:  begin partialResult1 <= {accType1, accVal1|regVal1}; end
				`OPa2r: begin partialResult1 <= {accType1, accVal1}; end
				`OPr2a: begin partialResult1 <= {regType1, regVal1}; end
				`OPadd: begin partialResult1 <= {accType1, accType1 ? fAdd1 : accVal1 + regVal1}; end
				`OPsub: begin partialResult1 <= {accType1, accType1 ? fSub1 : accVal1 - regVal1}; end
				`OPmul: begin partialResult1 <= {accType1, accType1 ? fMul1 : accVal1 * regVal1}; end
				`OPsh:  begin partialResult1 <= {accType1, accType1 ? fSh1  : accVal1 << regVal1}; end
				`OPcvt: begin partialResult1 <= {~regType1, regType1 ? f2i1 : i2f1}; end
				`OPdiv: begin partialResult1 <= {accType1, accType1 ? fRecip1 : accVal1 / regVal1}; end
				`OPslt: begin partialResult1 <= {16'h0000, regType1 ? fSlt1 : accVal1 < regVal1}; end
				`OPlf:  begin partialResult1 <= {1'b1, mainmem[accVal1]}; end
				`OPli:  begin partialResult1 <= {1'b0, mainmem[accVal1]}; end
				`OPjr:  begin partialResult1 <= regVal1; end
				`OPst:  begin mainmem[regVal1] <= accVal1; end
			endcase
			// e) Execute VLIW-2 operations
			case(op2)
				`OPnot: begin partialResult2 <= {accType2, ~regVal2}; end
				`OPxor: begin partialResult2 <= {accType2, accVal2^regVal2}; end
				`OPand: begin partialResult2 <= {accType2, accVal2&accVal2}; end
				`OPor:  begin partialResult2 <= {accType2, accVal2|regVal2}; end
				`OPa2r: begin partialResult2 <= {accType2, accVal2}; end
				`OPr2a: begin partialResult2 <= {regType2, regVal2}; end
				`OPadd: begin partialResult2 <= {accType2, accType2 ? fAdd1 : accVal2 + regVal2}; end
				`OPsub: begin partialResult2 <= {accType2, accType2 ? fSub1 : accVal2 - regVal2}; end
				`OPmul: begin partialResult2 <= {accType2, accType2 ? fMul1 : accVal2 * regVal2}; end
				`OPsh:  begin partialResult2 <= {accType2, accType2 ? fSh1  : accVal2 << regVal2}; end
				`OPcvt: begin partialResult2 <= {~regType2, regType2 ? f2i1 : i2f1}; end
				`OPdiv: begin partialResult2 <= {accType2, accType2 ? fRecip2 : accVal2 / regVal2}; end
				`OPslt: begin partialResult2 <= {16'h0000, regType2 ? fSlt1 : accVal2 < regVal2}; end
				`OPlf:  begin partialResult2 <= {1'b1, mainmem[accVal2]}; end
				`OPli:  begin partialResult2 <= {1'b0, mainmem[accVal2]}; end
				`OPjr:  begin partialResult2 <= regVal2; end
				`OPst:  begin mainmem[regVal2] <= accVal2; end
			endcase
		// 2) Non-VLIW Instructions
		end else if(instructions[2] `OPFIELD1 >= `NONVLIWMIN && instructions[2] `OPFIELD1 <= `NONVLIWMAX) begin
			// a) These instructions aren't float div's so make isDiv's false
			isDiv1 <= 0; 
			isDiv2 <= 0;
			// b) Set partial results and destinations
			case(op1)
				`OPcf8: begin 
					partialResult1 <= {1'b1, prefix, imm8}; 
					dest1 <= instructions[2] `REGFIELD1; end
				`OPci8: begin 
					partialResult1 <= {1'b0, prefix, imm8};
					dest1 <= instructions[2] `REGFIELD1; end 
				`OPjp8: begin 
					partialResult1 <= {prefix, imm8}; 
					dest1 <= `PCDEST; end
				`OPjz8: begin 
					partialResult1 <= {prefix, imm8}; 
					dest1 <= (regVal1 == 0) ? `PCDEST : `NODEST; end
				`OPjnz8: begin 
					partialResult1 <= {prefix, imm8}; 
					dest1 <= (regVal1 != 0) ? `PCDEST : `NODEST; end
				`OPsys: begin dest1 <= `HALT; end
			endcase
		end
	end
	// **********     End ALU/MEM 1     **********



	// **********         ALU 2         ********** instructions[3]
	// All this stage really does is check if the current operation was
	// a float divide. If it is, then it completes the operation, otherwise
	// it just pushes the data on through for writing.
	reg `REGWORD finalResult1, finalResult2;
	reg [3:0] finalDest1, finalDest2;
	wire `WORD fDiv1, fDiv2;
	fmul fdiv1(fDiv1, divTemp1, partialResult1 `WORD);
	fmul fdiv2(fDiv2, divTemp2, partialResult2 `WORD);
	always @(posedge clk) begin
		instructions[4] <= instructions[3];
		finalResult1 <= isDiv1 ? fDiv1 : partialResult1;
		finalResult2 <= isDiv2 ? fDiv2 : partialResult2;
		finalDest1 <= dest1;
		finalDest2 <= dest2;
	end
	// **********      End ALU 2        **********
		


	// **********      Reg Write        ********** instructions[4]
	always @(posedge clk) begin
		instructions[0] <= instructions[4]; // This doesn't do anything but it keeps the process consistent
		// 1) VLIW instructions
		if(instructions[4] `OPFIELD1 >= `VLIWMIN && instructions[4] `OPFIELD1 <= `VLIWMAX) begin 
			// A) Commit VLIW 1
			if(finalDest1 == `PCDEST) begin
				// a) Jump instruction, clear the pipeline 
				// Wait a tick to assure an overwrite of previous operations
				#1 pc <= finalResult1;
				#1 instructions[1] <= `SQUASH;
				#1 instructions[2] <= `SQUASH;
				#1 instructions[3] <= `SQUASH;
				#1 instructions[4] <= `SQUASH;
			end else if(finalDest1 != `NODEST) begin
				// b) Write to register destination
				regfile[finalDest1] <= finalResult1;
			end
			// B) Commit VLIW 2
			if(finalDest2 == `PCDEST) begin
				// a) Jump instruction, clear the pipeline 
				// Wait a tick to assure an overwrite of previous operations
				#1 pc <= finalResult2;
				#1 instructions[1] <= `SQUASH;
				#1 instructions[2] <= `SQUASH;
				#1 instructions[3] <= `SQUASH;
				#1 instructions[4] <= `SQUASH;
			end else if(finalDest2 != `NODEST) begin
				// b) Write to register destination
				regfile[finalDest2] <= finalResult2;
			end
		// 2) Non-VLIW Instructions
		end else if(instructions[4] `OPFIELD1 >= `NONVLIWMIN && instructions[4] `OPFIELD1 <= `NONVLIWMAX) begin
			if(finalDest1 == `PCDEST) begin
				// A) Jump instruction, clear the pipeline 
				// Wait a tick to assure an overwrite of previous operations
				#1 pc <= finalResult1;
				#1 instructions[1] <= `SQUASH;
				#1 instructions[2] <= `SQUASH;
				#1 instructions[3] <= `SQUASH;
				#1 instructions[4] <= `SQUASH;
			end else if(finalDest1 == `HALT) begin
				// B) SYS call: halt
				halt <= 1;
			end else if(finalDest1 != `NODEST) begin 
				// C) Write to register destination
				regfile[finalDest1] <= finalResult1;
			end 
		end
	end
	// **********    End Reg Write      **********
endmodule

module testbench;
	reg reset = 0;
	reg clk = 0;
	wire halted;
	processor PE(halted, reset, clk);
		initial begin
		$dumpfile;
		$dumpvars(1, PE.pc, PE.regfile[0], PE.regfile[1]);
		#10 reset = 1;
		#10 reset = 0;
		while (!halted) begin
			#10 clk = 1;
			#10 clk = 0;
		end
		$finish;
	end
endmodule

// ******************************************** Slow Mem ********************************************

// Slow Memory Module
// Created  by Henry Dietz, http://aggregate.org/hankd

module slowmem64(mfc, rdata, addr, wdata, rnotw, strobe, clk);
output reg mfc;
output reg `LINE rdata;
input `LINE addr, wdata;
input rnotw, strobe, clk;
reg [7:0] pend;
reg `LINE raddr;
reg `LINE m `LINES;

initial begin
  pend <= 0;
  // put your memory initialization code here
end

always @(posedge clk) begin
  if (strobe && rnotw) begin
    // new read request
    raddr <= addr;
    pend <= `MEMDELAY;
  end else begin
    if (strobe && !rnotw) begin
      // do write
      m[addr] <= wdata;
    end

    // pending read?
    if (pend) begin
      // write satisfies pending read
      if ((raddr == addr) && strobe && !rnotw) begin
        rdata <= wdata;
        mfc <= 1;
        pend <= 0;
      end else if (pend == 1) begin
        // finally ready
        rdata <= m[raddr];
        mfc <= 1;
        pend <= 0;
      end else begin
        pend <= pend - 1;
      end
    end else begin
      // return invalid data
      rdata <= 16'hxxxx;
      mfc <= 0;
    end
  end
end
endmodule

// ************************************************ Float ********************************************

// Floating point Verilog modules for CPE480
// Created February 19, 2019 by Henry Dietz, http://aggregate.org/hankd
// Distributed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/

// Count leading zeros, 16-bit (5-bit result) d=lead0s(s)
module lead0s(d, s);
	output wire [4:0] d;
	input wire `WORD s;
	wire [4:0] t;
	wire [7:0] s8;
	wire [3:0] s4;
	wire [1:0] s2;
	assign t[4] = 0;
	assign {t[3],s8} = ((|s[15:8]) ? {1'b0,s[15:8]} : {1'b1,s[7:0]});
	assign {t[2],s4} = ((|s8[7:4]) ? {1'b0,s8[7:4]} : {1'b1,s8[3:0]});
	assign {t[1],s2} = ((|s4[3:2]) ? {1'b0,s4[3:2]} : {1'b1,s4[1:0]});
	assign t[0] = !s2[1];
	assign d = (s ? t : 16);
endmodule

// Float set-less-than, 16-bit (1-bit result) torf=a<b
module fslt(torf, a, b);
	output wire torf;
	input wire `FLOAT a, b;
	assign torf = (a `FSIGN && !(b `FSIGN)) ||
			  (a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
			  (!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0]));
endmodule

// Floating-point addition, 16-bit r=a+b
module fadd(r, a, b);
	output wire `FLOAT r;
	input wire `FLOAT a, b;
	wire `FLOAT s;
	wire [8:0] sexp, sman, sfrac;
	wire [7:0] texp, taman, tbman;
	wire [4:0] slead;
	wire ssign, aegt, amgt, eqsgn;
	assign r = ((a == 0) ? b : ((b == 0) ? a : s));
	assign aegt = (a `FEXP > b `FEXP);
	assign texp = (aegt ? (a `FEXP) : (b `FEXP));
	assign taman = (aegt ? {1'b1, (a `FFRAC)} : ({1'b1, (a `FFRAC)} >> (texp - a `FEXP)));
	assign tbman = (aegt ? ({1'b1, (b `FFRAC)} >> (texp - b `FEXP)) : {1'b1, (b `FFRAC)});
	assign eqsgn = (a `FSIGN == b `FSIGN);
	assign amgt = (taman > tbman);
	assign sman = (eqsgn ? (taman + tbman) : (amgt ? (taman - tbman) : (tbman - taman)));
	lead0s m0(slead, {sman, 7'b0});
	assign ssign = (amgt ? (a `FSIGN) : (b `FSIGN));
	assign sfrac = sman << slead;
	assign sexp = (texp + 1) - slead;
	assign s = (sman ? (sexp ? {ssign, sexp[7:0], sfrac[7:1]} : 0) : 0);
endmodule

// Floating-point multiply, 16-bit r=a*b
module fmul(r, a, b);
	output wire `FLOAT r;
	input wire `FLOAT a, b;
	wire [15:0] m; // double the bits in a fraction, we need high bits
	wire [7:0] e;
	wire s;
	assign s = (a `FSIGN ^ b `FSIGN);
	assign m = ({1'b1, (a `FFRAC)} * {1'b1, (b `FFRAC)});
	assign e = (((a `FEXP) + (b `FEXP)) -127 + m[15]);
	assign r = (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]}));
endmodule

// Floating-point reciprocal, 16-bit r=1.0/a
// Note: requires initialized inverse fraction lookup table
module frecip(r, a);
	output wire `FLOAT r;
	input wire `FLOAT a;
	reg [6:0] look[127:0];
	initial $readmemh0(look);
	assign r `FSIGN = a `FSIGN;
	assign r `FEXP = 253 + (!(a `FFRAC)) - a `FEXP;
	assign r `FFRAC = look[a `FFRAC];
endmodule

// Floating-point shift, 16 bit
// Shift +left,-right by integer
module fshift(r, f, i);
	output wire `FLOAT r;
	input wire `FLOAT f;
	input wire `INT i;
	assign r `FFRAC = f `FFRAC;
	assign r `FSIGN = f `FSIGN;
	assign r `FEXP = (f ? (f `FEXP + i) : 0);
endmodule

// Integer to float conversion, 16 bit
module i2f(f, i);
	output wire `FLOAT f;
	input wire `INT i;
	wire [4:0] lead;
	wire `WORD pos;
	assign pos = (i[15] ? (-i) : i);
	lead0s m0(lead, pos);
	assign f `FFRAC = (i ? ({pos, 8'b0} >> (16 - lead)) : 0);
	assign f `FSIGN = i[15];
	assign f `FEXP = (i ? (128 + (14 - lead)) : 0);
endmodule

// Float to integer conversion, 16 bit
// Note: out-of-range values go to -32768 or 32767
module f2i(i, f);
	output wire `INT i;
	input wire `FLOAT f;
	wire `FLOAT ui;
	wire tiny, big;
	fslt m0(tiny, f, `F32768);
	fslt m1(big, `F32767, f);
	assign ui = {1'b1, f `FFRAC, 16'b0} >> ((128+22) - f `FEXP);
	assign i = (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui)));
endmodule
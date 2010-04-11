`define OP_AND   8'h00
`define OP_ANDI  8'h01
`define OP_NAND  8'h02
`define OP_NANDI 8'h03
`define OP_OR    8'h04
`define OP_ORI   8'h05
`define OP_NOR   8'h06
`define OP_NORI  8'h07
`define OP_XOR   8'h08
`define OP_XORI  8'h09
`define OP_EQ    8'h10
`define OP_EQI   8'h11
`define OP_NE    8'h12
`define OP_NEI   8'h13
`define OP_LT    8'h14
`define OP_LTI   8'h15
`define OP_LE    8'h16
`define OP_LEI   8'h17
`define OP_ADD   8'h20
`define OP_ADDI  8'h21
`define OP_SUB   8'h22
`define OP_SUBI  8'h23
`define OP_LW    8'h40
`define OP_SW    8'h60
`define OP_JRL   8'h80
`define OP_BEQ   8'h90
`define OP_BEQZ  8'h91
`define OP_BNE   8'h92
`define OP_BNEZ  8'h93

`define OP_RCTL  8'h50
`define OP_WCTL  8'h70

`define OP_BITS  IWORD[31:24]
`define RD_BITS  IWORD[23:20]
`define RS_BITS  IWORD[19:16]
`define RT_BITS  IWORD[15:12]
`define IM_BITS  IWORD[15:00]

`define RX_BITS  4'bXXXX

module CtlRReg(IWORD,RREGNO1,RREGNO2);
  input  wire [31:0] IWORD;
  output wire [3:0] RREGNO1,RREGNO2;
  wire reg1rd=IWORD[31]&&IWORD[28]&&IWORD[24];
  assign RREGNO1=reg1rd?`RD_BITS:`RS_BITS;
  wire reg2rt=(!IWORD[31])&&(!IWORD[30]);
  assign RREGNO2=reg2rt?`RT_BITS:`RD_BITS;
endmodule

module CtlWReg(IWORD,WREGNO,WRREG);
  input  wire [31:0] IWORD;

/*
   Put your code to generate WREGNO and WRREG here    DONE
*/
	output wire [3:0] WREGNO;
	output reg WRREG;
	
	//WRREG
	always @(IWORD) begin
		case(`OP_BITS)
		/* for alui, lw, JRL*/
		`OP_AND, `OP_NAND, `OP_OR, `OP_NOR, `OP_XOR, `OP_EQ, `OP_NE, `OP_LT, `OP_LE, `OP_ADD, `OP_SUB,
		`OP_ANDI,`OP_NANDI,`OP_ORI,`OP_NORI,`OP_XORI,`OP_EQI,`OP_NEI,`OP_LTI,`OP_LEI,`OP_ADDI,`OP_SUBI, `OP_LW, `OP_RCTL: 
			WRREG <= 1'b1;
		`OP_JRL:
			if( (IWORD && 32'hFF0000)!=32'hCC0000) // for RETI case
						WRREG <= 1'b1;
					else
						WRREG <= 1'b0;
		default:
			WRREG <= 1'b0;
		endcase
		if(IWORD[31:0] == 32'b0)  // check if nop is the instruction
			WRREG <= 1'b0;
	end
	
	//WREGNO is always IWORD[23:20]
	assign WREGNO = `RD_BITS;
	
endmodule

module CtlImm(IWORD,IMMVAL);
  parameter DBITS;
  input  wire [31:0] IWORD;

/*
   Put your code to generate IMMVAL here     DONE
*/
	output reg [(DBITS-1):0] IMMVAL;

	always @(IWORD) begin
		case(`OP_BITS)
		/* for alui, lw, sw */
		`OP_ANDI, `OP_NANDI, `OP_ORI, `OP_NORI, `OP_XORI, `OP_EQI, `OP_NEI, `OP_LTI, `OP_LEI, `OP_ADDI, `OP_SUBI, `OP_LW, `OP_SW:
			IMMVAL <= {{(16){IWORD[15]}}, IWORD[15:0]};
		/* for branches and jrl */
		`OP_JRL, `OP_BEQ, `OP_BEQZ, `OP_BNE, `OP_BNEZ:
			IMMVAL <= {{(14){IWORD[15]}}, IWORD[15:0], 2'b00};
		default:
			IMMVAL <= 32'bX;
		endcase
	end

endmodule

module CtlALUIn(IWORD,ALUIN2Z,ALUIN2I);
  input  wire [31:0] IWORD;
  output reg  ALUIN2Z,ALUIN2I;
  always @(IWORD) begin
    case(`OP_BITS)
    `OP_AND,`OP_NAND,`OP_OR,`OP_NOR,`OP_XOR,
    `OP_EQ,`OP_NE,`OP_LT,`OP_LE,
    `OP_ADD,`OP_SUB,
    `OP_BEQ,`OP_BNE:
      {ALUIN2Z,ALUIN2I}=2'bX0;
    `OP_ANDI,`OP_NANDI,`OP_ORI,`OP_NORI,`OP_XORI,
    `OP_EQI,`OP_NEI,`OP_LTI,`OP_LEI,
    `OP_ADDI,`OP_SUBI,
    `OP_LW,`OP_SW,
    `OP_JRL:
      {ALUIN2Z,ALUIN2I}=2'b01;
    `OP_BEQZ,`OP_BNEZ:
      {ALUIN2Z,ALUIN2I}=2'b11;
    default:
      {ALUIN2Z,ALUIN2I}=2'bXX;
    endcase
  end
endmodule

module CtlALU(IWORD,
              ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,
              ADDSUB,
              ALUCMP,CMPEQ,CMPLT,CMPNOT,
              ALUARI);
  input  wire [31:0] IWORD;

/*
  Put your code to generate all these ALU control signals here
*/
	//code
	// Logic result enabled if alulog is 1 (alulog)
	// Logic operation selected by aluand,aluor,aluxor
	// Logic result inverted if lognot is 1 (lognot)
	
	// Arithmetic and comparison operations use the adder/subtractor
	// It adds if addsub is 0, subtracts otherwise
	
	// Comparison result enabled if alucmp is 1
	// Comparison result is eq, lt, or both (le)
	// Comparison result inverted if cmpnot_g is 1
	
	// Enables arithmetic result (ALU result comes directly from the adder/subtractor) (aluari)
	
	
	output reg ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI;
	
	always @(IWORD) begin
 
		case(`OP_BITS)
			`OP_AND, `OP_ANDI, `OP_OR, `OP_ORI, `OP_XOR, `OP_XORI: begin
				{ALULOG,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={8'b10000000};
				case(`OP_BITS)
					`OP_AND, `OP_ANDI:
						{LOGAND,LOGOR,LOGXOR} = {3'b100};
					`OP_OR, `OP_ORI:
						{LOGAND,LOGOR,LOGXOR} = {3'b010};
					`OP_XOR, `OP_XORI:
						{LOGAND,LOGOR,LOGXOR} = {3'b001};
				default:
					{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'bXXXXXXXXXXX};
				endcase
			end
			`OP_NAND, `OP_NANDI, `OP_NOR, `OP_NORI: begin
				{ALULOG,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI,LOGXOR}={9'b110000000};
				case(`OP_BITS)
					`OP_NAND, `OP_NANDI:
						{LOGAND,LOGOR} = {2'b10};
					`OP_NOR, `OP_NORI:
						{LOGAND,LOGOR} = {2'b01};
				default:
					{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'bXXXXXXXXXXX};
				endcase
			end
			`OP_EQ, `OP_EQI, `OP_BEQ, `OP_BEQZ:
				{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'b00000011000};
			`OP_NE, `OP_NEI, `OP_BNE, `OP_BNEZ:
				{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'b00000011010};
			`OP_LT, `OP_LTI:
				{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'b00000110100};
			`OP_LE, `OP_LEI:
				{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'b00000111100};
			`OP_ADD, `OP_ADDI, `OP_JRL, `OP_LW, `OP_SW:
				{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'b00000000001};
			`OP_SUB, `OP_SUBI:
				{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'b00000100001};
			default:
				{ALULOG,LOGAND,LOGOR,LOGXOR,LOGNOT,ADDSUB,ALUCMP,CMPEQ,CMPLT,CMPNOT,ALUARI}={11'bXXXXXXXXXXX};
		endcase
	end	
	//end code

endmodule
// controls WE signal used for interrupts
// also controls read mem/write mem signals
module CtlWMem(IWORD,WMEM,RMEM);
  input  wire [31:0] IWORD;
  output reg WMEM,RMEM;
  always @(IWORD) begin
    case(`OP_BITS)
    `OP_SW:
      {WMEM,RMEM}={1'b1,1'b0};
    `OP_LW:
      {WMEM,RMEM}={1'b0,1'b1};
    `OP_AND,`OP_ANDI,`OP_NAND,`OP_NANDI,
    `OP_OR,`OP_ORI,`OP_NOR,`OP_NORI,
    `OP_XOR,`OP_XORI,
    `OP_EQ,`OP_EQI,`OP_NE,`OP_NEI,
    `OP_LT,`OP_LTI,`OP_LE,`OP_LEI,
    `OP_ADD,`OP_ADDI,`OP_SUB,`OP_SUBI,
    `OP_JRL:         
      {WMEM,RMEM}={1'b0,1'b0};
    default:
      {WMEM,RMEM}={1'b0,1'bX};
    endcase
  end
endmodule

module BraCtl(IWORD,ISBRANCH,ISJUMP,SAVEPC);
  input  wire [31:0] IWORD;

/*
  Put your code to generate ISBRANCH, ISJUMP, and SAVEPC here    DONE
*/
	output reg ISBRANCH, ISJUMP, SAVEPC;
	always @(IWORD) begin
		case(`OP_BITS)
			`OP_JRL:
				{ISBRANCH, ISJUMP, SAVEPC} <= {1'b0, 1'b1, 1'b1};
			`OP_BEQ, `OP_BEQZ, `OP_BNE, `OP_BNEZ:
				{ISBRANCH, ISJUMP, SAVEPC} <= {1'b1, 1'b0, 1'b1};
			default:
				{ISBRANCH, ISJUMP, SAVEPC} <= {1'b0, 1'b0, 1'b0};
		endcase
	end
endmodule

module Assign5(SW,KEY,LEDR,LEDG,HEX0,HEX1,HEX2,HEX3,CLOCK_50); 
  input  [9:0] SW;
  input  [3:0] KEY;
  input  CLOCK_50;
  output [9:0] LEDR;
  output [7:0] LEDG;
  output [6:0] HEX0,HEX1,HEX2,HEX3;

  wire [6:0] digit0,digit1,digit2,digit3;
  wire [9:0] ledred;
  wire [7:0] ledgreen;
  
  wire clk,lock;
/*
  Use the MegaWizard to create a PLL object called "Pll" so this can work    DONE
*/
//  Pll pll(.inclk0(CLOCK_50),.c0(clk),.locked(lock));
  assign lock = 1'b1;
  assign clk = KEY[0];
  wire [3:0] keys=KEY;
  wire [9:0] sws=SW;
  
assign ledgreen[3:0] = CTL0;
assign ledgreen[7:4] = aluin1_A[3:0];
assign ledred[3:0] = aluin2_A[3:0];
assign ledred[9:4] = PC[7:2];
//assign ledred[9:9] = intr;

//  SevenSeg ss3(.OUT(digit3),.IN(SW[9]?aluin1_A[31:28]:aluin1_A[15:12]));
//  SevenSeg ss2(.OUT(digit2),.IN(SW[9]?aluin1_A[27:24]:aluin1_A[11: 8]));
//  SevenSeg ss1(.OUT(digit1),.IN(SW[9]?aluin1_A[23:20]:aluin1_A[ 7: 4]));
//  SevenSeg ss0(.OUT(digit0),.IN(SW[9]?aluin1_A[19:16]:aluin1_A[ 3: 0]));

  parameter DBITS=32;
  
	// “init” signal active only in the first locked cycle
	reg init=1'b1;
	// stall used for stalling (prevent f->r stage FFs from getting new val, pc=pc, 'nop' r->a FF
	reg stall=1'b0;
	always @(posedge clk) if(lock) begin
		init<=1'b0;
	end
  

  // Program Counter (value used in F stage, updated using inputs from F and A stages)
  reg  [(DBITS-1):0] PC;
  // We update the PC only if clock signal is locked
  // Note that jumps and taken branches update the PC two cycles late, so the two instructions
  // that follow a jump or taken branch are executed anyway (effectively, we have two delay slots)
	
	
	// Initialize processor if init is active, e.g.
	reg [(DBITS-1):0] PCcorr_A = 32'h400;
	always @(posedge clk) if(lock) begin
		if(init)
			PC<=32'h400;
		else begin
			// Do normal PC stuff
			if(cancel_F==1'b0) begin
				if(usebtarg_A) begin
					PC<=PCBT_A;
					PCcorr_A<=PCBT_A;
				end
				else if(usejtarg_A) begin
					PC<=PCJT_A;
					PCcorr_A<=PCJT_A;
				end
				else if(stall) begin
					PC<=PC;
					PCcorr_A<=PC;
				end
				else begin
					PC<=PC4_F;
					PCcorr_A<=PC4_F;
				end
			end
		end
	end
	
	
  wire [(DBITS-1):0] PC_F=PC;
  
  assign {HEX0,HEX1,HEX2,HEX3,LEDR,LEDG}={digit0,digit1,digit2,digit3,ledred,ledgreen};
  
  wire [(DBITS-1):0] abus;
  tri  [(DBITS-1):0] dbus;
  wire               we;
// ADDED FOR ASSIGNMENT 5
	// CTL0 format: bit 0: IE bit1: OIE bit2: CM bit3: OM
	reg [3:0] CTL0 = 4'b0100;
	reg [3:0] CTL1 = 4'b0000; // Not really used yet... added to see if exceptions work

	wire [(DBITS-1):0] memaddr_M=aluout_M;

  // Address bus is driven with the memaddr value
  assign abus=memaddr_M;
  // The write-enable signal is driven with the wmem value
  assign we=wmem_M;
  // The data bus is driven by the processor only when writing
  // (When reading, dbus is driven by memory/device we read from)
  assign dbus=wmem_M?regout2_M:{DBITS{1'bz}};
  
  // This replaces the MemSys module - we now only have momory in its module, other
  // memory-mapped devices have their own modules
  Memory #(.BITS(DBITS),.RABITS(13),.MFILE("Test5.mif"))
  memory(.IADDR(PC_F),.IOUT(inst_F),
         .ABUS(abus),.DBUS(dbus),.WE(we),
         .CLK(clk),.LOCK(lock),.INIT(init));
  
  wire intr_timer;
  // DIVN is set for 25MHz, it tells the timer how many cycles need to pass
  // before it can decrement TCNT - and those decrements need to happen 1000 times per second
  // If you change the frequency, remember to change the timer's DIVN to still have TCNT
  // be decremented at 1000 times per second
  Timer #(.BITS(DBITS),.BASE(32'hFFFFF100),.DIVN(25000),.DIVB(17))
  timer(.ABUS(abus),.DBUS(dbus),.WE(we),.INTR(intr_timer),.CLK(clk),.LOCK(lock),.INIT(init),.DEBUG());

  Display #(.BITS(DBITS),.BASE(32'hFFFFF800))
  display(.ABUS(abus),.DBUS(dbus),.WE(we),.CLK(clk),.LOCK(lock),.INIT(init),
          .HEX0(digit0),.HEX1(digit1),.HEX2(digit2),.HEX3(digit3));
  
//  Leds #(.ABITS(DBITS),.DBITS(DBITS),.LBITS(10),.BASE(32'hFFFFF804))
//  ledsr(.ABUS(abus),.DBUS(dbus),.WE(we),.CLK(clk),.LOCK(lock),.INIT(init),.LED(ledred));
  
//  Leds #(.ABITS(DBITS),.DBITS(DBITS),.LBITS(8),.BASE(32'hFFFFF808))
//  ledsg(.ABUS(abus),.DBUS(dbus),.WE(we),.CLK(clk),.LOCK(lock),.INIT(init),.LED(ledgreen));
  
  wire intr_keys;
//  KeyDev #(.BITS(DBITS),.BASE(32'hFFFFF000))
//  keyDev(.ABUS(abus),.DBUS(dbus),.WE(we),.INTR(intr_keys),.CLK(clk),.LOCK(lock),.INIT(init),.KEY(keys),.DEBUG());
  
  wire intr_sws;
  // DEBN tells the SW device how many cycles must pass until a change in raw SW state is considered debounced
  // (At 25MHz, there are 250,000 cycles in 10ms; if you change the frequency, change DEBN here, too)
//  SwDev #(.BITS(DBITS),.BASE(32'hFFFFF004),.DEBN(18'd250000),.DEBB(18))
//  swDev(.ABUS(abus),.DBUS(dbus),.WE(we),.INTR(intr_sws),.CLK(clk),.LOCK(lock),.INIT(init),.SW(sws),.DEBUG());
  
  // Illegal instruction signal generated and used in R stage
  // Note how we don't signal an illegal instruction if we're going to
  // flush it because a mispredicted branch is in the A stage
  wire iinst_R=iinst_raw; ///&&(!mispred_A);  we dont have branch perdiction *****
  // Determine mode using the instruction word and current mode
  // CtlIInst simply looks at the instruction word and decides if it's legal
  // in the current mode (cmode_R, which is tied to CTL0[2])
  wire iinst_raw;
  wire cmode_R;
  assign cmode_R = CTL0[2];
  CtlIInst ctlIInst(.IWORD(inst_R),.CMODE(cmode_R),.IINST(iinst_raw));

  // We want to have an interrupt or exception if we have an illegal instruction (regardless of IE),
  // or if we have an interrupt request while interrupts are enabled (CTL0[0]) and while we are not already
  // in the process of jumping to the interrupt handler
  wire intr=iinst_R||(CTL0[0]&&/*(!inta_A)&&*/(intr_timer||intr_keys||intr_sws));
  // This is where we determine the number to put in CTL1
  // In my implementation CTLx are read/written in the A stage,
  // so I have an iinst_A signal to indicate that an instruction
  // that was detected as illegal (and was killed) is now in A stage
  wire [3:0] intn=
    iinst_A   ?4'h0:
    intr_timer?4'h1:
    intr_keys? 4'h2:
    intr_sws?  4'h3:
               4'hF;

// ADDED FOR ASSIGNMENT 5
	reg [(DBITS-1):0] IR = 32'b0; // interrupt return
	reg setPCToIR = 1'b0;
	always@ (posedge clk) if(lock) begin
		if(intr) begin
			//save correct pc to IR
			IR <= PCcorr_A;
			//disable interupts
			CTL0[0] <= 0;
			CTL0[1] = CTL0[0];
			CTL0[2] <= 1;
			CTL0[3] = CTL0[2];
			//load x200 into PC done in pc updating block
			//save cause ID into ctl1
			CTL1 <= intn;
		end
		// check if RETI
		// probably the wrong way of doing this
		else if( inst_R == 32'h80cc0000) begin
			// put IR value into PC (correct pc?)
			setPCToIR <= 1'b1;
			//enable interrupts
			// may need to do this like it's done in the slides
			CTL0[0] = CTL0[1];
			CTL0[1] <= 1'b0;
			CTL0[2] = CTL0[3];
			CTL0[3] <= 1'b0;
			// may need to clobber IR somewhere
		end
		else if(isWCTL) begin
			if(inst_A[23:20] == 0) begin
				CTL0 <= regout1_A[3:0];
			end
			else begin
				CTL1 <= regout1_A[3:0];
			end
		end
	end  
	
//	//Assign5 rctl
//	reg [(DBITS-1):0] rctlReg_M;
//	reg isRCTL_M = 0;
//	always@(posedge clk) if(lock) begin
//		if(inst_M[31:24] == `OP_RCTL) begin
//			isRCTL_M <= 1'b1;
//			if(inst_M[19:16] == 0) begin
//				rctlReg_M <= {28'b0,CTL0};
//			end
//			else begin
//				rctlReg_M <= {28'b0,CTL1};
//			end
//		end
//		else begin
//			{isRCTL_M, rctlReg_M} <= {1'b0, 32'bZ};
//		end
//	end
  
  // Register file, read in R stage, written in W stage
  RegFile  #(.DBITS(DBITS),.ABITS(4),.MFILE("Assign2R.mif"))
    regFile(.RADDR1(rregno1_R),.DOUT1(regout1_R),
            .RADDR2(rregno2_R),.DOUT2(regout2_R),
            .WADDR(wregno),.DIN(regval_W),
            .WE(wrreg),
            .CLK(clk));

  // PC+4 generated in the F stage (used in F to update PC, also used for branch and JRL insts in later stages)
  wire [(DBITS-1):0] PC4_F=PC_F+32'd4;
  // Instruction word, generated (from MemSys, see above) in the FE stage
  wire [(DBITS-1):0] inst_F;
  // Register numbers for reading generated and used in F stage
  wire [3:0] rregno1_F,rregno2_F;
  CtlRReg ctlRReg(.IWORD(inst_F),.RREGNO1(rregno1_F),.RREGNO2(rregno2_F));
  
	//generate cancel_F
	reg [1:0] flushcount;
	reg cancel_F=1'b0;
	
	always @(posedge clk) if(lock) begin
		if(init) begin
			flushcount<=2'b0;
			cancel_F<=1'b0;
		end
		else begin
			if(flushcount==2'd0) begin
				flushcount<=2'd2;
				cancel_F<=1'b0;
				stall <= 1'b0;
			end
			else begin
				flushcount<=flushcount-1'b1;
				cancel_F<=1'b1;
				stall <= 1'b0;
			end
		end
	end
  
  // Pipeline registers between F and R stages
  reg [(DBITS-1):0] inst_R=32'b0,PC4_R;
  reg [3:0] rregno1_R,rregno2_R;
  reg nopize_R;
  // Get new F->R signals each locked clock cycle
  always @(posedge clk) if(lock) begin
	if(init) begin
		inst_R<=32'b0;
		{PC4_R,rregno1_R,rregno2_R,nopize_R}<={PC4_F,rregno1_F,rregno2_F,cancel_F};
	end
	if(stall) begin
		{inst_R,PC4_R,rregno1_R,rregno2_R,nopize_R}<={inst_R,PC4_R,rregno1_R,rregno2_R,nopize_R};
	end
	else
		{inst_R,PC4_R,rregno1_R,rregno2_R,nopize_R}<={inst_F,PC4_F,rregno1_F,rregno2_F,cancel_F};
  end

  // Register outputs generated in R stage (from RegFile, see above), used in subsequent stages
  wire [(DBITS-1):0] regout1_R,regout2_R;
  // Immediate value sign-extension and *4 (if needed) is done in R and used in R (for ALU) and A (for branches)
  wire [(DBITS-1):0] immval_R;
  CtlImm #(.DBITS(DBITS)) ctlImm(.IWORD(inst_R),.IMMVAL(immval_R));

  // Control for the second ALU input value (reg, imm, or zero) is generated in R, used in R
  // aluin2i decides between register and non-register value for ALU input 2; it is used in A
  // aluin2z selects between sxt(imm) and zero for the non-reg value; it is used in R
  // Note that aluin2z matters only when aluin2i is 1
  wire aluin2z_R,aluin2i_R;
  CtlALUIn ctlALUIn(.IWORD(inst_R),.ALUIN2Z(aluin2z_R),.ALUIN2I(aluin2i_R));
  // If non-reg used as second ALU input value, it is zero or sxt(imm). We select between them in R
  // The result of this selection goes to A stage, where we select between it and regout2
  wire [(DBITS-1):0] aluimm_R=aluin2z_R?32'd0:immval_R;

  // ALU function controls generated in R, used in A stage
  // Logic result enabled if alulog is 1
  // Logic operation selected by aluand,aluor,aluxor
  // Logic result inverted if lognot is 1
  wire alulog_R,logand_R,logor_R,logxor_R,lognot_R;
  // Arithmetic and comparison operations use the adder/subtractor
  // It adds if addsub is 0, subtracts otherwise
  wire addsub_R;
  // Comparison result enabled if alucmp is 1
  // Comparison result is eq, lt, or both (le)
  // Comparison result inverted if cmpnot_g is 1
  wire alucmp_R,cmpeq_R,cmplt_R,cmpnot_R;
  // Enables arithmetic result (ALU result comes directly from the adder/subtractor)
  wire aluari_R;
  CtlALU ctlALU(.IWORD(inst_R),
                .ALULOG(alulog_R),.LOGAND(logand_R),.LOGOR(logor_R),.LOGXOR(logxor_R),.LOGNOT(lognot_R),
                .ADDSUB(addsub_R),
                .ALUCMP(alucmp_R),.CMPEQ(cmpeq_R),.CMPLT(cmplt_R),.CMPNOT(cmpnot_R),
                .ALUARI(aluari_R));

  // Branch/jump control signals generated in R, used in A stage
  wire isbranch_R,isjump_R,savepc_R;
  BraCtl braCtl(.IWORD(inst_R),.ISBRANCH(isbranch_R),.ISJUMP(isjump_R),.SAVEPC(savepc_R));

  // Memory read and write control (generated in R, used in M and W stages)
  wire wmem_R,rmem_R;
  CtlWMem ctlWMem(.IWORD(inst_R),.WMEM(wmem_R),.RMEM(rmem_R));

  // Register write control, generated in R and used in W stages
  wire [3:0] wregno_R;
  wire wrreg_R;
  CtlWReg ctlWReg(.IWORD(inst_R),.WREGNO(wregno_R),.WRREG(wrreg_R));
  
  
  	//assign5 wctl rtl
	wire isWCTL = (inst_A[31:24] == `OP_WCTL)?1'b1:1'b0;  ///TODO
	wire isRCTL = (inst_A[31:24] == `OP_RCTL)?1'b1:1'b0;
	wire [(DBITS-1):0] rctlReg = inst_A[16:16]?{28'b0, CTL1}:{28'b0, CTL0};
	
	
/*
  Put your code here for pipeline registers between R and A stages        DONE
*/
	//code
	reg [(DBITS-1):0] inst_A,PC4_A, regout1_A,regout2_A,immval_A,aluimm_A;
	reg [3:0] wregno_A;
	reg iinst_A, aluin2i_A,alulog_A,logand_A,logor_A,logxor_A,lognot_A,addsub_A,alucmp_A,cmpeq_A,cmplt_A,cmpnot_A,aluari_A,isbranch_A,isjump_A,savepc_A,wmem_A,rmem_A,wrreg_A;
	// Get new R->A signals each locked clock cycle
	always @(posedge clk) if(lock) begin
		if(nopize_R || stall) begin
			{inst_A,iinst_A, PC4_A, regout1_A,regout2_A,immval_A,aluimm_A,wregno_A,aluin2i_A,alulog_A,logand_A,logor_A,logxor_A,lognot_A,addsub_A,alucmp_A,cmpeq_A,cmplt_A,cmpnot_A,aluari_A,isbranch_A,isjump_A,savepc_A,wmem_A,rmem_A,wrreg_A}<={inst_R,iinst_R, PC4_R,regout1_R,regout2_R,immval_R,aluimm_R,wregno_R,aluin2i_R,alulog_R,logand_R,logor_R,logxor_R,lognot_R,addsub_R,alucmp_R,cmpeq_R,cmplt_R,cmpnot_R,aluari_R,1'b0,1'b0,savepc_R,1'b0,rmem_R,1'b0};
		end
		{inst_A,iinst_A, PC4_A, regout1_A,regout2_A,immval_A,aluimm_A,wregno_A,aluin2i_A,alulog_A,logand_A,logor_A,logxor_A,lognot_A,addsub_A,alucmp_A,cmpeq_A,cmplt_A,cmpnot_A,aluari_A,isbranch_A,isjump_A,savepc_A,wmem_A,rmem_A,wrreg_A}<={inst_R,iinst_R, PC4_R,regout1_R,regout2_R,immval_R,aluimm_R,wregno_R,aluin2i_R,alulog_R,logand_R,logor_R,logxor_R,lognot_R,addsub_R,alucmp_R,cmpeq_R,cmplt_R,cmpnot_R,aluari_R,isbranch_R,isjump_R,savepc_R,wmem_R,rmem_R,wrreg_R};
	end
	//end code

/*
  The code given below is the ALU.
  If you do cannot figure out how to generate control signals for it,
  you can make your own ALU and generate control signals for that.
*/

  // ALU input values (generated and used in A stage)
  wire [(DBITS-1):0] aluin1_A=regout1_A;
  wire [(DBITS-1):0] aluin2_A=aluin2i_A?aluimm_A:regout2_A;
  // The ALU generates its result in the A stage, it is used in A, M,and W stages
  // Generate logic result (or zero if  select it or ignore it)
  wire [(DBITS-1):0] andout_A={DBITS{logand_A}}&(aluin1_A&aluin2_A);
  wire [(DBITS-1):0] orout_A ={DBITS{logor_A }}&(aluin1_A|aluin2_A);
  wire [(DBITS-1):0] xorout_A={DBITS{logxor_A}}&(aluin1_A^aluin2_A);
  wire [(DBITS-1):0] logtmp_A=(andout_A|orout_A|xorout_A);
  wire [(DBITS-1):0] logres_A=lognot_A?(~logtmp_A):logtmp_A;
  wire [(DBITS-1):0] logout_A=logres_A&{DBITS{alulog_A}};
  // This is our adder/subtractor (if subtract, invert 2nd input, add 1)
  wire [(DBITS-1):0] arires_A=
    aluin1_A+(addsub_A?(~aluin2_A):aluin2_A)+addsub_A;
  wire [(DBITS-1):0] ariout_A=arires_A&{DBITS{aluari_A}};
  // Generate comparison result (lt uses subtration result, so addsub must be 1 for lt or lte)
  wire eqout_A=cmpeq_A&&(aluin1_A==aluin2_A);
  wire ltout_A=cmplt_A&&arires_A[31];
  wire cmptmp_A=eqout_A||ltout_A;
  wire cmpres_A=cmpnot_A?(!cmptmp_A):cmptmp_A;
  wire [(DBITS-1):0] cmpout_A={{(DBITS-1){1'b0}},cmpres_A&&alucmp_A};
  // Final ALU result
  wire [(DBITS-1):0] aluout_A=logout_A|ariout_A|cmpout_A;
  
/*
  Put your code here to generate these signals:
  
  PCBT_A (target address for a branch, will be used only if this is a taken branch)
  PCJT_A (target address for a jump, will be used only if this is a jump)
  usebtarg_A (one-bit signal that is set to 1 if this is a taken branch)                  DONE
  usejtarg_A (one-bit signal that is set to 1 if this is a jump)
*/
	//code
	wire [(DBITS-1):0] PCBT_A=PC4_A+immval_A;
	wire [(DBITS-1):0] PCJT_A=regout1_A+immval_A;
	wire usebtarg_A=isbranch_A&aluout_A?1'b1:1'b0;
	wire usejtarg_A=isjump_A?1'b1:1'b0;
	//end code
  
/*
  Put your code here for pipeline registers between A and M stages                   DONE
*/
	//code
	reg [(DBITS-1):0] inst_M,PC4_M, aluout_M, regout2_M;
	reg [3:0] wregno_M;
	reg wrreg_M,rmem_M, savepc_M, wmem_M;
	// Get new A->M signals each locked clock cycle
	always @(posedge clk) if(lock) begin
		{inst_M,wregno_M,wrreg_M,rmem_M, wmem_M, savepc_M, PC4_M, aluout_M, regout2_M}<={inst_A,wregno_A,wrreg_A,rmem_A, wmem_A, savepc_A, PC4_A, aluout_A, regout2_A};
	end
	//end code

  // Memory output generated in M, used in W
  wire [(DBITS-1):0] memout_M = dbus;			// Assign5
  // For non-memory results, choose between ALU result and PC+4
  wire [(DBITS-1):0] nonmem_M=savepc_M?PC4_M:aluout_M;
  
/*
  Put your code here for pipeline registers between M and W stages                            DONE
*/
	//code
	reg [(DBITS-1):0] memout_W, nonmem_W;
	reg [3:0] wregno_W;
	reg wrreg_W, rmem_W;
	// Get new M->W signals each locked clock cycle
	always @(posedge clk) if(lock) begin
		{memout_W,nonmem_W,wregno_W,wrreg_W,rmem_W}<={memout_M,nonmem_M,wregno_M,wrreg_M,rmem_M};
	end
	//end code

/*
  Put your code here for the W stage which puts the final value to write to the
  destination register (if any) into regval_W. Note that the code that connects                 DONE
  register write signals to the register file is already there in the instantiation
  of the RegFile module (see above).
*/
	wire [(DBITS-1):0] regval_W = isRCTL?rctlReg:rmem_W?memout_W:nonmem_W;
	wire [3:0] wregno = isRCTL?wregno_A:wregno_W;
	wire wrreg = isRCTL?1'b1:wrreg_W;
 
endmodule

module RegFile(RADDR1,DOUT1,RADDR2,DOUT2,WADDR,DIN,WE,CLK);
  parameter DBITS; // Number of data bits
  parameter ABITS; // Number of address bits
  parameter WORDS = (1<<ABITS);
  parameter MFILE = "";
  (* ram_init_file = MFILE *)
  reg [(DBITS-1):0] mem[(WORDS-1):0];
  input  [(ABITS-1):0] RADDR1,RADDR2,WADDR;
  input  [(DBITS-1):0] DIN;
  output wire [(DBITS-1):0] DOUT1,DOUT2;
  input CLK,WE;
  always @(posedge CLK)
    if(WE)
      mem[WADDR]=DIN;
  assign DOUT1=(WE&&(WADDR==RADDR1))?DIN:mem[RADDR1];
  assign DOUT2=(WE&&(WADDR==RADDR2))?DIN:mem[RADDR2];
endmodule

module MemSys(IAddr,IOut,DAddr,DOut,DIn,DWE,CLK,KEY,SW,HEX0,HEX1,HEX2,HEX3,temp);
  // File to initialize memory with
  parameter MFILE;
  // Number of bits in a memory word
  parameter DBITS;
  // Number of bits in the physical address
  parameter PABITS=DBITS;
  // Number of bits in the real address
  parameter RABITS;
  parameter RWORDS=(1<<(RABITS-2));
  input wire  [(PABITS-1):0] IAddr,DAddr;
  input wire  [(DBITS-1):0] DIn;
  input wire  DWE,CLK;
  output wire [(DBITS-1):0] IOut,DOut;
  input  wire [3:0] KEY;
  input  wire [9:0] SW;
  output wire [6:0] HEX0,HEX1,HEX2,HEX3;
  input wire [31:0] temp;
  // Real memory
  (* ram_init_file = MFILE *) (* ramstyle="no_rw_check" *)
  reg [(DBITS-1):0] marray[RWORDS];
  // Hex display
  // Note how you can use SW[9] to see the upper 16 bits of
  // the 32-bit value written to the hex display I/O address
  reg [31:0] HexOut=31'h6789ABCD;
  SevenSeg ss3(.OUT(HEX3),.IN(SW[9]?HexOut[31:28]:HexOut[15:12]));
  SevenSeg ss2(.OUT(HEX2),.IN(SW[9]?HexOut[27:24]:HexOut[11: 8]));
  SevenSeg ss1(.OUT(HEX1),.IN(SW[9]?HexOut[23:20]:HexOut[ 7: 4]));
  SevenSeg ss0(.OUT(HEX0),.IN(SW[9]?HexOut[19:16]:HexOut[ 3: 0]));
  assign DOut=
    (DAddr[(PABITS-1):RABITS]=={(PABITS-RABITS){1'b0}})?marray[DAddr[(RABITS-1):2]]:
    (DAddr==32'hFFFFF000)?{28'b0,KEY/*SW[3:0]*/}:
    (DAddr==32'hFFFFF004)?{22'b0,SW}:
    32'hXXXXXXXX;
  assign IOut=
    (IAddr[(PABITS-1):RABITS]=={(PABITS-RABITS){1'b0}})?marray[IAddr[(RABITS-1):2]]:
    32'hXXXXXXXX;
  always @(posedge CLK) begin
    if(DWE) begin
      if(DAddr[(PABITS-1):RABITS]=={(PABITS-RABITS){1'b0}})
        marray[DAddr[(RABITS-1):2]]=DIn;
      else if(DAddr==32'hFFFFF800)
        HexOut<=DIn[15:0];
    end
  end
endmodule

module SevenSeg(OUT, IN);
	output [6:0] OUT;
	input  [3:0] IN;
	
	assign OUT =
		(IN == 0)?7'b1000000:
		(IN == 1)?7'b1111001:
		(IN == 2)?7'b0100100:
		(IN == 3)?7'b0110000:
		(IN == 4)?7'b0011001:
		(IN == 5)?7'b0010010:
		(IN == 6)?7'b0000010:
		(IN == 7)?7'b1111000:
		(IN == 8)?7'b0000000:
		(IN == 9)?7'b0011000:
		(IN ==10)?7'b0001000:// A
		(IN ==11)?7'b0000011:// B
		(IN ==12)?7'b1000110:// C
		(IN ==13)?7'b0100001:// D
		(IN ==14)?7'b0000110:// E
		(IN ==15)?7'b0001110:// F
		           7'b1110111;
endmodule

// This is our new memory module
module Memory(IADDR,IOUT,ABUS,DBUS,WE,CLK,LOCK,INIT);
  // File to initialize memory with
  parameter MFILE;
  // Number of bits in a memory word
  parameter BITS;
  // Number of bits in the real address
  parameter RABITS;  
  parameter RWORDS=(1<<(RABITS-2));
  input wire [(BITS-1):0] IADDR,ABUS;
  output wire [(BITS-1):0] IOUT;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,INIT;
  // does address refer to actual memory
  wire selMem=(ABUS[(BITS-1):RABITS]=={(BITS-RABITS){1'b0}});
  // Will we write a memory location?
  wire wrMem=WE&&selMem;
  // Should we drive a value from memory to the bus?
  wire rdMem=(!WE)&&selMem;
  // Real memory
  (* ram_init_file = MFILE *) (* ramstyle="no_rw_check" *)
  reg [(BITS-1):0] marray[RWORDS];
  always @(posedge CLK) if(LOCK) begin
    if(INIT) begin
    end else begin
      if(wrMem)
        marray[ABUS[(RABITS-1):2]]<=DBUS;
    end
  end
  // This should be familiar
  assign DBUS=rdMem?marray[ABUS[(RABITS-1):2]]:
                    {BITS{1'bz}};
  // Instructions have their own read port that does not go to the data bus
  assign IOUT=
    (IADDR[(BITS-1):RABITS]=={(BITS-RABITS){1'b0}})?
    marray[IADDR[(RABITS-1):2]]:
    32'hDEADBEEF;
endmodule

// Timer device
module Timer(ABUS,DBUS,WE,INTR,CLK,LOCK,INIT,DEBUG);
  parameter BITS;
  parameter BASE;
  parameter DIVN;
  parameter DIVB;
  input wire [(BITS-1):0] ABUS;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,INIT;
  output wire INTR;
  // This can be used for debugging, just connect the DEBUG
  // output from this device to LEDG :)
  // Nut note how it remains unconnected in the instantiation above!
  output wire [7:0] DEBUG={TCNT[13:10],OR,AR,IP,IE};
  // This is our TCTL, but I chose to keep it as separate bits
  reg OR,AR,IP,IE;
  // TINI and TCNT registers
  reg [(BITS-1):0] TINI,TCNT;
  // Does the address refer to our TCTL or TCNT/TINI registers?
  wire selCtl=(ABUS==BASE);
  wire selCnt=(ABUS==BASE+3'h4);
  // Write and read enable signals for each of these
  wire wrCtl=WE&&selCtl;
  wire wrCnt=WE&&selCnt;
  wire rdCtl=(!WE)&&selCtl;
  wire rdCnt=(!WE)&&selCnt;
  always @(posedge CLK) if (LOCK) begin
  	if(INIT) begin
  		{OR,IE,IP}<=3'b000;
  		TINI<=0;
  		TCNT<=0;
  	end else if(wrCtl) begin
  			{OR,AR,IP,IE}<=DBUS[3:0]; // from slides? right ordering?
  		end
  		
  		if(wrCnt) begin
  			TINI<=DBUS[(BITS-1):0];
  			TCNT<=DBUS[(BITS-1):0];
  		end
  		// When TCNT reaches zero (from slides).
  		// if IP already 1, set OR to 1
  		// Set IP to 1 (if IE, raise interrupt signal for timer device)
  		// if AR is 1, TCNT gets value from TINI, counts again
  		// if AR is 0, TCNT remains zero until next write to TINI
  		if(TCNT == 0) begin
  			// make sure ordering is right
  			if(IP)
  				OR<=1'b1;
  			if(IE) begin
  				IP<=1'b1; // not sure about the If statement for IE
  			end
  			if(AR) begin
  				TCNT<=TINI; // counts again?
  			end
  			if(AR==1'b0)
  				TCNT<=0;
  		end
  
  end
  
  /*
    Your timer code goes here!
  */
  // Drive the bus only if processor is reading one of the two addresses
  // this device is responsible for
  assign DBUS=rdCtl?{{(BITS-4){1'b0}},OR,AR,IP,IE}:
              rdCnt?TCNT:
                    {BITS{1'bz}};
  // Ask for interrupt only if interrupt pending (IP) and enabled (IE)
  assign INTR=IP&&IE;
endmodule

// This is the display device
module Display(ABUS,DBUS,WE,CLK,LOCK,INIT,HEX0,HEX1,HEX2,HEX3);
  parameter BITS;
  parameter BASE;
  input wire [(BITS-1):0] ABUS;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,INIT;
  output wire [6:0] HEX0,HEX1,HEX2,HEX3;
  reg [15:0] HexVal;
  SevenSeg ss3(.OUT(HEX3),.IN(HexVal[15:12]));
  SevenSeg ss2(.OUT(HEX2),.IN(HexVal[11: 8]));
  SevenSeg ss1(.OUT(HEX1),.IN(HexVal[ 7: 4]));
  SevenSeg ss0(.OUT(HEX0),.IN(HexVal[ 3: 0]));
  wire selDisp=(ABUS==BASE);
  wire wrDisp=WE&&selDisp;
  always @(posedge CLK) if(LOCK) begin
    if(INIT)
      HexVal<=16'hDEAD;
    else if(wrDisp)
      HexVal<=DBUS[15:0];
  end
endmodule

// This is the device for LEDG and LEDR (same device used for both)
module Leds(ABUS,DBUS,WE,CLK,LOCK,INIT,LED);
  parameter ABITS;
  parameter DBITS;
  parameter LBITS;
  parameter BASE;
  input wire [(ABITS-1):0] ABUS;
  inout wire [(DBITS-1):0] DBUS;
  input wire WE,CLK,LOCK,INIT;
  output wire [(LBITS-1):0] LED=val;
  reg [(LBITS-1):0] val;
  wire sel=(ABUS==BASE);
  wire wri=WE&&sel;
  always @(posedge CLK) if(LOCK) begin
    if(INIT)
      val<={LBITS{1'b0}};
    else if(wri)
      val<=DBUS[(LBITS-1):0];
  end
endmodule

// This is the complete key device
module KeyDev(ABUS,DBUS,WE,INTR,CLK,LOCK,INIT,KEY,DEBUG);
  parameter BITS;
  parameter BASE;
  input wire [(BITS-1):0] ABUS;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,INIT;
  input wire [3:0] KEY;
  output wire INTR;
  output wire [7:0] DEBUG={1'b0,OR,IE,IP,KEY};
  wire selKey=(ABUS==BASE);
  wire wrKey=WE&&selKey;
  wire rdKey=(!WE)&&selKey;
  reg [3:0] prev;
  reg IP,IE,OR;
  // What does IP_curr do? Well, what if a key is pressed in
  // a cycle t/when a WCTL instruction is writing to KCTL
  wire IP_curr=wrKey?DBUS[4]:IP;
  always @(posedge CLK) if(LOCK) begin
    if(INIT) begin
      prev<=KEY;
      {OR,IE,IP}<=3'b000;
    end else begin
      if(wrKey) begin
        {OR,IE,IP}<=DBUS[6:4];
      end
      if(prev!=KEY) begin
        if(IP_curr)
          OR<=1'b1;
        IP<=1'b1;
        prev<=KEY;
      end
    end    
  end
  assign DBUS=rdKey?{{(BITS-7){1'b0}},OR,IE,IP,KEY}:
                    {BITS{1'bz}};
  assign INTR=IP&&IE;
endmodule

module SwDev(ABUS,DBUS,WE,INTR,CLK,LOCK,INIT,SW,DEBUG);
  parameter BITS;
  parameter BASE;
  // How many cycles to count to get to 10ms
  parameter DEBN;
  // How many bits do we need for that count?
  parameter DEBB;
  input wire [(BITS-1):0] ABUS;
  inout wire [(BITS-1):0] DBUS;
  input wire WE,CLK,LOCK,INIT;
  input wire [9:0] SW; // 10 switches? 
  // hopefully outputs are the same as for key dev
  output wire INTR;
  output wire [13:0] DEBUG={1'b0,OR,IE,IP,SW};
  
  wire selSw=(ABUS==BASE);
  wire wrSw=WE&&selSw;
  wire rdSw=(!WE)&&selSw; // do i need this?
  reg [(DEBB-1):0] count;
  // registers for debouncing
  reg [9:0] raw, temp, actual;
  
  reg IP,IE,OR;
  always @(posedge CLK) if (LOCK) begin
  	if (INIT) begin
  		count<=16'b0000000000000000;
  		{OR,IE,IP}<=3'b000;	
  	end else begin
			if(wrSw) begin
				{OR,IE,IP}<=DBUS[12:10];
	      	end
	      	// is this right? doesn't seem right
			raw<=SW;
			temp<=raw;
			
			if(temp!=raw)
				count<=0;
			else if(temp==raw)
				count<=count+2'd1;
				
	  		if(count == DEBN) begin // is this the changed logic?
	  			count<=0; // hope this works
	  			if(IP)
	  				OR<=1'b1;
	  			IP<=1'b1;
	  			actual<=raw;
	  			// switch has been debounced
	  		end
  		end
  end
/*
  You'll have to write your own SW device after all :)
*/
	assign DBUS=rdSw?{{(BITS-13){1'b0}},OR,IE,IP,actual}: // i think right ordering
                    {BITS{1'bz}};
	assign INTR=IP&&IE;
endmodule

// ADDED FOR ASSIGNMENT 5
module CtlIInst(IWORD, CMODE, IINST);
	input  wire [31:0] IWORD;
	input  wire        CMODE;
	output reg        IINST;
	
	always@ (IWORD) begin
		if(CMODE == 1) begin
			case(`OP_BITS)
				`OP_AND,`OP_NAND,`OP_OR,`OP_NOR,`OP_XOR,
				`OP_EQ,`OP_NE,`OP_LT,`OP_LE,
				`OP_ADD,`OP_SUB,
				`OP_BEQ,`OP_BNE,
				`OP_ANDI,`OP_NANDI,`OP_ORI,`OP_NORI,`OP_XORI,
				`OP_EQI,`OP_NEI,`OP_LTI,`OP_LEI,
				`OP_ADDI,`OP_SUBI,
				`OP_LW,`OP_SW,
				`OP_BEQZ,`OP_BNEZ,
				`OP_JRL,
				`OP_RCTL,`OP_WCTL:
					IINST <= 1'b0;
				default:
					IINST <= 1'b1;
			endcase
		end
		else begin
			case(`OP_BITS)
				`OP_AND,`OP_NAND,`OP_OR,`OP_NOR,`OP_XOR,
				`OP_EQ,`OP_NE,`OP_LT,`OP_LE,
				`OP_ADD,`OP_SUB,
				`OP_BEQ,`OP_BNE,
				`OP_ANDI,`OP_NANDI,`OP_ORI,`OP_NORI,`OP_XORI,
				`OP_EQI,`OP_NEI,`OP_LTI,`OP_LEI,
				`OP_ADDI,`OP_SUBI,
				`OP_LW,`OP_SW,
				`OP_BEQZ,`OP_BNEZ:
				//`OP_RCTL,`OP_WCTL:  these are system-only instructions
					IINST <= 1'b0;
				`OP_JRL:
					if( (IWORD && 32'hFF0000)!=32'hCC0000) // for RETI case
						IINST <= 1'b0;
					else
						IINST <= 1'b1;
				default:
					IINST <= 1'b1;
			endcase
		end
    end		
endmodule

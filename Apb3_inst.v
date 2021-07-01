// Generator : SpinalHDL v1.4.3    git head : adf552d8f500e7419fff395b7049228e4bc5de26
// Component : Apb3_inst



module Apb3_inst (
  input      [15:0]   apb_PADDR,
  input      [0:0]    apb_PSEL,
  input               apb_PENABLE,
  output              apb_PREADY,
  input               apb_PWRITE,
  input      [31:0]   apb_PWDATA,
  output reg [31:0]   apb_PRDATA,
  output     [0:0]    dmaTrig,
  input      [0:0]    dmaClr,
  input      [0:0]    dmaBusy,
  input      [18:0]   dmaDatTrans,
  input      [15:0]   dmaPeriSel,
  output     [31:0]   dmaMemStratAddr,
  input               clk,
  input               reset
);
  reg        [31:0]   DmaCtrlreg;
  reg        [31:0]   DmaCfgAreg;
  reg        [31:0]   DmaCfgBreg;
  wire                RegWr;
  wire                RegRd;

  assign dmaTrig = DmaCtrlreg[0 : 0];
  assign dmaMemStratAddr = DmaCfgBreg[31 : 0];
  assign RegWr = ((apb_PSEL[0] && apb_PENABLE) && apb_PWRITE);
  assign RegRd = ((apb_PSEL[0] && (! apb_PENABLE)) && (! apb_PWRITE));
  assign apb_PREADY = 1'b1;
  always @ (*) begin
    apb_PRDATA = 32'h0;
    if(RegRd)begin
      case(apb_PADDR)
        16'h000a : begin
          apb_PRDATA[0 : 0] = DmaCtrlreg[0 : 0];
          apb_PRDATA[1 : 1] = DmaCtrlreg[1 : 1];
          apb_PRDATA[3 : 2] = DmaCtrlreg[3 : 2];
          apb_PRDATA[4 : 4] = DmaCtrlreg[4 : 4];
          apb_PRDATA[7 : 5] = DmaCtrlreg[7 : 5];
          apb_PRDATA[26 : 8] = DmaCtrlreg[26 : 8];
          apb_PRDATA[31 : 27] = DmaCtrlreg[31 : 27];
        end
        16'h000e : begin
          apb_PRDATA[15 : 0] = DmaCfgAreg[15 : 0];
          apb_PRDATA[31 : 16] = DmaCfgAreg[31 : 16];
        end
        16'h0012 : begin
          apb_PRDATA[31 : 0] = DmaCfgBreg[31 : 0];
        end
        default : begin
        end
      endcase
    end
  end

  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      DmaCtrlreg <= 32'h30000bb0;
      DmaCfgAreg <= 32'h0;
      DmaCfgBreg <= 32'h000004d2;
    end else begin
      if((dmaClr == 1'b1))begin
        DmaCtrlreg[1 : 1] <= 1'b1;
      end
      if((dmaBusy == 1'b1))begin
        DmaCtrlreg[4 : 4] <= 1'b1;
      end
      DmaCtrlreg[26 : 8] <= dmaDatTrans;
      DmaCfgAreg[15 : 0] <= dmaPeriSel;
      if(RegWr)begin
        case(apb_PADDR)
          16'h000a : begin
            DmaCfgAreg[15 : 0] <= 16'h0;
            DmaCtrlreg[0 : 0] <= apb_PWDATA[0 : 0];
            DmaCtrlreg[1 : 1] <= (apb_PWDATA[1 : 1] | DmaCtrlreg[1 : 1]);
            DmaCtrlreg[4 : 4] <= ((~ apb_PWDATA[4 : 4]) & DmaCtrlreg[4 : 4]);
            DmaCtrlreg[31 : 27] <= apb_PWDATA[31 : 27];
          end
          16'h000e : begin
            DmaCfgAreg[15 : 0] <= ((~ apb_PWDATA[15 : 0]) & DmaCfgAreg[15 : 0]);
          end
          16'h0012 : begin
            DmaCfgBreg[31 : 0] <= apb_PWDATA[31 : 0];
          end
          default : begin
          end
        endcase
      end
      if(RegRd)begin
        case(apb_PADDR)
          16'h000a : begin
            DmaCfgBreg[31 : 0] <= 32'h00000d05;
          end
          default : begin
          end
        endcase
      end
    end
  end


endmodule


import java.io.PrintWriter
import java.io.File
import scala.io.Source

import spinal.core.{is, _}
import spinal.core.internals.Operator
import spinal.lib._
import java.io.StringReader
//import au.com.bytecode.opencsv.CSVReader

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import spinal.lib.experimental.math._
import sun.util.resources.cldr.ru.TimeZoneNames_ru

/* Interface */
case class Apb3Config(
                       addressWidth  : Int,
                       dataWidth     : Int,
                       selWidth      : Int     = 1,
                       useSlaveError : Boolean = true
                     )

case class Apb3(config: Apb3Config) extends Bundle with IMasterSlave {
  val PADDR      = UInt(config.addressWidth bit)
  val PSEL       = Bits(config.selWidth bits)
  val PENABLE    = Bool
  val PREADY     = Bool
  val PWRITE     = Bool
  val PWDATA     = Bits(config.dataWidth bit)
  val PRDATA     = Bits(config.dataWidth bit)
  val PSLVERROR  = if(config.useSlaveError) Bool else null

  override def asMaster(): Unit = {
    out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
    in(PREADY,PRDATA)
    if(config.useSlaveError) in(PSLVERROR)
  }
}

/* Reg TODO: to be extended */
case class RegConfig(){
  val file2=Source.fromFile("register.csv")
  val file1=Source.fromFile("register.csv")
  val num = file2.getLines().size
  val RegAddr = Vec(UInt(16 bits), num) //reg address
  val RegAddrIndex = new Array[String](num)
  val RegName = new Array[String](num)
  val RegInit = new Array[String](num)
  val FieldNum = new Array[Int](num)
  val FieldProperty =  ArrayBuffer[String]()
  val RegField = new Array[ArrayBuffer[String]](num) // num * FieldNum
  var line1 =  Array[String]()
  var exAddr = 0
  var M = 0
  var k = 0

  for(line <- file1.getLines)
  {
    line1 = line.split(",")
    for(i<-0 until line1.length) {
      //addr generate
      if(i == 0) {  //Reg addr
        exAddr = line1(0).toInt
        RegAddrIndex(M) = line1(0)
        RegAddr(M) := exAddr
      }
      if(i == 1) { //Reg name
        RegName(M) = line1(1)
      }
      if(i == 2) {  //field name
        FieldNum(M) = line1(2).toInt
      }
      if(i >= 3){  //field property
        FieldProperty += line1(i)
        if(i%5 == 1){ //field initial value
          if(i == 6){
            RegInit(M) = line1(i)
          }else{
            RegInit(M) = RegInit(M) + ',' + line1(i)
          }
        }
      }
    }

    /* Reg initial value Bin => Dec */
    var TempInit: Array[String] = RegInit(M).split(",")
    var binStr = ArrayBuffer[Int]()
    var constant = 0
    var decNum = 0
    var digit = 0
    for(n<-0 until FieldNum(M)){
      k = FieldNum(M) - n - 1
      decNum = TempInit(k).toInt
      digit = FieldProperty(5*k+1).toInt
      for (i <- digit - 1 to 0 by -1) {
        binStr += (decNum >> i) & 1
      }
    }

    for(i<-0 to 31){
      constant = constant + binStr(i)*(scala.math.pow(2,31-i).toInt)
    }
    RegInit(M) = constant.toString

    /* Store Reg field property */
    RegField(M) = FieldProperty.clone()
    FieldProperty.clear()
    M = M + 1
  }

  //def AddrGenerate
  def test = new Area {
    val reg = Vec(Reg(Bits(32 bits)), num) //reg
    var LowBitAddr = 0
    var UpBitAddr = 0
    var LowBit = ArrayBuffer[Int]()
    var UpBit = ArrayBuffer[Int]()
    val LowAddr = new Array[ArrayBuffer[Int]](num)
    val UpAddr = new Array[ArrayBuffer[Int]](num)

    //generate field address range
    for(i <- 0 to num - 1){
      LowBitAddr = 0
      UpBitAddr = 0
      for(j <- 0 until FieldNum(i)) {
        UpBitAddr = LowBitAddr + RegField(i)(5 * j + 1).toInt - 1
        UpBit += UpBitAddr
        LowBit += LowBitAddr
        LowBitAddr = UpBitAddr + 1
      }
      UpAddr(i) = UpBit.clone()
      LowAddr(i) = LowBit.clone()
      UpBit.clear()
      LowBit.clear()
    }

    //Reg rename
    for(i <- 0 to num - 1){
      /* reg reset value */
      reg(i).init(RegInit(i).toInt)
      reg(i).setName(RegName(i)+"reg")
      for(j <- 0 until FieldNum(i)){
        LowBitAddr = LowAddr(i)(j)
        UpBitAddr = UpAddr(i)(j)
        val tempField = Vec(Bits(UpBitAddr - LowBitAddr + 1 bits),FieldNum(i))
        if(RegField(i)(5*j) != "reserved"){
          //TODO temporary treat WR/WO as reg output , RC/RS/WC/WS as reg status , RO as reg input
          if(RegField(i)(5*j+2) == "WR" || RegField(i)(5*j+2) == "WO"){
            tempField(j).setName(RegField(i)(5*j)).asOutput()
            tempField(j) := reg(i)(UpBitAddr downto LowBitAddr)
          }else if(RegField(i)(5*j+2) == "RO"){
            tempField(j).setName(RegField(i)(5*j)).asInput()
            reg(i)(UpBitAddr downto LowBitAddr) := tempField(j)
          }else {
            tempField(j).setName(RegField(i)(5*j)).asInput()
            if((UpBitAddr - LowBitAddr) == 0) { //TODO 1 bit status,treat as error/int signal,clear until write/read clear
              when(tempField(j) === 1){
                reg(i)(UpBitAddr downto LowBitAddr) := 1
              }
            }else{ // TODO 2 or more bits status,treat as normal signal , follow by input signal
              reg(i)(UpBitAddr downto LowBitAddr) := tempField(j)
            }
          }
        }
      }
    }

    def Wr(WrAddr: UInt, WrData: Bits): Unit =  {
      var rwProperty = ""
      var wrEx = ""
      var wrExtend = Array[String]()
      var AddrIndex = 0
      switch(WrAddr) {
        for (i <- 0 to num - 1) {
          is(RegAddr(i)) {
            for(j <- 0 until FieldNum(i)){
              LowBitAddr = LowAddr(i)(j)
              UpBitAddr = UpAddr(i)(j)
              rwProperty = RegField(i)(5*j+2)

              //Extend Reg Operation
              wrEx = RegField(i)(5*j+4)
              if(wrEx != "none"){
                wrExtend = wrEx.split("/")
                for (i <- 0 to num - 1){
                  if(wrExtend(2).toInt == RegAddrIndex(i).toInt){
                    AddrIndex = i
                  }
                }
                if(wrExtend(0) == "W"){
                  if(wrExtend(1) == "C"){
                    reg(AddrIndex)(UpAddr(AddrIndex)(wrExtend(3).toInt-1) downto LowAddr(AddrIndex)(wrExtend(3).toInt-1)) := 0
                  }else if(wrExtend(1) == "S"){
                    reg(AddrIndex)(UpAddr(AddrIndex)(wrExtend(3).toInt-1) downto LowAddr(AddrIndex)(wrExtend(3).toInt-1)) := wrExtend(4).toInt
                  }
                }
              }

              //Reg Property
              if((rwProperty == "WR") || (rwProperty == "WO") || (rwProperty == "RC") || (rwProperty == "RS")) {  // WR WO RC
                reg(i)(UpBitAddr downto LowBitAddr) := WrData(UpBitAddr downto LowBitAddr)
              }else if(rwProperty == "WC"){ // WC
                reg(i)(UpBitAddr downto LowBitAddr) := ~WrData(UpBitAddr downto LowBitAddr) & reg(i)(UpBitAddr downto LowBitAddr)
              }else if(rwProperty == "WS"){ // WS
                reg(i)(UpBitAddr downto LowBitAddr) := WrData(UpBitAddr downto LowBitAddr) | reg(i)(UpBitAddr downto LowBitAddr)
              }else {  // RO
              }
            }
          }
        }
      }
    }

    def Rd(RdAddr: UInt, RdData: Bits): Unit = {
      var rwProperty = ""
      var rdEx = ""
      var rdExtend = Array[String]()
      var AddrIndex = 0
      switch(RdAddr) {
        for (i <- 0 to num - 1) {
          is(RegAddr(i)) {
            for(j <- 0 until FieldNum(i)){
              LowBitAddr = LowAddr(i)(j)
              UpBitAddr = UpAddr(i)(j)
              rwProperty = RegField(i)(5*j+2)

              //Extend Reg Operation
              rdEx = RegField(i)(5*j+4)
              if(rdEx != "none"){
                rdExtend = rdEx.split("/")
                for (i <- 0 to num - 1){
                  if(rdExtend(2).toInt == RegAddrIndex(i).toInt){
                    AddrIndex = i
                  }
                }
                if(rdExtend(0) == "R"){
                  if(rdExtend(1) == "C"){
                    reg(AddrIndex)(UpAddr(AddrIndex)(rdExtend(3).toInt-1) downto LowAddr(AddrIndex)(rdExtend(3).toInt-1)) := 0
                  }else if(rdExtend(1) == "S"){
                    reg(AddrIndex)(UpAddr(AddrIndex)(rdExtend(3).toInt-1) downto LowAddr(AddrIndex)(rdExtend(3).toInt-1)) := rdExtend(4).toInt
                  }
                }
              }

              //Reg property
              if((rwProperty == "WR") || (rwProperty == "RO") || (rwProperty == "WC") || (rwProperty == "WS")) {  // WR RO WC WS
                RdData(UpBitAddr downto LowBitAddr) := reg(i)(UpBitAddr downto LowBitAddr)
              }else if(rwProperty == "RC"){ // RC
                RdData(UpBitAddr downto LowBitAddr) := ((UpBitAddr downto LowBitAddr) -> False)
              }else if(rwProperty == "RS"){ // RS
                RdData(UpBitAddr downto LowBitAddr) := ((UpBitAddr downto LowBitAddr) -> True)
              }else {  // RO
              }
            }
          }
        }
      }
    }
  }
}

class Apb3_inst extends Component{
  /* Interface Config */
  val apbConfig = Apb3Config(
    addressWidth  = 16,
    dataWidth     = 32,
    selWidth      = 1,
    useSlaveError = false
  )

  /* Interface instance */
  val apb = slave(Apb3(apbConfig))

  /* Reg Config & instance */
  val RegCfg = new RegConfig()
  val test = RegCfg.test

  /* Reg & Interface bridge */
  val RegWr = apb.PSEL(0) && apb.PENABLE && apb.PWRITE
  val RegRd = apb.PSEL(0) && !apb.PENABLE && !apb.PWRITE

  /* Interface feature */
  apb.PREADY := True

  /* Reg write/read operation */
  apb.PRDATA := 0
  when(RegWr){  //write
    test.Wr(apb.PADDR,apb.PWDATA)
  }
  when(RegRd){  //read
    test.Rd(apb.PADDR,apb.PRDATA)
  }

  /* TODO other operation */
}

object Apb3_top {
  def main(args: Array[String]) {
    SpinalVerilog(new Apb3_inst)
  }
}

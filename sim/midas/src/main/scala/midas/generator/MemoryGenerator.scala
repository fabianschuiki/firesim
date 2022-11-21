package midas.generator

import midas.models.sram.AsyncMemChiselModel
import midas.targetutils.ElaborateChiselSubCircuit

import chisel3._
import chisel3.util._

import org.json4s._

import firrtl._

import freechips.rocketchip.config.{Parameters}


class AsyncMemChiselModelWrapper(val depth: Int, val dataWidth: Int, val nReads: Int, val nWrites: Int) extends Module {
  val reads_in = IO(Flipped(Vec(nReads, new Bundle {
    val addr = Decoupled(UInt(log2Ceil(depth).W))
    val en = Decoupled(Bool())
  })))

  val writes = IO(Flipped(Vec(nWrites, new Bundle {
    val addr = Decoupled(UInt(log2Ceil(depth).W))
    val en = Decoupled(Bool())
    val data = Decoupled(UInt(dataWidth.W))
    val mask = Decoupled(Bool())
  })))

  val reads_out = IO(Vec(nReads, new Bundle {
    val data = Decoupled(UInt(dataWidth.W))
  }))

  val model = Module(new AsyncMemChiselModel(depth, dataWidth, nReads, nWrites))
  model.channels.reset.valid := true.B
  model.channels.reset.bits := false.B

  for (i <- 0 until nReads) {
    val read_req = model.channels.read_cmds(i)
    val read_resp = model.channels.read_resps(i)
    val read_in = reads_in(i)
    val read_out = reads_out(i).data

    read_req.valid := read_in.en.valid && read_in.addr.valid
    read_req.bits.en := read_in.en.bits
    read_req.bits.addr := read_in.addr.bits
    read_in.addr.ready := read_req.ready && read_in.en.valid
    read_in.en.ready := read_req.ready && read_in.en.valid
    read_out.bits := read_resp.bits
    read_out.valid := read_resp.valid
    read_resp.ready := read_out.ready
  }

  for (i <- 0 until nWrites) {
    val write_req = model.channels.write_cmds(i)
    val write_chan = writes(i)

    write_chan.data.ready := write_req.ready && write_chan.mask.valid && write_chan.addr.valid && write_chan.en.valid
    write_chan.mask.ready := write_req.ready && write_chan.data.valid && write_chan.addr.valid && write_chan.en.valid
    write_chan.addr.ready := write_req.ready && write_chan.data.valid && write_chan.mask.valid && write_chan.en.valid
    write_chan.en.ready   := write_req.ready && write_chan.data.valid && write_chan.mask.valid && write_chan.addr.valid

    write_req.valid := write_chan.data.valid && write_chan.mask.valid && write_chan.addr.valid && write_chan.en.valid
    write_req.bits.data := write_chan.data.bits
    write_req.bits.mask := write_chan.mask.bits
    write_req.bits.addr := write_chan.addr.bits
    write_req.bits.en := write_chan.en.bits
  }
}

case class MultiCycleModelConfig(
  numReadPorts: Int,
  numReadWritePorts: Int,
  numWritePorts: Int,
  depth: Int,
  dataWidth: Int
)

/**
  * Generator for multi-cycle memory implementations.
  *
  * @param key Memory instantiation parameters.
  */
class MultiCycleMemory(key: JObject)(implicit p: Parameters) extends Generator {
  implicit val formats: Formats = DefaultFormats
  val config = key.extract[MultiCycleModelConfig]

  def elaborate() : (firrtl.ir.Circuit, AnnotationSeq) = {
    require(config.numReadWritePorts == 0, "read-write ports not yet supported")

    ElaborateChiselSubCircuit(new AsyncMemChiselModelWrapper(config.depth, config.dataWidth, config.numReadPorts, config.numWritePorts) {
      override def desiredName =
        s"MultiCycleMemory_${config.numReadPorts}_${config.numReadWritePorts}_${config.numWritePorts}"
    })
  }
}

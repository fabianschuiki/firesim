package midas.generator

import midas.core._
import midas.core.SimUtils._
import midas.targetutils.ElaborateChiselSubCircuit

import chisel3._
import chisel3.util._

import org.json4s._

import firrtl._

import freechips.rocketchip.config.{Parameters}


/**
  * Wrapper around pipe channels.
  *
  * Ensures port names match those emitted by rhodium.
  */
class PipeChannelWrapper(name: String, latency: Int, tpe: firrtl.ir.Type)(implicit p: Parameters) extends Module {
  override def desiredName = name

  val channelTy = typeToData(tpe)

  val in_0 = IO(Flipped(Decoupled(channelTy)))
  val out_0 = IO(Decoupled(channelTy))

  val channel = Module(new PipeChannel(channelTy, latency))
  channel.io.in <> in_0
  out_0 <> channel.io.out
}

/**
  * Wrapper around pipe channels.
  *
  * This is a pass-through module at the moment.
  */
class ClockChannelWrapper(name: String, numClocks: Int)(implicit p: Parameters) extends Module {
  override def desiredName = name

  val in_0 = IO(Flipped(Decoupled(Vec(numClocks, Bool()))))
  val out_0 = IO(Decoupled(Vec(numClocks, Bool())))

  out_0.valid := in_0.valid
  out_0.bits := in_0.bits
  in_0.ready := out_0.ready
}

/**
  * Wrapper around pipe channels.
  *
  * Adapts a ready-valid channel to the rhodium interface.
  */
class ReadyValidChannelWrapper(name: String, tpe: firrtl.ir.Type)(implicit p: Parameters) extends Module {
  override def desiredName = name

  val channelTy = typeToData(tpe)

  val in_0 = IO(Flipped(Decoupled(Valid(channelTy))))
  val in_1 = IO(Flipped(Decoupled(Bool())))
  val out_0 = IO(Decoupled(Valid(channelTy)))
  val out_1 = IO(Decoupled(Bool()))

  val channel = Module(new ReadyValidChannel(channelTy))
  channel.io.targetReset.bits := false.B
  channel.io.targetReset.valid := true.B

  val enq = channel.io.enq
  enq.fwd.hValid   := in_0.valid
  enq.target.valid := in_0.bits.valid
  enq.target.bits  := in_0.bits.bits
  in_0.ready       := enq.fwd.hReady
  out_1.valid      := enq.rev.hValid
  out_1.bits       := enq.target.ready
  enq.rev.hReady   := out_1.ready

  val deq = channel.io.deq
  deq.fwd.hReady     := out_0.ready
  out_0.valid        := deq.fwd.hValid
  out_0.bits.valid   := deq.target.valid
  out_0.bits.bits    := deq.target.bits
  deq.rev.hValid     := in_1.valid
  deq.target.ready   := in_1.bits
  in_1.ready         := deq.rev.hReady
}

case class ChannelInstanceConfig(
  name: String,
  attrs: JObject
)

/**
  * Generator for channel instances.
  *
  * @param key Channel instantiation parameters.
  */
class ChannelInstance(key: JObject)(implicit p: Parameters) extends Generator {
  implicit val formats: Formats = DefaultFormats
  val config = key.extract[ChannelInstanceConfig]

  def elaborate() : (firrtl.ir.Circuit, AnnotationSeq) = {
    implicit val formats: Formats = DefaultFormats

    ElaborateChiselSubCircuit((config.attrs \ "class").extract[String] match {
      case "pipe" => {
        val latency = (config.attrs \ "latency").extract[Int]
        val ty = firrtl.Parser.parseType((config.attrs \ "type").extract[String])
        new PipeChannelWrapper(config.name, latency, ty)
      }
      case "clock" =>
        new ClockChannelWrapper(config.name, (config.attrs \ "numClocks").extract[Int])
      case "ready_valid" => {
        val ty = firrtl.Parser.parseType((config.attrs \ "type").extract[String])
        new ReadyValidChannelWrapper(config.name, ty)
      }
    })
  }
}

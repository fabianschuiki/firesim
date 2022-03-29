package midas.generator

import java.io.{File, FileWriter}

import midas.core._
import midas.core.SimUtils._
import midas.platform.PlatformShim
import midas.Platform
import midas.targetutils.ElaborateChiselSubCircuit
import midas.stage.OutputFileBuilder

import firrtl._
import firrtl.annotations.{Annotation, JsonProtocol}

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.jackson.Serialization.write

import scala.collection.immutable.{ListMap}
import scala.collection.mutable.{ArrayBuffer}

case class RawPortType(
    name: String,
    kind: String,
    data: JValue
)
case class RawConfig(
    annotations: JArray,
    inputs: Seq[RawPortType],
    outputs: Seq[RawPortType],
    moduleName: String,
    readyValidPorts: Seq[ReadyValidPort]
)

/**
  * Generator for the FireSim shim.
  *
  * @param key JSON object describing the shim.
  */
class FPGATop(key: JValue)(implicit p: Parameters) extends Generator {
  // Decode the configuration of the shim from the JSON object.
  implicit val formats: Formats = DefaultFormats
  val raw = key.extract[RawConfig]

  def decodePort(port: RawPortType): (String, TargetPort) = {
    port.name -> (port.kind match {
      case "prim" =>
        PrimitivePort(firrtl.Parser.parseType(port.data.extract[String]))
      case "clock_channel" =>
        ClockChannelPort(port.data.extract[Int])
      case "channel" =>
        ChannelPort(firrtl.Parser.parseType(port.data.extract[String]))
    })
  }

  val annotations = JsonProtocol.deserialize(write(raw.annotations))
  val inputs = ListMap(raw.inputs.map(decodePort) : _*)
  val outputs = ListMap(raw.outputs.map(decodePort) : _*)
  val moduleName = raw.moduleName
  val readyValidPorts = raw.readyValidPorts

  // Produce the circuit for the platform shim.
  def elaborate() : (firrtl.ir.Circuit, AnnotationSeq) = {
    // Generate and elaborate the platform shim.
    val simWrapperConfig = SimWrapperConfigLite(
        annotations,
        inputs,
        outputs,
        readyValidPorts,
        moduleName
    )
    lazy val shim = PlatformShim(simWrapperConfig)(p)
    val (chirrtl, annos) = ElaborateChiselSubCircuit(LazyModule(shim).module)

    // Generate auxiliary header files.
    val csb = new OutputFileBuilder(
      """// Golden Gate-generated Driver Header
        |// This contains target-specific preprocessor macro definitions,
        |// and encodes all required bridge metadata to instantiate bridge drivers.
        |""".stripMargin,
      fileSuffix = ".const.h")
    csb append "#ifndef __%s_H\n".format(moduleName.toUpperCase)
    csb append "#define __%s_H\n".format(moduleName.toUpperCase)
    shim.genHeader(csb.getBuilder, moduleName)
    csb append "#endif  // __%s_H\n".format(moduleName.toUpperCase)

    val vsb = new OutputFileBuilder(
      """// Golden Gate-generated Verilog Header
        |// This file encodes variable width fields used in MIDAS-level simulation
        |// and is not used in FPGA compilation flows.
        |""".stripMargin,
      fileSuffix = ".const.vh")
    vsb append "`ifndef __%s_H\n".format(moduleName.toUpperCase)
    vsb append "`define __%s_H\n".format(moduleName.toUpperCase)
    shim.genVHeader(vsb.getBuilder, moduleName)
    vsb append "`endif  // __%s_H\n".format(moduleName.toUpperCase)

    // Emit the Tcl variables to control the FPGA flow.
    val envTcl = new OutputFileBuilder(
      "# FireSim Generated Environment Variables\n",
      fileSuffix = ".env.tcl")
    // envTcl append "%s\n".format(p(BuildStrategy).emitTcl)
    envTcl append "set strategy \"TIMING\"\n" // HACK

    (chirrtl, Seq(csb.toAnnotation, vsb.toAnnotation, envTcl.toAnnotation) ++ annos)
  }
}

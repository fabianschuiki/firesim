// See LICENSE for license details.

package midas.stage

import midas.core._
import midas.generator.Generator
import midas.stage.phases.ConfigParametersAnnotation

import firrtl._
import firrtl.annotations.{Annotation, NoTargetAnnotation, JsonProtocol}
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.options.{HasShellOptions, Phase, PhaseManager, Shell, ShellOption, Stage, StageMain, Dependency}
import firrtl.stage.OutputFileAnnotation
import firrtl.stage.transforms.Compiler
import firrtl.Mappers._

import freechips.rocketchip.config.Parameters

import org.json4s._
import org.json4s.native.JsonMethods._

import scala.collection.mutable.ArrayBuffer

trait PlatformShimElabCli { this: Shell =>
  parser.note("PlatformShim Elaboration Options")
  Seq(
    ElabParamsFileAnnotation,
    firrtl.EmitCircuitAnnotation,
    firrtl.stage.OutputFileAnnotation,
  ).map(_.addOptions(parser))
}
case class ElabParamsFileAnnotation(paramsFileName: String) extends NoTargetAnnotation

object ElabParamsFileAnnotation extends HasShellOptions {
  val options = Seq(
    new ShellOption[String](
      longOption = "elab-params",
      toAnnotationSeq = (a: String) => Seq(ElabParamsFileAnnotation(a)),
      helpText = "Specifies a JSON file with elaboration parameters.",
      shortOption = Some("ep"),
      helpValueName = Some("<filename>") ) )
}

/**
  * Descriptor for the parameters of a generator.
  *
  * @param name Name of the generator class to invoke.
  * @param args JSON object with the generator parameters.
  */
private case class GeneratorKey(name: String, args: JObject)

/**
  * Stage which reads a list of generator configurations and elaborates them to FIRRTL.
  */
class PlatformShimElabStage extends Stage {
  val shell: Shell = new Shell("platform-shim-elab") with GoldenGateCli with PlatformShimElabCli

  override def invalidates(a: Phase): Boolean = false;

  def emit(annos: AnnotationSeq, circuitAndAnnos: (firrtl.ir.Circuit, AnnotationSeq))(implicit p: Parameters): AnnotationSeq = {
    val (circuit, circuitAnnos) = circuitAndAnnos

    val emitter = new Compiler(Seq(Dependency[firrtl.ChirrtlEmitter]))
    val state = emitter.execute(CircuitState(
      circuit = circuit,
      form = ChirrtlForm,
      annotations = Seq(
        EmitCircuitAnnotation(classOf[firrtl.ChirrtlEmitter])
      ) ++ circuitAnnos,
      renames = None
    ))

    // Forward file-writing annotations to the emitter.  All other annos are
    // added to a JSON file specific to the generator and written separately.
    val outputAnnos = new ArrayBuffer[Annotation]()
    val genAnnos = new ArrayBuffer[Annotation]()
    val names = new ArrayBuffer[String]()

    state.annotations.foreach ({
      // Output all files.
      case EmittedFirrtlCircuitAnnotation(EmittedFirrtlCircuit(name, value, _)) => {
        names ++= Seq(name)
        outputAnnos ++= Seq(GoldenGateOutputFileAnnotation(value, s".${name}.fir"))
      }
      case EmitCircuitAnnotation(_) =>
      case anno : GoldenGateOutputFileAnnotation =>
        outputAnnos ++= Seq(anno)

      // Preserver all annotations.
      case anno => genAnnos ++= Seq(anno)
    })

    outputAnnos.toSeq ++ Seq(
        GoldenGateOutputFileAnnotation(JsonProtocol.serialize(genAnnos.toSeq), s".${names.head}.fir.anno.json")
    )
  }

  override def run(initialAnnotations: AnnotationSeq): AnnotationSeq = {
    val annos = new midas.stage.phases.CreateParametersInstancePhase().transform(initialAnnotations)

    // Deserialize the elaboration parameters from the JSON input.
    val generators = {
      implicit val formats: Formats = DefaultFormats
      val generatorParamsFile = annos.collectFirst({ case ElabParamsFileAnnotation(x) => x }).get
      val file = scala.io.Source.fromFile(generatorParamsFile)
      val parsed = parse(file.mkString)
      val p = parsed.extract[Array[GeneratorKey]]
      file.close()
      p
    }

    // Find the parameters object.
    implicit val p = annos.collectFirst({ case ConfigParametersAnnotation(p)  => p }).get

    // Find the class for each generator and elaborate them.
    annos ++ generators.flatMap { case GeneratorKey(name, args) => {
      println(s"Elaborating ${name}")
      val constructor = Class.forName(name).getConstructors()(0)
      val instance = constructor.newInstance(args, p).asInstanceOf[Generator]
      emit(annos, instance.elaborate)
    }}
  }
}

object PlatformShimElabMain extends StageMain(new PlatformShimElabStage)

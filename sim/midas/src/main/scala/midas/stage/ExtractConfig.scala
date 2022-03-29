package midas.stage

import midas._
import midas.stage.phases.CreateParametersFromConfigString
import java.io.PrintWriter

object ExtractConfig {
  def main(args: Array[String]) {
    val p = CreateParametersFromConfigString(args(0), args(1).split('_'))

    def flag(value: Boolean): String = if (value) "true" else "false"

    new PrintWriter(args(2)) {
      write(s"--enable-printf-synthesis=${flag(p(SynthPrints))} ")
      write(s"--enable-assert-synthesis=${flag(p(SynthAsserts))} ")
      write(s"--enable-multithreading=${flag(p(EnableModelMultiThreading))} ")
      if (p(EnableAutoCounter)) {
        write(s"--enable-counter-synthesis=true ")
        write(s"--counter-to-printf=${flag(p(AutoCounterUsePrintfImpl))} ")
      }
      if (p(EnableAutoILA)) {
        write(s"--enable-auto-ila=true ")
        write(s"--ila-depth=${p(ILADepthKey)} ")
        write(s"--ila-probe-triggers=${p(ILAProbeTriggersKey)} ")
      }
      if (p(GenerateMultiCycleRamModels)) {
        write(s"--generate-multi-cycle-ram-models=true ")
      }
      p(PreLinkCircuitPath).map(path => write(s"--pre-link-prefix=${path} "))
      p(PostLinkCircuitPath).map(path => write(s"--post-link-prefix=${path} "))
      close
    }
  }
}

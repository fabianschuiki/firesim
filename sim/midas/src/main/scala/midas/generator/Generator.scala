package midas.generator

import firrtl.AnnotationSeq

import freechips.rocketchip.config.{Parameters}

/**
  * Trait to be implemented by module generators.
  */
trait Generator {
  def elaborate(): (firrtl.ir.Circuit, AnnotationSeq)
}

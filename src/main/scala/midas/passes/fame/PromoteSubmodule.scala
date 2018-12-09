// See LICENSE for license details.

package firrtl
package transforms

import ir._
import Utils._
import Mappers._
import annotations.{CircuitTarget, InstanceTarget, Annotation, SingleTargetAnnotation}
import analyses.InstanceGraph
import graph.DiGraph
import scala.collection.mutable
import firrtl.passes.{InlineInstances,PassException}

/** Tags an annotation to be consumed by this transform */
case class PromoteSubmoduleAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

/**
 * Takes PromoteSubmodule annotations for instantiations and causes
 * each corresponding instance to be removed; ports are added to the
 * parent module and the submodule is added as a peer instance to all
 * modules instantiating the parent module.

 * Module nomenclature:
 * Grandparent = instantiator of parent.
 *     Transformed to instantiate child alongside parent, connect.
 * Parent = instantiator of child.
 *     Transformed to get port of child IO instead of instantiating child.
 * Child = submodule to be promoted.
 *     Does not get transformed.
 */

class PromoteSubmodule extends Transform {
  def inputForm = HighForm
  def outputForm = HighForm

  private def findPromotedInstances(iGraph: InstanceGraph, anns: Iterable[Annotation]): Set[WDefInstance] = {
    val annotatedInstances = anns.collect {
      case PromoteSubmoduleAnnotation(instTarget) =>
        /* For now, parent must be a module, not an instance.
         * Otherwise, the parent (and possibly grandparent) would potentially
         * need to be replicated into versions with and without the promoted child.
         */
        assert(instTarget.isLocal)
        (instTarget.module, instTarget.instance)
    }
    val annotatedInstanceSet = annotatedInstances.toSet
    val instancesToPromote = iGraph.getChildrenInstances.flatMap {
      case (modName, instanceSet) => instanceSet.flatMap { wi => Some(wi).filter(wi => annotatedInstanceSet((modName, wi.name))) }
    }
    instancesToPromote.toSet
  }

  private def instanceRef(inst: WDefInstance): WRef = WRef(inst.name, inst.tpe, InstanceKind, FEMALE)
  private def instanceField(inst: WDefInstance, field: String): WSubField = {
    val wref = instanceRef(inst)
    WSubField(wref, field, field_type(wref.tpe, field), FEMALE)
  }

  private def portBundle(m: DefModule): BundleType = {
    BundleType(m.ports map (p => Field(p.name, to_flip(p.direction), p.tpe)))
  }

  private def deleteSubStatement(stmt: Statement, subStmt: Statement): Statement = stmt match {
    case `subStmt` => EmptyStmt
    case Block(stmts) => Block(stmts map (s => deleteSubStatement(s, subStmt)))
    case s => s
  }

  private def instanceToPort(parent: Module, childInstance: WDefInstance, childModule: DefModule): Module = {
    val promotedPort = Port(childInstance.info, childInstance.name, Input, portBundle(childModule))
    parent.copy(ports = parent.ports :+ promotedPort, body = deleteSubStatement(parent.body, childInstance))
  }

  private def transformParentInstances(stmt: Statement, parentTemplate: WDefInstance, childTemplate: WDefInstance, namespace: Namespace, promotedNames: mutable.ArrayBuffer[String]): Statement = stmt match {
    case oldParentInstance @ WDefInstance(_, _, parentTemplate.module, _) =>
      val retypedParentInst = oldParentInstance.copy(tpe = parentTemplate.tpe)
      val childPeerInst = childTemplate.copy(name = namespace.newName(oldParentInstance.name + "_" + childTemplate.name))
      promotedNames += childPeerInst.name
      val connection = Connect(childTemplate.info, instanceField(retypedParentInst, childTemplate.name), instanceRef(childPeerInst))
      Block(Seq(retypedParentInst, childPeerInst, connection))
  case Block(stmts) => Block(stmts map (s => transformParentInstances(s, parentTemplate, childTemplate, namespace, promotedNames)))
    case s => s
  }

  override def execute(state: CircuitState): CircuitState = {
    val iGraph = new InstanceGraph(state.circuit)
    val updatedModules = new mutable.LinkedHashMap[String, Module]
    iGraph.moduleMap.foreach { case (k, v: Module) => updatedModules += (k -> v); case (k, v) => }
    val reversedIGraph = iGraph.graph.reverse
    val promoted = findPromotedInstances(iGraph, state.annotations)
    val order = reversedIGraph.linearize.filter(reversedIGraph.getEdges(_).size > 0).filter(promoted)
    val renames = RenameMap()
    for (childInstance <- order) {
      val childModule = updatedModules(childInstance.module)
      val parentInstances = reversedIGraph.getEdges(childInstance)
      val parentModule = updatedModules(parentInstances.head.module)
      val originalTarget = CircuitTarget(state.circuit.main).module(parentModule.name).instOf(childInstance.name, childInstance.module)
      if (parentModule.name == state.circuit.main) {
        throw new PassException("Cannot promote child instance ${childInstance.name} from top module ${parentModule.name}")
      }
      updatedModules(parentModule.name) = instanceToPort(parentModule, childInstance, childModule)
      val grandparentInstances = parentInstances.flatMap(reversedIGraph.getEdges(_))
      val grandparentModules = grandparentInstances.map(i => updatedModules(i.module)).toSet
      for (grandparent <- grandparentModules) {
        val parentTemplate = WDefInstance(NoInfo, "", parentModule.name, portBundle(parentModule))
        val namespace = Namespace(grandparent)
        val promotedNames = new mutable.ArrayBuffer[String]
        updatedModules(grandparent.name) = grandparent.copy(body = transformParentInstances(grandparent.body, parentTemplate, childInstance, namespace, promotedNames))
        // Record renames
        promotedNames.map(s => renames.record(originalTarget, originalTarget.copy(module = grandparent.name, instance = s)))
      }
    }
    state.copy(
      circuit = state.circuit.copy(modules = updatedModules.map({ case (k, v) => v }).toSeq),
      renames = Some(renames),
      annotations = state.annotations.filterNot(_.isInstanceOf[PromoteSubmoduleAnnotation])
    )
  }
}

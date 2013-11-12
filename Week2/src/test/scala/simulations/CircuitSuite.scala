package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate") {
    val in1, in2, out = new Wire
    
    orGate(in1, in2, out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")
    
    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or 4")
  }
  
  test("demux") {
    import scala.collection.mutable._
    def makeWireList(n: Int): List[Wire] =
      (0 until n).map(a => new Wire).foldLeft(List[Wire]())((a, b) => a :+ b)
    
    val in = new Wire
    val cs = makeWireList(4)
    val os = makeWireList(5)
    demux(in, cs, os)
    
    in.setSignal(false)
    cs.foreach(a => a.setSignal(false))
    run
    assert(os.forall(a => a.getSignal == false), "demux 1")
    
    in.setSignal(true)
    for (n <- 0 until cs.length)
    yield {
      if (n > 0) cs(n - 1).setSignal(false)
      cs(n).setSignal(true)
      run
      assert(os(n + 1).getSignal == true, "demux " + (n + 2))
    }
  }
}

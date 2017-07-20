package me.ayberkt
import org.scalatest._

class solveLinDiaphSpec extends FreeSpec {
  "x + 2y = 500" in {
    val cfs = List(1, 2)
    val rhs = 2000
    assert(solveLinDiaph(List(1, 2), 2000)
      .forall(solveLinDiaph.check(cfs)(rhs)))
  }
  "10x + 30y = 780" in {
    val cfs = List(10, 30)
    val rhs = 780
    assert(solveLinDiaph(List(10, 30), 780)
      .forall(solveLinDiaph.check(cfs)(rhs)))
  }
}

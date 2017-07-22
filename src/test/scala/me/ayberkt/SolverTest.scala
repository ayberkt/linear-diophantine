package me.ayberkt
import org.scalatest._

class solveLinDiaphSpec extends FreeSpec {
  "x + 2y = 500" in {
    val cfs = List(1, 2)
    val rhs = 200
    assert(solveLinDiaph(cfs, rhs).forall(solveLinDiaph.check(cfs)(rhs)))
  }

  "10x + 30y = 780" in {
    val cfs = List(10, 30)
    val rhs = 780
    assert(solveLinDiaph(cfs, rhs).forall(solveLinDiaph.check(cfs)(rhs)))
  }

  "3x + 5y + 12z = 780" in {
    val cfs = List(3, 5, 12)
    val rhs = 65
    assert(solveLinDiaph(cfs, rhs).forall(solveLinDiaph.check(cfs)(rhs)))
  }

  "20a + 12b + 26c + 2d = 50" in {
    val cfs = List(20, 12, 26, 2)
    val rhs = 150
    assert(solveLinDiaph(cfs, rhs).forall(solveLinDiaph.check(cfs)(rhs)))
  }
}

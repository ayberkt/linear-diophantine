package me.ayberkt

object solveLinDiaph extends ((List[Int], Int) => List[List[Int]]) {
  private def vector = (x : Int) => (elems : List[Int]) => (elems take x).toVector

  private def basis : Int => Set[Vector[Int]] =
    n => (List.fill(n)(n) map ((i : Int) => vector(i)(List.fill(i)(0)))).toSet

  private def prod : Vector[Int] => Vector[Int] => Int =
    v1 => v2 => v1.indices.map((i : Int) => v1(i) * v2(i)).sum

  private def lessEq : Vector[Int] => Vector[Int] => Boolean =
    v1 => v2 => v1.indices.forall((i : Int) => v1(i) <= v2(i))

  private def unnecessaryBranches :  Set[Vector[Int]]
                          => Set[Vector[Int]]
                          => Set[Vector[Int]] =
    vs1 => vs2 => {
      val g = (v1 : Vector[Int]) => (v2 : Vector[Int]) => !lessEq(v2)(v1)
      val f = (v : Vector[Int]) => vs2.toList.forall(g(v))
      vs1 filter f
    }

  private def setInsert(s : Set[Vector[Int]], x : Vector[Int]) = s.union(Set(x))

  private def bfs : Vector[Int] => Int => Set[Vector[Int]] => Set[Vector[Int]] =
    v => c => a => {
      val f : (Vector[Int], Set[Vector[Int]]) => Set[Vector[Int]] =
        (x, acc) => {
          val is =
            (x.indices.toVector.filter(k => (prod(v)(x) - c) * v(k) < 0)).toVector
          val zs = is.map((k : Int) => x updated (k, x(k) + 1))
          zs.foldLeft(acc)(setInsert)
        }
      a.foldRight(Set() : Set[Vector[Int]])(f)
    }

  private def newMinimalResults :  Vector[Int]
                        => Int
                        => Set[Vector[Int]]
                        => Set[Vector[Int]]
                        => List[Vector[Int]] =
    v => c => a => m => {
      if (a.isEmpty)
        List()
      else {
        def loop (m : Set[Vector[Int]], l : List[Vector[Int]]) : List[Vector[Int]] =
          l match {
            case y :: ys =>
              if (prod(v)(y) == c && !(m contains y))
                y::(loop (m + y, ys))
              else
                loop (m, ys)
            case List() =>
              val aPP = unnecessaryBranches(bfs(v)(c)(a))(m)
              newMinimalResults(v)(c)(aPP)(m)
          }
        loop(m, a.toList)
      }
    }

  def check (cfs : List[Int])(c : Int)(sol : List[Int]) : Boolean =
    ((cfs.zip(sol) map { case (x, y) => x * y }).sum) == c

  type Coefficients = List[Int]
  type RHS = Int
  type Solution = List[Int]

  def apply(v : Coefficients, c : RHS) : List[Solution] = {
      val arg1 : Vector[Int] = vector(v.length)(v.toList)
      val arg2 : Int = c
      val arg3 : Set[Vector[Int]] = basis(v.length)
      val arg4 : Set[Vector[Int]] = Set()

      val solutions =
        newMinimalResults(arg1)(arg2)(arg3)(arg4).map(xs => xs.toList).toList
      if (solutions.forall(check(v)(c)))
        println("All solutions correct.")
      else
        println("Some problems found.")
      solutions
    }
}

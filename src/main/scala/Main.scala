object Main {
  def main(args : Array[String]) : Unit = println("Hello world!")

  def vector = (x : Int) => (elems : List[Int]) => (elems take x).toArray

  def basis =
    (n : Int) =>
      List.fill(n)(n) map ((i : Int) => vector(i)(List.fill(i)(0)))

  def prod : Vector[Int] => Vector[Int] => Int =
    v1 => v2 => v1.indices.map((i : Int) => v1(i) * v2(i)).sum

  def lessEq : Vector[Int] => Vector[Int] => Boolean =
    v1 => v2 => v1.indices.forall((i : Int) => v1(i) <= v2(i))

  def unnecessaryBranches :  Set[Vector[Int]]
                          => Set[Vector[Int]]
                          => Set[Vector[Int]] =
    vs1 => vs2 => {
      val g = (v1 : Vector[Int]) => (v2 : Vector[Int]) => !lessEq(v2)(v1)
      val f = (v : Vector[Int]) => vs2.toList.forall(g(v))
      vs1 filter f
    }

  def bfs : Vector[Int] => Int => Set[Vector[Int]] => Set[Vector[Int]] =
    v => c => a => {
      val f : (Vector[Int], Set[Vector[Int]]) => Set[Vector[Int]] =
        (x, acc) => {
          val is : Vector[Int] =
            (x.indices
              .toVector
              .filter(k => (prod(v)(x) - c) * v(k) < 0)).toVector
          is.map(k => x updated (k, x(k) + 1)).toSet
        }
      a.foldRight(Set() : Set[Vector[Int]])(f)
    }
}

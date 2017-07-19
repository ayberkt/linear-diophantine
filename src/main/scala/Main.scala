object Main {

  def main(args : Array[String]) : Unit = println("Hello world!")

  def vector = (x : Int) => (elems : List[Int]) => (elems take x).toArray

  def basis =
    (n : Int) =>
      List.fill(n)(n) map ((i : Int) => vector(i)(List.fill(i)(0)))

  def prod : Vector[Int] => Vector[Int] => Int =
    v1 => v2 => v1.indices.map((i : Int) => v1(i) * v2(i)).sum


}

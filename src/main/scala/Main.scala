object Main {

  def main(args : Array[String]) : Unit = println("Hello world!")

  def vector = (x : Int) => (elems : List[Int]) => (elems take x).toArray

  def basis =
    (n : Int) =>
      List.fill(n)(n) map ((i : Int) => vector(i)(List.fill(i)(0)))

}

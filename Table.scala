object Table {
  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    list.map(x => println(table(x)) )
  }

  def table(value: Int) = {
    def calculate(num: Int, mul: Int, list: List[Int]): List[Int] = {

      if (mul > 10) list
      else calculate(num, mul + 1, (num * mul) :: list)
    }
    calculate(value, 1, List[Int]()).reverse

  }
}

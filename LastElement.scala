object LastElement {
def main(args: Array[String]): Unit = {

  val list = List(1, 2, 3, 4, 5, 6)
  val result = removeLast(list:+('e'))
  println(result)
}
  def removeLast(list: List[Any]):(Any,Int)={
    def removing(lst:List[Any],index:Int):(Any,Int)={
      if(lst(index)=='e') (lst(index-1),index-1)
      else removing(lst,index+1)
    }
    removing(list,0)
  }
}

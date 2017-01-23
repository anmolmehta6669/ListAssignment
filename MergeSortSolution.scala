
object MergeSortSolution {
  def merge(first:List[Int],second:List[Int]):List[Int]={
    (first,second) match {
      case (first,Nil)=>first
      case (Nil,second)=>second
      case(xfirst::xsfirst,xsecond::xssecond)=>
        if(xfirst<xsecond) xfirst::merge(xsfirst,second)
        else xsecond::merge(first,xssecond)
     }
  }
  def mergeSort(list:List[Int]):List[Int]={
    val size=list.size/2
    if(size==0) list
    else{
      val (first,second)=list.splitAt(size)
      merge(mergeSort(first),mergeSort(second))
    }
  }
  def main(args: Array[String]): Unit = {
      val list=List(10,30,5,67,87,34,22)
      val output=mergeSort(list)
      println(s"output : $output")
  }

}

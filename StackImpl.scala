

import scala.collection.mutable.ListBuffer


class stack[A]
{
  val list=new ListBuffer[A]


  def push(data:A): Unit =
  {
    list += data
    println(s"$data is added")

  }
  def empty()=if(list.size==0) 0 else 1
 //def resolve(o:Option[A])=o match case Some(x)= 
 def pop():Option[A]={
    if(empty!=0) {
      val output = list.head
      list.remove(0)
      Some(output)
}
else None
}
    
  def traverse()=    println(list)

}
object StackImpl {

  def main(args: Array[String]): Unit = {
    val s1=new stack[Int]
    s1.push(10)
    s1.push(20)
    s1.traverse()
    val delteddata=s1.pop()
    println(s"$delteddata: is deleted")
    s1.traverse()
    s1.pop()


  }
}

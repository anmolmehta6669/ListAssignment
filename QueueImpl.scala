import scala.collection.mutable.ListBuffer
class Queue[A]{
  val list=new ListBuffer[A]
  def enqueue(data:A): Unit =
  {
    list += data
    println(s"$data is enqueue")

  }
  def empty()=if(list.size==0) 0 else 1
def dequeue(): Option[A] =
  {
    if(empty!=0) {
      //val output = list.
      val output=list.remove(list.size-1)
      Some(output)
    }
    else
      None
  }
  def traverse()=    println(list)

}
object QueueImpl {
  def main(args: Array[String]): Unit = {


    val queue = new Queue[Int]
    queue.enqueue(20)
    queue.enqueue(40)
    queue.enqueue(50)
    queue.traverse()
    val deleteddata=queue.dequeue()


    println(s"$deleteddata is deleted")
    queue.traverse()
  }
}

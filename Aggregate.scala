

object Aggregate{

def main(args:Array[String]):Unit={
	val list1= List(1,2,3,4,5)
	val list2=List('a','b','c','d','e')
	/*var list3=List((list1(0),list2(0)))
	for(i<- 1 to list1.length-1) list3=list3:::List((list1(i),list2(i)))
	list3.map(println _)
	}*/

	val list3=recursiveAggregate(list1,list2)
	list3 map (println _)

	}
	def recursiveAggregate(list1: List[Int], list2: List[Char]):List[(Int,Char)]={
			def recAgg(list1: List[Int], list2: List[Char], result:List[(Int,Char)],len:Int):List[(Int,Char)]={
				if(len==list1.length) result
				else recAgg(list1,list2,(list1(len),list2(len))::result,len+1)
			}

		recAgg(list1,list2,List[(Int,Char)]():+(list1(0),list2(0)),1)
	}

}

object SumMultiplication{
	
	def main(args:Array[String]):Unit={
	val list=List(1,2,3,4,5)
	val result=sumMul(list)
	println (result)
	}

def sumMul(list:List[Int]):(Int,Int)={
val sum = recSum(list,list.length-1)
val mul = recMul(list,list.length-1)
(sum,mul)
}
	

	def recSum(list:List[Int], len:Int):Int={
			if(len<0) 0
			else list(len)+recSum(list,len-1)
	}

	def recMul(list:List[Int], len:Int):Int={
			if(len<0) 1
			else
			list(len)*recMul(list,len-1)
			
	}

}

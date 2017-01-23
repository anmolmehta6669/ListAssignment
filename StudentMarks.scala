


case class ScoreCard(studentId: Long, marks: Map[Long, Float], percentage: Float)

object StudentMarks {

  def main(args: Array[String]): Unit = {
    val studentsList = List(StudentCase(1, "Anmol",Gender.male), StudentCase(2, "Archana",Gender.female), StudentCase(3, "Saniya",Gender.female),
      StudentCase(4, "Babbar",Gender.male), StudentCase(5, "Dolly",Gender.female), StudentCase(6, "Jassi",Gender.female), StudentCase(7, "Shubham",Gender.male),
      StudentCase(8, "Anuja",Gender.female), StudentCase(9, "Prerna",Gender.female), StudentCase(10, "Shubham",Gender.male))
    val marksList = List(MarksCase(1, 1, 100), MarksCase(1, 2, 100), MarksCase(1, 3, 67), MarksCase(1, 4, 70), MarksCase(1, 5, 80),
      MarksCase(1, 6, 40), MarksCase(1, 7, 76), MarksCase(1, 8, 70), MarksCase(1, 9, 96), MarksCase(1, 10, 56),
      MarksCase(2, 1, 90), MarksCase(2, 2, 100), MarksCase(2, 3, 90), MarksCase(2, 4, 95), MarksCase(2, 5, 60),
      MarksCase(2, 6, 80), MarksCase(2, 7, 66), MarksCase(2, 8, 80), MarksCase(2, 9, 76), MarksCase(2, 10, 86),
      MarksCase(3, 1, 85), MarksCase(3, 2, 80), MarksCase(3, 3, 80), MarksCase(3, 4, 80), MarksCase(3, 5, 80),
      MarksCase(3, 6, 80), MarksCase(3, 7, 80), MarksCase(3, 8, 80), MarksCase(3, 9, 80), MarksCase(3, 10, 80),
      MarksCase(4, 1, 60), MarksCase(4, 2, 60), MarksCase(4, 3, 60), MarksCase(4, 4, 60), MarksCase(4, 5, 60),
      MarksCase(4, 6, 60), MarksCase(4, 7, 70), MarksCase(4, 8, 60), MarksCase(4, 9, 60), MarksCase(4, 10, 60),
      MarksCase(5, 1, 90), MarksCase(5, 2, 85), MarksCase(5, 3, 80), MarksCase(5, 4, 90), MarksCase(5, 5, 95),
      MarksCase(5, 6, 50), MarksCase(5, 7, 90), MarksCase(5, 8, 70), MarksCase(5, 9, 74), MarksCase(5, 10, 60)
    )
      val count1 = passCount(marksList, 1, 90, "pass")
    println(s"Number of students passed = $count1")
    val count2 = passCount(marksList, 2, 90, "fail")
    println(s"Number of students failed = $count2")
    val top5 = subjectTopBottom(marksList, studentsList, 1, 5, "top")
    println("The top 5 students are")
    top5.map(println _)
    val bottom5 = subjectTopBottom(marksList, studentsList, 1, 5, "Bottom")
    println("The bottom five are")
    bottom5.map(println _)
    println("the two overall toppers are")
    overAllTopBottom(marksList,studentsList,2, "Top").map(x=> println(s"${x._1} : ${x._2}%"))
    println("the two overall least scorers are")
    overAllTopBottom(marksList,studentsList,2, "Bottom").map(x=>println(s"${x._1} : ${x._2}%"))
    val amount:List[Double]=List(2000,500)
    val scholarsList=scholarship(marksList,studentsList,80,amount)
    println("The scholarship details are: ")
    scholarsList.map(println _)
    val passed=passFail(marksList,studentsList,"pass",70)
    println("The passed students are: ")
    passed.map(println _)
    val failed=passFail(marksList,studentsList,"Fail",70)
    println("The failed students are: ")
    failed.map(println _)
    val scorers=scoredAbove(marksList,studentsList,85)
    println(s"Students scoring above 85% are: ")
    scorers.map(println _)
    val reportCard=reportCardGenerator(marksList,studentsList)
    reportCard.map(println _)
    //val res=reportCard.flatMap(x=> Some(x))
    //res.map(println _)

    val resultScoreCard = collectionExample(marksList)
    val scoreCard=(resultScoreCard.map(x=>ScoreCard(x._1,x._2,x._3)))
    scoreCard.map(println _)
    val result=mapping(scoreCard,studentsList)
    result.map(println _)
    searchByName("shubham",result)
    //val x: List[ScoreCard] = List(ScoreCard(1, Map(5L -> 30.0F), 50.0F))
  }





  /**
    * 1)
    * Input:- (subjectId, percentage, pass/fail)
    * Output:- for input pass, evaluate that how much students(id, name) are passed in the inputted subjectId
    * for input fail, evaluate that how much students(id, name) are failed in the inputted subjectId
    * Note:- percentage is the input which defines the minimum passing criteria
    * e.g.
    * Pass count: 15
    * Fail count: 10
    */
  def passCount(marksList: List[MarksCase], id: Int, percent: Int, choice: String): Int = {
    choice match {
      case "pass" => marksList.flatMap(x => if (x.subjectId == id && x.marks > percent) Some(x) else None).size
      case "fail" => marksList.flatMap(x => if (x.subjectId == id && x.marks < percent) Some(x) else None).size
    }
  }

  /**
    * 2)
    * Input:- (subjectId, count, top/bottom)
    * Output:- based on the last input(top/bottom), output the students details who have scored max/min in that subjectId
    * e.g.
    * input: 1 5 top
    * output:
    * Kunal 85
    * Himanshu 84
    * Geetika 83
    * Anmol 82
    * Mahesh 81
    */
  def subjectTopBottom(marksList: List[MarksCase], studentsList: List[StudentCase], subjectId: Int, count: Int, choice: String): List[(String, Float)] = {
    choice.toLowerCase match {
      case "top" => {
        val result = calculateSubjectTopBottom(marksList, studentsList, subjectId, count)
        result.sortBy(x => x._2).reverse
      }

      case "bottom" => {
        val result = calculateSubjectTopBottom(marksList, studentsList, subjectId, count)
        result.sortBy(x => x._2)
      }
    }
  }

  def aggregate(list1: List[Float], list2: List[String]): List[(String, Float)] = {
    def recAgg(list1: List[Float], list2: List[String], result: List[(String, Float)], len: Int): List[(String, Float)] = {
      if (len == list1.length) result
      else recAgg(list1, list2, (list2(len), list1(len)) :: result, len + 1)
    }
    recAgg(list1, list2, List[(String, Float)]() :+(list2(0), list1(0)), 1)
  }

  def calculateSubjectTopBottom(marksList: List[MarksCase], studentsList: List[StudentCase], subjectId: Int, count: Int): List[(String, Float)] = {
    val tempList1 = marksList.flatMap(x => if (x.subjectId == subjectId) Some(x) else None).sortBy(x => x.marks).take(count)
    val result1 = tempList1.map(x => x.marks)
    val result2 = for (temp2 <- tempList1; temp1 <- studentsList if (temp1.id == temp2.studentId)) yield temp1.name
    aggregate(result1, result2)
  }


  /**
    * 3)
    * Input:-
    * (top/bottom, count)
    * OutPut:-
    * Overall top/least scorer based on all the subjects score, fetch students name
    * count- input defines that how much students name are to be printed on console
    * e.g.
    * input: top 2

    * output:
    * Himanshu 75%
    * Geetika 74%

    */
  def overAllTopBottom(marksList: List[MarksCase], studentList: List[StudentCase], count: Int, choice: String): List[(String, Float)] = {
    choice.toLowerCase match {
      case "top" => val result = calculationForOverAll(marksList, studentList)
        result.sortBy(x => x._2).reverse.take(count)
      case "bottom" => val result = calculationForOverAll(marksList, studentList)
        result.sortBy(x => x._2).take(count)
    }
  }


  def calculationForOverAll(marksList: List[MarksCase], studentList: List[StudentCase]): List[(String, Float)] = {
    val tempPair1 = marksList.groupBy(x => x.studentId) //Key value pair having studentId as key & list of list of MarksCase as value
    val tempList1 = tempPair1.map(x => (x._1, x._2.map(y => y.marks).sum)).toList // Creating a list having tuples of total marks with associated studentID
    val result = for (x <- tempList1; y <- studentList if (x._1 == y.id)) yield (y.name, (x._2) / 5) // combining the names and marks to get a new list of toppers
    result
  }

  /**
    * 4)
    * Input:-
    * (percentage, good_scholarship, normal_or_no_scholarship)
    * Output:- two groups of students with the amount of scholarship
    * e.g.
    * input: 85% 2000 500
    * output:
    * Kunal 2000
    * Himanshu 500
    * Geetika 2000
    * Mahesh 500

    */

  def scholarship(marksList: List[MarksCase], studentsList: List[StudentCase], percent: Float, amount: List[Double]): List[(String, Double)] = {
    val tempList = calculationForOverAll(marksList, studentsList)
    val result = tempList.map(x => if (x._2 >= percent) (x._1, amount(0)) else (x._1, amount(1)))
    result
  }

  /**
    * 5)
    * Input:-
    * (pass/fail, percentage)
    * count and print the number of students and all names who are passed/fail,
    * Pass or fail would be decided by percentage input field.
    * e.g.
    * input: fail 30
    * output:
    * Kunal 28%
    * Himanshu 29%
    */
  def passFail(marksList: List[MarksCase], studentsList: List[StudentCase], choice: String, percent: Float): List[(String, Float)] = {
    choice.toLowerCase match {
      case "pass" => {
        val result = calculationForOverAll(marksList, studentsList)
        result.flatMap(x => if (x._2 >= percent) Some(x) else None)
      }
      case "fail" => {
        val result = calculationForOverAll(marksList, studentsList)
        result.flatMap(x => if (x._2 < percent) Some(x) else None)
      }
    }
  }

  /**
    * 6) Find the student(s) who have scored 95% or above and print its details.
    * input: 95%
    * output:
    * Kunal 95%
    * Himanshu 96%
    * Geetika 97%
    */
  def scoredAbove(marksList: List[MarksCase], studentsList: List[StudentCase], percent: Float): List[(String, Float)] = {
    val result = calculationForOverAll(marksList, studentsList)
    result.flatMap(x => if (x._2 >= percent) Some(x) else None)
  }


  /**
    * 7) For every student, find its marks in detail (just like detailed Report card of a student.)
    * Note:- must use groupBy method of List
    * input: reportcard
    * output:
    * Kunal 75 70 80 75 75%
    * Himanshu 74 70 81 75 75%
    * Geetika 70 70 85 75 75%
    */

  def reportCardGenerator(marksList: List[MarksCase], studentList: List[StudentCase]): List[(String, List[Float])] = {
    val tempPair1 = marksList.groupBy(x => x.studentId) //Key value pair having studentId as key & list of list of MarksCase as value
    val tempList1 = tempPair1.map(x => (x._1, x._2.map(y => y.marks))).toList // Creating a list having tuples of total marks with associated studentID
    val result = for (x <- tempList1; y <- studentList if (x._1 == y.id)) yield (y.name, x._2) // combining the names and marks
    result
  }


  /**
    * Q1. Now, I require a case class named ScoreCard having fields (studentId: Long, marks: Map[Long, Float], percentage: Float).

    * Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef].

    * Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them other wise print "No data found". The print should be in increasing order of the student id.
    */

  def searchByName(name:String,map_ScoreCard:Map[String,AnyRef]){
    val result=for(x<-map_ScoreCard if(x._1.equalsIgnoreCase(name))) yield x._2
    if(!result.isEmpty) result.map(println _)
    else println("Name not found")
  }


  def mapping(scorelist: List[ScoreCard],namelist: List[StudentCase]): Map[String,AnyRef] =
  {
    val tempList=for(x<-scorelist;y<-namelist if(x.studentId==y.id)) yield (y.name,x)
    val temp=tempList.groupBy(x=>x._1)
    temp.map(x=> (x._1,x._2.map(y=>y._2)))
  }

  def collectionExample(marksList: List[MarksCase]): List[(Long, Map[Long, Float], Float)] = {
    {
      val tempPair1 = marksList.groupBy(x => x.studentId) //Key value pair having studentId as key & list of list of MarksCase as value
    val tempMap = tempPair1.map(x => (x._1, (x._2.map(y => (y.subjectId, y.marks))).toMap, x._2.map(y => y.marks).sum / 5))
      tempMap.toList
    }
  }

}













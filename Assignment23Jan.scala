/*
Q1. Now, I require a case class named ScoreCard having fields (studentId: Long, marks: Map[Long, Float], percentage: Float).

Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef]. 

Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them other wise print "No data found". The print should be in increasing order of the student id.
*/

case class Student(id:Long, name: String)
case class Marks(subjectId:Long,studentId: Long, marksObtained:Double)

case class  ScoreCard(studentId: Long, marks:Map[Long, Double],percentage: Double)

object Assignment23Jan extends App{

//Static Inputs 
val students:List[Student]= List(Student(1, "Kunal"),Student(6,"Himanshu"), Student(2, "Himanshu"), Student(3, "Geetika"), Student(4, "Anuj"), Student(5,"Shubham"))

val mark:List[Marks]=List(Marks(1, 1, 95), Marks(2, 1, 75),Marks(3, 1, 75), Marks(4, 1, 65),Marks(5, 1, 85), Marks(1, 2, 75),Marks(2, 2, 95), Marks(3, 2, 75),Marks(4, 2, 95), Marks(5, 2, 75),Marks(1,3,90),Marks(2, 3, 95), Marks(3, 3, 75),Marks(4, 3, 75), Marks(5, 3, 65),Marks(1, 4, 85), Marks(2, 4, 75),Marks(3, 4, 95), Marks(4, 4, 75),Marks(5, 4, 95), Marks(1, 5, 75),Marks(2,5,90),Marks(3,5,67),Marks(4,5,78),Marks(5,5,71),Marks(1, 6, 75),Marks(2,6,90),Marks(3,6,67),Marks(4,6,78),Marks(5,6,71))

//method to generate map for students
def generateMap()={
val res=students.map{std=>
val v1=mark.filter(x=>x.studentId==std.id).map(z=>(z.subjectId,z.marksObtained) )
val temp=v1.sorted.toMap 
val temp_res=mark.filter(x=>x.studentId==std.id).map(z=>z.marksObtained)
val result=Map(std.name->ScoreCard(std.id,temp,temp_res.sum/5))
println(result)
}
}


//Method to print scorecards of student
def printScoreCard(name:String)={
val list_id=students.filter(x=>x.name==name)
list_id.map{ls=>
               val temp_res=mark.filter(x=>x.studentId==ls.id).map(z=>z.marksObtained)
               val per=temp_res.sum/5
                       
               print(ls.name+"   "+ls.id+"   ")
               temp_res.map(x=>print("  "+x))
               print("    "+per+"%")
               println(" ")
               
               
}
}

generateMap()
printScoreCard("Himanshu")
}

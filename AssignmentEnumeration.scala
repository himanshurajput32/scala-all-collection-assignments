import scala.collection.mutable.ListBuffer
object Gender extends Enumeration {
type Gender=Value
val male,female= Value
}
import Gender._
case class Student(id:Long, name: String,gender:Gender.Value)
case class Marks(subjectId:Long,studentId: Long, marksObtained:Double)

case class  ScoreCard(studentId: Long, marks:Map[Long, Double],percentage: Double)

object AssignmentEnumeration extends App{
//Static Inputs 
val students:List[Student]= List(Student(1, "Kunal",male),Student(6,"Himanshu",male), Student(2, "Himanshu",male), Student(3, "Geetika",female), Student(4, "Anuj",female), Student(5,"Shubham",female))

val mark:List[Marks]=List(Marks(1, 1, 95), Marks(2, 1, 75),Marks(3, 1, 75), Marks(4, 1, 65),Marks(5, 1, 85), Marks(1, 2, 75),Marks(2, 2, 95), Marks(3, 2, 75),Marks(4, 2, 95), Marks(5, 2, 75),Marks(1,3,90),Marks(2, 3, 95), Marks(3, 3, 75),Marks(4, 3, 75), Marks(5, 3, 65),Marks(1, 4, 85), Marks(2, 4, 75),Marks(3, 4, 95), Marks(4, 4, 75),Marks(5, 4, 95), Marks(1, 5, 75),Marks(2,5,90),Marks(3,5,67),Marks(4,5,78),Marks(5,5,71),Marks(1, 6, 75),Marks(2,6,90),Marks(3,6,67),Marks(4,6,78),Marks(5,6,71))

def getScoreCardByGender():(List[ScoreCard],List[ScoreCard])={
val male_buffer=new ListBuffer[ScoreCard]
val female_buffer=new ListBuffer[ScoreCard]
val male_list=students.filter{std=>std.gender==male}
val female_list=students.filter(std=>std.gender==female)  
val res= male_list.map{std=>

              val v1=mark.filter(x=>x.studentId==std.id).map(z=>(z.subjectId,z.marksObtained) )
              val temp=v1.sorted.toMap

              val temp1=mark.filter(x=>x.studentId==std.id).map(z=>z.marksObtained)
            val per=temp1.sum/5
                 
               male_buffer += ScoreCard(std.id,temp,per)
                
                     }
               
val res_1= female_list.map{std=>

val v1=mark.filter(x=>x.studentId==std.id).map(z=>(z.subjectId,z.marksObtained) )
val temp=v1.sorted.toMap

val temp1=mark.filter(x=>x.studentId==std.id).map(z=>z.marksObtained)
val per=temp1.sum/5
female_buffer += ScoreCard(std.id,temp,per)

}        


(male_buffer.toList,female_buffer.toList)
}

println(getScoreCardByGender())
}



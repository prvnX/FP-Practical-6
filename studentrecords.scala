import scala.io.StdIn.readLine
object studentrecords {
def getStudentInfo(): (String, Int, Int, Double, Char) = {
    val name = readLine("Enter student name: ").trim
    val marks = readLine("Enter marks obtained: ").toInt
    val totalMarks = readLine("Enter total possible marks: ").toInt
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
    (name, marks, totalMarks, percentage, grade)
  }

def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
        val (name, marks, totalMarks, percentage, grade) = student
        println(s"Student Name: $name")
        println(s"Marks Obtained: $marks")
        println(s"Total Marks: $totalMarks")
        println(s"Percentage: $percentage")
        println(s"Grade: $grade")
}
def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be between 0 and total possible marks."))
    } else {
      (true, None)
    }
}
def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var (name, marks, totalMarks, percentage, grade)=getStudentInfo()
    var isValidInput = validateInput(name, marks, totalMarks)
    var valid = isValidInput._1
    var error = isValidInput._2
    if(valid){
        (name, marks, totalMarks, percentage, grade)
    }
    else{
        println(error.get)
        getStudentInfoWithRetry()
    }
}
def main(args: Array[String]): Unit = {
    val student = getStudentInfoWithRetry()
    printStudentRecord(student)
}
}
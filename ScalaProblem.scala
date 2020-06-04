package

import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap


object ScalaProblem {

  def main(args: Array[String]): Unit = {
    val actualIntList = List(1,2,3,4,5,6,7)
    val actualstrList = List("str1","str2","str3","str4","str5","str6")
    println("Actual int list : "+actualIntList)
    println("Reversed int list : "+reverseList(actualIntList))

    println("Actual string list : "+actualstrList)
    println("Reversed string list : "+reverseList(actualstrList))

    val str = "YFFFFAAAHKOODDSS"
    println("Actual string passed to count the character count : "+str)
    println("String with its character count : "+charCountString(str))

    println("Again getting origional string as :"+countStringToString(charCountString(str)))

  }

  @tailrec
  def reverseList[A](list: List[A], result: List[A] = Nil): List[A] = list match {
    case Nil => result
    case (x :: xs) => reverseList(xs, x :: result)
  }

  def charCountString(str: String): String = {
    val charWithCount = str.groupBy(identity).mapValues(_.size)
    var finalStr = ""
    charWithCount.foreach { elmnt =>
      val key = elmnt._1.toString
      val count = elmnt._2
      var value = ""
      if (count == 1) {
        value = ""
      } else {
        value = elmnt._2.toString
      }
      finalStr = finalStr.concat(key + value)
    }
    finalStr
  }

  def countStringToString(str: String): String = {
    var finalStr = ""
    val strArray = str.toCharArray
    var charWithCount = LinkedHashMap[String, Int]()
    for (i <- 1 to strArray.size) {
      if (i != strArray.length) {
        if (strArray(i).isDigit) {
          val scndDigit = strArray(i).toString.toInt
          charWithCount += (strArray(i - 1).toString -> scndDigit)
        } else {
          charWithCount += (strArray(i - 1).toString -> 1)
        }
      } else {
        charWithCount += (strArray(strArray.length - 1).toString -> 1)
      }
    }
    charWithCount.foreach { el =>
      val ss = el._1
      if (el._1.forall(_.isDigit)) {
        charWithCount.remove(el._1)
      }
    }
    charWithCount.foreach { el =>
      finalStr = finalStr.concat(el._1 * el._2)
    }
    finalStr
  }

}

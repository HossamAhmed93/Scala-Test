package com.mlh.spraysample

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.io.IO
import com.mlh.spraysample.basic._
import spray.can.Http

object Main extends App {
  implicit val system = ActorSystem("spray-sample-system")

  /* Use Akka to create our Spray Service */
  val service = system.actorOf(Props[SpraySampleActor], "spray-sample-service")

  /* and bind to Akka's I/O interface */
  println("Starting Server .......")


  IO(Http) ! Http.Bind(service, system.settings.config.getString("app.interface"), system.settings.config.getInt("app.port"))

  /*
   * Description: Return factorial of number
   * PreCondition: Integer numbers
   * PostCondition: Return factorial of it
   * */
  def factorial(num:Int):Int={
    if (num <= 1)
      1
    else
      num * factorial(num-1)
  }

  ////////////////////////Second problem/////////////////
  /*
   * Description: This function takes strings and return if first chars like last chars or nope
   * PreCondition: String with any length
   * PostCondition: Return if first chars like last chars or nope
   * */
  def palindrome(str:String):Boolean = {
    val strLength = str.length()
    val strMiddle:Int = strLength/2;
    var returnValue:Boolean = false;
    if(strLength>2){
      for (loopCount <- 0 to strMiddle){
        if(str.charAt(loopCount)==str.charAt(strLength-loopCount-1))
          returnValue = true
        else
          return returnValue
      }
    }else if(strLength==1)
      returnValue == true
    returnValue
  }

  ////////////////////////Third problem/////////////////
  /*
   * Description: This encoding function that take string
   * PreCondition: String length must greater than 2
   * PostCondition: return string and its duplication numbers
   * */
  def runLengthEncode(str:String):String={
    var strChars:Char = str.charAt(0)
    var charsCount:Int = 1
    var returnedStr:String = ""

    for(i <- 1 to str.length()-1){
      if(str.charAt(i) == strChars){
        charsCount += 1
      }else{
        returnedStr += strChars+charsCount.toString()
        strChars = str.charAt(i)
        charsCount = 1
      }
      if(i == str.length()-1){
        returnedStr += strChars+charsCount.toString()
      }
    }
    returnedStr
  }
  /*
   * Description: This Decode function that take string contains chars and duplication num
   * PreCondition: String length must greater than 2 and contain one char and his duplication num
   * PostCondition: Return decoded string
   * */
  def runLengthDecode(str:String):String={
    var strChars:Char = str.charAt(0)
    var charNum:Int = 0
    var returnedStr:String = ""
    var loopCount:Int = 1;
    var innerLoopCondtion = true
    while(loopCount <= str.length()-1){
      for(j <- loopCount+1 to str.length()-1 if innerLoopCondtion){
        try {
          charNum = str.substring(loopCount, j).toInt
        } catch {
          case e: NumberFormatException => {loopCount = j-1
            innerLoopCondtion = false}
        }
      }
      innerLoopCondtion = true
      for(j<-1 to charNum)
        returnedStr+=strChars
      strChars = str.charAt(loopCount)
      loopCount+=1
    }
    returnedStr
  }
  ////////////////////////Fourth problem/////////////////
  /*
   * https://rosettacode.org/wiki/Function_composition#Scala
   * Description: This function takes 2 function of generic types (A) i know that from upper like from c++14 section
   * PreCondition:2 function has same return type
   * PostCondition:return f(g(x))
   * */
  def compose[A](f: A => A, g: A => A) = { x: A => f(g(x)) }
  def square(num:Int):Int={
    num*num
  }
  def inc(num:Int):Int={
    num+1
  }

}

/* Our Server Actor is pretty lightweight; simply mixing in our route trait and logging */
class SpraySampleActor extends Actor with SpraySampleService with ActorLogging {
  def actorRefFactory = context
  def receive = runRoute(spraysampleRoute)
}

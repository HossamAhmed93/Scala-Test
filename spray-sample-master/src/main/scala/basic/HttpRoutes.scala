package com.mlh.spraysample
package basic

import akka.util.Timeout
import org.json4s._
import org.json4s.native.JsonMethods.{compact, parse, render}
import spray.http.HttpResponse
import spray.http.StatusCodes._
import spray.httpx.Json4sSupport
import spray.json.{DefaultJsonProtocol, _}
import spray.routing._
import scala.concurrent.duration._

/* Used to mix in Spray's Marshalling Support with json4s */
object Json4sProtocol extends Json4sSupport {
  implicit def json4sFormats: Formats = DefaultFormats
}

/* Our case class, used for request and responses */
case class Data(enName: Option[String],arName: Option[String],state: Option[String],routingMethod: Option[String],
                logo: Option[String],coverPhoto: Option[String],enDescription: Option[String],arDescription: Option[String],
                shortNumber: Option[String],facebookLink: Option[String],twitterLink: Option[String],youtubeLink: Option[String],
                website: Option[String],onlinePayment: Option[Boolean],client: Option[Boolean],pendingInfo: Option[Boolean],
                pendingMenu: Option[Boolean],closed: Option[Boolean])
case class Restaurants (uuid:String, data: Data)
/* Our route directives, the heart of the service.
 * Note you can mix-in dependencies should you so chose */
object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val dataFormat = jsonFormat18(Data)
  implicit val restaurantsFormat = jsonFormat2(Restaurants)
}
trait SpraySampleService extends HttpService {
    val resourcesPath = getClass.getResource("/sample-restaurant-data.json").getPath
    var json_string = scala.io.Source.fromFile(resourcesPath.toString).getLines.mkString
    var json = parse(json_string)
    //println(json_string)
    import Json4sProtocol._

    //These implicit values allow us to use futures
    //in this trait.
    implicit def executionContext = actorRefFactory.dispatcher

    implicit val timeout = Timeout(5 seconds)

    //Our worker Actor handles the work of the request.
   // val worker = actorRefFactory.actorOf(Props[WorkerActor], "worker")

    val spraysampleRoute = {
      path("api"/"restaurant") {
        parameters("closed"){closed=>
          get {
            println("____________")
            println("get Restaurant?closed => Done")
            complete(HttpResponse(entity = close(json_string,closed.toInt)))
          }
        } ~
          post {
            respondWithStatus(Created) {
              entity(as[Restaurants]) { someObject =>
                //doCreate(someObject)
                println("____________")
                println("Post Restaurant => Done")
                import MyJsonProtocol._
                json_string = json_string.dropRight(1) + "," +someObject.toJson.toString() + ']'
                //println(json_string)
                complete(HttpResponse(entity = "true")
                )
              }
            }
          }~
          get {
            println("____________")
            println("get Restaurant => Done")
            complete(HttpResponse(entity = json_string))
          }
      } ~path("api"/"restaurant"/Segment) { name =>
          put {
            respondWithStatus(Created) {
              entity(as[Restaurants]) { someObject =>
                println("____________")
                println("Put Restaurant => Done")
                json_string = ChangeResData(json_string, name.toString, someObject)
                complete(HttpResponse(entity = "true")
                )
              }
            }

        }
      }
    }
  def close(json_string:String,closed:Int):String={
    val json = parse(json_string)
    var counter = 0
    var returnedStr = "["
    var check:Boolean = if (closed==1) true else false

    //returnedStr+=compact(render((json\"data")(counter))).toString+','
    var loopCondition:Boolean = true
    while(loopCondition){
      try {
        if (compact(render((json \ "data") (counter) \ "closed")).toBoolean == check) {
          returnedStr += compact(render((json) (counter))).toString + ','

        }
      }catch {
        case _: Throwable =>{
          loopCondition = false
        }
      }
      counter+=1
    }

    returnedStr=returnedStr.dropRight(1)
    returnedStr+=']'
    returnedStr
  }
  def ChangeResData(json_string:String,uuid:String,ReplaceDataJson:Restaurants): String ={

    val json = parse(json_string)
    var returnedStr = "["
    var counter = 0

    var loopCondition:Boolean = true
    while(loopCondition){
      try {
        if (compact(render((json \ "uuid") (counter))).toString != '"'+uuid+'"') {
          returnedStr+=compact(render((json) (counter))).toString + ','
        }else{
          import MyJsonProtocol._
          returnedStr+=ReplaceDataJson.toJson.toString()+','
        }

      }catch {
        case _: Throwable =>{
          loopCondition = false
        }
      }
      counter+=1
    }
    returnedStr=returnedStr.dropRight(1)
    returnedStr+=']'
    returnedStr
  }
  sealed abstract class JValue
  case object JNothing extends JValue // 'zero' for JValue
  case object JNull extends JValue
  case class JString(s: String) extends JValue
  case class JDouble(num: Double) extends JValue
  case class JDecimal(num: BigDecimal) extends JValue
  case class JInt(num: BigInt) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JObject(obj: List[JField]) extends JValue
  case class JArray(arr: List[JValue]) extends JValue

}

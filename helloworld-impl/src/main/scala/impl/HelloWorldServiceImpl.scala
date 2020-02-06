package impl


import akka.NotUsed
import api.HelloWorldService
import com.lightbend.lagom.javadsl.api.ServiceCall
import javax.inject.Inject
import play.api.libs.json.{JsObject, JsPath, JsResult, Json}
import play.api.mvc.{AnyContent, Request}

import scala.concurrent.{ExecutionContext, Future}


class HelloWorldServiceImpl @Inject()()(implicit ex: ExecutionContext) extends HelloWorldService {

  // Needed to convert some Scala types to Java
  import converter.ServiceCallConverter._
  override def hello(id: String): ServiceCall[NotUsed, String] = {rew => Future.successful("Hello " + id)}

  def compliance : ServiceCall[NotUsed, String] = { implicit request: Request[AnyContent] =>
    val r1 = randomToTwoPlaces
    val r2 = randomToTwoPlaces
    val r3 = randomToTwoPlaces
    val json = Json.parse (s"{\n\"compliance\": {\n\"gdpr\": $r1,\n\"unprotected-devices\": $r2, \n \"uninspectable-data\": ${r3}\n}\n}")
    Ok(Json.toJson(json))
  }

  def response(complianceType: String) : ServiceCall[NotUsed, String] = { _ =>

    val r1 = randomToTwoPlaces
    val r2 = randomToTwoPlaces
    val r3 = randomToTwoPlaces

    val path1 = JsPath \ "compliance" \ "gdpr"
    val path2 = JsPath \ "compliance" \ "unprotected-devices"
    val path3 = JsPath \ "compliance" \ "uninspectable-data"

    val json = Json parse ("{\n\"compliance\": {\n\"gdpr\": " + r1 + ",\n\"unprotected-devices\": " + r2 + ", \n \"uninspectable-data\": " + r3 + "\n}\n}")
    val jsonObj: JsObject = complianceType match {
      case "gdpr" =>
        var r: JsResult[JsObject] = path2.prune(jsonObj)
        r = path3.prune(r.get)
        r.get
      case "unprotected-devices" =>
        var r = path1.prune(jsonObj)
        r = path3.prune(r.get)
        r.get
      case "uninspectable-data" =>
        var r = path1.prune(jsonObj)
        r = path2.prune(r.get)
        r.get
      case "" => json.as[JsObject]
    }

    Ok(Json.toJson(jsonObj))
  }

  def randomToTwoPlaces: Double = {
    val ran = scala.util.Random
    val r = ran.nextDouble
    r - (r % 0.01)
  }
}


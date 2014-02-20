package controllers

import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import com.ning.http.client.FluentCaseInsensitiveStringsMap
import scala.collection.immutable.TreeMap
import scala.collection.JavaConverters
import play.core.utils.CaseInsensitiveOrdered

object Application extends Controller {
  import scala.collection.JavaConverters._

  def handleResponse(r: Response) = {
    var ok = new Status(r.status)(r.getAHCResponse.getResponseBodyAsBytes)
    var headers = ningHeadersToMap(r.getAHCResponse.getHeaders)

    headers -= "Transfer-Encoding"

    val via = headers.get("Via").getOrElse("")
    headers += "Via" -> Seq(via + " delayProxy")
    //headers += "Connection" -> Seq("close")

    headers.foreach(p => {
      ok = ok.withHeaders((p._1, p._2.mkString(" ")))
    })
    ok
  }

  def get(query: String) = Action.async(parse.anyContent) {
    request =>
      val host = request.host

      play.Logger.debug(request.method + " " + host + "/" + query)
//      play.Logger.debug(Play.application.configuration.getInt("http.port").getOrElse(0).toString)
      WS.WSRequestHolder(s"http://${host}/${query}", request.headers.toMap, request.queryString, None, None, None, None, None).
        get().map {
        r =>
          play.Logger.debug("return: " + host + "/" + query)
          handleResponse(r)
      }.recover {
        case ex: Throwable =>
          InternalServerError(ex.getMessage)
      }
  }

  def delete(query: String) = Action.async(parse.anyContent) {
    request =>
      val host = request.host

      play.Logger.debug(request.method + " " + host + "/")
      WS.WSRequestHolder(s"http://${host}/${query}", request.headers.toMap, request.queryString, None, None, None, None, None).
        delete().map {
        r =>
          play.Logger.debug("return: " + host + "/" + query)
          handleResponse(r)
      }.recover {
        case ex: Throwable =>
          InternalServerError(ex.getMessage)
      }
  }

  def post(query: String) = Action.async(parse.raw) {
    request =>

      val host = request.host
      play.Logger.debug(request.method + " " + host + "/" + query)

      WS.WSRequestHolder(s"http://${host}/${query}", request.headers.toMap, request.queryString, None, None, None, None, None).
        post(
          request.body.asFile
        ).map({
          r => handleResponse(r)
        }).recover {
        case ex: Throwable =>
          InternalServerError(ex.getMessage)
      }
  }

  def put(query: String) = Action.async(parse.raw) {
    request =>

      val host = request.host
      play.Logger.debug(request.method + " " + host + "/" + query)

      WS.WSRequestHolder(s"http://${host}/${query}", request.headers.toMap, request.queryString, None, None, None, None, None).
        put(
          request.body.asFile
        ).map({
        r => handleResponse(r)
      }).recover {
        case ex: Throwable =>
          InternalServerError(ex.getMessage)
      }
  }

  // copied from WSRequest
  def ningHeadersToMap(headers: FluentCaseInsensitiveStringsMap) = {
    val res = JavaConverters.mapAsScalaMapConverter(headers).asScala.map(e => e._1 -> e._2.asScala.toSeq).toMap
    //todo: wrap the case insensitive ning map instead of creating a new one (unless perhaps immutabilty is important)
    TreeMap(res.toSeq: _*)(CaseInsensitiveOrdered)
  }

}

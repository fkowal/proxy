package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.BodyParsers.parse
import play.api.libs.ws._
import scala.concurrent.{Promise, Future}
import scala.Option
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee.{Iteratee, Enumerator}
import com.ning.http.client.FluentCaseInsensitiveStringsMap
import scala.collection.immutable.TreeMap
import scala.collection.JavaConverters
import play.core.utils.CaseInsensitiveOrdered
import play.Logger.ALogger
import play.api.http.Writeable

object Application extends Controller {
  import scala.collection.JavaConverters._

  def get(query: String) = Action.async(parse.anyContent) { request=>

    play.Logger.debug("GET " +request.headers.get("host").getOrElse("")+"/"+query)

    request.headers.get("host") match {
      case Some(host) =>
        WS.WSRequestHolder(s"http://${host}/${query}", request.headers.toMap, request.queryString, None, None, None, None, None).
        get().map { r =>
        play.Logger.debug("return: "+request.headers.get("host").getOrElse("")+"/"+query)
        var ok = new Status(r.status)(r.getAHCResponse.getResponseBodyAsBytes)
        var h = Map[String, String]()
        var h2 = ningHeadersToMap(r.getAHCResponse.getHeaders)
        val headers = r.getAHCResponse.getHeaders

        h2 -= "Transfer-Encoding"

        val via = h2.get("Via").getOrElse("")
        h2 += "Via" -> Seq(via + " delayProxy")
        //h2 += "Connection" -> Seq("close")

        h2.foreach(p => { ok = ok.withHeaders((p._1, p._2.mkString(" ")))})
        ok
      }.recover {
        case ex: Throwable =>
          InternalServerError("some exception...")
      }
    }
  }

  def post(query: String) = Action.async(parse.raw) { request =>
    play.Logger.debug("POST " +request.headers.get("host").getOrElse("")+"/"+query)
    val contentType = request.contentType.getOrElse("")
    request.headers.get("host") match {
      case Some(host) =>
        //WS.url(s"http://${host}/${query}").
        WS.WSRequestHolder(s"http://${host}/${query}", request.headers.toMap, request.queryString, None, None, None, None, None).
      post(
        request.body.asFile
      ).map { r =>
        play.Logger.debug("POST return: "+request.headers.get("host").getOrElse("")+"/"+query)
        var ok = new Status(r.status)(r.getAHCResponse.getResponseBodyAsBytes)
        var h = Map[String, String]()
        var h2 = ningHeadersToMap(r.getAHCResponse.getHeaders)
        val headers = r.getAHCResponse.getHeaders

        h2 -= "Transfer-Encoding"

        val via = h2.get("Via").getOrElse("")
        h2 += "Via" -> Seq(via + " delayProxy")
        //h2 += "Connection" -> Seq("close")

        h2.foreach(p => { ok = ok.withHeaders((p._1, p._2.mkString(" ")))})
        ok
      }.recover {
        case ex: Throwable =>
          InternalServerError("some exception...")
      }
    }
  }

  // copied from WSRequest
  def ningHeadersToMap(headers: FluentCaseInsensitiveStringsMap) = {
    val res = JavaConverters.mapAsScalaMapConverter(headers).asScala.map(e => e._1 -> e._2.asScala.toSeq).toMap
    //todo: wrap the case insensitive ning map instead of creating a new one (unless perhaps immutabilty is important)
    TreeMap(res.toSeq: _*)(CaseInsensitiveOrdered)
  }

}

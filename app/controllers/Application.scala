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

object Application extends Controller {
  import scala.collection.JavaConverters._

  def index(query: String) = Action.async(parse.anyContent) { request=>
    play.Logger.debug(request.headers.get("host").getOrElse("")+"/"+query)

    request.headers.get("host") match {
      case Some(host) => WS.url(s"http://${host}/${query}").get().map { r =>
        //var ok = new Status(r.status)(r.body)
        var ok = Ok(r.getAHCResponse.getResponseBodyAsBytes)
        var h = Map[String, String]()
        var h2 = ningHeadersToMap(r.getAHCResponse.getHeaders)
        val headers = r.getAHCResponse.getHeaders

        h2 -= "Transfer-Encoding"

        val via = h2.get("Via").getOrElse("")
        h2 += "Via" -> Seq(via + " delayProxy")

        //val h3 = h2 + "Via" -> via + "delayProxy"
        h2.foreach(p => { ok = ok.withHeaders((p._1, p._2.mkString(" ")))})
        ok

      }
      .recover {
        case ex: Throwable =>
          InternalServerError("some exception...")
      }
    }
    //def addHeader(ok,)
    //Ok("got:"+name+request.headers.toString())
    //WS.url(request.headers.get('host'))
//    val body: AnyContent = request.body
//    val textBody: Option[String] = body.asText
//
//    // Expecting text body
//    textBody.map { text =>
//      Ok("Got: " + text)
//    }.getOrElse {
//      BadRequest("Expecting text/plain request body")
//    }
  }


  def ningHeadersToMap(headers: FluentCaseInsensitiveStringsMap) = {
    val res = JavaConverters.mapAsScalaMapConverter(headers).asScala.map(e => e._1 -> e._2.asScala.toSeq).toMap
    //todo: wrap the case insensitive ning map instead of creating a new one (unless perhaps immutabilty is important)
    TreeMap(res.toSeq: _*)(CaseInsensitiveOrdered)
  }


}
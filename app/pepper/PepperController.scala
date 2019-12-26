package pepper

import java.util.Base64

import javax.inject.{Inject, Singleton}
import play.api.{Configuration, Logger}
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Properties

@Singleton
class PepperController @Inject()(implicit cc: ControllerComponents,
                                 ws         : WSClient,
                                 config     : Configuration) extends AbstractController(cc) {

    private val logger = Logger(classOf[PepperController])

    val API_URL = config.getOptional[String]("api.url").getOrElse("")

    def index(query: String) = Action.async { implicit request =>
        Json.parse(new String(Base64.getDecoder.decode(query), "UTF-8")).validate[Seq[QueryInput]] fold(
          error => {
              logger.error("Error to parse query :\n" + JsError.toJson(error).toString())
              Future.successful(BadRequest)
          },
          queries => {
              val seq = ListBuffer.empty[QueryInput]
              seq ++= queries

              val init = seq.filter(_.depends.isEmpty)
              seq --= init
              val res = ListBuffer(init)

              while (seq.size > 0) {
                  val filter = seq.filter(f => res.last.exists(r => f.depends.map(_.on).getOrElse("").equals(r.key)))
                  res += filter
                  seq --= filter
              }

              serialiseFutures(res, request.headers.get("Authorization"))
              .map(response => {
                  if ((response \\ "error").size > 0) {
                      InternalServerError
                  } else {
                      Ok(response)
                  }
              })
          }
        )
    }

    def serialiseFutures(list: Seq[Seq[QueryInput]],
                         auth: Option[String]): Future[JsValue] = {
        Future {
            list.foldLeft(Json.obj()) { (acc,
                                         nextItem) => {
                val requests = nextItem.flatMap(q => {
                    val key = q.key

                    q.depends match {
                        case Some(depends) => {
                            val bind = depends.bind.getOrElse(depends.field)
                            val cumulate = depends.cumulate.getOrElse(false)
                            val dependency = (acc \ depends.on).get
                            if (dependency.isInstanceOf[JsArray]) {
                                val params = dependency.asInstanceOf[JsArray].value
                                                                             .map(v => (v \ depends.field).get)
                                                                             .filter(!_.isInstanceOf[JsNull.type]).map(_.toString())
                                if (cumulate) {
                                    Seq(createRequest(q.route.replace(":" + bind, params.mkString(",")), auth, key, cumulate))
                                } else {
                                    params.map(param => createRequest(q.route.replace(":" + bind, param), auth, key, cumulate))
                                }
                            } else {
                                Seq(createRequest(q.route.replace(":" + bind, (dependency \ depends.field).as[String]), auth, key, cumulate))
                            }
                        }
                        case None          => Seq(createRequest(q.route, auth, key, true))
                    }
                }
                )

                val result = Await.result(Future.sequence(requests), 3 minutes)
                result.zipWithIndex.foldLeft(acc) {
                    case (ac, (value, index)) => {
                        val (response, key, singleCall) = value.head
                        response.status match {
                            case OK  => {
                                if (singleCall) {
                                    ac ++ Json.obj(key -> response.json)
                                } else {
                                    val prepareArray = Json.arr(response.json)
                                    if (!prepareArray.value.isEmpty) {
                                        val newObj = (ac \ key).asOpt[JsArray] match {
                                            case Some(array) => Json.obj(key -> (array ++ prepareArray))
                                            case None        => Json.obj(key -> prepareArray)
                                        }
                                        ac ++ newObj
                                    } else {
                                        ac
                                    }
                                }
                            }
                            case err => {
                                logger.error(s"Error in ${key} : ${err} => ${response.body}")
                                ac ++ Json.obj("error" -> err)
                            }
                        }
                    }
                }
            }
            }
        }
    }

    private def createRequest(route     : String,
                              auth      : Option[String],
                              key       : String,
                              singleCall: Boolean): Future[Vector[(WSResponse, String, Boolean)]] = {

        val seqTuple: Vector[(Future[WSResponse], String, Boolean)] = Vector((auth
                                                                              .map(a => ws.url(API_URL + route).withHttpHeaders("Authorization" -> a))
                                                                              .getOrElse(ws.url(API_URL + route)).get(), key, singleCall))
        Future.traverse(seqTuple) {
            case (f, v, c) => {
                f.map(r => (r, v, c))
            }
        }
    }


}

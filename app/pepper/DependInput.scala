package pepper

import play.api.libs.json.Json

case class DependInput(on: String,
                       field: String,
                       bind: Option[String] = None,
                       cumulate: Option[Boolean] = None)

object DependInput {
    implicit val reads = Json.reads[DependInput]
}

package pepper

import play.api.libs.json.Json

case class QueryInput(key: String,
                      route: String,
                      depends: Option[DependInput] = None)

object QueryInput {
    implicit val reads = Json.reads[QueryInput]
}

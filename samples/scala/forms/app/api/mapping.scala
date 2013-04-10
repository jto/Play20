package play.api.data.validation2

import scala.language.implicitConversions
import Validations._

// This is almost a Mapping
trait Extractor[To]{
  def apply[Source](data: Source)(implicit m: Path => Mapping[String, Source, To]): Validation[String, To] //: Validation[(Path, Seq[String]), To]
}
sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode
case class Path(path: List[KeyPathNode] = List()) {

  import play.api.libs.functional.syntax._
  import Validation._

  def \(child: String): Path = this \ KeyPathNode(child)
  def \(child: KeyPathNode): Path = Path(path :+ child)

  def compose(p: Path): Path = Path(this.path ++ p.path)

  def validate[To]: Extractor[To] = validate(Constraints.noConstraint: Constraint[To])
  def validate[To](v: Constraint[To]): Extractor[To] = {
    val path = this
    new Extractor[To] {
      def apply[Source](data: Source)(implicit m: Path => Mapping[String, Source, To]) = {
        m(path)(data).flatMap(v)
      }
    }
  }

  def validate[To](sub: Extractor[To]): Extractor[To] = {
    val parent = this
    new Extractor[To] {
      def apply[Source](data: Source)(implicit m: Path => Mapping[String, Source, To]) =
        sub(data){ path => m(parent.compose(path)) }
    }
  }

  override def toString = "Path \\ " + path.mkString(" \\ ")

}
object Path extends Path(List.empty)

object Extractors {

  implicit def pickAll(p: Path): Mapping[String, Map[String, Seq[String]], Seq[String]] =
    _.get(p.path.map(_.key).mkString(".")).map(Success.apply[String, Seq[String]] _).getOrElse(Failure(Seq("validation.required")))

  implicit def pickFirst(p: Path): Mapping[String, Map[String, Seq[String]], String] =
    data => pickAll(p)(data).map(_.head)

  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, _ }
  private def pathToJsPath(p: Path): JsPath =
    JsPath(p.path.map(k => JSKeyPathNode(k.key)))

  implicit def pickJson(p: Path): Mapping[String, JsValue, Seq[JsValue]] = { json =>
    pathToJsPath(p)(json) match {
      case Nil => Failure(Seq("validation.required"))
      case js => Success(js)
    }
  }

  implicit def pickStringInJson(p: Path): Mapping[String, JsValue, String] = { json =>
    pickJson(p)(json).flatMap {
      case JsString(v) :: _ => Success(v)
      case _ => Failure(Seq("validation.type-mismatch"))
    }
  }
}

object Constraints {
  import scala.util.matching.Regex
  import Validation._

  def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
    v => if(!pred(v)) Failure(Seq(msg)) else Success(v)

  def nonEmptyText = validateWith("validation.notemptytext"){ !(_: String).isEmpty }
  def noConstraint[From]: Constraint[From] = Success(_)

}
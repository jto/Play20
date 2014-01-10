/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api.data.mapping

import org.specs2.mutable._
import play.api.libs.functional.syntax._

object FormatSpec extends Specification {
	case class User(id: Long, name: String)
	val luigi = User(1, "Luigi")

	"Format" should {

		"serialize and deserialize primitives" in {
			import Rules._
			import Writes._

			val f = Formatting[UrlFormEncoded] { __ =>
				(__ \ "id").format[Long]
			}

			val m = Map("id" -> Seq("1"))

      f.writes(1L) mustEqual(m)
      f.validate(m) mustEqual(Success(1L))

      (Path \ "id").from[UrlFormEncoded](f.toRule).validate(Map.empty) mustEqual(Failure(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize String" in {
			import Rules._
			import Writes._

			val f = Formatting[UrlFormEncoded] { __ =>
				(__ \ "id").format[String]
			}

			val m = Map("id" -> Seq("CAFEBABE"))

      f.writes("CAFEBABE") mustEqual(m)
      f.validate(m) mustEqual(Success("CAFEBABE"))

      (Path \ "id").from[UrlFormEncoded](f.toRule).validate(Map.empty) mustEqual(Failure(Seq(Path \ "id" -> Seq(ValidationError("error.required")))))
    }

    "serialize and deserialize Seq[String]" in {
			import Rules._
			import Writes._

			val f = Formatting[UrlFormEncoded] { __ => (__ \ "ids").format[Seq[String]] }
			val m = Map("ids[0]" -> Seq("CAFEBABE"), "ids[1]" -> Seq("FOOBAR"))

			f.validate(m) mustEqual(Success(Seq("CAFEBABE", "FOOBAR")))
      f.writes(Seq("CAFEBABE", "FOOBAR")) mustEqual(m)
    }

    "serialize and deserialize User case class" in {
    	import Rules._
    	import Writes._

	    implicit val userF: Format[UrlFormEncoded, User] = Formatting[UrlFormEncoded] { __ =>
				((__ \ "id").format[Long] ~
			   (__ \ "name").format[String])(User.apply _, unlift(User.unapply _))
			}

			val m = Map("id" -> Seq("1"), "name" -> Seq("Luigi"))
			userF.validate(m) mustEqual(Success(luigi))
		}

	}

}
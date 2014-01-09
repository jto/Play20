package play.api.data.mapping

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

@implicitNotFound("No Format found for types ${I}, ${O}. Try to implement an implicit Format[${I}, ${O}].")
trait Format[I, O] extends Rule[I, O] with Write[O, I]

/**
 * Default formatters.
 */
object Format extends DefaultFormat {

	import play.api.libs.functional._

  def apply[I, O](r: Rule[I, O], w: Write[O, I]): Format[I, O] = {
    new Format[I, O] {
      def validate(i: I) = r.validate(i)
      def writes(o: O) = w.writes(o)
    }
  }

  implicit def invariantFunctorFormat[I]: InvariantFunctor[({ type λ[O] = Format[I, O] })#λ] = new InvariantFunctor[({ type λ[O] = Format[I, O] })#λ] {
    def inmap[A, B](fa: Format[I, A], f1: A => B, f2: B => A): Format[I, B] =
    	Format[I, B](fa.fmap(f1), fa.contramap(f2))
  }
}

/**
 * Default formatters.
 */
trait DefaultFormat {
  implicit def GenericFormat[I, O](implicit r: Rule[I, O], w: Write[O, I]): Format[I, O] =
    Format(r, w)

  implicit def pickPath[I, O](p: Path)(implicit fr: Path => Rule[I, O], fw: Path => Write[O, I]): Format[I, O] =
  	Format(fr(p), fw(p))
}
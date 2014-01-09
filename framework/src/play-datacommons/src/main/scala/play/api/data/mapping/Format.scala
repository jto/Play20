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

  implicit def functionalCanBuildFormat[I](implicit rcb: FunctionalCanBuild[({ type λ[O] = Rule[I, O] })#λ], wcb: FunctionalCanBuild[({ type λ[O] = Write[O, I] })#λ]): FunctionalCanBuild[({ type λ[O] = Format[I, O] })#λ] =
    new FunctionalCanBuild[({ type λ[O] = Format[I, O] })#λ] {
      def apply[A, B](fa: Format[I, A], fb: Format[I, B]): Format[I, A ~ B] =
        Format[I, A ~ B](rcb(fa, fb), wcb(fa, fb))
    }

  // XXX: Helps the compiler a bit
  import play.api.libs.functional.syntax._
  implicit def fboF[I, O](implicit rcb: FunctionalCanBuild[({ type λ[O] = Rule[I, O] })#λ], wcb: FunctionalCanBuild[({ type λ[O] = Write[O, I] })#λ]) =
    toFunctionalBuilderOps[({ type λ[O] = Format[I, O] })#λ, O](_: Format[I, O])(functionalCanBuildFormat)

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
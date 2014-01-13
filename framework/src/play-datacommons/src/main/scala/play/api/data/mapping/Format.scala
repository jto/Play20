package play.api.data.mapping

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

@implicitNotFound("No Format found for types ${I}, ${O}. Try to implement an implicit Format[${I}, ${O}].")
trait Format[I, O] {
   def validate(data: I): VA[I, O]
   def writes(i: O): I

   def toRule = Rule[I, O](this.validate _)
   def toWrite = Write[O, I](this.writes _)

   // TODO
   // def bimap = ???
}

/**
 * Default formatters.
 */
object Format extends DefaultFormat {

  def apply[I, O](r: Rule[I, O], w: Write[O, I]): Format[I, O] = {
    new Format[I, O] {
      def validate(i: I) = r.validate(i)
      def writes(o: O) = w.writes(o)
    }
  }

  import play.api.libs.functional._

  implicit def invariantFunctorFormat[I]: InvariantFunctor[({ type λ[O] = Format[I, O] })#λ] = new InvariantFunctor[({ type λ[O] = Format[I, O] })#λ] {
    def inmap[A, B](fa: Format[I, A], f1: A => B, f2: B => A): Format[I, B] =
      Format[I, B](fa.toRule.fmap(f1), fa.toWrite.contramap(f2))
  }

  implicit def functionalCanBuildFormat[I: Monoid](implicit rcb: FunctionalCanBuild[({ type λ[O] = Rule[I, O] })#λ], wcb: FunctionalCanBuild[({ type λ[O] = Write[O, I] })#λ]): FunctionalCanBuild[({ type λ[O] = Format[I, O] })#λ] =
    new FunctionalCanBuild[({ type λ[O] = Format[I, O] })#λ] {
      def apply[A, B](fa: Format[I, A], fb: Format[I, B]): Format[I, A ~ B] =
        Format[I, A ~ B](rcb(fa.toRule, fb.toRule), wcb(fa.toWrite, fb.toWrite))
    }

  // XXX: Helps the compiler a bit
  import play.api.libs.functional.syntax._
  implicit def fboFormat[I: Monoid, O](f: Format[I, O])(implicit fcb: FunctionalCanBuild[({ type λ[O] = Format[I, O] })#λ]) =
    toFunctionalBuilderOps[({ type λ[O] = Format[I, O] })#λ, O](f)(fcb)

}

/**
 * Default formatters.
 */
trait DefaultFormat {
  // implicit def GenericFormat[I, O](implicit r: Rule[I, O], w: Write[O, I]): Format[I, O] =
  //   Format(r, w)

  // implicit def pickPath[I, O](p: Path)(implicit fr: Path => Rule[I, O], fw: Path => Write[O, I]): Format[I, O] =
  // 	Format(fr(p), fw(p))
}
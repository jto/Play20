package play.api.data.mapping

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

@implicitNotFound("No Format found for types ${I}, ${O}. Try to implement an implicit Format[${I}, ${O}].")
trait Format[I, O] extends RuleLike[I, O] with WriteLike[O, I] {

  def toRule = Rule.toRule(this)
  def toWrite = Write.toWrite(this)

   // TODO
   // def bimap = ???
}

/**
 * Default formatters.
 */
object Format {

  def apply[I, O](r: RuleLike[I, O], w: WriteLike[O, I]): Format[I, O] = {
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
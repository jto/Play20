package play.api.data.mapping

import scala.language.implicitConversions

trait Write[I, O] {
  def writes(i: I): O

  def map[B](f: O => B) = Write[I, B] {
    f compose (this.writes _)
  }
}

trait DefaultMonoids {
  import play.api.libs.functional.Monoid

  implicit def mapMonoid = new Monoid[Map[String, Seq[String]]] {
    def append(a1: Map[String, Seq[String]], a2: Map[String, Seq[String]]) = a1 ++ a2
    def identity = Map.empty
  }
}

trait DefaultWrites {
  import play.api.libs.functional.Monoid

  implicit def seq[I, O](implicit w: Write[I, O]) = Write[Seq[I], Seq[O]] {
    _.map(w.writes)
  }

  implicit def head[I, O](implicit w: Write[I, O]): Write[I, Seq[O]] = w.map(Seq(_))
}

object Writes extends DefaultWrites {

  import PM._

  implicit def anyval[T <: AnyVal] = Write((i: T) => i.toString)
  implicit def scalanumber[T <: scala.math.ScalaNumber] = Write((i: T) => i.toString)
  implicit def javanumber[T <: java.lang.Number] = Write((i: T) => i.toString)

  implicit def opm[O](implicit w: Write[O, M]) = Write[O, PM] {
    o => toPM(w.writes(o))
  }

  implicit def map[I](implicit w: Write[I, Seq[String]]) = Write[Map[String, I], PM] {
    m => toPM(m.mapValues(w.writes))
  }

  implicit def spm[O](implicit w: Write[O, PM]) =
    Write[Seq[O], PM]{ os =>
      os.zipWithIndex
        .toMap
        .flatMap{ case(o, i) =>
          repathPM(w.writes(o), (Path() \ i) ++ _)
        }
    }

  implicit def array[O](implicit w: Write[Seq[O], PM]) =
    Write((w.writes _) compose ((_: Array[O]).toSeq))

  implicit def traversable[O](implicit w: Write[Seq[O], PM]) =
    Write((w.writes _) compose ((_: Traversable[O]).toSeq))

  implicit def writeM[I](path: Path)(implicit w: Write[I, PM]) = Write[I, M] { i =>
    toM(repathPM(w.writes(i), path ++ _))
  }

  implicit def ospm[I](implicit w: Write[I, Seq[String]]) = Write[I, PM]{ i =>
    Map(Path() -> w.writes(i))
  }

  implicit def option[I](implicit w: Write[I, Seq[String]]) = Write[Option[I], PM] { m =>
    m.map(s => Map(Path() -> w.writes(s)))
     .getOrElse(Map.empty)
  }
}

object Write extends DefaultMonoids {

  def apply[I, O](w: I => O): Write[I, O] = new Write[I, O] {
    def writes(i: I) = w(i)
  }

  implicit def zero[I]: Write[I, I] = Write(identity[I] _)

  import play.api.libs.functional._
  implicit def functionalCanBuildWrite[O](implicit m: Monoid[O]) = new FunctionalCanBuild[({type f[I] = Write[I, O]})#f] {
    def apply[A, B](wa: Write[A, O], wb: Write[B, O]): Write[A ~ B, O] = Write[A ~ B, O] { (x: A ~ B) =>
      x match {
        case a ~ b => m.append(wa.writes(a), wb.writes(b))
      }
    }
  }

  implicit def contravariantfunctorOWrite[O] = new ContravariantFunctor[({type f[I] = Write[I, O]})#f] {
    def contramap[A, B](wa: Write[A, O], f: B => A): Write[B, O] = Write[B, O]( (b: B) => wa.writes(f(b)) )
  }

  import play.api.libs.functional.syntax._
  implicit def fbow[I, O](a: Write[I, O])(implicit m: Monoid[O]) = toFunctionalBuilderOps[({type f[I] = Write[I, O]})#f, I](a)
}

package scalapb.lenses

import scala.language.higherKinds
import scala.language.implicitConversions

object CompatLensImplicits {

  /** Implicit that adds some syntactic sugar if our lens watches a Seq-like collection. */
  class SeqLikeLens[U, A, Coll[A] <: collection.SeqLike[A, Coll[A]], P <: Path](
      val lens: Lens.Aux[U, Coll[A], P]
  ) extends AnyVal {
    type CBF = collection.generic.CanBuildFrom[Coll[A], A, Coll[A]]

    type PathToIndex = Path.Single[Path.Element.Apply[Int]]

    private def field(getter: Coll[A] => A
                    )(setter: (Coll[A], A) => Coll[A]
                    )(pather: Coll[A] => PathToIndex
                    )(implicit composer: Path.Composer[P, PathToIndex]): Lens.Aux[U, A, composer.Out] =
      lens.compose[A, PathToIndex](Lens[Coll[A], A, PathToIndex](getter)(setter)(pather))

    def apply(i: Int)(implicit cbf: CBF, composer: Path.Composer[P, PathToIndex]): Lens.Aux[U, A, composer.Out] =
      field(_.apply(i))((c, v) => c.updated(i, v))(_ => Path.Empty.appendApply(i))

    def head(implicit cbf: CBF, composer: Path.Composer[P, PathToIndex]): Lens.Aux[U, A, composer.Out] = apply(0)

    def last(implicit cbf: CBF, composer: Path.Composer[P, PathToIndex]): Lens.Aux[U, A, composer.Out] =
      field(_.last)((c, v) => c.updated(c.size - 1, v))(c => Path.Empty.appendApply(c.size - 1))

    def :+=(item: A)(implicit cbf: CBF) = lens.modify(_ :+ item)

    def :++=(item: scala.collection.GenTraversableOnce[A])(implicit cbf: CBF) =
      lens.modify(_ ++ item)

    def foreach(f: Lens[A, A] => Mutation[A])(implicit cbf: CBF): Mutation[U] =
      lens.modify(s =>
        s.map { (m: A) =>
          val field = Lens.unit[A]
          val p: Mutation[A]    = f(field)
          p(m)
        }
      )
  }

  /** Implicit that adds some syntactic sugar if our lens watches a Set-like collection. */
  class SetLikeLens[U, A, Coll[A] <: collection.SetLike[A, Coll[A]] with Set[A], P <: Path](
      val lens: Lens.Aux[U, Coll[A], P]
  ) extends AnyVal {
    type CBF = collection.generic.CanBuildFrom[Coll[A], A, Coll[A]]

    def :+=(item: A) = lens.modify(_ + item)

    def :++=(item: scala.collection.GenTraversableOnce[A]) =
      lens.modify(_ ++ item)

    def foreach(f: Lens[A, A] => Mutation[A])(implicit cbf: CBF): Mutation[U] =
      lens.modify(s =>
        s.map { (m: A) =>
          val field = Lens.unit[A]
          val p: Mutation[A]    = f(field)
          p(m)
        }
      )
  }
}

trait CompatLensImplicits {
  import CompatLensImplicits._
  implicit def seqLikeLens[U, A, Coll[A] <: collection.SeqLike[A, Coll[A]], P <: Path](
      lens: Lens.Aux[U, Coll[A], P]
  ) =
    new SeqLikeLens[U, A, Coll, P](lens)

  implicit def SetLikeLens[U, A, Coll[A] <: collection.SetLike[A, Coll[A]] with Set[A], P <: Path](
      lens: Lens.Aux[U, Coll[A], P]
  ) = new SetLikeLens[U, A, Coll, P](lens)
}

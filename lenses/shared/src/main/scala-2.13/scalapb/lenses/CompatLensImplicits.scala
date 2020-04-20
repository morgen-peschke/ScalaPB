package scalapb.lenses
import scala.collection.SeqOps
import scala.language.implicitConversions

object CompatLensImplicits {

  /** Implicit that adds some syntactic sugar if our lens watches a Seq-like collection. */
  class SeqLikeLens[U, A, CC[A] <: SeqOps[A, CC, CC[A]], P <: Path](val lens: Lens[U, CC[A], P]) extends AnyVal {
    type C = CC[A]
    type PathToIndex = Path.Single[Path.Element.Apply[Int]]

    private def field(getter: C => A
                    )(setter: (C, A) => C
                    )(pather: C => PathToIndex
                    )(implicit composer: Path.Composer[P, PathToIndex]): Lens[U, A, composer.Out] =
      lens.compose[A, PathToIndex](Lens[C, A, PathToIndex](getter)(setter)(pather))

    def apply(i: Int)(implicit ev: CC[A] =:= C, composer: Path.Composer[P, PathToIndex]): Lens[U, A, composer.Out] =
      field(_.apply(i))((c, v) => c.updated(i, v))(_ => Path.Empty.appendApply(i))

    def head(implicit ev: CC[A] =:= C, composer: Path.Composer[P, PathToIndex]): Lens[U, A, composer.Out] = apply(0)

    def last(implicit ev: CC[A] =:= C, composer: Path.Composer[P, PathToIndex]): Lens[U, A, composer.Out] =
      field(_.last)((c, v) => c.updated(c.size - 1, v))(c => Path.Empty.appendApply(c.size - 1))

    def :+=(item: A)(implicit ev: CC[A] =:= C) = lens.modify(_ :+ item)

    def :++=(item: IterableOnce[A])(implicit ev: CC[A] =:= C) =
      lens.modify(_ ++ item)

    def foreach(f: Lens[A, A, Path.Empty.type] => Mutation[A])(implicit ev: CC[A] =:= C): Mutation[U] =
      lens.modify(s =>
        s.map { (m: A) =>
          val field: Lens[A, A, Path.Empty.type] = Lens.unit[A]
          val p: Mutation[A]    = f(field)
          p(m)
        }
      )
  }

  /** Implicit that adds some syntactic sugar if our lens watches a Set-like collection. */
  class SetLens[U, A, CC[A] <: collection.immutable.SetOps[A, CC, CC[A]], P <: Path](val lens: Lens[U, CC[A], P])
      extends AnyVal {
    type C = CC[A]

    def :+=(item: A) = lens.modify(_ + item)

    def :++=(item: scala.collection.IterableOnce[A])(implicit ev: CC[A] =:= C) =
      lens.modify(_ ++ item)

    def foreach(f: Lens[A, A, Path.Empty.type] => Mutation[A])(implicit ev: CC[A] =:= C): Mutation[U] =
      lens.modify(s =>
        s.map { (m: A) =>
          val field: Lens[A, A, Path.Empty.type] = Lens.unit[A]
          val p: Mutation[A]    = f(field)
          p(m)
        }
      )
  }
}

trait CompatLensImplicits {
  import CompatLensImplicits._

  implicit def seqLikeLens[U, A, CC[A] <: SeqOps[A, CC, CC[A]], P <: Path](lens: Lens[U, CC[A], P]) =
    new SeqLikeLens(lens)

  implicit def setLens[U, A, CC[A] <: collection.immutable.SetOps[A, CC, CC[A]], P <: Path](
      lens: Lens[U, CC[A], P]
  ) =
    new SetLens(lens)
}

package scalapb.lenses

trait Lens[Container, A, P <: Path] extends Any {
  self =>

  /** path knows how to construct a path to some field of type `A` from a container */
  def path(c: Container): P

  /** get knows how to extract some field of type `A` from a container */
  def get(c: Container): A

  /** Represents an assignment operator.
    *
    * Given a value of type A, sets knows how to transform a container such that `a` is
    * assigned to the field.
    *
    * We must have get(set(a)(c)) == a
    */
  def set(a: A): Mutation[Container]

  /** alias to set */
  def :=(a: A) = set(a)

  /** Optional assignment.
    *
    * Given a `Some[A]`, assign the `Some`'s value to the field. Given `None`, the
    * container is unchanged.
    */
  def setIfDefined(aOpt: Option[A]): Mutation[Container] =
    c => aOpt.fold(c)(set(_)(c))

  /** Represent an update operator (like x.y += 1 ) */
  def modify(f: A => A): Mutation[Container] = c => set(f(get(c)))(c)

  /** Composes two lenses, this enables nesting.
    *
    * If our field of type A has a sub-field of type B, then given a lens for it
    * (other: Lens[A, B]) we can create a single lens from Container to B.
    */
  def compose[B, PB <: Path](other: Lens[A, B, PB])(implicit composer: Path.Composer[P, PB]): Lens[Container, B, composer.Out] = new Lens[Container, B, composer.Out] {
    def path(c: Container): composer.Out = composer.compose(self.path(c), other.path(self.get(c)))
    def get(c: Container): B = other.get(self.get(c))
    def set(b: B): Mutation[Container] = self.modify(other.set(b))
  }

  /** Given two lenses with the same origin, returns a new lens that can mutate both values
    * represented by both lenses through a tuple.
    */
  def zip[B, PB <: Path](other: Lens[Container, B, PB])(implicit zipper: Path.Zipper[P, PB]): Lens[Container, (A, B), zipper.Out] = new Lens[Container, (A, B), zipper.Out] {
    def path(c: Container): zipper.Out = zipper.zip(self.path(c), other.path(c))
    def get(c: Container): (A, B)           = (self.get(c), other.get(c))
    def set(t: (A, B)): Mutation[Container] = self.set(t._1).andThen(other.set(t._2))
  }
}

object Lens extends CompatLensImplicits {
  /* Create a Lens from getter and setter. */
  def apply[Container, A, P <: Path](
      getter: Container => A
  )(setter: (Container, A) => Container)(pather: Container => P): Lens[Container, A, P] = new Lens[Container, A, P] {
    def path(c: Container): P = pather(c)

    def get(c: Container): A = getter(c)

    def set(a: A): Mutation[Container] = setter(_, a)
  }

  /** This is the unit lens, with respect to the compose operation defined above. That is,
    * len.compose(unit) == len == unit.compose(len)
    *
    * More practically, you can view it as a len that mutates the entire object, instead of
    * just a field of it: get() gives the original object, and set() returns the assigned value,
    * no matter what the original value was.
    */
  def unit[U]: Lens[U, U, Path.Empty.type] = Lens(identity[U])((_, v) => v)(_ => Path.Empty)

  /** Implicit that adds some syntactic sugar if our lens watches an Option[_]. */
  implicit class OptLens[U, A, P <: Path](val lens: Lens[U, Option[A], P]) extends AnyVal {
    def inplaceMap(f: Lens[A, A, _ <: Path] => Mutation[A]): Mutation[U] =
      lens.modify(opt =>
        opt.map { (m: A) =>
          val field: Lens[A, A, Path.Empty.type] = Lens.unit[A]
          val p: Mutation[A]    = f(field)
          p(m)
        }
      )
  }

  /** Implicit that adds some syntactic sugar if our lens watches a Map[_, _]. */
  implicit class MapLens[U, A, B, P <: Path](val lens: Lens[U, Map[A, B], P]) extends AnyVal {
    def apply(key: A)(implicit composer: Path.Composer[P, Path.Single[Path.Element.Apply[A]]]): Lens[U, B, composer.Out] =
      lens.compose[B, Path.Single[Path.Element.Apply[A]]] {
        Lens[Map[A, B], B, Path.Single[Path.Element.Apply[A]]](_.apply(key)
        )((map, value) => map.updated(key, value)
        )(_ => Path.Empty.appendApply(key))
      }(composer)

    def :+=(pair: (A, B)) = lens.modify(_ + pair)

    def :++=(item: Iterable[(A, B)]) = lens.modify(_ ++ item)

    def foreach(f: Lens[(A, B), (A, B), _ <: Path] => Mutation[(A, B)]): Mutation[U] =
      lens.modify(s =>
        s.map { (pair: (A, B)) =>
          val field: Lens[(A, B), (A, B), Path.Empty.type] = Lens.unit[(A, B)]
          val p: Mutation[(A, B)]         = f(field)
          p(pair)
        }
      )

    def foreachValue(f: Lens[B, B, _ <: Path] => Mutation[B]): Mutation[U] =
      lens.modify(s =>
        s.map {
          case (k, m) =>
            val field: Lens[B, B, Path.Empty.type] = Lens.unit[B]
            val p: Mutation[B]    = f(field)
            (k, p(m))
        }
      )

    def mapValues(f: B => B): Mutation[U] = foreachValue(_.modify(f))
  }
}

/** Represents a lens that has sub-lenses. */
class ObjectLens[U, Container, P <: Path](val self: Lens[U, Container, P]) extends Lens[U, Container, P] {

  /** Creates a sub-lens */
  def field[A, P1 <: Path](lens: Lens[Container, A, P1])(implicit composer: Path.Composer[P, P1]): Lens[U, A, composer.Out] =
    self.compose(lens)(composer)

  /** Creates a sub-lens */
  def field[A, P1 <: Path](getter: Container => A)(setter: (Container, A) => Container)(pather: Container => P1)(implicit composer: Path.Composer[P, P1]): Lens[U, A, composer.Out] =
    field(Lens(getter)(setter)(pather))(composer)

  override def path(c: U): P = self.path(c)

  override def get(u: U): Container = self.get(u)

  override def set(c: Container): Mutation[U] = self.set(c)

  def update(ms: (Lens[Container, Container, Path.Empty.type] => Mutation[Container])*): Mutation[U] =
    u => set(ms.foldLeft[Container](get(u))((p, m) => m(Lens.unit[Container])(p)))(u)
}

trait Updatable[A] extends Any {
  self: A =>
  def update(ms: (Lens[A, A, Path.Empty.type] => Mutation[A])*): A =
    ms.foldLeft[A](self)((p, m) => m(Lens.unit[A])(p))
}

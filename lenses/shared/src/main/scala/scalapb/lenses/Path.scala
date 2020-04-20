package scalapb.lenses

sealed abstract class Path extends Product with Serializable
object Path {
  sealed abstract class Element
  object Element {
    final case class Field[Descriptor](field: Descriptor) extends Element
    final case class Apply[K](key: K)                     extends Element
  }

  case object Empty                                                   extends Path
  case class Single[E <: Element](element: E)                         extends Path
  case class Appended[P <: Path, E <: Element](parent: P, element: E) extends Path
  case class Zipped[P0 <: Path, P1 <: Path](p0: P0, p1: P1)           extends Path
  case class Composed[P <: Path, S <: Path](prefix: P, suffix: S)     extends Path

  trait Appender[P <: Path, E <: Element] {
    type Out <: Path
    def append(path: P, element: E): Out
  }
  trait LowPriorityAppenders {
    implicit def defaultAppender[P <: Path, E <: Element]: Appender.Aux[P, E, Appended[P, E]] =
      new Appender[P, E] {
        type Out = Appended[P, E]
        def append(path: P, element: E): Out = Appended[P, E](path, element)
      }
  }

  object Appender extends LowPriorityAppenders {
    type Aux[P0 <: Path, E0 <: Element, O <: Path] = Appender[P0, E0] {
      type Out = O
    }

    implicit def emptyAppender[E <: Element]: Appender.Aux[Empty.type, E, Single[E]] =
      new Appender[Empty.type, E] {
        type Out = Single[E]
        def append(path: Empty.type, element: E): Out = Single[E](element)
      }

    implicit def singleAppender[E0 <: Element, E1 <: Element]
        : Appender.Aux[Single[E0], E1, Appended[Single[E0], E1]] =
      new Appender[Single[E0], E1] {
        type Out = Appended[Single[E0], E1]
        def append(path: Single[E0], element: E1): Out = Appended[Single[E0], E1](path, element)
      }
  }

  trait Composer[P0 <: Path, P1 <: Path] {
    type Out <: Path
    def compose(p0: P0, p1: P1): Out
  }
  object Composer {
    type Aux[PA <: Path, PB <: Path, O <: Path] = Composer[PA, PB] {
      type Out = O
    }

    implicit def defaultComposer[P0 <: Path, P1 <: Path]: Composer.Aux[P0, P1, Composed[P0, P1]] =
      new Composer[P0, P1] {
        type Out = Composed[P0, P1]
        def compose(p0: P0, p1: P1): Out = Composed[P0, P1](p0, p1)
      }
  }

  trait Zipper[P0 <: Path, P1 <: Path] {
    type Out <: Path
    def zip(p0: P0, p1: P1): Out
  }
  object Zipper {
    type Aux[PA <: Path, PB <: Path, O <: Path] = Zipper[PA, PB] {
      type Out = O
    }

    implicit def defaultZipper[P0 <: Path, P1 <: Path]: Zipper.Aux[P0, P1, Zipped[P0, P1]] =
      new Zipper[P0, P1] {
        type Out = Zipped[P0, P1]
        def zip(p0: P0, p1: P1): Out = Zipped[P0, P1](p0, p1)
      }
  }

  implicit class PathOps[P0 <: Path](val p0: P0) extends AnyVal {
    def appendField[Descriptor](
        descriptor: Descriptor
    )(implicit appender: Appender[P0, Element.Field[Descriptor]]): appender.Out =
      appender.append(p0, Element.Field[Descriptor](descriptor))

    def appendApply[Key](
        key: Key
    )(implicit appender: Appender[P0, Element.Apply[Key]]): appender.Out =
      appender.append(p0, Element.Apply[Key](key))

    def compose[P1 <: Path](p1: P1)(implicit composer: Composer[P0, P1]): composer.Out =
      composer.compose(p0, p1)

    def zip[P1 <: Path](p1: P1)(implicit zipper: Zipper[P0, P1]): zipper.Out =
      zipper.zip(p0, p1)
  }
}

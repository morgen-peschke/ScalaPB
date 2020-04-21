package scalapb.lenses

import java.lang.reflect.Method

import utest._
import com.github.ghik.silencer.silent

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.WeakTypeTag

case class Person(firstName: String, lastName: String, age: Int, address: Address)
    extends Updatable[Person]

case class Address(street: String, city: String, state: String, residents: Seq[Person] = Nil)
    extends Updatable[Address]

case class Role(name: String, person: Person, replacement: Option[Person] = None)
    extends Updatable[Role]

case class MapTest(
    intMap: Map[Int, String] = Map.empty,
    nameMap: Map[String, Person] = Map.empty,
    addressMap: Map[Person, Address] = Map.empty
) extends Updatable[MapTest]

case class CollectionTypes(
    iSeq: collection.immutable.Seq[String] = Nil,
    vector: Vector[String] = Vector.empty,
    list: List[String] = Nil,
    sett: Set[String] = Set.empty
) extends Updatable[CollectionTypes]

case class ZipPathTest(
  direct: Person,
  indirect: Vector[Person]
)

case class NestedZipPathTest(people: ZipPathTest)

trait Render[A] {
  def render(a: A): String = describeUsing(new StringBuilder().append('_'), a).toString()
  def describeUsing(sb: StringBuilder, a: A): StringBuilder
}
object Render {
  def render[A](a: A)(implicit r: Render[A]): String = r.render(a)

  def renderType[T: WeakTypeTag]: String = implicitly[WeakTypeTag[T]].tpe.toString

  implicit val renderMethodField: Render[Path.Element.Field[Method]] =
    (sb: StringBuilder, element: Path.Element.Field[Method]) =>
      sb.append('.').append(element.field.getName)

  implicit val renderStringApply: Render[Path.Element.Apply[String]] =
      (sb: StringBuilder, element: Path.Element.Apply[String]) =>
        sb.append('(')
          .append('"')
          .append(element.key)
          .append('"')
          .append(')')

  implicit val renderIntApply: Render[Path.Element.Apply[Int]] =
      (sb: StringBuilder, element: Path.Element.Apply[Int]) =>
        sb.append('(')
          .append(element.key)
          .append(')')

  implicit val renderPersonApply: Render[Path.Element.Apply[Person]] =
    (sb: StringBuilder, element: Path.Element.Apply[Person]) =>
      sb.append('(')
        .append(element.key.lastName)
        .append(", ")
        .append(element.key.firstName)
        .append(')')

  implicit val renderEmpty: Render[Path.Empty.type] = (sb, _) => sb

  implicit def renderComposed[P <: Path, S <: Path](implicit renderPrefix: Render[P], renderSuffix: Render[S]): Render[Path.Composed[P, S]] =
    (sb: StringBuilder, path: Path.Composed[P, S]) =>
      renderSuffix.describeUsing(
        renderPrefix.describeUsing(sb, path.prefix),
        path.suffix
      )

  implicit def renderZipped[P0 <: Path, P1 <: Path](implicit renderP0: Render[P0], renderP1: Render[P1]): Render[Path.Zipped[P0, P1]] =
    new Render[Path.Zipped[P0, P1]] {
      override def render(path: Path.Zipped[P0, P1]): String = describeUsing(new StringBuilder(), path).toString()

      override def describeUsing(sb: StringBuilder, path: Path.Zipped[P0, P1]): StringBuilder = {
        val withOpenParen = sb.append('(').append('_')
        val withP0 = renderP0.describeUsing(withOpenParen, path.p0)
        val withSeparator = withP0.append(',').append('_')
        val withP1 = renderP1.describeUsing(withSeparator, path.p1)
        withP1.append(')')
      }
    }

  implicit def renderAppended[P <: Path, E <: Path.Element](implicit renderParent: Render[P], renderElement: Render[E]): Render[Path.Appended[P, E]] =
     (sb: StringBuilder, path: Path.Appended[P, E]) =>
       renderElement.describeUsing(
         renderParent.describeUsing(sb, path.parent),
         path.element
       )

  implicit def renderSingle[E <: Path.Element](implicit renderElement: Render[E]): Render[Path.Single[E]] =
    (sb: StringBuilder, path: Path.Single[E]) =>
      renderElement.describeUsing(sb, path.element)
}

@silent("discarded non-Unit value")
object SimpleTest extends TestSuite {
  /**
   * Helper to grab the named method.
   * This is very specific to this test, and doesn't care about overloads, etc
   */
  def lookupMethod[A: ClassTag](name: String): Method = {
    val cte = implicitly[ClassTag[A]]
    cte.runtimeClass.getMethods.find(_.getName == name).getOrElse {
      throw new NoSuchElementException(s"Unable to find method named <$name> in $cte")
    }
  }

  implicit class RoleMutation[U, P <: Path](f: Lens.Aux[U, Role, P]) extends ObjectLens[U, Role, P](f) {
    def name = field(_.name)((p, f) => p.copy(name = f))(_ => Path.Empty.appendField(lookupMethod[Role]("name")))

    def person = field(_.person)((p, f) => p.copy(person = f))(_ => Path.Empty.appendField(lookupMethod[Role]("person")))

    def replacement = field(_.replacement)((p, f) => p.copy(replacement = f))(_ => Path.Empty.appendField(lookupMethod[Role]("replacement")))
  }

  implicit class PersonMutation[U, P <: Path](f: Lens.Aux[U, Person, P]) extends ObjectLens[U, Person, P](f) {
    def firstName = field(_.firstName)((p, f) => p.copy(firstName = f))(_ => Path.Empty.appendField(lookupMethod[Person]("firstName")))

    def lastName = field(_.lastName)((p, f) => p.copy(lastName = f))(_ => Path.Empty.appendField(lookupMethod[Person]("lastName")))

    def address = field(_.address)((p, f) => p.copy(address = f))(_ => Path.Empty.appendField(lookupMethod[Person]("address")))
  }

  implicit class AddressLens[U, P <: Path](val f: Lens.Aux[U, Address, P]) extends ObjectLens[U, Address, P](f) {
    def city = field(_.city)((p, f) => p.copy(city = f))(_ => Path.Empty.appendField(lookupMethod[Address]("city")))

    def street = field(_.street)((p, f) => p.copy(street = f))(_ => Path.Empty.appendField(lookupMethod[Address]("street")))

    def residents = field(_.residents)((p, f) => p.copy(residents = f))(_ => Path.Empty.appendField(lookupMethod[Address]("residents")))
  }

  implicit class MapTestLens[U, P <: Path](val f: Lens.Aux[U, MapTest, P]) extends ObjectLens[U, MapTest, P](f) {
    def intMap = field(_.intMap)((p, f) => p.copy(intMap = f))(_ => Path.Empty.appendField(lookupMethod[MapTest]("intMap")))

    def nameMap = field(_.nameMap)((p, f) => p.copy(nameMap = f))(_ => Path.Empty.appendField(lookupMethod[MapTest]("nameMap")))

    def addressMap = field(_.addressMap)((p, f) => p.copy(addressMap = f))(_ => Path.Empty.appendField(lookupMethod[MapTest]("addressMap")))
  }

  implicit class CollectionTypesLens[U, P <: Path](val f: Lens.Aux[U, CollectionTypes, P])
      extends ObjectLens[U, CollectionTypes, P](f) {
    def iSeq = field(_.iSeq)((p, f) => p.copy(iSeq = f))(_ => Path.Empty.appendField(lookupMethod[CollectionTypes]("iSeq")))

    def vector = field(_.vector)((p, f) => p.copy(vector = f))(_ => Path.Empty.appendField(lookupMethod[CollectionTypes]("vector")))

    def list = field(_.list)((p, f) => p.copy(list = f))(_ => Path.Empty.appendField(lookupMethod[CollectionTypes]("list")))

    def sett = field(_.sett)((p, f) => p.copy(sett = f))(_ => Path.Empty.appendField(lookupMethod[CollectionTypes]("sett")))
  }

  implicit class ZipPathTestLens[U, P <: Path](val f: Lens.Aux[U, ZipPathTest, P]) extends ObjectLens[U, ZipPathTest, P](f) {
    def direct = field(_.direct)((p, f) => p.copy(direct = f))(_ => Path.Empty.appendField(lookupMethod[ZipPathTest]("direct")))

    def indirect = field(_.indirect)((p, f) => p.copy(indirect = f))(_ => Path.Empty.appendField(lookupMethod[ZipPathTest]("indirect")))
  }

  implicit class NestedZipPathTestLens[U, P <: Path](val f: Lens.Aux[U, NestedZipPathTest, P]) extends ObjectLens[U, NestedZipPathTest, P](f) {
    def people = field(_.people)((p, f) => p.copy(people = f))(_ => Path.Empty.appendField(lookupMethod[NestedZipPathTest]("people")))
  }

  object RoleMutation extends RoleMutation(Lens.unit)

  val mosh = Person(
    firstName = "Mosh",
    lastName = "Ben",
    age = 19,
    address = Address("Main St.", "San Jose", "CA")
  )
  val josh = Person(
    firstName = "Josh",
    lastName = "Z",
    age = 19,
    address = Address("Fremont", "Sunnyvale", "CA")
  )
  val chef = Role(name = "Chef", person = mosh)

  val mapTest = MapTest(
    intMap = Map(3        -> "three", 4 -> "four"),
    addressMap = Map(mosh -> Address("someStreet", "someCity", "someState"))
  )

  val zipPath = NestedZipPathTest(
    ZipPathTest(
      mosh,
      Vector(mosh, josh)
    )
  )

  val tests = Tests {
    "update should return an updated object" - {
      mosh.update(_.firstName := "foo") ==> (mosh.copy(firstName = "foo"))
    }

    "it should allow mutating nested fields" - {
      mosh.update(_.address.city := "Valejo") ==> (mosh.copy(
        address = mosh.address.copy(city = "Valejo")
      ))
    }

    "it should allow nested updates" - {
      mosh.update(
        _.address.update(
          _.city := "Valejo",
          _.street := "Fourth"
        )
      ) ==> (mosh.copy(address = mosh.address.copy(city = "Valejo", street = "Fourth")))
    }

    "it should allow replacing an entire field" - {
      val portland = Address("2nd", "Portland", "Oregon")
      mosh.update(_.address := portland) ==> (mosh.copy(address = portland))
    }

    "it should support an existing value for an optional set" - {
      mosh.update(_.firstName setIfDefined Some("foo")) ==> mosh.copy(firstName = "foo")
    }

    "it should support a non-existing value for an optional set" - {
      mosh.update(_.firstName setIfDefined None) ==> mosh
    }

    "it should allow adding to a sequence" - {
      mosh.update(_.address.residents :+= josh) ==> (mosh.copy(
        address = mosh.address.copy(residents = mosh.address.residents :+ josh)
      ))
    }

    "it should allow replacing a sequence" - {
      mosh.update(_.address.residents := Seq(josh, mosh)) ==> (mosh.copy(
        address = mosh.address.copy(residents = Seq(josh, mosh))
      ))
    }

    "it should allow mutating an element of a sequence by index" - {
      mosh.update(
        _.address.residents := Seq(josh, mosh),
        _.address.residents(1).firstName := "ModName"
      ) ==> (mosh.copy(
        address = mosh.address.copy(residents = Seq(josh, mosh.copy(firstName = "ModName")))
      ))
    }

    "it should allow mutating all element of a sequence with forEach" - {
      mosh.update(
        _.address.residents := Seq(josh, mosh),
        _.address.residents.foreach(_.lastName.modify(_ + "Suffix"))
      ) ==> (mosh.copy(
        address = mosh.address
          .copy(residents = Seq(josh.copy(lastName = "ZSuffix"), mosh.copy(lastName = "BenSuffix")))
      ))
    }

    "it should allow mapping over an option" - {
      chef.update(
        _.replacement.inplaceMap(_.firstName := "Zoo")
      ) ==> (chef)

      chef
        .update(
          _.replacement := Some(josh),
          _.replacement.inplaceMap(_.firstName := "Yosh")
        )
        .replacement
        .get ==> (josh.copy(firstName = "Yosh"))
    }

    "it should allow updating a map" - {
      mapTest.update(_.intMap(5) := "hello") ==> (mapTest.copy(
        intMap = mapTest.intMap.updated(5, "hello")
      ))
      mapTest.update(_.intMap(2) := "ttt") ==> (mapTest.copy(
        intMap = mapTest.intMap.updated(2, "ttt")
      ))
      mapTest.update(_.nameMap("mmm") := mosh) ==> (mapTest.copy(
        nameMap = mapTest.nameMap.updated("mmm", mosh)
      ))
      mapTest.update(_.addressMap(josh) := mosh.address) ==> (mapTest.copy(
        addressMap = mapTest.addressMap.updated(josh, mosh.address)
      ))
    }

    "it should allow nested updated in a map" - {
      mapTest.update(_.nameMap("mosh") := mosh, _.nameMap("mosh").firstName := "boo") ==> (mapTest
        .copy(nameMap = mapTest.nameMap.updated("mosh", mosh.copy(firstName = "boo"))))
    }

    "it should raise an exception on nested key update for a missing key" - {
      intercept[NoSuchElementException] {
        mapTest.update(
          _.nameMap("mosh").firstName := "Boo"
        )
      }
    }

    "it should allow transforming the map values with forEachValue" - {
      mapTest
        .update(
          _.nameMap("mosh") := mosh,
          _.nameMap("josh") := josh,
          _.nameMap.foreachValue(_.firstName := "ttt")
        )
        .nameMap
        .values
        .map(_.firstName)
        .toSeq ==> (Seq("ttt", "ttt"))
    }

    "it should allow transforming the map values with mapValues" - {
      mapTest
        .update(
          _.intMap.mapValues("hello " + _)
        )
        .intMap ==> (Map(3 -> "hello three", 4 -> "hello four"))

      mapTest
        .update(
          _.nameMap("mosh") := mosh,
          _.nameMap("josh") := josh,
          _.nameMap.mapValues(m => m.update(_.firstName := "*" + m.firstName))
        )
        .nameMap
        .values
        .map(_.firstName)
        .toSeq ==> (Seq("*Mosh", "*Josh"))
    }

    "it should allow transforming the map values with forEach" - {
      mapTest.update(_.intMap.foreach(_.modify(k => (k._1 - 1, "*" + k._2)))).intMap ==> Map(
        2 -> "*three",
        3 -> "*four"
      )
    }

    "it should support other collection types" - {
      val ct = CollectionTypes().update(
        _.iSeq := collection.immutable.Seq("3", "4", "5"),
        _.iSeq :+= "foo",
        _.iSeq :++= collection.immutable.Seq("6", "7", "8"),
        _.iSeq :++= Seq("6", "7", "8"),
        _.iSeq(5) := "11",
        _.vector := Vector("3", "4", "5"),
        _.vector :+= "foo",
        _.vector :++= collection.immutable.Seq("6", "7", "8"),
        _.vector :++= Seq("6", "7", "8"),
        _.vector(5) := "11",
        _.list := List("3", "4", "5"),
        _.list :+= "foo",
        _.list :++= collection.immutable.Seq("6", "7", "8"),
        _.list :++= Seq("6", "7", "8"),
        _.list(5) := "11",
        _.sett := Set("3", "4", "5"),
        _.sett :+= "foo",
        _.sett :++= collection.immutable.Seq("6", "7", "8"),
        _.sett :++= Seq("6", "7", "8")
      )
      val expected = Seq("3", "4", "5", "foo", "6", "11", "8", "6", "7", "8")
      ct.iSeq ==> expected
      ct.vector ==> expected
      ct.list ==> expected
    }

    "it should work with zipped lenses" - {
      CollectionTypes().update(k => k.list zip k.vector := ((List("3", "4"), Vector("x", "y")))) ==> CollectionTypes(
        list = List("3", "4"),
        vector = Vector("x", "y")
      )
    }

    def renderLens[U, A, P <: Path](base: U, lens: Lens.Aux[U, A, P])(implicit renderPath: Render[P]): String =
      Render.render(lens.path(base))

    "it should build the expected lens paths for simple lenses" - {
      renderLens(mosh, Lens.unit[Person].address) ==> "_.address"
    }

    "it should build the expected lens path for nested lenses" - {
      renderLens(mosh, Lens.unit[Person].address.city) ==> "_.address.city"
    }

    "it should build the expected lens path for apply selections" - {
      renderLens(mapTest, Lens.unit[MapTest].addressMap(mosh).street) ==> "_.addressMap(Ben, Mosh).street"
      renderLens(mapTest, Lens.unit[MapTest].intMap(3)) ==> "_.intMap(3)"
    }

    "it should build the expected lens path for zipped lenses" - {
      renderLens(
        mapTest,
        Lens.unit[MapTest].intMap(3) zip Lens.unit[MapTest].addressMap(mosh).street
      ) ==> "(_.intMap(3),_.addressMap(Ben, Mosh).street)"

      renderLens(
        zipPath,
        Lens.unit[NestedZipPathTest].people.compose {
          Lens.unit[ZipPathTest].direct zip Lens.unit[ZipPathTest].indirect(1)
        }
      ) ==> "_.people(_.direct,_.indirect(1))"
    }
  }
}

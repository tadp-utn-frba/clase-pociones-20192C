package tadp.pociones

object Pociones2 {

  type Persona
  type Nivel = Int

  case class Ingrediente(
    nombre: String,
    cantidad: Int,
    efectos: Seq[Efecto]
  )

  type Efecto = Niveles => Niveles

  case class Pocion(
    nombre: String,
    ingredientes: Seq[Ingrediente]
  ) {
    def efectos: Seq[Efecto] =
      ingredientes.flatMap(_.efectos)
  }

  case class Niveles(
    suerte: Nivel,
    convencimiento: Nivel,
    fuerza: Nivel
  ) {
    def map(operacion: Nivel => Nivel) = {
      copy(
        operacion(suerte),
        operacion(convencimiento),
        operacion(fuerza)
      )
    }

    def duplica = map(_ * 2)
    def alMenos7 = map(_ max 7)
  }

  val efectoLoco: Efecto =
    _.duplica.alMenos7

  val multijugos = Pocion("Multijugos", List(
    Ingrediente("Cuerno de Bicornio en Polvo",
      10,
      List(_.duplica, efectoLoco)
    ),
    Ingrediente("Sanguijuela hormonal",
      54,
      List(_.duplica, _.alMenos7)
    )
  ))

  def esHeavy(pocion: Pocion): Boolean =
    pocion.efectos.size >= 2

  object PocionHeavy {
    def unapply(pocion: Pocion):
      Option[(String, Seq[Ingrediente])] = {
      Option(pocion)
        .filter(esHeavy)
        .map(p => (p.nombre, p.ingredientes))
    }
  }

  val nombreDePocionHeavy: PartialFunction[Pocion, String] = {
    case PocionHeavy(nombre, _) => nombre
  }

  nombreDePocionHeavy
    .isDefinedAt(multijugos) //true

  private val aire = Pocion("aire", Seq())
  nombreDePocionHeavy
    .isDefinedAt(aire) //false

  val nombrePocionHeavyOption
    = nombreDePocionHeavy.lift

  nombrePocionHeavyOption(multijugos)
    // Some("multijugos")
  nombrePocionHeavyOption(aire)
    // None


  val nombrePocionHeavyConFallback = {
    val fallback: PartialFunction[Pocion, String] = {
      case Pocion(nombre, _) =>
        s"$nombre no es heavy"
    }
    nombreDePocionHeavy orElse fallback
  }



  nombrePocionHeavyConFallback(aire)
    //aire no es heavy

  def nombrePocionesHeavy(pociones: Seq[Pocion]) =
    pociones.collect(nombreDePocionHeavy)

//  val nombre: Pocion => String = _.nombre

  def nombrePocionesHeavy2(pociones: Seq[Pocion]) =
    pociones.filter(esHeavy).map(_.nombre)

}

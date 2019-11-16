package tadp.pociones

object Pociones {

  type Persona
  type Nivel = Int
  type Niveles = (Nivel, Nivel, Nivel)

  type Efecto = Niveles => Niveles

  val duplica: Efecto =
    mapNiveles(_ * 2)

//  def duplica(niveles: Niveles): Niveles =
//    mapNiveles(n => n * 2)(niveles)

  val alMenos7: Efecto = mapNiveles(_ max 7)

//  def alMenos7(niveles: Niveles): Niveles =
//    mapNiveles(n => n.max(7))(niveles)

  def mapNiveles(operacion: Nivel => Nivel)(niveles: Niveles) =
    (operacion(niveles._1),
      operacion(niveles._2),
      operacion(niveles._3))

  //Duplica y después al menos 7
  val efectoLoco: Efecto =
    duplica andThen alMenos7

  val efectoMasLoco: Efecto =
    duplica
      .andThen(mapNiveles(_ * 7))
      .andThen(mapNiveles(_ / 3))
      .andThen(mapNiveles(_.max(8)))

  val duplicarShorthand: Efecto = {
    case (suerte, convencimiento, fuerza) => (suerte * 2, convencimiento * 2, fuerza * 2)
  }

}

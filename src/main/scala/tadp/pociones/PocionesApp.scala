package tadp.pociones

import tadp.pociones.Pociones2._

object PocionesApp extends App {
  val algunosNiveles: Niveles = Niveles(1,2,3)
  val otrosNiveles: Niveles = Niveles(7,3,9)

  println(algunosNiveles.alMenos7)
  println(otrosNiveles.alMenos7)

//  println(efectoLoco(otrosNiveles))

  val tupla: Seq[(Int, String)] =
    Seq(1,2,3).zip(Seq("hola", "como", "estas"))

  object Don {
    def unapply(nombre: String): Option[String] =
      Option(nombre).filter(_.toLowerCase.startsWith("don"))
  }

  val saludoALosDones: String => String = {
    case Don(s) =>
      "Hola " + s
  }

  println(saludoALosDones("Don Pepito"))
  println(saludoALosDones("Don José"))
//  println(saludoALosDones("Juan")) //Explota!!!

  val unMapa = Map("hello" -> 1, "world" -> 2)
//  println(unMapa
//    .map {
//      case (key, valor) => key.length + valor
//    })

  println(unMapa.map((t:(String, Int)) => {
    val (key, value) = t
    (key, key.length + value)
  }))



  val unaPocion = Pocion("aire", Seq())

  val Pocion(nombre, _) = unaPocion

  val PocionHeavy(otroNombre, _) = multijugos

  println(otroNombre)

  val ::(head, tail) = List(1,2,3)

  val list: Seq[Int] = ::(head, tail)
}

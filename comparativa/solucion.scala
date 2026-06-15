// Definición de los colores del semáforo
enum Color {
  case Rojo, Verde, Amarillo, IntermitenteAmarillo, IntermitenteVerde
}

//1er Requerimiento: Estados de Transición
object SistemaSemaforo {
    def transicion(colorActual: Color, colorCambiar: Color): Either[String, (String, String)] = {
    (colorActual, colorCambiar) match {
      // Cambios válidos
      case (Color.Verde, Color.Amarillo) => 
        Right(("Intermitente-Verde", "cambiar-a-Amarillo"))
      case (Color.Amarillo, Color.Rojo) => 
        Right(("Intermitente-Amarillo", "cambiar-a-Rojo"))
      case (Color.Rojo, Color.Verde) => 
        Right(("Intermitente-Amarillo", "cambiar-a-Verde"))
        
      // Cambios no válidos
      case (Color.Verde, Color.Rojo) => 
        Left("Cambio esperado: AMARILLO")
      case (Color.Amarillo, Color.Verde) => 
        Left("Cambio esperado: ROJO")
      case (Color.Rojo, Color.Amarillo) => 
        Left("Cambio esperado: VERDE")
      case _ => 
        Left("Datos no Válidos")
    }
  }
}
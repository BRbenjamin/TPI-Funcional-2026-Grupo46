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

  /**
    2do Requerimiento: Temporizador Automático
    Funcion: temporizador
    Naturaleza: Pura
    Estrategia: Utilizacion de residuo matemático % y Pattern Matching con Guards
    Impacto: No Destructiva
   */
  def temporizador(tiempoUnix: Long): Color = {
    val tiempoCiclo = 225 
    val resto = Math.floorMod(tiempoUnix, tiempoCiclo)

    resto match {
      case r if r >= 0   && r <= 89  => Color.Rojo
      case r if r >= 90  && r <= 92  => Color.IntermitenteAmarillo
      case r if r >= 93  && r <= 212 => Color.Verde
      case r if r >= 213 && r <= 215 => Color.IntermitenteVerde
      case r if r >= 216 && r <= 221 => Color.Amarillo
      case r if r >= 222 && r <= 224 => Color.IntermitenteAmarillo
      case _                         => Color.Rojo
    }
  }
}

import scala.io.StdIn.readLine

@main def auditoriaInteractiva() = {
  print("Ingrese tiempo Unix para saber el color: ")
  val Color = readLine()
  
  // Convertimos el texto ingresado a número largo (Long)
  val tiempo = Color.toLong 
  
  val resultado = SistemaSemaforo.temporizador(tiempo)
  
  // Imprime únicamente la respuesta pura
  println(s"Color: $resultado")
}
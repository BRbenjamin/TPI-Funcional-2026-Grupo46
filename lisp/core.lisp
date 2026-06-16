(ql:quickload "local-time")
;1er Requerimiento: Estados de Transición
; ----------------------------------------------------------------------------
; Funcion: Transicion
; Naturaleza: Impura (Pasados "color-Actual" y "cambiar-a", imprime en pantalla, si la opcion es valida)
; Estrategia: Utilizacion de la Condicional Cond.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------

(defun transicion(color-Actual cambiar-a)
	(cond 
		((and (equal color-Actual 'en-Verde) (equal cambiar-a 'amarillo)) ;cambio valido
		(list color-Actual (format nil "cambiar-a-~a" cambiar-a)))
		((and (equal color-Actual 'en-Amarillo) (equal cambiar-a 'rojo)) ;cambio valido
		(list color-Actual (format nil "cambiar-a-~a" cambiar-a)))
		((and (equal color-Actual 'en-Rojo) (equal cambiar-a 'verde)) ;cambio valido
		(list color-Actual (format nil "cambiar-a-~a" cambiar-a)))
		((and (equal color-Actual 'en-Verde) (equal cambiar-a 'rojo)) ;cambio NO valido
		(list color-Actual "Cambio esperado: AMARILLO"))
		((and (equal color-Actual 'en-Amarillo) (equal cambiar-a 'verde)) ;cambio NO valido
		(list color-Actual "Cambio esperado: ROJO"))
		((and (equal color-Actual 'en-Rojo) (equal cambiar-a 'amarillo)) ;cambio NO valido
		(list color-Actual "Cambio esperado: VERDE"))
		(t (list "Datos no Validos"))
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

;2do Requerimiento: Temporizador Automático
; ----------------------------------------------------------------------------
; Funcion: Timer
; Naturaleza: Pura (Pasada una cantidad de tiempo en formato Unix, devuelve en que color estaria el semaforo)
; Estrategia: Utilizacion de la Condicional Cond para clasificar los posibles colores del semaforo, en conjunto con la funcion Mod, para que segun el resto que devuelva, indique el color en el que esta el semaforo.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------

(defun Temporizador(tiempo_Unix)

	(let ((duracion_Ciclo 216)) ; tiempo del ciclo completo	
		(cond
			((and (>= (mod tiempo_Unix duracion_Ciclo) 0) (<= (mod tiempo_Unix duracion_Ciclo) 89)) 'Rojo)
			((and (>= (mod tiempo_Unix duracion_Ciclo) 90) (<= (mod tiempo_Unix duracion_Ciclo) 209))  'Verde)
			((and (>= (mod tiempo_Unix duracion_Ciclo) 210) (<= (mod tiempo_Unixduracion_Ciclo) 215)) 'Amarillo)
		)
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

; 3er Requerimiento: Auditoria
; ----------------------------------------------------------------------------
; Funcion: Auditoria
; Naturaleza: Impura (Ingreso de datos a través de un loggin)
; Estrategia: Mediante la biblioteca "local-time" obtener la fecha pasada por medio de tiempo Unix.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------

(defun Auditoria()

		(format t "Ingrese su ID: ")
		(finish-output)
			(let ((id (read)))
		(format t "Ingrese la clave(1234): ")
		(finish-output)
			(let ((clave(read)))
			(if (equal clave 1234)
				(progn
				  (format t "Ingrese tiempo Unix para saber el color anterior y el siguiente: ")
				  (finish-output)
					(let ((tiempo(read)))
					(list (local-time:format-timestring nil
					(local-time:unix-to-timestamp tiempo) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
					(calcular-tiempo  tiempo)))
				)
			)
		)
	)
)
;llamar solamente a (Auditoria)

;Auditoria la funcion Aux
; ----------------------------------------------------------------------------
; Funcion: calcular-tiempo
; Naturaleza: Pura (Mostrar cronologicamente el color anterior y el siguiente del semaforo)
; Estrategia: Mediante el uso de mod y los segundos totales del ciclo (216), se obtiene la transicion del semaforo.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------
(defun calcular-tiempo (tiempo)

	(let ((ciclo-semaforo 216))
		(cond
			((and (>= (mod tiempo ciclo-semaforo) 0) (<= (mod tiempo ciclo-semaforo) 89))  "La luz ha cambiado de Amarillo a Rojo")

			((and (>= (mod tiempo ciclo-semaforo) 90) (<= (mod tiempo ciclo-semaforo) 209))  "La luz ha cambiado de Rojo a Verde")	
			
			((and (>= (mod tiempo ciclo-semaforo) 210) (<= (mod tiempo ciclo-semaforo) 215))  "La luz ha cambiado de Verde a Amarillo")
		)
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

;Requerimiento 4 
;4a
; ----------------------------------------------------------------------------
; Funcion: duracion-ciclo
; Naturaleza: Impura (Imprime en pantalla el porcentaje de duracion de cada color)
; Estrategia: Segun la cantidad de segundos ingresados, se saca el porcentaje de tiempo de cada color.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------
(defun duracion-ciclo(tiempo-ciclo)
	(cond
		((and(>= tiempo-ciclo 35) (<= tiempo-ciclo 150)) T)
		(t nil)
	)
)
;4b
; ----------------------------------------------------------------------------
; Funcion: recomendacion-ciclo (Funcion Axiliar)
; Naturaleza: Impura (Imprime en pantalla la recomendacion de la duracion del semaforo)
; Estrategia: Segun que condicion cumpla se recomienda uno u otra. 
; Impacto: No Destructiva
; ----------------------------------------------------------------------------
(defun recomendacion-ciclo(ciclo)

	(cond
		((< ciclo 35) (format nil "Recomendacion: Aumentar ~a segundos" (- 35 ciclo)))
		((> ciclo 150) (format nil "Recomendacion: Recortar ~a segundos" (- ciclo 150)))
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

; 5to Requerimiento: Planificación Temporal
; ----------------------------------------------------------------------------
; Funcion: ciclos-por-tiempo
; Naturaleza: Impura (Imprime en pantalla la cantidad de ciclos del semaforo completados)
; Estrategia: Calcula la cantidad de ciclos mediante la funcion Mod. 
; Impacto: No Destructiva
; ----------------------------------------------------------------------------
(defun ciclos-por-tiempo(minutos)

	(let ((segundos-ciclo 216))

		(cond
			((= (mod (* minutos 60) segundos-ciclo) 0) 
			(format nil "Ciclos Completados: ~a" (floor(/ (* minutos 60)  segundos-ciclo)))) ; si es 0 el resto, el ultimo ciclo fue completado
			((> (mod (* minutos 60) segundos-ciclo) 0) 
			(format nil "Ciclos Completados: ~a ~%~
				         Para completar el siguiente ciclo quedan: ~a segundos"
		    (floor (/ (* minutos 60) segundos-ciclo)) 
		    (- segundos-ciclo (mod (* minutos 60) segundos-ciclo)))) ; si NO fue completado, que diga cuantos segundos transcurrieron
			(t (list "Datos no Validos"))
		)
	)
)
;llamar solamente a (distribucion-temporal)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------


;requerimiento 6
; ----------------------------------------------------------------------------
; Funcion: distribucion-temporal
; Naturaleza: Impura (Imprime en pantalla, el porcentaje de tiempo que estuvo cada color durante 1 hora)
; Estrategia: Calcular la distribucion teniendo en cuenta los segundos de duracion de cada color respecto a la duracion total del ciclo (216), pasarlo a hora y dividirlo en 1 hora.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------

(defun distribucion-temporal()

	(let ((porcentaje-rojo (* (/ (* (/ 90 216) 3600) 3600) 100)) (porcentaje-verde (* (/ (* (/ 120 216) 3600) 3600) 100)) 
																					(porcentaje-amarillo (* (/ (* (/ 6 216) 3600) 3600) 100)))
		(format t "Rojo: ,2f% | Verde: ,2f% | Amarillo: ,2f%" porcentaje-rojo porcentaje-verde porcentaje-amarillo)
	)
)
(distribucion-temporal)


;------------------------------------------------------------------------------------------------------
;7to Requerimiento: Aseguramiento de la calidad
;Punto 1
;(transicion 'en-Rojo 'verde)  ; Ejemplo Valido
;(transicion 'en-Rojo 'amarillo) ; Ejemplo Valido
;(transicion 'en-Rojo 0) ; Ejemplo Error

; Punto 2
;(Temporizador 10000) ; Ejemplo Valido
;(Temporizador 12300) ; Ejemplo Valido
;(Temporizador i) ; Ejemplo Error

; Punto 3
;1717958400 ; Ejemplo Valido
;1781621963 ; Ejemplo Valido
; d124232 ; Ejemplo Error

; Punto 4
;(duracion-ciclo 140) ; Ejemplo Valido
;(duracion-ciclo 260) ; Ejemplo Valido
;(duracion-ciclo e) ; Ejemplo Error

; Punto 5
;(ciclos-por-tiempo 15); Ejemplo Valido
;(ciclos-por-tiempo 20); Ejemplo Valido
;(ciclos-por-tiempo d); Ejemplo Error



;Extensión 1

;1er Requerimiento: Estados de Transición
; ----------------------------------------------------------------------------
; Funcion: Transicion
; Naturaleza: Impura (Pasados "color-Actual" y "cambiar-a", imprime en pantalla, si la opcion es valida)
; Estrategia: Utilizacion de la Condicional Cond.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------

(defun transicion2(estado_actual color-cambiar)
	(cond 
		((and (equal color-Actual 'en-Verde) (equal cambiar-a 'amarillo)) ;cambio valido
		(list (format nil "Intermitente-~a" color-Actual) (format nil "cambiar-a-~a" cambiar-a)))
		((and (equal color-Actual 'en-Amarillo) (equal cambiar-a 'rojo)) ;cambio valido
		(list (format nil "Intermitente-~a" color-Actual) (format nil "cambiar-a-~a" cambiar-a)))
		((and (equal color-Actual 'en-Rojo) (equal cambiar-a 'verde)) ;cambio valido
		(list (format nil "Intermitente-~a" color-Actual) (format nil "cambiar-a-~a" cambiar-a)))
		((and (equal color-Actual 'en-Verde) (equal cambiar-a 'rojo)) ;cambio NO valido
		(list color-Actual "Cambio esperado: AMARILLO"))
		((and (equal color-Actual 'en-Amarillo) (equal cambiar-a 'verde)) ;cambio NO valido
		(list color-Actual "Cambio esperado: ROJO"))
		((and (equal color-Actual 'en-Rojo) (equal cambiar-a 'amarillo)) ;cambio NO valido
		(list color-Actual "Cambio esperado: VERDE"))
		(t (list "Datos no Validos"))
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

;2do Requerimiento: Temporizador Automático
; ----------------------------------------------------------------------------
; Funcion: Timer
; Naturaleza: Pura (Pasada una cantidad de tiempo en formato Unix, devuelve en que color estaria el semaforo)
; Estrategia: Utilizacion de la Condicional Cond para clasificar los posibles colores del semaforo, en conjunto con la funcion Mod, para que segun el resto que devuelva, indique el color en el que esta el semaforo.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------

(defun temporizador2(tiempo_Unix)

	(let ((tiempo 225)) ; tiempo del ciclo completo	
		(cond
			((and (>= (mod tiempo_Unix tiempo) 0) (<= (mod tiempo_Unix tiempo) 89))  'rojo)
			((and (>= (mod tiempo_Unix tiempo) 90) (<= (mod tiempo_Unix tiempo) 92)) 'intermitente-amarillo)

			((and (>= (mod tiempo_Unix tiempo) 93) (<= (mod tiempo_Unix tiempo) 212))  'verde)	
			((and (>= (mod tiempo_Unix tiempo) 213) (<= (mod tiempo_Unix tiempo) 215)) 'intermitente-verde)

			((and (>= (mod tiempo_Unix tiempo) 216) (<= (mod tiempo_Unix tiempo) 221))  'amarillo)
			((and (>= (mod tiempo_Unix tiempo) 222) (<= (mod tiempo_Unix tiempo) 224))  'intermitente-amarillo)
		)
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

; 3er Requerimiento: Auditoria
; ----------------------------------------------------------------------------
; Funcion: Auditoria
; Naturaleza: Impura (Ingreso de datos a través de un loggin)
; Estrategia: Mediante la biblioteca "local-time" obtener la fecha pasada por medio de tiempo Unix.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------

(defun Auditoria2()

		(format t "Ingrese su ID: ")
		(finish-output)
			(let ((id (read)))
		(format t "Ingrese la clave(1234): ")
		(finish-output)
			(let ((clave(read)))
			(if (equal clave 1234)
				(progn
				  (format t "Ingrese tiempo Unix para saber el color anterior y el siguiente: ")
				  (finish-output)
					(let ((tiempo(read)))
					(list (local-time:format-timestring nil
					(local-time:unix-to-timestamp tiempo) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
					(calcular-tiempo  tiempo)))
				)
			)
		)
	)
)

(defun calcular-tiempo2(tiempo)

	(let ((ciclo-semaforo 225))
		(cond
			((and (>= (mod tiempo ciclo-semaforo) 0) (<= (mod tiempo ciclo-semaforo) 89))  "La luz ha cambiado de Amarillo a Rojo")
			((and (>= (mod tiempo ciclo-semaforo) 90) (<= (mod tiempo ciclo-semaforo) 92)) "Luz Intermitente en Rojo cambiando a Verde")

			((and (>= (mod tiempo ciclo-semaforo) 93) (<= (mod tiempo ciclo-semaforo) 212))  "La luz ha cambiado de Rojo a Verde")	
			((and (>= (mod tiempo ciclo-semaforo) 213) (<= (mod tiempo ciclo-semaforo) 215)) "Luz Intermitente en Verde cambiando a Amarillo")

			((and (>= (mod tiempo ciclo-semaforo) 216) (<= (mod tiempo ciclo-semaforo) 221))  "La luz ha cambiado de Verde a Amarillo")
			((and (>= (mod tiempo ciclo-semaforo) 222) (<= (mod tiempo ciclo-semaforo) 224))  "Luz Intermitente en Amarillo cambiando a Rojo")
		)
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------
;4to Requerimiento: Análisis de Ciclos Semafóricos
;4.a
; ----------------------------------------------------------------------------
; Funcion: duracion-ciclo
; Naturaleza: Impura (Imprime en pantalla el porcentaje de duracion de cada color)
; Estrategia: Segun la cantidad de segundos ingresados, se saca el porcentaje de tiempo de cada color.
; Impacto: No Destructiva
; ----------------------------------------------------------------------------
(defun duracion-ciclo2(segundos)
  (let ((rojo (* segundos (/ 90 225))) (verde (* segundos (/ 120 225)))
  	    (amarillo (* segundos (/ 6 225)))(intermitente (* segundos (/ 9 225))))
    (format t "Duración para un Ciclo de ~a segundos~%~
    	       Fase Roja:~,2f segundos~%~
    	       Fase Verde:~,2f segundos~%~
    	       Fase Amarilla:~,2f segundos~%~
    	       Fases Intermitentes:~,2f segundos" segundos rojo verde amarillo intermitente)
   )
)
;4.b
; ----------------------------------------------------------------------------
; Funcion: recomendacion-ciclo (Funcion Axiliar)
; Naturaleza: Impura (Imprime en pantalla la recomendacion de la duracion del semaforo)
; Estrategia: Segun que condicion cumpla se recomienda uno u otra. 
; Impacto: No Destructiva
; ----------------------------------------------------------------------------
(defun recomendacion-ciclo2(ciclo)
	(cond
		((and (>= ciclo 35) (<= ciclo 150)) "La duración está en el rango óptimo")
		((< ciclo 35) (format nil "Recomendacion: Aumentar ~a segundos" (- 35 ciclo)))
		((> ciclo 150) (format nil "Recomendacion: Recortar ~a segundos" (- ciclo 150)))
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

; 5to Requerimiento: Planificación Temporal

(defun ciclos-por-tiempo2(minutos)

	(let ((segundos-ciclo 216))

		(cond
			((= (mod (* minutos 60) segundos-ciclo) 0) 
			(format nil "Ciclos Completados: ~a" (/ (* minutos 60)  segundos-ciclo))) ; si es 0 el resto, el ultimo ciclo fue completado
			((> (mod (* minutos 60) segundos-ciclo) 0) 
			(format nil "Ciclos Completados: ~a.Para completar el siguiente ciclo quedan: ~a segundos"
			(floor (/ (* minutos 60) segundos-ciclo)) (- segundos-ciclo (mod (* minutos 60) segundos-ciclo)))) ; si NO fue completado, que diga cuantos segundos transcurrieron
			(t (list "Datos no Validos"))
		)
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

;6to Requerimiento: Informe de Distribución Temporal
;------------------------------------------------------------------------------------------------------
; Funcion: distribucion-temporal
; Naturaleza: Impura (Imprime en pantalla, el porcentaje de tiempo que estuvo cada color durante 1 hora)
; Estrategia: Calcular la distribucion teniendo en cuenta los segundos de duracion de cada color respecto a la duracion total del ciclo (216), pasarlo a hora y dividirlo en 1 hora.
; Impacto: No Destructiva
;------------------------------------------------------------------------------------------------------

(defun distribucion-temporal2()
  (let* ((ciclo 225)
         (porcentaje-rojo (* (/ 90 ciclo) 100))
         (porcentaje-verde (* (/ 120 ciclo) 100))
         (porcentaje-amarillo (* (/ 6 ciclo) 100))
         (porcentaje-intermitente (* (/ 9 ciclo) 100)))
    (format t "Distribución Temporal en una hora~%~
               Rojo: ~,2f%~%~
               Verde: ~,2f%~%~
               Amarillo: ~,2f%~%~
               Intermitente: ~,2f%~%" 
            porcentaje-rojo porcentaje-verde 
            porcentaje-amarillo porcentaje-intermitente)
   )
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------
;7to Requerimiento: Aseguramiento de la calidad
;Punto 1
;(transicion 'en-Rojo 'verde)  ; Ejemplo Valido
;(transicion 'en-Rojo 'amarillo) ; Ejemplo Valido
;(transicion 'en-Rojo 0) ; Ejemplo Error

; Punto 2
;(timer 10000) ; Ejemplo Valido
;(timer 12300) ; Ejemplo Valido
;(timer i) ; Ejemplo Error

; Punto 3
;1717958400 ; Ejemplo Valido
;1781621963 ; Ejemplo Valido
; d124232 ; Ejemplo Error

; Punto 4
;(duracion-ciclo 140) ; Ejemplo Valido
;(duracion-ciclo 260) ; Ejemplo Valido
;(duracion-ciclo e) ; Ejemplo Error

; Punto 5
;(ciclos-por-tiempo 15); Ejemplo Valido
;(ciclos-por-tiempo 20); Ejemplo Valido
;(ciclos-por-tiempo d); Ejemplo Error

; Extension 2
(ql:quickload "local-time")

(defun informe(datos)

	(with-open-file (stream "informe-ejecucion-semaforo.txt" ; Poner direccion de archivo.
		             :direction :output 
					 :if-exists :append      ;si ya existia agrega 
                     :if-does-not-exist :create) ;si no existe crea uno nuevo

		(format stream "~%Informe de Ejecucion del Sistema Semaforico~%")
		
		(format stream "~a - [~a] --- Transición: ~a~%" (cadr datos) (car datos) (caddr datos))
		(format stream "=========================================================")
	)
)

(defun AuditoriaInforme()

		(format t "Ingrese su ID: ")
		(finish-output)
			(let ((id (read)))
		(format t "Ingrese la clave(1234): ")
		(finish-output)
			(let ((clave(read)))

			(if (equal clave 1234)

				(progn
					(format t "Ingrese tiempo Unix para saber el color anterior y el siguiente: ")
					(finish-output)
						(let ((tiempo(read)))
					(let ((guardar-info
								(list (format nil "Usuario: ~a|| Tiempo Unix ingresado: ~a" id tiempo)  (local-time:format-timestring nil
										 								(local-time:unix-to-timestamp tiempo) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
																					    (calcular-tiempo  tiempo))))
						(informe guardar-info)
						guardar-info	
					)
				)
			)
		)
	)
	)
)

(defun calcular-tiempo (tiempo)

	(let ((ciclo-semaforo 225))
		(cond
			((and (>= (mod tiempo ciclo-semaforo) 0) (<= (mod tiempo ciclo-semaforo) 89))  "La luz ha cambiado de Amarillo a Rojo")
			((and (>= (mod tiempo ciclo-semaforo) 90) (<= (mod tiempo ciclo-semaforo) 92)) "Luz Intermitente en Rojo cambiando a Verde")

			((and (>= (mod tiempo ciclo-semaforo) 93) (<= (mod tiempo ciclo-semaforo) 212))  "La luz ha cambiado de Rojo a Verde")	
			((and (>= (mod tiempo ciclo-semaforo) 213) (<= (mod tiempo ciclo-semaforo) 215)) "Luz Intermitente en Verde cambiando a Amarillo")

			((and (>= (mod tiempo ciclo-semaforo) 216) (<= (mod tiempo ciclo-semaforo) 221))  "La luz ha cambiado de Verde a Amarillo")
			((and (>= (mod tiempo ciclo-semaforo) 222) (<= (mod tiempo ciclo-semaforo) 224))  "Luz Intermitente en Amarillo cambiando a Rojo")
		)
	)
)

(AuditoriaInforme)

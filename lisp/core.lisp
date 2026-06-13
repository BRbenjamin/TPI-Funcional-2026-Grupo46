;1er Requerimiento: Estados de Transición
(defun transicion(estado_actual color-cambiar)
	(cond
		((and(equal estado_actual 'en-rojo)(equal color-cambiar 'verde)) 
			(list estado_actual (format nil "cambiar-a-~a" color-cambiar)))
		((and(equal estado_actual 'en-Verde)(equal color-cambiar 'amarillo)) 
			(list estado_actual (format nil "cambiar-a-~a" color-cambiar)))
		((and(equal estado_actual 'en-amarillo)(equal color-cambiar 'rojo)) 
			(list estado_actual (format nil "cambiar-a-~a" color-cambiar)))
		((and(equal estado_actual 'en-amarillo)(equal color-cambiar 'verde)) 
			(list estado_actual (format nil "cambiar-a-~a" color-cambiar)))
		((and(equal estado_actual 'en-verde)(equal color-cambiar 'rojo)) 
			(list estado_actual (format nil "cambiar-a-~a" color-cambiar)))
		((and(equal estado_actual 'en-verde)(equal color-cambiar 'amarillo)) 
			(list estado_actual (format nil "cambiar-a-~a" color-cambiar)))
		(t (list estado_actual "accion-por-defecto"))
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

;2do Requerimiento: Temporizador Automático
(defun temporizador(tiempo_actual)
	(cond
		((and (>= (mod tiempo_actual 216) 0) (<= (mod tiempo_actual 216) 89)) 'rojo)
		((and (>= (mod tiempo_actual 216) 90) (<= (mod tiempo_actual 216) 95)) 'amarillo)
		((and (>= (mod tiempo_actual 216) 96) (<= (mod tiempo_actual 216) 215)) 'verde)
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

; 3er Requerimiento: Auditoria
(ql:quickload "local-time")

 (defun Auditoria()

	(let ((id) (clave) (tiempo))
		(print "Ingrese su ID: ")
			(setq id(read))
		(print "Ingrese la clave: ")
			(setq clave(read))

			(if (equal clave 1234)

				(progn
					(print "Ingrese tiempo Unix para saber el color anterior y el siguiente: ")
						(setq tiempo(read))
					(list (local-time:format-timestring nil
					 								(local-time:unix-to-timestamp tiempo) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
																    (calcular-tiempo  tiempo)))
			)
	)
)

; Funcion auxiliar para calcular el cambio de color del semaforo. 
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
;4to Requerimiento: Análisis de Ciclos Semafóricos
;4.a
(defun duracion-ciclo(tiempo-ciclo)
	(cond
		((and(>= tiempo-ciclo 35) (<= tiempo-ciclo 150)) T)
		(t nil)
	)
)
;4.b
(defun recomendacion-ciclo(ciclo)

	(cond
		((< ciclo 35) (format nil "Recomendacion: Aumentar ~a segundos" (- 35 ciclo)))
		((> ciclo 150) (format nil "Recomendacion: Recortar ~a segundos" (- ciclo 150)))
	)
)

;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------

; 5to Requerimiento: Planificación Temporal

(defun ciclos-por-tiempo(minutos)

	(let ((segundos-ciclo 216))

		(cond
			((= (mod (* minutos 60) segundos-ciclo) 0) (format nil "Ciclos Completados: ~a" (/ (* minutos 60)  segundos-ciclo))) ; si es 0 el resto, el ultimo ciclo fue completado
			((> (mod (* minutos 60) segundos-ciclo) 0) (format nil "Ciclos Completados: ~a.Para completar el siguiente ciclo quedan: ~a segundos"
													   (floor (/ (* minutos 60) segundos-ciclo)) (- segundos-ciclo (mod (* minutos 60) segundos-ciclo)))) ; si NO fue completado, que diga cuantos segundos transcurrieron
			(t (list "Datos no Validos"))
		)
	)
)





;------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------
;6to Requerimiento: Informe de Distribución Temporal

(defun distribucion-temporal()

	(let ((porcentaje-rojo (* (/ (* (/ 90 216) 3600) 3600) 100)) (porcentaje-verde (* (/ (* (/ 120 216) 3600) 3600) 100)) 
																					(porcentaje-amarillo (* (/ (* (/ 6 216) 3600) 3600) 100)))
		(format t "Rojo: ,2f% | Verde: ,2f% | Amarillo: ,2f%" porcentaje-rojo porcentaje-verde porcentaje-amarillo)
	)
)
(distribucion-temporal)



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







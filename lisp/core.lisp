;1er requerimiento: Estados de Transición
(defun transicion(estado_actual color-cambiar)
	(cond
		((and(equal estado_actual 'en-rojo)(equal color-cambiar 'amarillo)) 
			(list estado_actual (format nil "cambiar-a-~a" color-cambiar)))
		((and(equal estado_actual 'en-rojo)(equal color-cambiar 'verde)) 
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

;2do requerimiento: Temporizador Automático
(defun temporizador(tiempo_actual)
	(cond
		((and (>= (mod tiempo_actual 216) 0) (<= (mod tiempo_actual 216) 89)) 'rojo)
		((and (>= (mod tiempo_actual 216) 90) (<= (mod tiempo_actual 216) 95)) 'amarillo)
		((and (>= (mod tiempo_actual 216) 96) (<= (mod tiempo_actual 216) 215)) 'verde)
	)
)

; 3er requerimiento: Auditoria
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

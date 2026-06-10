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
(defun timer(tiempo_actual)
	(cond
		((and (>= (mod tiempo_actual 216) 0) (<= (mod tiempo_actual 216) 89)) 'rojo)
		((and (>= (mod tiempo_actual 216) 90) (<= (mod tiempo_actual 216) 95)) 'amarillo)
		((and (>= (mod tiempo_actual 216) 96) (<= (mod tiempo_actual 216) 215)) 'verde)
	)
)
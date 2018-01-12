;;;==========================================================================
;;;   Sistema Experto de Eleccion de Telefono Movil
;;;
;;;     Prototipo del Sistema (sintacticamente incorrecto) que incluye
;;;     estructura modular y algunas reglas
;;;
;;;     Falta la definicion del mapa de conocimiento mediante COOL
;;;
;;;    (n.a.)Para ejecutar, primero "load", despues "reset" y finalmente "run".
;;;==========================================================================
(defmodule MAIN (export ?ALL))

;;******************************************
;;	DATOS
;;******************************************
(defmodule DATOS (export ?ALL)
		(import MAIN ?ALL))

;estructura que representa un telefono movil
(deftemplate buscamovil
	(slot espacio)
	(slot memoria)
	(slot sistema-operativo)
	(slot gama)
	(slot cam-frontal)
	(slot cam-trasera))

(deftemplate rmovil
	(slot modelo)
	(slot precio (type INTEGER))
	(slot espacio)
	(slot memoria)
	(slot sistema-operativo)
	(slot gama)
	(slot cam-frontal)
	(slot cam-trasera))

;estructura que representa al usuario/comprador
(deftemplate comprador
	(slot presupuesto (type INTEGER))
	(slot camara)
	(slot selfies)
	(slot ecosistema-apple)
	(slot juegos-y-apps)
	(slot espacio))

;;*******************************
;;*  DEFINICIONES DE FUNCIONES  *
;;*******************************

(deffunction preguntar (?pregunta $?valores-permitidos)
   (printout t ?pregunta)
   (bind ?respuesta (read))
   (if (lexemep ?respuesta)
       then (bind ?respuesta (lowcase ?respuesta)))
   (while (not (member ?respuesta ?valores-permitidos)) do
      (printout t ?pregunta)
      (bind ?respuesta (read))
      (if (lexemep ?respuesta)
          then (bind ?respuesta (lowcase ?respuesta)))
	 )
   ?respuesta
)

(deffunction p-si-o-no (?pregunta)
   (bind ?respuesta (preguntar ?pregunta si no s n))
   (if (or (eq ?respuesta si) (eq ?respuesta s))
       then TRUE
       else FALSE
		)
)

(deffunction p-cantidad (?pregunta)
	(printout t ?pregunta)
	(bind ?respuesta (read-number))
	(while ((eq ?respuesta "*** READ ERROR ***" )) do
		(printout t ?pregunta)
		(bind ?respuesta (read-number))
	)
	?respuesta
)


;;;*******************************************************************
;;;* MODULO DE OBTENCION DE PERFIL DE USUARIO
;;;*
;;;*******************************************************************
(defmodule PERFIL (import DATOS deftemplate deffunction));TODO: importar solo lo necesario

;TODO revisar y escribir bien las reglas e incluir las restantes

(defrule PERFIL::R1-determinar-presupuesto ""
  ?c <- (comprador (presupuesto 0));presupuesto del usuario no contiene valor
  =>
  (bind ?r
     (p-cantidad "¿Cual es su presupuesto? (Numero entero)"));comprobacion de valor permitido
	(modify ?c (presupuesto ?r))
)

(defrule PERFIL::R2-determinar-necesidad ""
  ?c <- (comprador (camara nil)(ecosistema-apple nil)(juegos-y-apps nil)(espacio nil));necesidad del usuario no contiene valor
  =>
  (if (p-si-o-no "¿Le da usted importancia a la camara del terminal (si/no)? ")
  	then (modify ?c (camara buena))
		else (modify ?c (camara indiferente))
  )

	(if (p-si-o-no "¿Utiliza usted la camara para hacerse selfies a menudo(si/no)? ")
  	then (modify ?c (selfies si))
		else (modify ?c (selfies no))
  )

	(if (p-si-o-no "¿Tiene usted otros dispositivos de apple (si/no)? ")
  	then (modify ?c (ecosistema-apple si))
		else (modify ?c (ecosistema-apple no))
  )

	(bind ?rja
		 (preguntar "Cuantas apps y juegos usa (pocos/algunos/muchos)? " pocos algunos muchos))
	(modify ?c (juegos-y-apps ?rja))

	(if (p-si-o-no "¿Almacena usted muchos archivos o fotos en su dispositivo (si/no)? ")
  	then (modify ?c (espacio mucho))
		else (modify ?c (espacio poco))
  )
)

;;;****************************************************
;;;* PERFIL DE MOVIL *
;;;****************************************************
(defmodule PERFILM (import MAIN ?ALL));TODO: importar solo lo necesario

(defrule PERFILM::R3-determinar-gama ""
	?m <- (buscamovil (gama media))
	(comprador (presupuesto ?p)(juegos-y-apps ?ja)(camara ?c))
	=>
	(if (or (>= ?p 400) (eq ?ja muchos))
		then (modify ?m (gama alta))
	)
	(if (or (< ?p 150 (eq ?ja pocos)))
		then (modify ?m (gama baja))
	)
)

(defrule PERFILM::R4-determinar-sistema-operativo ""
  ?m <- (buscamovil (sistema-operativo nil))
	(comprador (ecosistema-apple ?ea))
	=>
  (if (eq ?ea si)
		then (modify ?m (sistema-operativo iOS))
		else (modify ?m (sistema-operativo Android))
	)
)

(defrule PERFILM::R5-determinar-espacio ""
  ?m <- (buscamovil (espacio nil))
	(comprador (espacio ?e))
	=>
  (modify ?m (espacio ?e))
)
(defrule PERFILM::R6-determinar-memoria
	?m <- (buscamovil (memoria nil))
	(comprador (juegos-y-apps ?ja))
  =>
	(if (or (eq ?ja muchos) (eq ?ja algunos))
		then (modify ?m (memoria mucha))
		else (modify ?m (memoria poca))
	)
	(modify ?m (memoria ?mem))
)

;;;*****************************************************
;;;* DECISION *
;;;*****************************************************

(defrule DECISION::R7-determinar-procesador-y-memoria ""
	(buscamovil (espacio ?e)(gama ?g)(sistema-operativo ?s)(cam-frontal ?f)(cam-trasera ?t))
	?rm<- (rmovil (modelo ?n)(gama ?g)(sistema-operativo ?s)(cam-frontal ?f)(cam-trasera ?t)(memoria ?m1))

	?lp <- (listapre $?lista)
	=> (modify ?lp (listapre ?lista ?n))
)

;;;*****************************************************
;;;* MODULO DE PRESENTACION DEL SISTEMA Y CONCLUSIONES *
;;;*****************************************************
(defmodule PRESENTACION (export ?ALL) (import MAIN ?ALL))
;TODO: importar solo lo necesario
(defrule PRESENTACION::R8 - intro ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Prototipo 1: Sistema Experto para eleccion de moviles")
  (printout t crlf crlf)
)

;(printout t crlf crlf)
;(printout t "Terminales sugeridos:")

(defrule PRESENTACION::R9 - resultado ""
  (declare (salience 10))
  (moviles ?m $?resto)
  =>

	(printout t ?m)
  (printout t crlf crlf)
  ;presentar caracteristicas del telefono
)

;******************************************
;	Modulo MAIN
;******************************************

(defrule MAIN::inicio
	(initial-fact)
=>	(focus PERFIL PERFILM DECISION PRESENTACION));TODO definir orden de operacion

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
	(slot sistema-operativo)
	(slot gama)
	(slot cam-frontal)
	(slot cam-trasera)
	(slot memoria))

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

(deftemplate listapre
	(multislot lista-moviles))

;(rmovil(precio)(modelo)(espacio)(memoria)(sistema-operativo)(gama)(cam-frontal)(cam-trasera))
(defrule inicio
	(initial-fact)
=>
	(assert (listapre))
	(load-facts "DATOS.CLP")
)

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
	(while (eq ?respuesta "*** READ ERROR ***" ) do
		(printout t ?pregunta)
		(bind ?respuesta (read-number))
	)
	?respuesta
)


;;;*******************************************************************
;;;* MODULO DE OBTENCION DE PERFIL DE USUARIO
;;;*
;;;*******************************************************************
(defmodule PERFIL (import DATOS ?ALL));TODO: importar solo lo necesario

;TODO revisar y escribir bien las reglas e incluir las restantes

(defrule PERFIL::R1-determinar-presupuesto ""
  ?c <- (comprador (presupuesto 0));presupuesto del usuario no contiene valor
  =>
  (bind ?r
     (p-cantidad "¿Cual es su presupuesto? (Numero entero)"));comprobacion de valor permitido
	(modify ?c (presupuesto ?r))
)

(defrule PERFIL::R2-determinar-camara ""
  ?c <- (comprador (camara nil));necesidad del usuario no contiene valor
  =>
  (if (p-si-o-no "¿Le da usted importancia a la camara del terminal (si/no)? ")
  	then (modify ?c (camara buena))
		else (modify ?c (camara indiferente))
  )
)

(defrule PERFIL::R-determinar-selfies ""
	?c <- (comprador (selfies nil))
	=>
	(if (p-si-o-no "¿Utiliza usted la camara para hacerse selfies a menudo(si/no)? ")
  	then (modify ?c (selfies si))
		else (modify ?c (selfies no))
  )
)

(defrule PERFIL::R-determinar-cam-ecosistema-apple ""
	?c <- (comprador (ecosistema-apple nil))
	=>
	(if (p-si-o-no "¿Tiene usted otros dispositivos de apple (si/no)? ")
  	then (modify ?c (ecosistema-apple si))
		else (modify ?c (ecosistema-apple no))
  )
)

(defrule PERFIL::R-determinar-juegos-apps ""
	?c <- (comprador (juegos-y-apps nil))
	=>
	(bind ?rja
		 (preguntar "Cuantas apps y juegos usa (pocos/algunos/muchos)? " pocos algunos muchos))
	(modify ?c (juegos-y-apps ?rja))
)

(defrule PERFIL::R2-determinar-almacenamiento ""
	?c <- (comprador (espacio nil))
	=>
	(if (p-si-o-no "¿Almacena usted muchos archivos o fotos en su dispositivo (si/no)? ")
  	then (modify ?c (espacio mucho))
		else (modify ?c (espacio poco))
  )
)
;;;****************************************************
;;;* PERFIL DE MOVIL *
;;;****************************************************
(defmodule PERFILM (import DATOS ?ALL));TODO: importar solo lo necesario

(defrule PERFILM::R3-determinar-gama-alta ""
	?m <- (buscamovil (gama media))
	(comprador (presupuesto ?p &: (>= ?p 400))(juegos-y-apps ?ja &: (eq ?ja muchos))(camara ?c))
	=>
	(modify ?m (gama alta))
)

(defrule PERFILM::R3-determinar-gama-alta ""
	?m <- (buscamovil (gama media))
	(comprador (presupuesto ?p &: (< ?p 150))(juegos-y-apps ?ja &: (eq ?ja pocos))(camara ?c))
	=>
	(modify ?m (gama baja))
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

(defrule PERFILM::R-determinar-cam-trasera-buena ""
	?m <- (buscamovil (cam-trasera nil))
	(comprador (camara buena))
  =>
	(modify ?m (cam-trasera buena))
)

(defrule PERFILM::R-determinar-cam-trasera-mala ""
	?m <- (buscamovil (cam-trasera nil))
	(comprador (camara indiferente))
	=>
	(modify ?m (cam-trasera mala))
)

(defrule PERFILM::R-determinar-cam-frontal-buena ""
	?m <- (buscamovil (cam-frontal nil))
	(comprador (selfies si))
  =>
	(modify ?m (cam-frontal buena))
)

(defrule PERFILM::R-determinar-cam-frontal-mala ""
	?m <- (buscamovil (cam-frontal nil))
	(comprador (selfies no))
	=>
	else (modify ?m (cam-frontal mala))
)

(defrule PERFILM::R6-determinar-memoria-mucha
	?m <- (buscamovil (memoria nil))
	(comprador (juegos-y-apps ?ja &: (or (eq ?ja algunos)(eq ?ja muchos))))
	=>
	(modify ?m (memoria mucha))
)

(defrule PERFILM::R6-determinar-memoria-poca
	?m <- (buscamovil (memoria nil))
	(comprador (juegos-y-apps ?ja &: (eq ?ja pocos)))
	=>
	(modify ?m (memoria poca))
)

;;;*****************************************************
;;;* DECISION *
;;;*****************************************************
(defmodule DECISION (import DATOS ?ALL))
(defrule DECISION::R7-determinar-moviles ""
	(buscamovil (espacio ?e)(gama ?g)(sistema-operativo ?s)(cam-frontal ?f)(cam-trasera ?t)(memoria ?mem))
	?rm <- (rmovil (modelo ?n)(espacio ?e)(gama ?g)(memoria ?mem)(sistema-operativo ?s)(cam-frontal ?f)(cam-trasera ?t))

	;(rmovil (modelo "Bq Aquaris E5 16GB 1GB RAM")(precio 120)(espacio poco)(memoria poca)(sistema-operativo Android)(gama media)(cam-frontal mala)(cam-trasera buena))
	?lp <- (listapre (lista-moviles $?lmoviles))
	(test (not(member ?n $?lmoviles)))
	=>
	(modify ?lp (lista-moviles ?lmoviles ?n))
)

;;;*****************************************************
;;;* MODULO DE PRESENTACION DEL SISTEMA Y CONCLUSIONES *
;;;*****************************************************
(defmodule PRESENTACION (import DATOS ?ALL))
;TODO: importar solo lo necesario
; (defrule PRESENTACION::R8-intro ""
;   (declare (salience 10))
;   =>
;   (printout t crlf crlf)
;   (printout t "Prototipo 1: Sistema Experto para eleccion de moviles")
;   (printout t crlf crlf)
; )

;(printout t crlf crlf)
;(printout t "Terminales sugeridos:")

(defrule PRESENTACION::R9-resultado ""
  ?lp <- (listapre (lista-moviles ?m $?lmoviles))

  =>

	(printout t ?m)
  (printout t crlf crlf)
		(modify ?lp (lista-moviles ?lmoviles))
  ;presentar caracteristicas del telefono
)

;******************************************
;	Modulo MAIN
;******************************************

(defrule MAIN::inicio
	(initial-fact)
=>	(focus DATOS PERFIL PERFILM DECISION PRESENTACION));TODO definir orden de operacion

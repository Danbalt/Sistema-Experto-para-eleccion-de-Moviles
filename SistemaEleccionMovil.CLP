;;;==========================================================================
;;;   Sistema Experto de Eleccion de Telefono Movil
;;;
;;;     Prototipo del Sistema (sintacticamente incorrecto) que incluye
;;;     estructura modular y algunas reglas
;;;
;;;    Para ejecutar, primero "load", despues "reset" y finalmente "run".
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

(deftemplate lista
	(multislot lista-moviles))

(defrule inicio
	(initial-fact)
=>
	(assert(lista))
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
(defmodule PERFIL (import DATOS ?ALL))


(defrule PERFIL::R1-determinar-presupuesto ""
  ?c <- (comprador (presupuesto 0));presupuesto del usuario no contiene valor
  =>
  (bind ?r
     (p-cantidad "Cual es su presupuesto? (Numero entero)"));comprobacion de valor permitido
	(modify ?c (presupuesto ?r))
)

(defrule PERFIL::R2-determinar-camara ""
  ?c <- (comprador (camara nil));necesidad del usuario no contiene valor
  =>
  (if (p-si-o-no "Le da usted importancia a la camara del terminal (si/no)? ")
  	then (modify ?c (camara buena))
		else (modify ?c (camara indiferente))
  )
)

(defrule PERFIL::R-determinar-selfies ""
	?c <- (comprador (selfies nil))
	=>
	(if (p-si-o-no "Utiliza usted la camara para hacerse selfies a menudo(si/no)? ")
  	then (modify ?c (selfies si))
		else (modify ?c (selfies no))
  )
)

(defrule PERFIL::R-determinar-cam-ecosistema-apple ""
	?c <- (comprador (ecosistema-apple nil))
	=>
	(if (p-si-o-no "Tiene usted otros dispositivos de apple (si/no)? ")
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
	(if (p-si-o-no "Almacena usted muchos archivos o fotos en su dispositivo (si/no)? ")
  	then (modify ?c (espacio mucho))
		else (modify ?c (espacio poco))
  )
)
;;;****************************************************
;;;* PERFIL DE MOVIL *
;;;****************************************************
(defmodule PERFILM (import DATOS ?ALL));
(defrule PERFILM::R3-determinar-gama-alta ""
	?m <- (buscamovil (gama media))
	(or(comprador (presupuesto ?p &: (>= ?p 400))(juegos-y-apps ?ja &: (eq ?ja muchos)))
		(comprador (presupuesto ?p &: (< ?p 400))(juegos-y-apps ?ja &: (eq ?ja muchos))))
	=>
	(modify ?m (gama alta))
)

(defrule PERFILM::R3-determinar-gama-baja ""
	?m <- (buscamovil (gama media))
	(or(comprador (presupuesto ?p &: (<= ?p 150)))
		(comprador (presupuesto ?p &: (> ?p 150))(juegos-y-apps ?ja &: (eq ?ja pocos))))
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
(defrule PERFILM::R7-lista-inicial""
	(comprador(presupuesto ?p))
	(buscamovil (gama ?g)(sistema-operativo ?s))
	?rm <- (rmovil (modelo ?n)(gama ?g)(sistema-operativo ?s)(precio ?pr &: (<= ?pr ?p)))

	?l <- (lista (lista-moviles $?lmoviles))
	(test (not(member ?n $?lmoviles)))
	=>
	(modify ?l (lista-moviles ?lmoviles ?n))

)
;;;****************************************************
;;;* PERFIL DE MOVIL *
;;;****************************************************
(defmodule FILTRO (import DATOS ?ALL));

(defrule FILTRO::R5-determinar-espacio ""
	(comprador (espacio ?e &: (eq ?e mucho)))
	?l <- (lista (lista-moviles $?lmov1 ?mov $?lmov2 ))
	(rmovil (modelo ?mov)(espacio poco))
	=>
	(modify ?l (lista-moviles ?lmov1 ?lmov2))
)

(defrule FILTRO::R5-determinar-cam-trasera ""
	(comprador (camara ?c &: (eq ?c buena)))
	?l <- (lista (lista-moviles $?lmov1 ?mov $?lmov2 ))
	(rmovil (modelo ?mov)(cam-trasera mala))
	=>
	(modify ?l (lista-moviles ?lmov1 ?lmov2))
)

(defrule FILTRO::R5-determinar-cam-frontal ""
	(comprador (selfies ?s &: (eq ?s si)))
	?l <- (lista (lista-moviles $?lmov1 ?mov $?lmov2 ))
	(rmovil (modelo ?mov)(cam-frontal mala))
	=>
	(modify ?l (lista-moviles ?lmov1 ?lmov2))
)

(defrule FILTRO::R6-determinar-memoria-mucha
	(comprador (juegos-y-apps ?ja &: (neq ?ja pocos)))
	?l <- (lista (lista-moviles $?lmov1 ?mov $?lmov2 ))
	(rmovil (modelo ?mov)(memoria poca))
	=>
	(modify ?l (lista-moviles ?lmov1 ?lmov2))
)

;;;*****************************************************
;;;* DECISION *
;;;*****************************************************
(defmodule DECISION (import DATOS ?ALL))
; (defrule DECISION::Rx-imprimir-busqueda ""
; 	(declare (salience 10))
; 	(buscamovil (espacio ?e)(gama ?g)(sistema-operativo ?s)(cam-frontal ?f)(cam-trasera ?t)(memoria ?mem))
;
; 	=>
; 	(printout t crlf crlf)
; 	(printout t ?e ?g ?s ?f ?t ?mem)
;   (printout t crlf crlf)
;
; )

;;;*****************************************************
;;;* MODULO DE PRESENTACION DEL SISTEMA Y CONCLUSIONES *
;;;*****************************************************
(defmodule PRESENTACION (import DATOS ?ALL))
(defrule PRESENTACION::R11-resultado ""
	(declare (salience 10))
  ?lp <- (lista (lista-moviles $?lmovilesprev1 ?m1 $?lmovilessig1))
				(lista (lista-moviles $?lmovilesprev2 ?m2 $?lmovilessig2))
	(test (neq ?m2 ?m1))

	(and
	  (rmovil (modelo ?m1)(precio ?p1))
	  (not (rmovil (modelo ?m2)(precio ?p2 &: (< ?p2 ?p1))))
	)
	=>
	(printout t crlf crlf)
	(printout t ?m1 ", precio:" ?p1)
  (printout t crlf crlf)
		(modify ?lp (lista-moviles ?lmovilesprev1 ?lmovilessig1))
  ;presentar caracteristicas del telefono
)
(defrule PRESENTACION::R11-resultado-unico""
  ?lp <- (lista (lista-moviles ?m ))
	(rmovil (modelo ?m)(precio ?p))
	=>
	(printout t crlf crlf)
	(printout t ?m ", precio:" ?p)
  (printout t crlf crlf)
		(modify ?lp (lista-moviles ))
  ;presentar caracteristicas del telefono
)

;******************************************
;	Modulo MAIN
;******************************************

(defrule MAIN::inicio
	(initial-fact)
=>	(focus DATOS PERFIL PERFILM FILTRO DECISION PRESENTACION))

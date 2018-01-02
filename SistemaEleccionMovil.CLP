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
(defmodule DATOS (export deftemplate ?ALL)
		(import MAIN ?ALL))

;TODO especificar cada tipo de dato y sus restricciones

;estructura que representa un telefono movil
(deftemplate telefono
	(slot gama)
	(slot precio
	      (type INTEGER))
	(slot procesador)
	(slot sistema-operativo)
	(slot espacio
 	      (type INTEGER))
	(slot camara))

;estructura que representa al usuario/comprador
(deftemplate usuario
	(slot presupuesto (type INTEGER))
	(slot necesidad))

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
          then (bind ?respuesta (lowcase ?respuesta))))
   ?respuesta)

(deffunction p-si-o-no (?pregunta)
   (bind ?respuesta (preguntar ?pregunta si no s n))
   (if (or (eq ?respuesta si) (eq ?respuesta s))
       then TRUE
       else FALSE))

;;;*******************************************************************
;;;* MODULO DE OBTENCION DE PERFIL DE USUARIO
;;;*
;;;*******************************************************************
(defmodule PERFIL (import MAIN ?ALL));TODO: importar solo lo necesario

;TODO revisar y escribir bien las reglas e incluir las restantes

(defrule PERFIL::R-determinar-presupuesto ""
  (not (usuario.presupuesto ?));presupuesto del usuario no contiene valor
  =>
  (bind ?respuesta
     (preguntar "Presupuesto
                              (cantidad)? "
                   ?r &: (r sea un numero)));comprobacion de valor permitido
  (if (< ?respuesta 100)
   then
        (usuario.presupuesto bajo);presupuesto del usuario = bajo
   else (if (< ?respuesta 400)
               then (usuario.presupuesto medio)));presupuesto del usuario = medio
   else (usuario.presupuesto alto);presupuesto del usuario = alto
)

(defrule PERFIL::R-determinar-necesidad ""
  (not (usuario.necesidad... ?));necesidad del usuario no contiene valor
  =>
  (if (p-si-o-no "Usas juegos y aplicaciones (si/no)? ")
  then
    ;(if (p-si-o-no "Usas _muchos_ juegos y aplicaciones (si/no)? ")
    ;  then (usuario.necesidad alta);necesidad muy exigente
    ;  else
        (usuario.necesidad mayor);necesidad mayor
    ;)
  else
    (usuario.necesidad basica);necesidad basica
  )
)

;;;****************************************************
;;;* MODULO DE DECISION *
;;;****************************************************
(defmodule DECISION (import MAIN ?ALL));TODO: importar solo lo necesario

;TODO corregir e incluir reglas que faltan

(defrule DECISION::R-determinar-gama-alta "" ;o deberia evaluarse la gama (alta,media,baja) en una sola regla?
  (not (telefono.gama... ?));gama del telefono no contiene valor
  (or
    (usuario.necesidad mayor)
    (usuario.camara si)
    ;resto de condiciones
  )
  =>
    (telefono.gama alta);gama del movil = alta
)

(defrule DECISION::R-decision-sistema-operativo ""
  (declare (salience 10))
  (usuario.tieneproductosapple);el usuario dispone de (ecosistema de) productos Apple
  ;resto de condiciones
=>
  (telefono.sistema-operativo iOS))

;para sistema Android, iOS o Windows Phone

;;;****************************************************
;;;* MODULO DE CONCLUSION (posible planteamiento)     *
;;;****************************************************

;TODO incluir o no este modulo

;(defmodule CONCLUSION (import MAIN ?ALL));TODO: importar solo lo necesario
;(defrule CONCLUSION::R-conclusion-sistema-operativo-X ""
; (declare (salience 10))
; (sistema-operativo X)
;   =>
; (assert (decision "Sistema Operativo X.")))
;
;...

;;;*****************************************************
;;;* MODULO DE PRESENTACION DEL SISTEMA Y CONCLUSIONES *
;;;*****************************************************
(defmodule PRESENTACION (export ?ALL) (import MAIN ?ALL));TODO: importar solo lo necesario
(defrule PRESENTACION::R - intro ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Prototipo 1: Sistema Experto para eleccion de moviles")
  (printout t crlf crlf)
)

(defrule PRESENTACION::R - resultado ""
  (declare (salience 10))
  (telefono ?tel)
  =>
  (printout t crlf crlf)
  (printout t "Terminal sugerido:")
  (printout t crlf crlf)
  ;presentar caracteristicas del telefono
)

;******************************************
;	Modulo MAIN
;******************************************

(defrule MAIN::inicio
	(initial-fact)
=>	(focus PERFIL DECISION PRESENTACION));TODO definir orden de operacion

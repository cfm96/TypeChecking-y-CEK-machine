#lang play

#|
  Tipo de datos correspondientes a las expresiones permitidas en el lenguaje
  
  s-expr  ::= <num>
          | <boolean>
          | (list '+ <s-expr> <s-expr>)
          | (list '- <s-expr> <s-expr>)
          | (list '* <s-expr> <s-expr>)
          | (list '<= <s-expr> <s-expr>)
          | (list 'if <s-expr> <s-expr> <s-expr>)
          | <id>
          | (fun (<id> : <s-type>) <s-expr>)
          | (<s-expr> <s-expr>)
          | (<s-type>)

  Expr ::=  (num <num>)
          | (tt)
          | (ff)
          | (binop <op> <Expr> <Expr>)
          | (ifc <Expr> <Expr> <Expr>)
          | (id <id>)
          | (fun (<id> : <Type>) <Expr>)
          | (<Expr> <Expr>);

  op ::=  '+
        | '-
        | '*
        | '<=

  s-type ::= 'Number
           | (list '-> <s-type> <s-type>)
           | 'Boolean

|#
(deftype Expr
  ;; core
  (num n)
  (binop op l r)
  ;; unary first-class functions
  (id x)
  (fun binder binderType body)
  (app callee arg)
  ;; language extension
  (tt) ; true
  (ff) ; false
  (ifc c l r) ; sentencia if
  )

#|
  Tipo de datos Type correspondiente a los tipos permitidos por el lenguaje

  Type ::= (numT)
         | (arrowT <Type> <Type>)
         | (boolT)

|#
(deftype Type
  (numT) ;tipo de los numeros
  (arrowT t1 t2) ;tipo flecha
  (boolT) ;tipo booleano
  )

;; parse-type : s-type -> Type
;; Funcion que parsea los tipos
(define (parse-type t)
  (match t
    ['Number (numT)] ;numeros
    ['Boolean (boolT)] ;booleanos
    [(list '-> t1 t2) (arrowT (parse-type t1) (parse-type t2))])) ;flecha

;; parse : s-expr -> Expr
;; Funcion que dada una expresion en sintaxis concreta devuelve la expresion en sintaxis abstracta
(define (parse s)
  (match s
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)] ;simbolos
    [#t (tt)] ;true
    [#f (ff)] ;false
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list '<= l r) (binop '<= (parse l) (parse r))] ;menor o igual
    [(list 'if con lc rc) (ifc (parse con) (parse lc) (parse rc))] ;if
    [(list 'fun (list binder ': type) body) (fun binder (parse-type type) (parse body))] ;funcion
    [(list callee arg) (app (parse callee) (parse arg))] ;aplicacion
    [_ (error 'parse "invalid syntax: ~a" s)]))

;; Implementación de ambientes de tipos
;; (análoga a la de ambientes de valores)

;; TypeEnv ::= ⋅ | <TypeEnv>, <id> : <Type>
(deftype TypeEnv (mtTenv) (aTenv id type env))
(define empty-tenv (mtTenv))
(define extend-tenv aTenv)

(define (tenv-lookup x env)
  (match env
    [(mtTenv) (error 'tenv-lookup "free identifier: ~a" id)]
    [(aTenv id type rest) (if (symbol=? id x) type (tenv-lookup x rest))]
    ))

;; infer-type : Expr TypeEnv -> Type
;; Funcion que dada una expresion y un ambiente de datos infiere el tipo de datos asociado a la expresion
(define (infer-type expr tenv)
  (match expr
    [(num _) (numT)] ;numeros
    [(id x) (def typ (tenv-lookup x tenv)) typ] ;simbolos
    [(binop '<= l r) (def tl (infer-type l tenv)) ;operacion binaria menor o igual
                     (def tr (infer-type r tenv))
                     (if (and (numT? tl) (numT? tr))
                             (boolT)
                             (error 'infer-type "invalid operand type for <="))]
    [(binop op l r) (def tl (infer-type l tenv)) ;operacion binaria restantes
                    (def tr (infer-type r tenv))
                    (if (and (numT? tl) (numT? tr))
                             tl
                             (error 'infer-type "invalid operand type for ~a" op))]
    [(fun binder t b) (arrowT t (infer-type b (aTenv binder t tenv)))] ;funcion
    [(app callee arg) (def tc (infer-type callee tenv)) ;aplicacion
                      (def ta (infer-type arg tenv))
                      (if (arrowT? tc)
                          (match tc
                            [(arrowT t1 t2)
                             (cond
                                   [(and (numT? t1) (numT? ta)) t2]
                                   [(and (boolT? t1) (boolT? ta)) t2]
                                   [else (error 'infer-type "function argument type mismatch")])])
                          (error 'infer-type "function application to a non-function")
                       )
                      ]
    [(tt) (boolT)] ;true
    [(ff) (boolT)] ;false
    [(ifc con lc rc) (def tcon (infer-type con tenv)) ;if
                     (def tlc (infer-type lc tenv))
                     (def trc (infer-type rc tenv))
                     (if (boolT? tcon)
                                 (cond
                                   [(and (numT? tlc) (numT? trc)) tlc]
                                   [(and (boolT? tlc) (boolT? trc)) tlc]
                                   [else (error 'infer-type "if branches type mismatch")])
                                 (error 'infer-type "if condition must be a boolean"))]))

;; ambiente de sustitución diferida
;; Tipo de datos que representa el ambiente con el que se trabajara
(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

;; interface ADT (abstract data type) del ambiente
;; Definicion del ambiente vacio
(define empty-env (mtEnv))

;; "Simplemente" asigna un nuevo identificador para aEnv
;(define extend-env aEnv)
;;
;; es lo mismo que definir extend-env así:
;; (concepto técnico 'eta expansion')
;; Funcion que extiende el ambiente dado un simbolo y su valor asociado
(define (extend-env id val env) (aEnv id val env))


;; Funcion que busca el valor asociado a un id en un ambiente
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x) val (env-lookup x rest))]))

;; num2num-op : (Number Number -> Number) -> Val Val -> Val
;; Funcion que dada una operacion crea una funcion anonima que aplicara a dos argumentos numericos
(define (num2num-op op)
  (λ (l r)
    (match (cons l r)
      [(cons (num n) (num m)) (num (op n m))]
      [_ (error 'num-op "invalid operands")])))


(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))

;; final? : Expr -> Boolean
;; Funcion que dada una expresion devuelve un valor de verdad indicando si la expresion es un valor o no 
(define (final? e)
  (match e
    [(num _) #t] ;numeros si
    [(id _) #f] ;simbolos no
    [(binop _ _ _) #f] ;operaciones binarias no
    [(fun _ _ _) #t] ;funciones si
    [(app _ _) #f]) ;aplicacion si
  )

;; Tipo de datos Kont que contiene los 5 constructores de la continuacion
(deftype Kont
  (mt-k) ; empty kont continuacion vacia
  (binop-r-k op rightc enve prevk) ;componente derecho ob
  (binop-l-k op leftc enve prevk) ;componente izquierdo ob
  (arg-k farg enve prevk) ;argumento de una funcion
  (fun-k efun enve prevk) ;funcion ya evaluada
  )

;; Definicion de la continuacion vacia
(define empty-kont (mt-k))

;; State ::= (<Expr>, <Env>, <Kont>)
;; Tipo de datos State que contiene los 3 componentes de la maquina CEK: control, ambiente y continuacion
(deftype State
  (st expr env kont)
  )

;; inject : Expr -> State
;; Funcion que dada una expresion crea un estado que contiene la expresion, el ambiente vacio y la continuacion vacia
(define (inject expr)
  (st expr empty-env empty-kont))

;; step : State -> State
;; Funcion que dado un estado de la maquina CEK produce uno nuevo segun reglas de reduccion
(define (step c)
  (def (st expr env kont) c)
  (match kont
    [(mt-k)  ; empty kont continuacion vacia
     (match expr ;comienzo de evaluacion
       [(binop op l r) (st l env (binop-r-k op r env kont))]
       [(id x) (def (cons v y) (env-lookup x env))(st v y kont)]
       [(app c a) (st c env (arg-k a env kont))])]
    [(binop-r-k op rc ee pk) (st rc ee (binop-l-k op expr env pk))] ;componente derecho ob
    [(binop-l-k op rc _ pk) ;componente izquierdo ob
     (match op
       ['+ (st (num+ rc expr) env pk)]
       ['- (st (num- rc expr) env pk)]
       ['* (st (num* rc expr) env pk)])]
    [(arg-k fa ee pk) (st fa ee (fun-k expr env pk))] ;argumento de una funcion
    [(fun-k ef ee pk) ;funcion ya evaluada
     (match ef
       [(fun b _ bd) (st bd (extend-env b (cons expr env) ee) pk)])]))

;; eval : Expr -> Expr
;; Funcion que dada una expresion en sintaxis abstracta la evalua para entregar el resultado luego de la aplicacion consecutiva de step
(define (eval expr)
  (define (eval-until-final state)
    (def (st expr _ kont) state)
    (if (and (final? expr) (mt-k? kont))
        expr
        (eval-until-final (step state))))
  (eval-until-final (inject expr)))

;; run : s-exp -> Pair (<Expr>,<Type>)
;; Funcion que dada una expresion en sintaxis concreta, la parsea, para luego entregar un par con la expresión evaluada y su tipo asociado
(define (run s-expr)
  (def parsede (parse s-expr)) ;se parse la expresion en sintaxis concreta
  (def typeofe (infer-type parsede empty-tenv)) ;se extrae el tipo de la expresion
  (def evaluatede (eval parsede)) ;se evalua la expresion
  (cons evaluatede typeofe) ;se devuelve un par con la expresion evaluada y su tipo asociado
  )
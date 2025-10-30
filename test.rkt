#lang play
(require "TypeChecking_and_CEK_machines.rkt")

(print-only-errors #f)

;; TypeChecking Tests

(test (parse-type 'Number) ;numeros
      (numT))

(test (parse-type '(-> Number Number)) ;flechas
      (arrowT (numT) (numT)))

(test (parse-type '(-> (-> Number Number) Number))
      (arrowT (arrowT (numT) (numT)) (numT)))

(test (parse-type '(-> (-> Number Number) (-> Number Number)))
      (arrowT (arrowT (numT) (numT)) (arrowT (numT) (numT))))

(test (parse-type '(-> (-> Number Number) (-> Number (-> Number Number))))
      (arrowT (arrowT (numT) (numT)) (arrowT (numT) (arrowT (numT) (numT)))))

(test (infer-type (num 52) empty-tenv) ;numeros
      (numT))

(test (infer-type (binop '+ (num 1) (num 7)) empty-tenv) ;operaciones binarias
      (numT))

(test/exn (infer-type (binop '+ (num 1) (tt)) empty-tenv)
          "invalid operand type for +")

(test (infer-type (binop '- (num 2) (num 9)) empty-tenv)
      (numT))

(test/exn (infer-type (binop '- (num 33) (ff)) empty-tenv)
          "invalid operand type for -")

(test (infer-type (binop '* (num 5) (num 2)) empty-tenv)
      (numT))

(test/exn (infer-type (binop '* (num 77) (tt)) empty-tenv)
          "invalid operand type for *")

(test (parse-type 'Boolean) ;booleanos
      (boolT))

(test (infer-type (binop '<= (num 1) (num 7)) empty-tenv) ;menor o igual
      (boolT))

(test/exn (infer-type (binop '<= (num 1) (ff)) empty-tenv)
          "invalid operand type for <=")

(test (infer-type (fun 'x (numT) (id 'x)) empty-tenv) ;funciones
      (arrowT (numT) (numT)))

(test (infer-type (fun 'x (arrowT (numT) (numT)) (id 'x)) empty-tenv)
      (arrowT (arrowT (numT) (numT)) (arrowT (numT) (numT))))

(test/exn (infer-type (app (num 1) (num 2)) empty-tenv) ;aplicacion
          "function application to a non-function")

(test/exn (infer-type (app (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x))) empty-tenv)
          "function argument type mismatch")

(test (infer-type (parse '{fun {f : {-> Number Boolean}} {if {f 2} 1 0}}) empty-tenv)
      (arrowT (arrowT (numT) (boolT)) (numT)))

(test (parse '(if (<= 5 6) #t #f))
      (ifc (binop '<= (num 5) (num 6)) (tt) (ff)))

(test (infer-type (tt) empty-tenv) ;true false
      (boolT))

(test (infer-type (ff) empty-tenv) ;true false
      (boolT))

(test (infer-type (ifc (binop '<= (num 5) (num 6)) (num 2) (num 3)) empty-tenv) ;menor o igual
      (numT))

(test/exn (infer-type (ifc (num 5) (num 2) (num 3)) empty-tenv) ;errores
          "if condition must be a boolean")

(test/exn  (infer-type (ifc (binop '<= (num 5) (num 6)) (num 2) (tt)) empty-tenv)
           "if branches type mismatch")

;; CEK Machine Tests

(test (final? (num 1))
      #t)

(test (final? (id 'x))
      #f)

(test (final? (binop '+ (num 1) (num 2)))
      #f)

(test (final? (fun 'x (numT) (id 'x)))
      #t)

(test  (step (st (binop '+ (num 1) (num 2)) (mtEnv) (mt-k)))
       (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k))) )

(test  (step (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k))))
       (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k))))

(test  (step (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k))))
       (st (num 3) (mtEnv) (mt-k)))

(test (step (st (app (fun 'x (numT) (id 'x)) (num 2)) (mtEnv) (mt-k)))
      (st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2) (mtEnv) (mt-k))))

(test  (step (st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2) (mtEnv) (mt-k))))
       (st (num 2) (mtEnv) (fun-k (fun 'x (numT) (id 'x)) (mtEnv) (mt-k))))

(test (step (st (num 2) (mtEnv) (fun-k (fun 'x (numT) (id 'x)) (mtEnv) (mt-k))))
      (st (id 'x) (aEnv 'x (cons (num 2) (mtEnv)) (mtEnv)) (mt-k)))

(test  (step (st (num 2) (mtEnv) (fun-k (fun 'x (numT) (id 'x)) (mtEnv) (mt-k))))
       (st (id 'x) (extend-env 'x (cons (num 2) (mtEnv)) (mtEnv)) (mt-k)))

(test (step (st (id 'x) (extend-env 'x (cons (num 2) (mtEnv)) (mtEnv)) (mt-k)))
      (st (num 2) (mtEnv) (mt-k)))

(test  (step (st (id 'x) (extend-env 'x (cons (num 2) (mtEnv)) (mtEnv)) (mt-k)))
       (st (num 2) (mtEnv) (mt-k)))

(test (run '(+ 1 2))
      (cons (num 3) (numT)))

(test (run '((fun (x : Number) (fun (y : Number) (+ x y))) 2))
      (cons (fun 'y (numT) (binop '+ (id 'x) (id 'y))) (arrowT (numT) (numT))))
#lang pl

#|
Q.2 Expanding the FLANG BNF language:

<FLANG> ::= <num> (1)
           | { + <FLANG> <FLANG> } (2)
           | { - <FLANG> <FLANG> } (3)
           | { * <FLANG> <FLANG> } (4)
           | { / <FLANG> <FLANG> } (5)
           | { with { <id> <FLANG> } <FLANG> } (6)
           | <id> (7)
           | { fun { <id> } <FLANG> } (8)
           | { call <FLANG> <FLANG> } (9)
           | { bool true } (10)
           | { bool false } (11)
           | { = <FLANG> <FLANG> } (12)
           | { < <FLANG> <FLANG> } (13)
           | { > <FLANG> <FLANG> } (14)
           | { not <FLANG> } (15)
           | { if <FLANG> {then-do <FLANG>} {else-do <FLANG>}} (16)
|#

#|
Q.3 Extending the Parser
|#

(define-type FLANG
  [Num Number]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Mul FLANG FLANG]
  [Div FLANG FLANG]
  [With Symbol FLANG FLANG]
  [Id Symbol]
  [Fun Symbol FLANG]
  [Call FLANG FLANG]
  [Bool Boolean]
  [Bigger FLANG FLANG]
  [Smaller FLANG FLANG]
  [Equal FLANG FLANG]
  [Not FLANG]
  [If FLANG FLANG FLANG])

#|
Here we will take as input a Sexpr, and the output would be a FLANG
And in general we want to convecr s-expressions into FLANGs
Basically we took the code written in the class,
and added some new options for the conditions
|#
(: parse-sexpr : Sexpr -> FLANG)
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    ['True (Bool true)]
    ['False (Bool false)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
      (match sexpr
        [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
        [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
      (match sexpr
        [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
        [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'not exp) (Not (parse-sexpr exp))]
    [(cons 'if more)
      (match sexpr
        [(list 'if condition  (list 'then-do then-exp) (list 'else-do else-exp))
        (If (parse-sexpr condition) (parse-sexpr then-exp) (parse-sexpr else-exp))]
        [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])] 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#|
Q.4 Extending subst and eval
In the subst we will take as input FLANG , Symbol and FLANG
And the output would be FLANG
 |#
 
(: subst : FLANG Symbol FLANG -> FLANG)
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With name named body)
      (With name
            (subst named from to)
            (if (eq? name from)
              body
              (subst body from to)))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun name body)
      (if (eq? name from)
        expr
        (Fun name (subst body from to)))]
    [(Bool b) expr]
    [(Equal l r) (Equal (subst l from to) (subst r from to))]
    [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
    [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
    [(Not e) (Not (subst e from to))]
    [(If cond-e then-e else-e) (If (subst cond-e from to) (subst then-e from to) (subst else-e from to))]))




#|
The following function is used in multiple places below
hence, it is now a top-level definition
|#
(: Num->number : FLANG -> Number)
(define (Num->number e)
  (cases e
    [(Num n) n]
    [else (error 'Num->number "expected a number, got: ~s" e)]))


;; gets a Racket numeric binary operator
;; and uses it within a FLANG `Num' wrapper
(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
(define (arith-op op expr1 expr2)
  (Num (op (Num->number expr1) (Num->number expr2))))


;; gets a Racket Boolean binary operator (on numbers)
;; and applies it to two `Num' wrapped FLANGs
(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
(define (logic-op op expr1 expr2)
  (Bool (op (Num->number expr1) (Num->number expr2))))

#|
The function gets a Flang E (of any kind) and returns if its appropriate
Boolean value -- wich is true if and only if E does not
represent false
|#
(: flang->bool : FLANG -> Boolean)
(define (flang->bool e)
  (cases e
    [(Bool B) B]
    [else true]))

#|
Evaluates FLANG expressions by reducing them to *expressions*
|#
(: eval : FLANG -> FLANG)
(define (eval expr)
(cases expr
  [(Num n) expr]
  [(Add l r) (arith-op + (eval l) (eval r))]
  [(Sub l r) (arith-op - (eval l) (eval r))]
  [(Mul l r) (arith-op * (eval l) (eval r))]
  [(Div l r) (arith-op / (eval l) (eval r))]
  [(With name named body)
    (eval (subst body
                name
                (eval named)))]
  [(Id name) (error 'eval "free identifier: ~s" name)]
  [(Fun name body) expr]
  [(Call fun-expr arg-expr)
    (let([fval (eval fun-expr)])
      (cases fval
        [(Fun name body)
        (eval (subst body
                      name
                      (eval arg-expr)))]
        [else (error 'eval "`call' expects a function, got: ~s"
                          fval)]))]
  [(Bool b) expr]
  [(Equal l r) (logic-op = (eval l) (eval r))]
  [(Bigger l r) (logic-op > (eval l) (eval r))]
  [(Smaller  l r) (logic-op < (eval l) (eval r))]
  [(If l m r)
        (let ([cond (flang->bool (eval l))])
          (if cond (eval m) (eval r)))]
  [(Not exp) (Bool (not (flang->bool (eval exp))))]))
            

#|
Q.5 Extending the run procedure
Evaluate a FLANG program contained in a string
Here we taking the function run from the class
The input remain String
But the output would be union between Number Boolean and FLANG
If we recieved Bool we just return it
If we recieved Num we just return it
Else we just return the result
The main difficulty was to understand how to compute the
Run function in just 3 rows as asked by the teacher
|#
(: run : String -> (U Number Boolean FLANG))
(define (run str)
    (let ([result (eval (parse str))])
        (cases result
          [(Bool B) B]
          [(Num n) n]
          [else result])))


#|
Bellow is some of the given tests
In addition with our new tests
|#

(test (run "True") => true)
(test (run "{> 3 44}") => false)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8}
{if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
{if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {foo {fun {x}
{if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
=> (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0}
{if {> x 0} {/ 2 x} x}}")
=error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< False 5}")
=error> "Num->number: expected a number, got: #(struct:Bool #f)")
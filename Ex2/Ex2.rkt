#lang pl 02
;;--------------------------------------------------------------------------------------------------

#|
General comments:
We did not consult with others.
There were difficulties in understanding question 3 as a whole in terms of the structure of the functions given to us.
I spent an hour reading the language's documentation and examples.
Each function took me about 25 minutes on average except for the BNF questions where a little longer.
|#
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#|

#ֿ\<num> -> Given a character from a type #\v where v is a digit.
  <SES> -> Given a string of type SES (String expression String)
  <SEN> -> Given a string of type SEN (String expression Number)
  <Strings> ->  build string 
  <D> -> build number
  <C> -> build #ֿ\<num> #ֿ\<num> ...

Q1.a
     <SE>:=  #ֿ\<num> (0)
            | <SES>  (1)
            | <SEN>  (2)

     <SES>:=  <Strings>                            (3)
            | {string <C>}                         (4)
            | {string-append <SES> <SES>}          (5)
            | {string-insert <SES> #\<num> <SEN> } (6)
            | {number->string <SEN>}               (7)

     <SEN>:=  <D>                       (9)
            | {string-length <String>}  (10)

     <Strings>:=  λ          (11)
            | <Char>         (12)
            | <Char><String> (13)

     <D>:=    <num>     (14)
            | <num><D> (15)

     <C>:=  #\<num>       (16)
            | #\<num> <C> (17)

     <Char>:=  '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'  (18)
     <num> :=   1 |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  0    (19)
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Q1.b

1) given input : "12344"
         output : <SE> -> <SES> -> (3) -> (13) -> (13) -> (13) -> (13) -> (12)

2) given input : 12
         output : <SE> -> <SEN> -> (9) -> (15) -> (14)

3) given input : (string-insert "1357" #\4 66)
         output : <SE> -> <SES> -> (6) {string-insert <SES> #\<num> <SEN> }  -> {string-insert (3) (0) (9)} ->
                 {string-insert (13) #\4 (15)} -> {string-insert (13) #\4 (14)} -> {string-insert (13) #\4 66} ->
                 {string-insert (12) #\4 66} -> {string-insert "1357" #\4 66}
|#

;; --------------------------------------------------------------------------------------------
#|
Q2
Here we first use the map built in function that takes every time 2 elements (Numbers) from the given list
And applying the multiplication function on each on of them (square of each element)
Then, we are calling the built in foldl function, that takes the square of each element and summing them with the
'+ 0' operator.
|#
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map * lst lst)))

(test (sum-of-squares '(2 5 8)) => 93)
(test (sum-of-squares '(3 14 5)) => 230)
(test (sum-of-squares '(0 0 10)) => 100)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '()) => 0)

#|
Q3.a
* In this function, we were asked to write a function createPolynomial
* that takes as arguments a list of 𝑘 numbers 𝑎0, … , 𝑎𝑘−1 
* and returns as output a function. 
* The returned function takes a number 𝑥0 and return the value of 
* the polynomial 𝑎0 ⋅ 𝑥0 + ⋯ + 𝑎𝑘−1 ⋅ 𝑥𝑛−1 at 𝑥0. 
|#
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs) 
  (: poly : (Listof Number) Number Integer Number -> Number) 
  (define (poly argsL x power accum)
    (if (null? argsL) accum
    (poly (rest argsL) x (+ 1 power) (+ accum (* (first argsL) (expt x power))))))
  (: polyX : Number -> Number)
  (define (polyX x) 
     (poly coeffs x 0 0))
  polyX)

(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))
(test (p2345 12) => (+ (* 2 (expt 12 0)) (* 3 (expt 12 1)) (* 4 (expt 12 2)) (* 5 (expt 12 3))))
(test (p2345 14) => (+ (* 2 (expt 14 0)) (* 3 (expt 14 1)) (* 4 (expt 14 2)) (* 5 (expt 14 3))))
(test (p2345 115) => (+ (* 2 (expt 115 0)) (* 3 (expt 115 1)) (* 4 (expt 115 2)) (* 5 (expt 115 3))))
(test (p2345 16) => (+ (* 2 (expt 16 0)) (* 3 (expt 16 1)) (* 4 (expt 16 2)) (* 5 (expt 16 3))))
(test (p2345 17) => (+ (* 2 (expt 17 0)) (* 3 (expt 17 1)) (* 4 (expt 17 2)) (* 5 (expt 17 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6(expt 11 2))))
(test (p536 12) => (+ (* 5 (expt 12 0)) (* 3 (expt 12 1)) (* 6(expt 12 2))))
(test (p536 0) => (+ (* 5 (expt 0 0)) (* 3 (expt 0 1)) (* 6(expt 0 2))))
(test (p536 1) => (+ (* 5 (expt 1 0)) (* 3 (expt 1 1)) (* 6(expt 1 2))))

(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)
(test (p_0 2) => 0)
(test (p_0 3) => 0)
(test (p_0 5) => 0)

#|
Q3.b.1
Here we wrote the BNF for the given language, such as after the Poly expression
      we could get a list of AE expressions and another list of AE expressions

    <PLANG> ::= {{poly <AEs>} <AEs>}

    <AEs> ::= <AE>
            | <AE> <AEs>

    <AE> ::= <num>
            | {+ <AE> <AE> }
            | {- <AE> <AE> }
            | {/ <AE> <AE> }
            | {* <AE> <AE> }
    <num> = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
|#

#|
Q3.b.2
Here we've created a parser for the new language
Defined 2 types of expressions according to our BNF tree
Poly - takes to list's of AE
and AE that takes 2 numbers and return the sum/div/...
|#

(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sexpr)
  (match sexpr
  [(number: n) (Num n)]
  [(list '+ lhs rhs) (Add (parse-sexpr lhs)
    (parse-sexpr rhs))]
  [(list '- lhs rhs) (Sub (parse-sexpr lhs)
    (parse-sexpr rhs))]
  [(list '* lhs rhs) (Mul (parse-sexpr lhs)
    (parse-sexpr rhs))]
  [(list '/ lhs rhs) (Div (parse-sexpr lhs)
    (parse-sexpr rhs))]
  [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



(: parse : String -> PLANG)
(define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code
        [(list (list 'poly) (list param2 ...)) (error 'parse "at least one coefficient is required in: ~s" code)]
        [(list (list 'poly param1 ...) (list)) (error 'parse "at least one point is required in: ~s" code)]
        [(list (list 'poly param1 ...) (list param2 ...)) (Poly (make param1) (make  param2))]
        [else (error 'parse "bad syntax in: ~s" code)])))

(: make : (Listof Sexpr) -> (Listof AE))
(define (make str)
  (map parse-sexpr str))

(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in") 
(test (parse "{{poly 1 2 3 4} {1 2 3 4}}") => (Poly (list (Num 1) (Num 2) (Num 3) (Num 4))(list (Num 1) (Num 2) (Num 3) (Num 4))))         
(test (parse "{{poly 1 2 3 } {1 2 3 }}") => (Poly (list (Num 1) (Num 2) (Num 3))(list (Num 1) (Num 2) (Num 3))))
(test (parse-sexpr (string->sexpr "{}"))  =error> "bad syntax in") 
(test (parse "{{poly } {}}") =error> "parse: at least one coefficient is required in")
(test (parse "{{poly } {1}}") =error> "parse: at least one coefficient is required in")
(test (parse "{{poly } {1 3}}") =error> "parse: at least one coefficient is required in")
(test (parse "{{poly } { 1 3 4}}") =error> "parse: at least one coefficient is required in")
(test (parse "{{poly 1} {}}") =error> "parse: at least one point is required in")
(test (parse "{{poly 1 2} {}}") =error> "parse: at least one point is required in")
(test (parse "{{}  }") =error> "parse: bad syntax in")
(test (parse "{ {} }") =error> "parse: bad syntax in")
(test (parse "{{} {} }") =error> "parse: bad syntax in")
(test (parse "{{poly 4/5 } {1/2 2/3 3}}")  => (Poly (list (Num 4/5)) (list (Num 1/2) (Num 2/3) (Num 3)))) 
(test (parse "{{poly 4/5 {+ 0 1}} {}}")  =error> "parse: at least one point is required in")    
(test (parse "{{poly 4/5 } {1/2 2/3 3} {poly 1 2 4} {1 2}}" ) =error> "parse: bad syntax in") 
(test (parse "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}")  
     => (Poly (list (Add (Num 0) (Num 1)) (Num 1) (Mul (Num 0) (Num 9)))  
              (list (Sub (Num 4) (Num 5)) (Num 3) (Div (Num 27) (Num 9)))))

     

#|
Q3.b.3
Here we creating a function for evaluating the given AE
And return a list of numbers.
|#
(: eval : AE -> Number)
  (define (eval expr) 
    (cases expr 
      [(Num n)  n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))

#|
To build our function correctly we must use the createPolynomial function which creates a polynomial in the poly AE and then activates it in each element in the second list of the poly.
So poly = first list of AE, x= second list of AE
and we will get a list of results.
|#
(: eval-poly : PLANG -> (Listof Number)) 
(define (eval-poly p-expr) 
  (cases p-expr 
    [(Poly l r)  (map (createPolynomial (map eval l)) (map eval r))])) 
 
  (: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str))) 


(test (run "{{poly {+ 1 1} {- 4 1} {* 1 4} 5} {{- 4 4} {* 1 5} 3}}") => '(2 742 182))
(test (run "{{poly {- 1 1} 1 7} {-1 2 3}}")  => '(6 30 66))
(test (run "{{poly 2} {{- 5 6}}}")  => '(2))
(test (run "{{poly {/ 6 8} {- 2 1}} {{- 8 4}}}") => '(19/4)) 
(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}") =>'(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}") =>'(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")=> '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}") =>'(14))
(test (run "{{poly 1 1 0} {-1 3 3}}") =>'(0 4 4))
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}")=> '(14))
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3{/ 27 9}}}")=> '(0 4 4))



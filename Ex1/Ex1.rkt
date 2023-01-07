#lang pl
#|
Q1, a.
Here we are taking a list that holds a list in it
And the main goal is to combine the inner lists into one list
The main approach is to have base condition -> if the list is empty then return null
                                            -> else, take the first element of the list and append it to the rest of the list in recoursive way
The main difficulty here was to understand how the list inside a list is worked and needed to be approach
|#
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list(rest lst)))]))

;; TEST FOR 'a'
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test (open-list '(() (2 3 3 4 100) (0) (-1000 1 12))) => '(2 3 3 4 100 0 -1000 1 12))
(test (open-list '((0))) => '(0))
(test (open-list '(())) => '())

;; ---------------------------------------

#|
Q1, b.
Given list of list, we need to return pair of minimum and maximum numbers from the inner list
The main problem here is to compare each two numbers from the list, we can't hold the elements in some object and do it in a iterative way
So every run we just take two pairs with the 'car' and 'cdr' operators of the racket language
Where 'car' -> returns the first element of the given pair
      'cdr' -> returns the second element of the given pair
|#

;; helper function that returns the minimum number from given list
;; if the list is empty, we will return +inf.0
(: my_min : (Listof Number) -> Number)
(define (my_min lst)
  (cond [(null? lst) +inf.0]
        [(< (car lst) (my_min (cdr lst))) (car lst)]
        [else (my_min (cdr lst))]))
;; TEST FOR 'my_min'
(test (my_min '(-10 0 12 13 4 50)) => -10)
(test (my_min '(0)) => 0)
(test (my_min '()) => +inf.0)

;; helper function that returns the maximun number from given list
;; if the list is empty, we will return -inf.0
(: my_max : (Listof Number) -> Number)
(define (my_max lst)
  (cond [(null? lst) -inf.0]
        [(> (car lst) (my_max (cdr lst))) (car lst)]
        [else (my_max (cdr lst))]))
;; TEST FOR 'my_max'
(test (my_max '(-10 0 12 13 4 50)) => 50)
(test (my_max '(0)) => 0)
(test (my_max '()) => -inf.0)

;; the actual function that returns pair of min and max with the help of the two functions above
;; here I used the min and max functions that I've wrote above, combined with the 'open-list' function from first question
(: min&max : (Listof(Listof Number)) -> (Listof Number))
(define (min&max lst)
  (list (+ 0.0 (my_min(open-list lst))) (+ 0.0 (my_max(open-list lst)))))

;; TEST FOR 'b'
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((-10 0 1) (50 14 -2) (-100 100 23 4) (0 6))) => '(-100.0 100.0))
(test (min&max '((98 14) (-10 0 2) (45 1542 23 -56))) => '(-56.0 1542.0))
(test (min&max '((-inf.0 14 -100) (10 +inf.0 0))) => '(-inf.0 +inf.0))
(test (min&max '(())) => '(+inf.0 -inf.0))

;; ---------------------------------------

#|
Q1, c.
Here we are going to implement the same method as in the previous question, but with the 'apply' built in function
So we actually used the built in 'min' and 'max' functions of racket, in order to use the apply function
Where apply need to recieve some operator (+ | - | / | sort...) and a list, and then produces the output
The main difficulty was to understand what the apply function need to recieve, and where to insert the apply
|#
(: min&max_apply : (Listof(Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
  (list (apply min(open-list lst)) (apply max(open-list lst))))

;; TEST FOR 'c'
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
(test (min&max_apply '((-inf.0 -10000 2) (0 -12 1 14 50) (9 2 -1) (233 11 +inf.0))) => '(-inf.0 +inf.0))
(test (min&max_apply '((0 0 0) (0))) => '(0 0))

;; ---------------------------------------
#|
Q2.
In this section we will implement simple Table data structure.
First, we will create a new type called Table
The main difficulty for me was to understand how the table should work
And how the arguments needed to be provided to tha data structure.
This question took me the most time to solve from all the assigment.
|#
(define-type Table
  [EmptyTbl]
  [Add Symbol String Table])

;; 2.3 (search in the table)
;; need to recieve a symbol (key) and a table
;; and return the first value that is keyed
(: search-table : Symbol Table -> (U String #f))
(define (search-table symb tb)
  (cases tb
    [(EmptyTbl) #f]
    [(Add a b c) (if (eq? a symb) b (search-table symb c))]))

;; 2.4 (remove from table)
;; need to recieve a table and a key
;; and return new table without the given key(item)
(: remove-item : Table Symbol -> Table)
(define (remove-item tb item)
  (cases tb
    [(EmptyTbl) (EmptyTbl)]
    [(Add a b c) (cond [(not (eq? a item)) (Add a b (remove-item c item))]
                       [else c])]))
;; TEST FOR Q2
(test (EmptyTbl) => (EmptyTbl))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) =>
(Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) =>
(Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
=> "AAA")
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'a)
=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b)
=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))
                                          







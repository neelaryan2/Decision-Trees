#lang racket

(require "decision_functions.rkt")
(require 2htdp/batch-io)

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

(provide titanicout)
(define titanicout "../output/titanic-decision-tree.dot")

(provide mushroomout)
(define mushroomout "../output/mushroom-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw (cdr (read-csv-file toytrain)))

(provide titanic-raw)
(define titanic-raw (map (lambda (x) (cddr x))
                         (cdr (read-csv-file titanictrain))))

(provide mushroom-raw)
(define  mushroom-raw (cdr (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data)
  (let ([l (map (lambda (x) (string->number x)) data)])
    (cons (cdr l) (car l))))

;list of (features . result)
(provide toy)
(define toy
  (map (lambda (x) (format x)) toy-raw))

(provide titanic)
(define titanic
  (map (lambda (x) (format x)) titanic-raw))

(provide mushroom)
(define mushroom
  (map (lambda (x) (format x)) mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (let ([l (map (lambda (x) (cdr x)) data)])
    (/ (foldr + 0 l) (length l))))

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (let* ([p (get-leaf-prob data)]
         [n (- 1 p)])
  (if (or (= p 0) (= n 0)) 0
      (- (+ (* p (log p 2)) (* n (log n 2)))))))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide packer)
(define (packer l f)
  (if (null? (cdr l)) (list l)
      (let ([next (packer (cdr l) f)])
        (cond [(equal? (f (caadr l)) (f (caar l)))
               (cons (cons (car l) (car next)) (cdr next))]
              [else (cons (list (car l)) next)]))))

(provide entropy-diff)
(define (entropy-diff f data)
  (let* ([g (if (procedure? f) f (cdr f))]
         [lst1 (map (lambda (x) (cons (g (car x)) (cdr x))) data)]
         [lst2 (sort lst1 (lambda (x y) (< (car x) (car y))))]
         [lst3 (packer lst2 (lambda (x) x))]
         [weights (foldr + 0 (map (lambda (x) (* (get-entropy x) (length x))) lst3))]         )
    (- (get-entropy data) (/ weights (length data)))))
    
;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
    (argmax (lambda (x) (entropy-diff x data)) candidates))

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide list-equal?)  
(define (list-equal? lst)
 (andmap (lambda (x) (equal? (cdr x) (cdar lst))) lst))

(provide build-tree)
(define (build-tree candidates data depth)
  (cond [(null? candidates) (DTree (number->string (get-leaf-prob data)) "" '())]
        [(= depth 0) (DTree (number->string (get-leaf-prob data)) "" '())]
        ;[(list-equal? data) (DTree (number->string (cdar data)) "" '())]
        [else (let* ([fx (choose-f candidates data)]
                     [l1 (sort data (lambda (x y) (< ((cdr fx) (car x))
                                                     ((cdr fx) (car y)))))]
                     [l2 (packer l1 (cdr fx))]
                     [l3 (map (lambda (x) ((cdr fx) (caar x))) l2)])
                (DTree (car fx) (cons l3 (cdr fx))
                       (map (lambda (x)
                              (build-tree (remove fx candidates) x (- depth 1))) l2)))]))


;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
;(provide make-decision)
(provide make-decision)
(define (make-decision tree test)
  (match tree
    [(DTree prob "" '()) (string->number prob)]
    [(DTree name (cons l f) k) (let ([ind (index-of l (f test))])
                                 (if (not ind) 0
                                     (make-decision (list-ref k (index-of l (f test))) test)))]))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1)))))

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")))) children)))

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs))))

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}" )))
;============================================================================================================
;============================================================================================================
;============================================================================================================

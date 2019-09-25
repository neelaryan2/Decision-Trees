#lang racket

(require "master.rkt")
(require "decision_functions.rkt")
(require "testdata.rkt")
(require 2htdp/batch-io)
(provide toyout)
(define toyout "../output/toy-decision-tree-test.dot")

(provide titanicout)
(define titanicout "../output/titanic-decision-tree-test.dot")

(provide mushroomout)
(define mushroomout "../output/mushroom-decision-tree-test.dot")

(define dotfile1
  (display-tree (build-tree (list y1 y2 y3 y4>62) toy 4) toyout))
(define dotfile2
  (display-tree (build-tree (list pclass sex age>25 sibsp parch fare>50 emb) titanic 6) titanicout))
(define dotfile3
  (display-tree (build-tree (list cshape csurf bruise odor gatch gspace gsize sshape nring pop hab) mushroom 10) mushroomout))

(define test1
  (let* ([train toy]
         [test toy_test]
         [candidates (list y1 y2 y3 y4>62)]
         [dtree (build-tree candidates train 4)])
    (map (lambda (x) (make-decision dtree x)) test)
    )
  )

(define test2
  (let* ([train titanic]
         [test titanic_test]
         [candidates (list pclass sex age>25 sibsp parch fare>50 emb)]
         [dtree (build-tree candidates train 5)])
    (map (lambda (x) (make-decision dtree x)) test)
    )
  )

(define test3
  (let* ([train mushroom]
         [test mushroom_test]
         [candidates (list cshape csurf bruise odor gatch gspace gsize sshape nring pop hab)]
         [dtree (build-tree candidates train 8)])
    (map (lambda (x) (make-decision dtree x)) test)
    )
  )
#lang racket/base
 
(require rackunit
         "compiler.rkt")
(require "utilities.rkt")
(require rackunit/text-ui)

; test the string representation of var_test_{i} after the rco pass is correct
(define (test-file file-name expected-output)
  (define a (read-program file-name))
  (define b (uniquify a))
  (define c (remove-complex-opera* b))
  (define o (open-output-string))
  (print c o)
  (check-equal? expected-output (get-output-string o))
  )

(define rco-tests
  (test-suite
   "Tests for rco pass"
   (test-file "tests/var_test_1.rkt" "(Program '() (Int 42))")
   (test-file "tests/var_test_2.rkt" "(Program '() (Let 'tmp.1 (Let 'tmp.2 (Prim '- (list (Int 10))) (Prim '- (list (Var 'tmp.2)))) (Prim '- (list (Var 'tmp.1)))))")
   (test-file "tests/var_test_3.rkt" "(Program '() (Prim '+ (list (Int 20) (Int 22))))")
   (test-file "tests/var_test_4.rkt" "(Program '() (Let 'tmp.1 (Prim '+ (list (Int 42) (Int 10))) (Let 'tmp.2 (Prim '- (list (Int 10))) (Prim '+ (list (Var 'tmp.1) (Var 'tmp.2))))))")
   (test-file "tests/var_test_5.rkt" "(Program '() (Let 'x.1 (Int 41) (Prim '+ (list (Var 'x.1) (Int 1)))))")
   (test-file "tests/var_test_6.rkt" "(Program '() (Let 'x.1 (Int 42) (Let 'x.2 (Var 'x.1) (Var 'x.2))))")
   (test-file "tests/var_test_7.rkt" "(Program '() (Let 'x.1 (Let 'x.2 (Int 4) (Prim '+ (list (Var 'x.2) (Int 1)))) (Prim '+ (list (Var 'x.1) (Int 2)))))")
   (test-file "tests/var_test_8.rkt" "(Program '() (Let 'x.1 (Int 32) (Let 'tmp.1 (Int 10) (Prim '+ (list (Var 'tmp.1) (Var 'x.1))))))")
   (test-file "tests/var_test_9.rkt" "(Program '() (Let 'x.1 (Let 'x.2 (Int 20) (Let 'tmp.1 (Int 22) (Prim '+ (list (Var 'x.2) (Var 'tmp.1))))) (Var 'x.1)))")
   ))

(run-tests rco-tests)
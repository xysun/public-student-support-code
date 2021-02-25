#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Rint.rkt")
(require "utilities.rkt")
(require "mycompiler.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Rint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Rint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))


(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info ((rco-exp '()) e))]
  ))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
  [(Program info e)
    (define cvar (explicate-tail e))
    ;(println cvar)
    (CProgram info `((start . ,cvar)))
  ]
  )
)

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info `((start . ,tail)))
      (define instrs (select-instructions-tail tail '()))
      (define block (Block '() instrs))
      (X86Program info `((start . ,block)))
    ]
  ))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (match p
    [(X86Program info dict)
      (define block (dict-ref dict 'start))
      (define-values (new-block env) (assign-homes-block block '()))
      (define ss (dict-ref env 'stack-space 0))
      ; make sure it's divisible by 16
      (define ss16 (cond [(zero? (remainder ss 16)) ss] [else (+ 8 ss)]))
      (X86Program (dict-set info 'stack-space ss16) `((start . ,new-block)))
    ]
  ))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [(X86Program info dict)
      (define block (dict-ref dict 'start))
      (match block
        [(Block blockinfo instrs) 
        (define patched-instrs (flatten (map patch-instructions-instr instrs))) 
        (define new-block (Block blockinfo patched-instrs))
        (X86Program info (dict-set dict 'start new-block))
        ]
      )
    ]
  ))

;; print-x86 : x86 -> string
(define (print-x86 p)
  (error "TODO: code goes here (print-x86)"))

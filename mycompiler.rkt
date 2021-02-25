#lang racket

(require racket/dict)
(require racket/format)
(provide uniquify-exp rco-exp explicate-tail select-instructions-tail)
(require "utilities.rkt")

; represent Rvar
; operations: read, -, +

; (struct Int (value));
; (struct Prim (op args));
; (struct Program (info body));
; (struct Var (var));
; (struct Let (var e body));
; 

;interpreter for Rvar
(define interp-Rvar-class
  (class object%
    (super-new)

    (define/public ((interp-exp env) e) ; currying
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (define r (read))
         (cond [(fixnum? r) r]
               [else (error 'interp-exp "expected an integer" r)])]
        [(Prim '- (list e)) (- 0 ((interp-exp env) e))]
        [(Prim '+ (list e1 e2)) (+ ((interp-exp env) e1) ((interp-exp env) e2))]
        [(Var x) (dict-ref env x)]
        [(Let x e body)
         (define new-env (dict-set env x ((interp-exp env) e)))
         ((interp-exp new-env) body)]
      ))

    (define/public (interp-program p)
      (match p
        [(Program '() e) ((interp-exp '()) e)]))
    
    )
)

(define (interp-Rvar p)
  (send (new interp-Rvar-class) interp-program p))

; representing X86Int, NOT X86Var
; (struct Imm (value)) ; int
; (struct Reg (name)) ; register
; (struct Deref (reg offset))
; (struct Instr (name arg*))
; (struct Callq (target arity))
; (struct Retq ())
; ; note: Pushq and Popq are not defined in the public support code
; (struct Pushq (src))
; (struct Popq (dest))
; (struct Jmp (target))
; (struct Block (info instr*))
; (struct X86Program (info CFG)) ; CFG = control flow graph, right now is a map of label -> block
; ; no Pus
; 
; ; extra structs for CVar intermediate language
; (struct CProgram (info CFG)); CFG same as above; info is a list of local variables
; (struct Return (arg));
; (struct Seq (stmt tail));
; (struct Assign (lhs rhs));
; 

; uniquify pass

(define (uniquify-exp env)
  (lambda (exp)
    (match exp
      [(Int n) (Int n)]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))]
      [(Var x) (Var (dict-ref env x))]
      [(Let x e body)

       ; TODO: use (gen-var)
       (define new-i (add1 (dict-ref env 'i 0)))
       (define new-x (string->symbol (string-append-immutable "x." (~v new-i))))

       (define new-env
         (dict-set
          (dict-set env 'i new-i)
          x new-x))
       
       (Let new-x ((uniquify-exp new-env) e) ((uniquify-exp new-env) body))]
      )))


; pass 2: remove complex operands;
; arguments of operations: -, + are atomic
(define (gen-var env)
  (define new-i (add1 (dict-ref env 'i 0)))
  (define new-x (string->symbol (string-append-immutable "tmp." (~v new-i))))
  (define new-env (dict-set env 'i new-i))
  (values new-x new-env)
  )


(define (rco-atom env)
  (lambda (exp)
    (match exp
      [(Int n) (values exp env)]
      [(Var v) (values exp env)]
      [(Prim '- (list e))
       (define-values (tmp-var new-env) (gen-var env))
       ; for the e
       (define-values (tmp-var-2 env2) ((rco-atom new-env) e))
       (match tmp-var-2
         [(Int v) (values (Var tmp-var) (dict-set env2 tmp-var exp))]
         [(Var v) (values (Var tmp-var) (dict-set env2 tmp-var (Let v (dict-ref env2 v) (Prim '- (list tmp-var-2)))))]
         )]

      
      [(Prim '+ (list e1 e2))
       
       (define-values (tmp-var new-env) (gen-var env))
       ; for the es
       (define-values (tmp2 env2) ((rco-atom new-env) e1))
       (define-values (tmp3 env3) ((rco-atom env2) e2))
       (match (list tmp2 tmp3)
         [(list (Int i1) (Int i2)) (values (Var tmp-var) (dict-set env3 tmp-var exp))]
         [(list (Var v1) (Int i2)) (values (Var tmp-var) (dict-set env3 tmp-var (Let v1 (dict-ref env3 v1) (Prim '+ (list tmp2 tmp3)))))]
         [(list (Int i1) (Var v2)) (values (Var tmp-var) (dict-set env3 tmp-var (Let v2 (dict-ref env3 v2) (Prim '+ (list tmp2 tmp3)))))]
         [(list (Var v1) (Var v2)) (values (Var tmp-var) (dict-set env3 tmp-var (Let v1 (dict-ref env3 v1) (Let v2 (dict-ref env3 v2) (Prim '+ (list tmp2 tmp3))))))]
         )
       ]
      ; let: naive variable
      ; optimization: if body == var, then the let binding can just be `e`
      [(Let var e body)
       (define-values (tmp-var new-env) (gen-var env))
       (match body
         [(Var v)
          (cond
            [(eq? v var) (values (Var tmp-var) (dict-set new-env tmp-var e))]
            [else (values (Var tmp-var) (dict-set new-env tmp-var exp))]
            )]
         [else (values (Var tmp-var) (dict-set new-env tmp-var exp))]
      )])
  ))

(define (rco-exp env)
  (lambda (e)
  (match e
    [(Int value) e]
    [(Var v) e]
    [(Prim 'read '()) e]
    [(Prim '- (list exp))
     (define-values (atm new-env) ((rco-atom env) exp))
     (match atm
       [(Int v) e]
       [(Var tmp)
        (cond
          [(dict-has-key? new-env tmp) (Let tmp (dict-ref new-env tmp) (Prim '- (list atm)))]
          [else e])
        ])
     ]
    [(Prim '+ (list e1 e2))
     (define-values (atm1 env1) ((rco-atom env) e1))
     (define-values (atm2 env2) ((rco-atom env1) e2))
     (match (list atm1 atm2) ; TODO: lots of duplicate with rco-atom
       
       [(list (Int i1) (Int i2)) e]
       [(list (Int i1) (Var v2))
        (cond
          [(dict-has-key? env2 v2) (Let v2 (dict-ref env2 v2) (Prim '+ (list atm1 atm2)))]
          [else e])
        ]
       [(list (Var v1) (Int i2))
        (cond
          [(dict-has-key? env2 v1) (Let v1 (dict-ref env2 v1) (Prim '+ (list atm1 atm2)))]
          [else e])
        ]
       [(list (Var v1) (Var v2))
        (cond
          [(dict-has-key? env2 v1)
           (cond
             [(dict-has-key? env2 v2) (Let v1 (dict-ref env2 v1) (Let v2 (dict-ref env2 v2) (Prim '+ (list atm1 atm2))))]
             [else (Let v1 (dict-ref env2 v1) (Prim '+ (list atm1 e2))) ])] ; v1 is new, v2 is original
          [else
           (cond
             [(dict-has-key? env2 v2) (Let v2 (dict-ref env2 v2) (Prim '+ (list e1 atm2))) ]; v1 is original v2 is new
             [else e])])
        ]
    )]
    [(Let var exp body)
     (define e1 ((rco-exp env) exp))
     (define e2 ((rco-exp env) body))
     (Let var e1 e2)
     ]
    )
))

; pass 3: explicate control
(define (explicate-tail e)
  (match e
    [(Int n) (Return e)]
    [(Var v) (Return e)]
    [(Prim op es) (Return e)] ; is this right?
    [(Let x rhs body)
     (define cont (explicate-tail body))
     (explicate-assign rhs x cont)]
    [else (error "explicate-tail unhandled case" e)]
    )
  )

(define (assign-return x tail cont)
  ; replace last Return from tail by Assign, then attach cont
  (match tail
    [(Return e) (Seq (Assign (Var x) e) cont)]
    [(Seq stmt tl) (Seq stmt (assign-return x tl cont))]))
    

(define (explicate-assign e x cont)
  (match e
    [(Int n) (Seq (Assign (Var x) e) cont)]
    [(Var v) (Seq (Assign (Var x) e) cont)]
    [(Prim op es) (Seq (Assign (Var x) e) cont)]
    [(Let _ _ _)
     (define let-result (explicate-tail e))
     (assign-return x let-result cont)
     ]
    [else (error "explicate-assign unhandled case" e)]
    )
  )

; pass 4: select instructions
(define (select-instructions-atm atm)
  (match atm
    [(Var v) atm]
    [(Int i) (Imm i)]
    ))

(define (select-instructions-stmt stmt) ; return list of instructions
  (match stmt
    [(Assign var exp)
     (match exp
       [(or (Var _) (Int _)) (list (Instr 'movq (list (select-instructions-atm exp) var)))]
       [(Prim 'read _) (list (Instr 'callq 'read_int) (Instr 'movq (list (Reg 'rax) var)))]
       ; optimization for var = (+ arg var); only need one addq
       [(or (Prim '+ (list v atm)) (Prim '+ (list atm v))) #:when (eq? v var) (list (Instr 'addq (list (select-instructions-atm atm) var)))]
       [(Prim '+ (list atm1 atm2)) (list (Instr 'movq (list (select-instructions-atm atm1) var)) (Instr 'addq (list (select-instructions-atm atm2) var)))]
       [(Prim '- (list atm)) (list (Instr 'movq (list (select-instructions-atm atm) var)) (Instr 'negq (list var)))]
       )]))

(define (select-instructions-tail tail acc)
  (define last-instr (Jmp 'conclusion))
  (match tail
    [(Return exp) ; turn this into a stmt of (Assign (Var %rax) exp)
     (define stmt (Assign (Reg 'rax) exp))
     (append acc (select-instructions-stmt stmt) (list last-instr))
       ]
    [(Seq stmt tl)
     (define stmt-instr (append acc (select-instructions-stmt stmt)))
     (select-instructions-tail tl stmt-instr)]
    ))


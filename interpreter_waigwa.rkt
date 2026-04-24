#lang racket
(provide (all-defined-out))

;; ============================================================
;; ABSTRACTION: binding
;; ============================================================

(define (make-binding var val)
  (list var val))

(define (binding-variable binding)
  (first binding))

(define (binding-value binding)
  (second binding))

;; ============================================================
;; ABSTRACTION: frame
;; ============================================================

(define (make-frame vars vals)
  (cond
    ((and (empty? vars) (empty? vals)) '())
    ((empty? vars) (error "Error: too many values!"))
    ((empty? vals) (error "Error: too many variables!"))
    (else (cons (make-binding (first vars) (first vals))
                (make-frame (rest vars) (rest vals))))))

(define (empty-frame? frame)
  (null? frame))

(define (first-binding frame)
  (first frame))

(define (rest-of-bindings frame)
  (rest frame))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (binding-in-frame var frame)
  (cond
    ((empty-frame? frame) #f)
    ((equal? var (binding-variable (first-binding frame)))
     (binding-value (first-binding frame)))
    (else (binding-in-frame var (rest-of-bindings frame)))))

;; ============================================================
;; HELPER: tagged-list?
;; ============================================================
(define (tagged-list? exp tag)
  (and (list? exp)
       (not (empty? exp))
       (equal? (first exp) tag)))

(define (tagged-list-length-n? exp tag n)
  (cond
    ((not (tagged-list? exp tag))
     #f)                              ; not even a tagged list
    ((= (length exp) n) #t)           ; correct length
    (else
     (if (< (length exp) n)
         (error "too few arguments for" tag)
         (error "too many arguments for" tag)))))

(define (tagged-list-min-length-n? exp tag n)
  (cond
    ((not (tagged-list? exp tag))
     #f)                              ; not even a tagged list
    ((>= (length exp) n) #t)          ; at least n elements
    (else
     (error "too few arguments for" tag))))

;; ============================================================
;; ABSTRACTION: primitive
;; ============================================================

(define make-primitive
  (lambda (name proc)
    (list 'primitive name proc)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-name proc)
  (second proc))

(define (primitive-implementation proc)
  (third proc))

(define apply-primitive-procedure
  (lambda (proc vals)
    (apply (primitive-implementation proc) vals)))

(define primitive-procedures
  (list
   (make-primitive 'car car)
   (make-primitive 'cdr cdr)
   (make-primitive 'cons cons)
   (make-primitive 'first first)
   (make-primitive 'rest rest)
   (make-primitive 'list list)
   (make-primitive 'null? null?)
   (make-primitive 'pair? pair?)
   (make-primitive 'number? number?)
   (make-primitive 'symbol? symbol?)
   (make-primitive 'string? string?)
   (make-primitive 'boolean? boolean?)
   (make-primitive '+ +)
   (make-primitive '- -)
   (make-primitive '* *)
   (make-primitive '/ /)
   (make-primitive '= =)
   (make-primitive '< <)
   (make-primitive '> >)
   (make-primitive '<= <=)
   (make-primitive '>= >=)
   (make-primitive 'not not)
   (make-primitive 'equal? equal?)
   (make-primitive 'display display)
   (make-primitive 'newline newline)
   (make-primitive 'void void)
   (make-primitive 'zero? zero?)
   (make-primitive 'add1 add1)
   (make-primitive 'sub1 sub1)
   (make-primitive 'length length)
   (make-primitive 'append append)
   (make-primitive 'reverse reverse)
   (make-primitive 'map map)
   (make-primitive 'apply apply)))

;; ============================================================
;; ABSTRACTION: environment (mutable lists)
;; ============================================================

(define (empty-env)
  (mcons '() '()))

(define (empty-env? env)
  (and (null? (mcar env)) (null? (mcdr env))))

(define (first-frame env)
  (mcar env))

(define (rest-of-frames env)
  (mcdr env))

(define (set-first-frame! env new-frame)
  (set-mcar! env new-frame))

(define (adjoin-frame frame env)
  (mcons frame env))

(define (extend-env vars vals base-env)
  (adjoin-frame (make-frame vars vals) base-env))

(define (binding-in-env var env)
  (cond
    ((empty-env? env) #f)
    (else
     (let ((result (binding-in-frame var (first-frame env))))
       (if result
           result
           (binding-in-env var (rest-of-frames env)))))))

;; ============================================================
;; IMPLEMENTATION: variables
;; ============================================================

(define (lookup-variable var env)
  (let ((result (binding-in-env var env)))
    (if result
        result
        (error "Error: is unbound in this environment!" var))))

(define (variable? exp)
  (symbol? exp))

;; ============================================================
;; IMPLEMENTATION: quote
;; ============================================================

(define (quoted? exp)
  (tagged-list-length-n? exp 'quote 2))

(define (text-of-quotation exp)
  (second exp))

;; ============================================================
;; IMPLEMENTATION: define
;; ============================================================

(define (definition? exp)
  (tagged-list-length-n? exp 'define 3))

(define (definition-variable exp)
  (second exp))

(define (definition-value exp)
  (third exp))

(define (define-variable! var val env)
  (let ((existing (binding-in-frame var (first-frame env))))
    (if existing
        (error "error: duplicate definition for identifier" var)
        (set-first-frame! env
                          (adjoin-binding (make-binding var val)
                                         (first-frame env))))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (i-eval (definition-value exp) env)
                    env)
  (definition-variable exp))

;; ============================================================
;; IMPLEMENTATION: application
;; ============================================================

(define (application? exp)
  (and (list? exp)
       (not (null? exp))))

(define (operator exp)
  (first exp))

(define (operands exp)
  (rest exp))

(define (eval-operands operands env)
  (map (lambda (operand) (i-eval operand env)) operands))

(define (eval-application exp env)
  (i-apply (i-eval (operator exp) env)
           (eval-operands (operands exp) env)))

(define i-apply
  (lambda (proc vals)
    (cond
      ((primitive-procedure? proc)
       (apply-primitive-procedure proc vals))
      (else (error "i-apply::unknown procedure type" proc)))))

;; ============================================================
;; IMPLEMENTATION: begin
;; ============================================================

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-expressions exp)
  (rest exp))

(define (eval-begin exp env)
  (let ((exps (begin-expressions exp)))
    (cond
      ((null? exps) (void))                        
      ((null? (rest exps))                         
       (i-eval (first exps) env))
      (else                                        
       (i-eval (first exps) env)                   
       (eval-begin (cons 'begin (rest exps)) env)))))

;; ============================================================
;; IMPLEMENTATION: if
;; ============================================================

(define (if? exp)
  (tagged-list? exp 'if))

(define (test-expression exp)
  (second exp))

(define (then-expression exp)
  (third exp))

(define (else-expression exp)
  (if (> (length exp) 3)
      (fourth exp)
      #f))           

(define (has-else? exp)
  (> (length exp) 3))

(define (eval-if exp env)
  (if (i-eval (test-expression exp) env)
      (i-eval (then-expression exp) env)
      (if (has-else? exp)
          (i-eval (else-expression exp) env)
          (void))))  

;; ============================================================
;; IMPLEMENTATION: cond
;; ============================================================

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (first-cond-exp exp)
  (second exp))         

(define (rest-of-cond-exps exp)
  (cons 'cond (cddr exp))) 

(define (cond-test clause)
  (first clause))      

(define (cond-body clause)
  (rest clause))       

(define (eval-cond exp env)
  (cond
    ((null? (rest exp)) (void))  
    (else
     (let* ((clause (first-cond-exp exp))
            (test   (cond-test clause))
            (body   (cond-body clause)))
       (cond
         ((equal? test 'else)
          (if (null? (cddr exp))  
              (if (null? body)
                  #t
                  (eval-begin (cons 'begin body) env))
              (error "else must be the last clause in cond")))
         ((not (eq? #f (i-eval test env)))
          (if (null? body)
              (i-eval test env)  
              (eval-begin (cons 'begin body) env)))
         (else
          (eval-cond (rest-of-cond-exps exp) env)))))))

;; ============================================================
;; EXTRA CREDIT: and / or
;; ============================================================

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-and exp env)
  (let ((exps (rest exp)))
    (cond
      ((null? exps) #t)                          
      ((null? (rest exps))                       
       (i-eval (first exps) env))
      (else
       (let ((val (i-eval (first exps) env)))
         (if (eq? val #f)
             #f                                  
             (eval-and (cons 'and (rest exps)) env)))))))

(define (eval-or exp env)
  (let ((exps (rest exp)))
    (cond
      ((null? exps) #f)                         
      (else
       (let ((val (i-eval (first exps) env)))
         (if (not (eq? val #f))
             val                                 
             (eval-or (cons 'or (rest exps)) env)))))))

;; ============================================================
;; IMPLEMENTATION: i-eval
;; ============================================================

(define i-eval
  (lambda (exp env)
    (cond
      ((boolean? exp)      exp)
      ((number? exp)       exp)
      ((string? exp)       exp)
      ((quoted? exp)       (text-of-quotation exp))
      ((definition? exp)   (eval-definition exp env))
      ((begin? exp)        (eval-begin exp env))
      ((if? exp)           (eval-if exp env))
      ((cond? exp)         (eval-cond exp env))
      ((and? exp)          (eval-and exp env))       
      ((or? exp)           (eval-or exp env))        
      ((variable? exp)     (lookup-variable exp env))
      ((application? exp)  (eval-application exp env))
      (else (error "i-eval::unknown expression type" exp)))))

;; ============================================================
;; Global environment
;; ============================================================

(define setup-env
  (lambda ()
    (extend-env
     (map primitive-name primitive-procedures)
     primitive-procedures
     (extend-env '(null) '(()) (empty-env)))))

(define global-env (setup-env))

;; ============================================================
;; REPL
;; ============================================================

(define read-eval-print-loop
  (lambda ()
    (display "INTERPRETER> ")
    (let ((user-input (read)))
      (if (equal? user-input 'exit)
          (display "INTERPRETER done.")
          (begin
            (display (i-eval user-input global-env))
            (newline)
            (read-eval-print-loop))))))

(define repl read-eval-print-loop)
#lang racket
(require rackunit)
(require "interpreter_waigwa.rkt")

;; ============================================================
;; TESTS: Part 1
;; ============================================================
;; ============================================================
;; TESTS: binding
;; ============================================================

(display "=== BINDING TESTS ===") (newline)

(define b1 (make-binding 'x 3))
(define b2 (make-binding 'today 'monday))
(define b3 (make-binding 'lst '(1 2 3)))

;construction
(check-equal? b1 '(x 3)
              "make-binding symbol->number")
(check-equal? b2 '(today monday)         
              "make-binding symbol->symbol")
(check-equal? b3 '(lst (1 2 3))
              "make-binding symbol->list")

; selectors
(check-equal? (binding-variable b1) 'x
              "binding-variable basic")
(check-equal? (binding-value b1) 3
              "binding-value number")
(check-equal? (binding-value b2) 'monday
              "binding-value symbol")
(check-equal? (binding-value b3) '(1 2 3)
              "binding-value list")

; edge: binding to boolean true
(check-equal? (binding-value (make-binding 'flag #t)) #t
              "binding-value boolean true")

; edge: binding to boolean false
; NOTE: this is a KNOWN DESIGN ISSUE — #f as a stored value
; is indistinguishable from #f meaning "not found. We can store f as false, or f also returning as error"
(check-equal? (binding-value (make-binding 'flag #f)) #f
              "binding-value boolean false — known #f ambiguity")

; edge: binding to empty list
(check-equal? (binding-value (make-binding 'empty '())) '()
              "binding-value empty list")

; edge: binding to string
(check-equal? (binding-value (make-binding 'name "alice")) "alice"
              "binding-value string")

; edge: binding to nested list
(check-equal? (binding-value (make-binding 'nested '((1 2) (3 4)))) '((1 2) (3 4))
              "binding-value nested list")

; edge: binding to zero
(check-equal? (binding-value (make-binding 'zero 0)) 0
              "binding-value zero")

; edge: binding to negative number
(check-equal? (binding-value (make-binding 'neg -42)) -42
              "binding-value negative number")

; edge: binding to float
(check-equal? (binding-value (make-binding 'pi 3.14)) 3.14
              "binding-value float")

;; =============================================================
;; TESTS: frame
;; ============================================================

(display "=== FRAME TESTS ===") (newline)

(define f1 (make-frame '(a b c) '(1 2 3)))
(define f2 (make-frame '(d e) '((1 2) (3 4))))

;construction
(check-equal? f1 '((a 1) (b 2) (c 3))
              "make-frame basic")
(check-equal? f2 '((d (1 2)) (e (3 4)))
              "make-frame with list values")
(check-equal? (make-frame '() '()) '()
              "make-frame both empty")

; error cases
(check-exn exn:fail?
           (lambda () (make-frame '(x y z) '(#t #f)))
           "make-frame too many variables")
(check-exn exn:fail?
           (lambda () (make-frame '(today tomorrow) '(mon tue wed)))
           "make-frame too many values")

; edge: single element frame
(check-equal? (make-frame '(x) '(1)) '((x 1))
              "make-frame single element")

; edge: frame with boolean values
(check-equal? (make-frame '(a b) '(#t #f)) '((a #t) (b #f))
              "make-frame boolean values")

; edge: frame with string values
(check-equal? (make-frame '(name age) '("alice" 30)) '((name "alice") (age 30))
              "make-frame string values")

; edge: frame with mixed value types
(check-equal? (make-frame '(a b c) '(1 "two" #t)) '((a 1) (b "two") (c #t))
              "make-frame mixed value types")

; empty-frame?
(check-equal? (empty-frame? '()) #t
              "empty-frame? on empty")
(check-equal? (empty-frame? f1) #f
              "empty-frame? on non-empty")

; edge: empty-frame? after adjoin-binding
(check-equal? (empty-frame? (adjoin-binding (make-binding 'x 1) '())) #f
              "empty-frame? false after adjoin")

; first-binding
(check-equal? (first-binding f1) '(a 1)
              "first-binding basic")

; rest-of-bindings
(check-equal? (rest-of-bindings f1) '((b 2) (c 3))
              "rest-of-bindings multiple")

; edge: rest-of-bindings on single element
(check-equal? (rest-of-bindings (make-frame '(x) '(1))) '()
              "rest-of-bindings single element gives empty")

; adjoin-binding
(check-equal? (adjoin-binding (make-binding 'z 99) f1)
              '((z 99) (a 1) (b 2) (c 3))
              "adjoin-binding adds to front")

; edge: adjoin to empty frame
(check-equal? (adjoin-binding (make-binding 'x 1) '())
              '((x 1))
              "adjoin-binding to empty frame")

; binding-in-frame
(check-equal? (binding-in-frame 'a f1) 1
              "binding-in-frame first element")
(check-equal? (binding-in-frame 'b f1) 2
              "binding-in-frame middle element")
(check-equal? (binding-in-frame 'c f1) 3
              "binding-in-frame last element")
(check-equal? (binding-in-frame 'x f1) #f
              "binding-in-frame not found")

; edge: binding-in-frame on empty frame
(check-equal? (binding-in-frame 'x '()) #f
              "binding-in-frame empty frame returns #f")

; edge: #f ambiguity — documented as known design issue
; binding-in-frame returns #f for "not found"
(define f-with-false (make-frame '(flag) '(#f)))
(check-equal? (binding-in-frame 'flag f-with-false) #f
              "KNOWN ISSUE: #f value indistinguishable from not-found")

;; ============================================================
;; TESTS: environment
;; ============================================================

(display "=== ENVIRONMENT TESTS ===") (newline)

(define env0 (empty-env))
(define env1 (extend-env '(a b c) '(1 2 3) (empty-env)))
(define env2 (extend-env '(a c d e) '(red blue green yellow) env1))
(define env3 (extend-env '(a f) '(#t #f) env2))

; empty-env?
(check-equal? (empty-env? env0) #t
              "empty-env? on empty")
(check-equal? (empty-env? env1) #f
              "empty-env? on non-empty")

; first-frame
(check-equal? (first-frame env1) '((a 1) (b 2) (c 3))
              "first-frame basic")

; extend-env builds correctly
(check-equal? (first-frame env2) '((a red) (c blue) (d green) (e yellow))
              "extend-env first frame of env2")

; binding-in-env — shadowing
(check-equal? (binding-in-env 'a env3) #t
              "binding-in-env most recent frame wins")
(check-equal? (binding-in-env 'c env3) 'blue
              "binding-in-env second frame shadows deepest")
(check-equal? (binding-in-env 'b env3) 2
              "binding-in-env found in deepest frame")
(check-equal? (binding-in-env 'z env3) #f
              "binding-in-env not found anywhere")

; edge: binding-in-env on empty env
(check-equal? (binding-in-env 'x env0) #f
              "binding-in-env empty env returns #f")

; edge: binding-in-env single frame found
(check-equal? (binding-in-env 'a env1) 1
              "binding-in-env single frame found")

; edge: binding-in-env single frame not found
(check-equal? (binding-in-env 'z env1) #f
              "binding-in-env single frame not found")

; lookup-variable
(check-equal? (lookup-variable 'c env3) 'blue
              "lookup-variable found in middle frame")
(check-equal? (lookup-variable 'b env3) 2
              "lookup-variable found in deepest frame")
(check-equal? (lookup-variable 'a env3) #t
              "lookup-variable most recent binding wins")

; edge: lookup-variable unbound throws
(check-exn exn:fail?
           (lambda () (lookup-variable 'g env3))
           "lookup-variable unbound throws error")

; edge: lookup-variable in empty env throws
(check-exn exn:fail?
           (lambda () (lookup-variable 'x env0))
           "lookup-variable empty env throws error")

; set-first-frame
(define env-mut (extend-env '(x) '(1) (empty-env)))
(set-first-frame! env-mut (make-frame '(y) '(2)))
(check-equal? (first-frame env-mut) '((y 2))
              "set-first-frame! mutates environment")

; adjoin-frame adds to front
(define new-frame (make-frame '(z) '(99)))
(define env-adj (adjoin-frame new-frame env1))
(check-equal? (first-frame env-adj) '((z 99))
              "adjoin-frame adds new frame to front")

;; ============================================================
;; TESTS: tagged-list?
;; ============================================================

(display "=== TAGGED-LIST TESTS ===") (newline)

(check-equal? (tagged-list? '(define x 8) 'define) #t
              "tagged-list? match")
(check-equal? (tagged-list? '(quote x 6) 'define) #f
              "tagged-list? wrong tag")
(check-equal? (tagged-list? "some string" 'define) #f
              "tagged-list? not a list")
(check-equal? (tagged-list? '() 'define) #f
              "tagged-list? empty list")

; edge: numeric tag
(check-equal? (tagged-list? '(1 2 3) 1) #t
              "tagged-list? numeric tag")

; edge: single element list matching tag
(check-equal? (tagged-list? '(define) 'define) #t
              "tagged-list? single element matches tag")

; edge: number input
(check-equal? (tagged-list? 42 'define) #f
              "tagged-list? number input")

; edge: boolean input
(check-equal? (tagged-list? #t 'define) #f
              "tagged-list? boolean input")

; edge: nested first element does not match
(check-equal? (tagged-list? '((define x) 8) 'define) #f
              "tagged-list? nested first element doesnt match")

;; ============================================================
;; TESTS: quote
;; ============================================================

(display "=== QUOTE TESTS ===") (newline)

(check-equal? (quoted? '(quote x)) #t
              "quoted? symbol")
(check-equal? (quoted? '(quote (1 2 3))) #t
              "quoted? list")
(check-equal? (quoted? 7) #f
              "quoted? number")
(check-equal? (quoted? "hello") #f
              "quoted? string")
(check-equal? (quoted? #t) #f
              "quoted? boolean")

; edge: quoted empty list
(check-equal? (quoted? '(quote ())) #t
              "quoted? empty list")

; edge: quoted boolean
(check-equal? (quoted? '(quote #t)) #t
              "quoted? quoted boolean")

; edge: quoted number
(check-equal? (quoted? '(quote 42)) #t
              "quoted? quoted number")

; edge: nested quote
(check-equal? (quoted? '(quote (quote x))) #t
              "quoted? nested quote")

; text-of-quotation
(check-equal? (text-of-quotation '(quote x)) 'x
              "text-of-quotation symbol")
(check-equal? (text-of-quotation '(quote (1 2 3))) '(1 2 3)
              "text-of-quotation list")

; edge: empty list
(check-equal? (text-of-quotation '(quote ())) '()
              "text-of-quotation empty list")

; edge: boolean
(check-equal? (text-of-quotation '(quote #t)) #t
              "text-of-quotation boolean")

; edge: number
(check-equal? (text-of-quotation '(quote 42)) 42
              "text-of-quotation number")

; edge: nested quote
(check-equal? (text-of-quotation '(quote (quote x))) '(quote x)
              "text-of-quotation nested quote returns inner quote unevaluated")

;; ============================================================
;; TESTS: define
;; ============================================================

(display "=== DEFINE TESTS ===") (newline)

(check-equal? (definition? '(define y 3)) #t
              "definition? valid")
(check-equal? (definition? 5) #f
              "definition? number")
(check-equal? (definition? '(quote x)) #f
              "definition? wrong tag")
(check-equal? (definition? "define") #f
              "definition? string not list")
(check-equal? (definition? '()) #f
              "definition? empty list")

(check-exn exn:fail?
           (lambda () (definition? '(define x)))
           "definition? missing value throws error")
(check-exn exn:fail?
           (lambda () (definition? '(define x 3 5)))
           "definition? extra elements throws error")

; selectors
(check-equal? (definition-variable '(define x 6)) 'x
              "definition-variable basic")
(check-equal? (definition-value '(define x 6)) 6
              "definition-value number")

; edge: value is quoted expression
(check-equal? (definition-value '(define x (quote apple))) '(quote apple)
              "definition-value quoted expression unevaluated")

; edge: value is nested expression
(check-equal? (definition-value '(define x (+ 1 2))) '(+ 1 2)
              "definition-value nested expression unevaluated")

;; ============================================================
;; TESTS: tagged-list-length-n?
;; ============================================================

(display "=== TAGGED-LIST-LENGTH-N TESTS ===") (newline)

; exactly n elements
(check-equal? (tagged-list-length-n? '(define x 3) 'define 3) #t
              "tagged-list-length-n? exact match")

; wrong tag
(check-equal? (tagged-list-length-n? '(quote x) 'define 3) #f
              "tagged-list-length-n? wrong tag")

; not a list
(check-equal? (tagged-list-length-n? 42 'define 3) #f
              "tagged-list-length-n? not a list")

; empty list
(check-equal? (tagged-list-length-n? '() 'define 3) #f
              "tagged-list-length-n? empty list")

; too few
(check-exn exn:fail?
           (lambda () (tagged-list-length-n? '(define x) 'define 3))
           "tagged-list-length-n? too few throws")

; too many
(check-exn exn:fail?
           (lambda () (tagged-list-length-n? '(define x 3 5) 'define 3))
           "tagged-list-length-n? too many throws")

; edge
(check-equal? (tagged-list-length-n? '(quote x) 'quote 2) #t
              "tagged-list-length-n? quote exact match")

; edge: too few for quote
(check-exn exn:fail?
           (lambda () (tagged-list-length-n? '(quote) 'quote 2))
           "tagged-list-length-n? quote too few throws")

; edge: too many for quote 
(check-exn exn:fail?
           (lambda () (tagged-list-length-n? '(quote x y) 'quote 2))
           "tagged-list-length-n? quote too many throws")

;; ============================================================
;; TESTS: tagged-list-min-length-n?
;; ============================================================

(display "=== TAGGED-LIST-MIN-LENGTH-N TESTS ===") (newline)

; exact minimum
(check-equal? (tagged-list-min-length-n? '(define x 3) 'define 3) #t
              "tagged-list-min-length-n? exact minimum")

; more than minimum
(check-equal? (tagged-list-min-length-n? '(define x 3 4 5) 'define 3) #t
              "tagged-list-min-length-n? more than minimum")

; wrong tag
(check-equal? (tagged-list-min-length-n? '(quote x) 'define 3) #f
              "tagged-list-min-length-n? wrong tag")

; not a list
(check-equal? (tagged-list-min-length-n? 42 'define 3) #f
              "tagged-list-min-length-n? not a list")

; empty list
(check-equal? (tagged-list-min-length-n? '() 'define 3) #f
              "tagged-list-min-length-n? empty list")

; too few
(check-exn exn:fail?
           (lambda () (tagged-list-min-length-n? '(define x) 'define 3))
           "tagged-list-min-length-n? too few throws")

; edge: minimum of 1 — just the tag itself
(check-equal? (tagged-list-min-length-n? '(define) 'define 1) #t
              "tagged-list-min-length-n? minimum of 1")

; edge: minimum of 2
(check-equal? (tagged-list-min-length-n? '(define x) 'define 2) #t
              "tagged-list-min-length-n? minimum of 2 exact")

;; ============================================================
;; TESTS: i-eval
;; ============================================================

(display "=== I-EVAL TESTS ===") (newline)

(define test-env (setup-env))

; self-evaluating
(check-equal? (i-eval 42 test-env) 42
              "i-eval positive integer")
(check-equal? (i-eval -3.14 test-env) -3.14
              "i-eval negative float")
(check-equal? (i-eval 0 test-env) 0
              "i-eval zero")
(check-equal? (i-eval #t test-env) #t
              "i-eval boolean true")
(check-equal? (i-eval #f test-env) #f
              "i-eval boolean false")
(check-equal? (i-eval "hello" test-env) "hello"
              "i-eval string")
(check-equal? (i-eval "" test-env) ""
              "i-eval empty string")

; quote
(check-equal? (i-eval '(quote apple) test-env) 'apple
              "i-eval quoted symbol")
(check-equal? (i-eval '(quote (1 2 3)) test-env) '(1 2 3)
              "i-eval quoted list")
(check-equal? (i-eval '(quote ()) test-env) '()
              "i-eval quoted empty list")
(check-equal? (i-eval '(quote #t) test-env) #t
              "i-eval quoted boolean")
(check-equal? (i-eval '(quote 42) test-env) 42
              "i-eval quoted number")

; define then lookup
(check-equal? (begin (i-eval '(define pi 3.1415) test-env)
                     (i-eval 'pi test-env))
              3.1415
              "i-eval define number then lookup")

(check-equal? (begin (i-eval '(define fruit (quote apple)) test-env)
                     (i-eval 'fruit test-env))
              'apple
              "i-eval define quoted value then lookup")

(check-equal? (begin (i-eval '(define flag #t) test-env)
                     (i-eval 'flag test-env))
              #t
              "i-eval define boolean then lookup")

(check-equal? (begin (i-eval '(define greeting "hello") test-env)
                     (i-eval 'greeting test-env))
              "hello"
              "i-eval define string then lookup")

; edge: define returns variable name
(check-equal? (i-eval '(define myvar 99) test-env) 'myvar
              "i-eval define returns variable name")

; edge: define variable from another variable
(check-equal? (begin (i-eval '(define a 10) test-env)
                     (i-eval '(define b a) test-env)
                     (i-eval 'b test-env))
              10
              "i-eval define variable from another variable")

; edge: multiple defines
(check-equal? (begin (i-eval '(define p 1) test-env)
                     (i-eval '(define q 2) test-env)
                     (i-eval '(define r 3) test-env)
                     (i-eval 'r test-env))
              3
              "i-eval multiple sequential defines")

; edge: null predefined in global env
(check-equal? (i-eval 'null test-env) '()
              "i-eval null predefined in global-env")

; error: duplicate define throws
(check-exn exn:fail?
           (lambda ()
             (i-eval '(define dup 1) test-env)
             (i-eval '(define dup 2) test-env))
           "i-eval duplicate define throws")

; error: unbound variable throws
(check-exn exn:fail?
           (lambda () (i-eval 'unbound test-env))
           "i-eval unbound variable throws")

; error: unknown expression type throws
(check-exn exn:fail?
           (lambda () (i-eval '(blahblah 1 2) test-env))
           "i-eval unknown expression throws")

; edge: shadowing — most recent frame wins
(define shadow-env (extend-env '(x) '(1) (empty-env)))
(define shadow-env2 (extend-env '(x) '(99) shadow-env))
(check-equal? (lookup-variable 'x shadow-env2) 99
              "shadowed variable returns most recent binding")

; edge: define in fresh env works fine
(define fresh-env (setup-env))
(check-equal? (begin (i-eval '(define x 1) fresh-env)
                     (i-eval 'x fresh-env))
              1
              "i-eval define in fresh env works fine")

; edge: define quoted list then lookup
(check-equal? (begin (i-eval '(define mylist (quote (1 2 3))) test-env)
                     (i-eval 'mylist test-env))
              '(1 2 3)
              "i-eval define quoted list then lookup")

;; ============================================================
;; TESTS: Part 2
;; ============================================================

;; ============================================================
;; TESTS: primitive
;; ============================================================

(display "=== PRIMITIVE TESTS ===") (newline)

(define p1 (make-primitive 'car car))
(define p2 (make-primitive '+ +))
(define prim-test-env (setup-env))
; basic construction
(check-equal? (primitive-procedure? p1) #t
              "primitive-procedure? on primitive")
(check-equal? (primitive-procedure? '(not-primitive x)) #f
              "primitive-procedure? on non-primitive")
(check-equal? (primitive-procedure? 42) #f
              "primitive-procedure? on number")
(check-equal? (primitive-procedure? 'car) #f
              "primitive-procedure? on symbol")

; selectors
(check-equal? (primitive-name p1) 'car
              "primitive-name car")
(check-equal? (primitive-name p2) '+
              "primitive-name +")
(check-equal? (primitive-implementation p1) car
              "primitive-implementation car")
(check-equal? (primitive-implementation p2) +
              "primitive-implementation +")

; apply-primitive-procedure
(check-equal? (apply-primitive-procedure p2 '(1 2)) 3
              "apply-primitive-procedure + basic")
(check-equal? (apply-primitive-procedure p2 '(1 2 3 4 5)) 15
              "apply-primitive-procedure + multiple args")
(check-equal? (apply-primitive-procedure
               (make-primitive 'cons cons) '(1 (2 3))) '(1 2 3)
              "apply-primitive-procedure cons")

;; ============================================================
;; TESTS: application
;; ============================================================

(display "=== APPLICATION TESTS ===") (newline)

; application?
(check-equal? (application? '(cons 1 y)) #t
              "application? basic")
(check-equal? (application? '(newline)) #t
              "application? no args")
(check-equal? (application? 'cons) #f
              "application? symbol not application")
(check-equal? (application? '()) #f
              "application? empty list not application")
(check-equal? (application? 42) #f
              "application? number not application")
(check-equal? (application? #t) #f
              "application? boolean not application")

; operator / operands
(check-equal? (operator '(cons 1 y)) 'cons
              "operator basic")
(check-equal? (operands '(cons 1 y)) '(1 y)
              "operands basic")
(check-equal? (operands '(newline)) '()
              "operands empty")
(check-equal? (operator '(+ 1 2 3)) '+
              "operator +")
(check-equal? (operands '(+ 1 2 3)) '(1 2 3)
              "operands multiple")

; eval-operands
(define eval-env (extend-env '(a b c) '(1 0 8) (empty-env)))
(check-equal? (eval-operands '(a 9 b c a) eval-env) '(1 9 0 8 1)
              "eval-operands mixed vars and literals")
(check-equal? (eval-operands '() eval-env) '()
              "eval-operands empty")
(check-equal? (eval-operands '(1 2 3) eval-env) '(1 2 3)
              "eval-operands all literals")

;; ============================================================
;; TESTS: i-eval primitives
;; ============================================================

(display "=== I-EVAL PRIMITIVE TESTS ===") (newline)

; arithmetic
(check-equal? (i-eval '(+ 1 2) prim-test-env) 3
              "i-eval + basic")
(check-equal? (i-eval '(+ 1 2 3 4 5) prim-test-env) 15
              "i-eval + multiple args")
(check-equal? (i-eval '(- 10 3) prim-test-env) 7
              "i-eval - basic")
(check-equal? (i-eval '(* 3 4) prim-test-env) 12
              "i-eval * basic")
(check-equal? (i-eval '(/ 10 2) prim-test-env) 5
              "i-eval / basic")
(check-equal? (i-eval '(+ 0 0) prim-test-env) 0
              "i-eval + zeros")
(check-equal? (i-eval '(* 0 99) prim-test-env) 0
              "i-eval * by zero")
(check-equal? (i-eval '(- 5) prim-test-env) -5
              "i-eval - negation")

; comparison
(check-equal? (i-eval '(= 3 3) prim-test-env) #t
              "i-eval = true")
(check-equal? (i-eval '(= 3 4) prim-test-env) #f
              "i-eval = false")
(check-equal? (i-eval '(< 3 4) prim-test-env) #t
              "i-eval < true")
(check-equal? (i-eval '(> 4 3) prim-test-env) #t
              "i-eval > true")
(check-equal? (i-eval '(<= 3 3) prim-test-env) #t
              "i-eval <= equal")
(check-equal? (i-eval '(>= 4 3) prim-test-env) #t
              "i-eval >= greater")

; list operations
(check-equal? (i-eval '(cons 1 '(2 3)) prim-test-env) '(1 2 3)
              "i-eval cons basic")
(check-equal? (i-eval '(first '(1 2 3)) prim-test-env) 1
              "i-eval first")
(check-equal? (i-eval '(rest '(1 2 3)) prim-test-env) '(2 3)
              "i-eval rest")
(check-equal? (i-eval '(list 1 2 3) prim-test-env) '(1 2 3)
              "i-eval list")
(check-equal? (i-eval '(null? '()) prim-test-env) #t
              "i-eval null? empty")
(check-equal? (i-eval '(null? '(1 2)) prim-test-env) #f
              "i-eval null? non-empty")
(check-equal? (i-eval 'null prim-test-env) '()
              "i-eval null predefined")

; type predicates
(check-equal? (i-eval '(number? 42) prim-test-env) #t
              "i-eval number? true")
(check-equal? (i-eval '(number? "hi") prim-test-env) #f
              "i-eval number? false")
(check-equal? (i-eval '(symbol? 'x) prim-test-env) #t
              "i-eval symbol? true")
(check-equal? (i-eval '(boolean? #t) prim-test-env) #t
              "i-eval boolean? true")
(check-equal? (i-eval '(string? "hello") prim-test-env) #t
              "i-eval string? true")
(check-equal? (i-eval '(pair? '(1 2)) prim-test-env) #t
              "i-eval pair? true")
(check-equal? (i-eval '(pair? '()) prim-test-env) #f
              "i-eval pair? empty list false")

; edge: nested applications
(check-equal? (i-eval '(+ (* 2 3) (- 10 4)) prim-test-env) 12
              "i-eval nested arithmetic")
(check-equal? (i-eval '(first (rest '(1 2 3))) prim-test-env) 2
              "i-eval nested list ops")

; edge: define then use in application
(check-equal? (begin (i-eval '(define x 8) prim-test-env)
                     (i-eval '(define y (+ x 1)) prim-test-env)
                     (i-eval 'y prim-test-env))
              9
              "i-eval define with primitive application")

; edge: assign procedure to variable then call it
(check-equal? (begin (i-eval '(define w *) prim-test-env)
                     (i-eval '(w 3 4) prim-test-env))
              12
              "i-eval procedure assigned to variable then called")

; error: unknown procedure type
(check-exn exn:fail?
           (lambda () (i-apply 'not-a-proc '(1 2)))
           "i-apply unknown procedure type throws")

;; ============================================================
;; TESTS: begin
;; ============================================================

(display "=== BEGIN TESTS ===") (newline)

(define begin-env (setup-env))

; basic
(check-equal? (i-eval '(begin (+ 1 2)) begin-env) 3
              "begin single expression")
(check-equal? (i-eval '(begin 1 2 3) begin-env) 3
              "begin returns last value")
(check-equal? (i-eval '(begin) begin-env) (void)
              "begin empty returns void")

; edge: begin with define
(check-equal? (begin
                (i-eval '(begin (define x 5) (define y 10)) begin-env)
                (i-eval 'y begin-env))
              10
              "begin with multiple defines")

; edge: begin with side effects
(check-equal? (i-eval '(begin (+ 1 1) (+ 2 2) (+ 3 3)) begin-env) 6
              "begin evaluates all returns last")

; edge: begin with single define 
(check-equal? (i-eval '(begin (define z 99)) begin-env) 'z
              "begin single define returns var name")

; edge: nested begin
(check-equal? (i-eval '(begin (begin 1 2) (begin 3 4)) begin-env) 4
              "begin nested returns last of last")

;; ============================================================
;; TESTS: if
;; ============================================================

(display "=== IF TESTS ===") (newline)

(define if-env (setup-env))

; basic true/false
(check-equal? (i-eval '(if #t 'yes 'no) if-env) 'yes
              "if true branch")
(check-equal? (i-eval '(if #f 'yes 'no) if-env) 'no
              "if false branch")

; edge: anything not #f is true
(check-equal? (i-eval '(if 0 'yes 'no) if-env) 'yes
              "if 0 is truthy")
(check-equal? (i-eval '(if "" 'yes 'no) if-env) 'yes
              "if empty string is truthy")
(check-equal? (i-eval '(if '() 'yes 'no) if-env) 'yes
              "if empty list is truthy")
(check-equal? (i-eval '(if (cons 1 2) 'yes 'no) if-env) 'yes
              "if cons is truthy")

; edge: no else returns void when false
(check-equal? (i-eval '(if #f 'yes) if-env) (void)
              "if no else false returns void")
(check-equal? (i-eval '(if #t 'yes) if-env) 'yes
              "if no else true returns then")

; edge: test expression is evaluated
(check-equal? (i-eval '(if (< 3 4) 'less 'not-less) if-env) 'less
              "if with expression test true")
(check-equal? (i-eval '(if (< 4 3) 'less 'not-less) if-env) 'not-less
              "if with expression test false")

; edge: nested if
(check-equal? (i-eval '(if #t (if #f 'inner-no 'inner-yes) 'outer-no) if-env)
              'inner-yes
              "if nested")

; edge: if with arithmetic
(check-equal? (i-eval '(* 5 (if (= (+ 3 3) (- 8 2)) 10 20)) if-env) 50
              "if inside arithmetic")

;; ============================================================
;; TESTS: cond
;; ============================================================

(display "=== COND TESTS ===") (newline)

(define cond-env (setup-env))

; basic
(check-equal? (i-eval '(cond (#t 'yes)) cond-env) 'yes
              "cond single true clause")
(check-equal? (i-eval '(cond (#f 'no) (#t 'yes)) cond-env) 'yes
              "cond first false second true")
(check-equal? (i-eval '(cond) cond-env) (void)
              "cond empty returns void")

; else
(check-equal? (i-eval '(cond (#f 'no) (else 'default)) cond-env) 'default
              "cond else reached")
(check-equal? (i-eval '(cond (#t 'yes) (else 'default)) cond-env) 'yes
              "cond else not reached")

; edge: no matching clause returns void
(check-equal? (i-eval '(cond (#f 'a) (#f 'b) (#f 'c)) cond-env) (void)
              "cond no match returns void")

; edge: non-#f truthy values
(check-equal? (i-eval '(cond (1 'one-is-truthy)) cond-env) 'one-is-truthy
              "cond non-#f number is truthy")
(check-equal? (i-eval '(cond (0 'zero-is-truthy)) cond-env) 'zero-is-truthy
              "cond 0 is truthy")

; edge: test with no body returns test value
(check-equal? (i-eval '(cond ((+ 1 1))) cond-env) 2
              "cond true test no body returns test value")

; edge: multiple expressions in body
(check-equal? (i-eval '(cond (#t 1 2 3)) cond-env) 3
              "cond multiple body exprs returns last")

; edge: else not last throws
(check-exn exn:fail?
           (lambda () (i-eval '(cond (else 'a) (#t 'b)) cond-env))
           "cond else not last throws error")

; edge: complex test expressions
(check-equal? (i-eval '(cond ((= 3 5) 'no)
                             ((= 3 3) 'yes)
                             (else 'never)) cond-env)
              'yes
              "cond complex test expressions")

;; ============================================================
;; TESTS: and (extra credit)
;; ============================================================

(display "=== AND TESTS ===") (newline)

(define and-env (setup-env))
(i-eval '(define x 5) and-env)
(i-eval '(define y 20) and-env)

; basic
(check-equal? (i-eval '(and) and-env) #t
              "and empty returns #t")
(check-equal? (i-eval '(and #t) and-env) #t
              "and single true")
(check-equal? (i-eval '(and #f) and-env) #f
              "and single false")
(check-equal? (i-eval '(and #t #t) and-env) #t
              "and two true")
(check-equal? (i-eval '(and #t #f) and-env) #f
              "and true then false")
(check-equal? (i-eval '(and #f #t) and-env) #f
              "and false then true short circuits")

; edge: returns last value not just #t
(check-equal? (i-eval '(and #t #t (+ 1 1)) and-env) 2
              "and returns last value")
(check-equal? (i-eval '(and (= x 5) (< y 30) (+ x y)) and-env) 25
              "and complex returns last value")

; edge: short circuit — stops at first #f
(check-equal? (i-eval '(and #f (/ 1 0)) and-env) #f
              "and short circuits on false")

; edge: non-#f is truthy
(check-equal? (i-eval '(and 1 2 3) and-env) 3
              "and all truthy returns last")

;; ============================================================
;; TESTS: or (extra credit)
;; ============================================================

(display "=== OR TESTS ===") (newline)

(define or-env (setup-env))
(i-eval '(define x 5) or-env)
(i-eval '(define y 20) or-env)

; basic
(check-equal? (i-eval '(or) or-env) #f
              "or empty returns #f")
(check-equal? (i-eval '(or #f) or-env) #f
              "or single false")
(check-equal? (i-eval '(or #t) or-env) #t
              "or single true")
(check-equal? (i-eval '(or #f #f) or-env) #f
              "or all false")
(check-equal? (i-eval '(or #f #t) or-env) #t
              "or second true")

; edge: returns actual value not just #t
(check-equal? (i-eval '(or #f #f (+ 3 4)) or-env) 7
              "or returns first true value")
(check-equal? (i-eval '(or (= x 3) (< y 10) (+ x y)) or-env) 25
              "or complex returns first true value")

; edge: short circuit — stops at first true
(check-equal? (i-eval '(or #t (/ 1 0)) or-env) #t
              "or short circuits on true")

; edge: non-#f is truthy
(check-equal? (i-eval '(or #f 42) or-env) 42
              "or returns first truthy value")

;; ============================================================
;; TESTS: integration
;; ============================================================

(display "=== INTEGRATION TESTS ===") (newline)

(define int-env (setup-env))

(check-equal? (begin
                (i-eval '(define x 8) int-env)
                (i-eval '(define y (+ x 1)) int-env)
                (i-eval 'y int-env))
              9
              "integration define with expression")

(check-equal? (begin
                (i-eval '(define lst (cons x (cons '(x y z) (cons y null)))) int-env)
                (i-eval 'lst int-env))
              '(8 (x y z) 9)
              "integration nested cons")

(check-equal? (begin
                (i-eval '(define z (+ (first lst) (first (rest (rest lst))))) int-env)
                (i-eval 'z int-env))
              17
              "integration nested list access")

(check-equal? (i-eval '(= (+ x y) z) int-env) #t
              "integration arithmetic equality")

; edge: if inside cond
(check-equal? (i-eval '(cond ((< 1 2) (if #t 'inner 'no))) int-env)
              'inner
              "integration if inside cond")

; edge: and inside if
(check-equal? (i-eval '(if (and (< 1 2) (< 2 3)) 'yes 'no) int-env)
              'yes
              "integration and inside if")

; edge: or inside cond
(check-equal? (i-eval '(cond ((or #f #f) 'no) (else 'yes)) int-env)
              'yes
              "integration or inside cond false")

; edge: begin inside if
(check-equal? (i-eval '(if #t (begin 1 2 3) 'no) int-env)
              3
              "integration begin inside if")

; edge: nested arithmetic with comparisons
(check-equal? (i-eval '(if (> (* 3 3) (+ 4 4)) 'yes 'no) int-env)
              'yes
              "integration nested arithmetic in if")

(display "ALL TESTS PASSED") (newline)
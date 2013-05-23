#lang racket
;we require the standard library for classes and objects
(require racket/class)

;the handler class
;every handler contains:
;- the class of the exception to be handled
;- the lambda of the real oprations to be done (the real handler)
(define handler%
  (class object%
    (super-new)
    (init class handler)
    (field (self-class class))
    (field (self-handler handler))
    (define/public (get-class) self-class)
    (define/public (get-handler) self-handler)))

;code for the stack which contains handlers
(define *handlers* '())

(define (push-h handler)
  (set! *handlers* (cons handler *handlers*)))

(define (pop-h)
  (let ((handler (car *handlers*)))
    (set! *handlers* (cdr *handlers*))
    handler))

;only reads the first handler without popping it
(define (read-h)
  (car *handlers*))

;empties the list
(define (flush-h)
  (set! *handlers* '()))

;throw method
;boolean function to understand if we can handle the exception thrown
(define (can-handle? exception)
      (let ((h (read-h))) ;there is at least one handler
        (is-a? exception (send h get-class)))
            ;is the catch-exception's class a superclass (or the same class as)
            ;of the thrown-exception's class?
  )

(define (throw exception)
  (if (empty? *handlers*) ;no handlers... we can't handle
      (error "EXCEPTION:" (send exception get-message))
      (if (can-handle? exception)
          ((send (pop-h) get-handler) exception)
          (begin
            (pop-h)
            (throw exception)))))

;try-catch block (MACRO)
(define-syntax try
  (syntax-rules (catch)
    ((try (exp1 ...) catch ((ex-class ex-name)) (exp2 ...))
     (call/cc (lambda (cont)
                ;create a new handler
                (let ((h (new handler%
                             (class ex-class) (handler 
                                               (lambda (ex-name) (cont 
                                                           (begin exp2 ...)))))))
                ;push the handler
                (push-h h)
                (let ((result (begin exp1 ...)))
                  (pop-h)
                  result)))))))







;some examples of classes;
(define exception%
  (class object%
    (super-new)
    (init message)
    (field (self-message message))
    (define/public (get-message) self-message)))

(define pippo-exception%
  (class exception%
    (super-new (message "pippo exception"))))

(define pluto-exception%
  (class exception%
    (super-new (message "pluto exception"))))

(define paperino-exception%
  (class exception%
    (super-new (message "paperino exception"))))

;examples of use:
(define (pippo)
  (throw (new pippo-exception%)))

(define (pluto)
  (throw (new pluto-exception%)))

(define (paperino)
  (throw (new paperino-exception%)))

(+ 1 (try ((pluto) (pippo))
            catch ((pluto-exception% e)) (1)))

(try ((pippo))
     catch ((pippo-exception% e))
     ((displayln "pippo caught")
     (try ((pluto))
          catch ((pluto-exception% e))
          ((displayln "pluto caught")))))

(try ((pippo))
       catch ((pippo-exception% e))
       ((display "pippo caught: ")
        (displayln (send e get-message))
        (try ((pluto))
             catch ((pluto-exception% e))
             ((display "pluto caught: ")
              (displayln (send e get-message))))))

(try (
     (try (
           (try ((paperino))
                catch ((pluto-exception% e))
                ((displayln "third catch"))))
          catch ((pluto-exception% e))
          ((displayln "second catch"))))
     catch ((paperino-exception% e))
     ((displayln "first catch")))
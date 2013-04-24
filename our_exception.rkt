#lang racket
(require racket/class)

(define *handlers* '())

(define (push-h handler)
  (set! *handlers* (cons handler *handlers*)))

(define (pop-h)
  (let ((handler (car *handlers*)))
    (set! *handlers* (cdr *handlers*))
    handler))

(define exception%
  (class object%
    (init message)
    (define current-message message)
    (super-new)
    (define/public (get-message) current-message)))

(define null-pointer-exception%
  (class exception%
    (super-new (message "null pointer"))))

(define (throw exception)
  (if (empty? *handlers*)
      (error "EXCEPTION" (send exception get-message))
      ((pop-h))))

(define-syntax try
  (syntax-rules (catch)
    ((try (exp1 ...) catch (exp2 ...))
     (begin
     (call/cc (lambda (cont)
                (push-h (lambda () (cont 
                                    (begin exp2 ...))))
                (let ((result (begin exp1 ...)))
                  (pop-h)
                  result)))))))

(define (foo)
  (throw (new exception% (message "Generic exception thrown"))))

;example of use:
(foo)

(+ 1 (try ((foo))
            catch (1)))

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
      ;se non esiste handler lancio un errore
      (error "EXCEPTION" (send exception get-message))
      ;se esiste handler lo poppo e lo eseguo
      ((pop-h))))

(define-syntax try
  (syntax-rules (catch)
    ((try (exp1 ...) catch (exp2 ...))
     (begin
       (call/cc (lambda (cont)
                  ;salva un handler contenente la continuazione
                  ;e il codiche della catch
                  ;sarà eseguito se si verifica una throw
                  (push-h (lambda () (cont 
                                      (begin exp2 ...))))
                  
                  ;eseguo il codice della try
                  (let ((result (begin exp1 ...)))
                    ;se non c'è stata eccezzione arrivo qui
                    ;ho installato un handler che non serve
                    ;poppo l'handler senza eseguirl
                    (pop-h)
                    result)))))))

(define (foo)
  (throw (new exception% (message "Generic exception thrown"))))

;example of use:
(foo)

(+ 1 (try ((foo))
          catch (1)))

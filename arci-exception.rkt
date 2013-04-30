#lang racket
(require racket/class)

;; ----- Object definitions ----- ;;
(define exception%
  (class object%
    (init name message)   
    (define current-name name)
    (define current-message message) 
    (super-new)
    
    (define/public (get-name)
      current-name)
    
    (define/public (get-message)
      current-message)))

(define negative-number-exception%
  (class exception%
    (init message)  
    (define current-message message)   
    (super-new [name "NEGATIVE NUMBER EXCEPTION"] [message current-message])))

(define handler%
  (class object%
    (init handled-ex proc)   
    (define self-handled-ex handled-ex)
    (define self-proc proc) 
    (super-new)
    
    (define/public (get-handled-ex)
      self-handled-ex)
    
    ;; execute handler procedure
    (define/public (exec-proc ex)
      (self-proc ex))))

;; ----- Handlers  ----- ;;
(define *handlers* (list))

(define (push-handler handler)
  (set! *handlers* (cons handler *handlers*)))

(define (pop-handler)
  (let ((handler (car *handlers*)))
    (set! *handlers* (cdr *handlers*))
    handler))

(define (read-handlers)
  (car *handlers*))

(define (can-handle? exception)
  (if (empty? *handlers*)
      #f
      (let ([handled-ex (read-handlers)])
        (if (is-a? exception (send handled-ex get-handled-ex))
            #t
            #f))))

(define (throw exception)
  (if (can-handle? exception)
      ;execute handler giving him the thrown exception
      (send (pop-handler) exec-proc exception)
      (error (string-append (send exception get-name) ": " (send exception get-message)))))

(define-syntax try
  (syntax-rules (catch)
    ((_ [expr1 ...] catch (exception e) [expr2 ...])
     (begin
       (call/cc (lambda (continuation)
                  ;handler% object push
                  (push-handler 
                   (new handler%
                        [handled-ex exception]
                        [proc (lambda (e) 
                                  (continuation 
                                   (begin expr2 ...)))]))
                  (let ([result (begin expr1 ...)])
                    ;if no exception throw, remove handler without execute it
                    (pop-handler)
                    result)))))))


;; ----- Usage ----- ;;
(define (foo x)
  (cond
    [(< x 0) (throw (new negative-number-exception% [message "negative numbers are not allowed"]))]
    [else (display "here's your x: ") (display x) (newline)]))

(define (generic-foo)
    (throw (new exception% [name "GENERIC EXCEPTION"] [message "generic error"])))

(try
 [(displayln "(foo 4)")
  (foo 4)] ;will not throw any exception
 catch (negative-number-exception% nnex)
 [(display "exception thrown with message: ")
  (send nnex get-message)])

(try
 [(displayln "(foo -4)")
  (foo -4)] ;will throw negative-number-exception%
 catch (negative-number-exception% nnex)
 [(display "exception thrown with message: ")
  (send nnex get-message)])

(try
 [(displayln "(foo -4)")
  (foo -4)] ;will throw negative-number-exception%
 catch (exception% e)
 [(display "exception thrown with message: ")
  (send e get-message)])

(try
 [(displayln "(generic-foo)")
  (generic-foo)] ;will throw exception%
 catch (exception% e)
 [(display "exception thrown with message: ")
  (send e get-message)])

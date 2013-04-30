#lang racket
;12/4: (optional) small project proposal:
;extend the throw/catch mechanism shown in class
;to actually throw an object and catching only it in the handler.
;It must be composable.
;Value: 1 to 2 points.
;Deadline for presenting it: May 31st.
;Note: I need only the (appropriately commented) source code,
;no documentation/manuals.
(define *handlers* ( list ))

(define (push-handler proc)
  (set! *handlers* ( cons proc *handlers*)))

(define (pop-handler)
  (let ((h ( car *handlers*)))
  (set! *handlers* ( cdr *handlers*))
  h))

(define (throw ex-name message)
  (if (pair? *handlers*)
      ((pop-handler))
      ((error ex-name message))))

(define-syntax catch
  (syntax-rules ()
  ((_ handler exp1 ...)
    (call/cc (lambda (exit)
      ; ; install the handler
      (push-handler (lambda ()
        (exit handler)))
    (let ((res ; ; evaluate the body
          (begin exp1 ...)))
          ; ok : discard the handler
          (pop-handler)
          res))))))

(define (foo) (throw "arciException" "porcoddue"))
(define (bar)
      (displayln "bar: now I call foo")
      (foo))
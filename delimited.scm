;; run this file with 'guile', e.g.
;; $ guile delimited.scm

(define (reset f)
  ;; construct a shift function to pass to the reset() body.
  ;; this makes reset re-entrant.

  (let* ((stack '())
         ;; the stack of calls made to continuations -- necessary because delimited continuations eventually return

         (top   (lambda ()  (car stack)))
         ;; shorthand for the place to return to

         (push  (lambda (k) (set! stack (cons k stack))))
         (pop   (lambda ()  (let ((x (top))) (set! stack (cdr stack)) x)))
         ;; stack accessors

         (tail  (lambda (x) ((pop) x) x))
         ;; eta-expansion to prevent premature evaluation of (pop)
         ;; This function's purpose is kind of subtle. Basically, since we're returning to a 'shift' block somewhere, we need to know when we hit the end of a lambda. The best way to do this
         ;; is to take the result of that lambda and send it up the call stack (by invoking the stack top on it).

         (stash (lambda (f) (call/cc (lambda (cc) (push cc) (f)))))
         ;; you could think of this as a 'with-current-continuation-pushed' modifier

         (shift (lambda (f)
                  (stash (lambda ()                  ; grab context of 'shift' call
                    (let ((k (pop)))                 ; save that context so that 'k' always uses it (regardless of stack)
                      (tail (f (lambda (x)           ; call the body with a continuation-ish function that ...
                        (stash (lambda ()            ; remembers its context (so that it can return there) and ...
                          (tail (k x)))))))))))))    ; forwards its parameter to the 'rest' of the shift's context, tailing to the stack top in case that isn't done automatically

    (stash (lambda () (tail (f shift))))))

(define (p x) (display x) (newline))
(define example
  (let ((example-count 0))
    (lambda (name block)
      (set! example-count (+ 1 example-count))
      (display "example ")
      (p (number->string example-count))
      (p name)
      (block)
      (newline))))

(example "control-flow modification using nested delimited continuations" (lambda ()
  (reset (lambda (shift)
    (display "A ")
    (shift (lambda (k)
      (display "B ")
      (k '())
      (display "C ")))
    (display "D ")
    (shift (lambda (k)
      (display "E ")
      (k '())
      (display "F ")))
    (display "G ")))
  (newline)))

(example "composing the continuation" (lambda ()
  (p (reset (lambda (shift)
    (+ 1 (shift (lambda (k)
      (k (k (k 7)))))))))))

(example "a continuation that escapes from its 'reset' block" (lambda ()
  (let ((k (reset (lambda (shift)
             (+ 1 (shift (lambda (k)
               k)))))))
    (p (k 1))
    (p (k (k 1))))))

(example "a 'shift' function that escapes its 'reset' block (mostly meaningless because it has no 'reset' context anymore)" (lambda ()
  (let ((shift (reset (lambda (shift)
                 (+ 1 (shift (lambda (k)
                   shift)))))))

    (p (shift (lambda (k) (k (k 10))))))))

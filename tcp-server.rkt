#lang racket

(require racket/tcp)
(require "grand-syntax.rkt")
(require "ground-scheme.rkt")
(provide tcp-server tcp-thread tcp-send)


(define (tcp-thread listener handler)
  (let* ((input output (tcp-accept listener))
         (receiver (thread (lambda ()
                             (let* ((input (thread-receive))
                                    (output (thread-receive)))
                               (handler input output))))))
    (thread-send receiver input)
    (thread-send receiver output)
    receiver))

(define (tcp-send output message)
  ;;(out "writing "message" to server")
  (writeln message output)
  (flush-output output))

(define (tcp-server port handler)
  (let ((server (tcp-listen port)))
    (thread
     (lambda ()
       (while
        #true
        (tcp-thread
         server
         (lambda (input output)
           (while #true
                  (let* ((line (read-line input))
                         (message (string-trim line #px"(\\s|\u00)+")))
                    (tcp-send output (handler message)))))))))))



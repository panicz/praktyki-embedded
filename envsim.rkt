#lang racket
;; https://github.com/panicz/praktyki-embedded

(require racket/gui/base)
(require racket/tcp)
(require "grand-syntax.rkt")
(require "ground-scheme.rkt")
(require "tcp-server.rkt")

(define simulator-window
  (new frame% [label "Symulator środowiska"]))

(define temperature
  (new slider%
       [label "Temperatura"]
       [min-value -273]
       [max-value 1000]
       [init-value 20]
       [style '(vertical vertical-label)]
       [parent simulator-window]))

(send simulator-window show #true)

(tcp-server
 12345
 ("temperature" (send temperature get-value))
 (_ 0))

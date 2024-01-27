#lang racket

(require "zi.rkt")

(define/contract (xilian zis #:upper? upper?)
  (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))
  ; TODO
  (list zis)
  )

(define/contract (tongyong zis #:upper? upper?)
  (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))
  ; TODO
  (list zis)
  )

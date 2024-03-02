#lang racket

;; zi struct consists of zi: 被切字, upper: 反切上字, and lower: 反切下字
(struct zi (zi upper lower))

(define/contract (zi->string zi)
  (-> zi? string?)
  (string-append (zi-zi zi) " " (zi-upper zi) (zi-lower zi))
  )

;; string? "X XX" e.g. "東 德紅" -> zi struct
(define/contract (string->zi fanqie)
  (-> string? zi?)
  (let ([cols (string-split fanqie)])
    (zi (car cols)
        (substring (cadr cols) 0 1)
        (substring (cadr cols) 1 2))
    ))

;; To be used to parse a multi-line file
(define/contract (string->zis fanqies)
  (-> string? (listof zi?))
  (filter-map
   (λ (str)
     (and (not (null? str))
          (string->zi str)))
   (string-split fanqies "\n"))
  )

(provide
 (contract-out
  [struct zi ((zi string?) (upper string?) (lower string?))]
  [zi->string (-> zi? string?)]
  [string->zi (-> string? zi?)]
  [string->zis (-> string? (listof zi?))]
  ))

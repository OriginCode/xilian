#lang racket

(require "zi.rkt")

(define/contract (assoc-upper upper zis)
  (-> string? (listof zi?) (or/c zi? #f))
  (if (null? zis)
      #f
      (if (= upper (zi-upper (car zis)))
          (car zis)
          (assoc-upper upper (cdr zis))))
  )

(define/contract (assoc-lower lower zis)
  (-> string? (listof zi?) (or/c zi? #f))
  (if (null? zis)
      #f
      (if (= lower (zi-lower (car zis)))
          (car zis)
          (assoc-lower lower (cdr zis))))
  )

(define/contract (zi->upperzi zi)
  (-> zi? (cons/c string? zi?))
  (cons (zi-upper zi) zi)
  )

(define/contract (zi->lowerzi zi)
  (-> zi? (cons/c string? zi?))
  (cons (zi-lower zi) zi)
  )

(define/contract (xilian zis #:upper? upper)
  (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))
  ; TODO
  (list zis)
  )

(define/contract (tongyong zis #:upper? upper)
  (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))
  (let ([spellerzis (if upper
                        (map zi->upperzi zis)
                        (map zi->lowerzi zis))])
    (hash-map (foldr (λ (spellerzi acc)
                       (hash-update acc (car spellerzi)
                                    (λ (zis) (cons (cdr spellerzi) zis))
                                    (list)))
                     (hash)
                     spellerzis)
              (λ (_ val) val))
  ))
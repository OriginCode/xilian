#lang racket

(require "zi.rkt")

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
  (diyong-huyong (tongyong zis #:upper? upper) #:upper? upper)
  )

(define/contract (tongyong zis #:upper? upper)
  (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))
  (let ([spellerzis (if upper
                        (map zi->upperzi zis)
                        (map zi->lowerzi zis))])
    (hash-map (foldr
               (λ (spellerzi acc)
                 (hash-update
                  acc
                  (car spellerzi)
                  (λ (zis) (cons (cdr spellerzi) zis))
                  (list)))
               (hash)
               spellerzis)
              (λ (_ val) val))
  ))

(define/contract (diyong-huyong classes #:upper? upper)
  (-> (listof (listof zi?)) #:upper? boolean? (listof (listof zi?)))
  (letrec ([zi-speller (if upper zi-upper zi-lower)]
           [assoc-zi (λ (zi lst)
                       (if (null? lst)
                           #f
                           (if (equal? zi (zi-zi (car lst)))
                               (car lst)
                               (assoc-zi zi (cdr lst)))))]
           [find-merge (λ (prev zis left)
                         (if (null? left)
                             (cons zis prev)
                             (if (or (assoc-zi (zi-speller (car zis)) (car left))
                                     (assoc-zi (zi-speller (caar left)) zis))
                                 (find-merge prev (append zis (car left)) (cdr left))
                                 (find-merge (cons (car left) prev) zis (cdr left)))))]
           [fold-merge (λ (acc classes)
                         (if (null? classes)
                             acc
                             (let ([merged (find-merge (list) (car classes) (cdr classes))])
                               (fold-merge (cons (car merged) acc) (cdr merged)))))])
    (fold-merge (list) classes)
    ))

;; Test case
;; (define zis (string->zis "東 德紅
;; 同 徒紅
;; 中 陟弓
;; 終 職戎
;; 蟲 直弓
;; 忡 敕中
;; 崇 鋤弓
;; 嵩 息弓
;; 戎 如融
;; 弓 居戎
;; 融 以戎
;; 雄 羽弓
;; 穹 去宮
;; 窮 渠弓
;; 馮 房戎
;; 風 方戎
;; 豐 敷空
;; 充 昌終
;; 隆 力中
;; 空 苦紅
;; 公 古紅
;; 蒙 莫紅
;; 籠 力董
;; 洪 後公
;; 從 徂紅
;; 怱 蒼紅
;; 通 他紅
;; 蓬 薄紅
;; 烘 呼東
;; 㟅 五東
;; 檧 蘇公
;; "))
;; 
;; (map (λ (zis) (map zi->string zis)) (xilian zis #:upper? #f))
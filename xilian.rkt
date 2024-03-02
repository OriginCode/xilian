#lang racket

(require "zi.rkt")

;; xilian: zis |> tongyong |> diyong-huyong
(define/contract (xilian zis #:upper? upper)
  (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))
  (diyong-huyong (tongyong zis #:upper? upper) #:upper? upper)
  )

(define/contract (tongyong zis #:upper? upper)
  (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))
  (let ([spellerzis (if upper
                        (map (λ (zi) (cons (zi-upper zi) zi)) zis)
                        (map (λ (zi) (cons (zi-lower zi) zi)) zis))])
    (hash-map ; only get values of the hashmap
     (foldr ; partition the list by speller
      (λ (spellerzi acc)
        (hash-update ; hashmap: key: speller value: zi
         acc
         (car spellerzi)
         (λ (zis) (cons (cdr spellerzi) zis))
         (list)))
      (hash)
      spellerzis)
     (λ (_ val) val))
    ))

;; Requires a preprocessed list of classes (most likely the result from tongyong)
(define/contract (diyong-huyong classes #:upper? upper)
  (-> (listof (listof zi?)) #:upper? boolean? (listof (listof zi?)))
  (letrec ([zi-speller (if upper zi-upper zi-lower)]
           ;; Helper lambda to assoc zi obj with a zi string
           [assoc-zi (λ (zi lst)
                       (if (null? lst)
                           #f
                           (if (equal? zi (zi-zi (car lst)))
                               (car lst)
                               (assoc-zi zi (cdr lst)))))]
           [merge (λ (acc classes)
                    (if (null? classes)
                        acc
                        (let ([curr (car classes)])
                          (if (null? acc)
                              (merge (cons curr acc) (cdr classes)) ; brand new acc, we are the first class
                              (merge
                               (let ([new-acc
                                      (foldr
                                       (λ (acc-class new-acc)
                                         (if (car new-acc)
                                             (cons #t (cons acc-class (cdr new-acc))) ; if already merged, skip until the end
                                             (if (or (ormap (λ (zi) (assoc-zi (zi-speller zi) curr)) acc-class) ; otherwise, try to find matching classes to merge
                                                     (assoc-zi (zi-speller (car curr)) acc-class))
                                                 (cons #t (cons (append acc-class curr) (cdr new-acc))) ; found diyong/huyong, merge
                                                 (cons #f (cons acc-class (cdr new-acc)))))) ; diyong/huyong not found, skip
                                       (list #f)
                                       acc)])
                                 (if (car new-acc)
                                     (cdr new-acc) ; if already merged, we're done
                                     (cons (car classes) (cdr new-acc)))) ; otherwise, we're a new class
                               (cdr classes))))))])
    (merge (list) classes)
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

(provide
 (contract-out
  [xilian (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))]
  [tongyong (-> (listof zi?) #:upper? boolean? (listof (listof zi?)))]
  [diyong-huyong (-> (listof (listof zi?)) #:upper? boolean? (listof (listof zi?)))]
  ))
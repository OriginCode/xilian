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
           [merge (λ (acc curr classes)
                    (if (null? classes)
                        acc
                        (if (null? acc)
                            (merge (cons curr acc) (car classes) (cdr classes))
                            (merge (let ([new-acc (foldr (λ (acc-class new-acc)
                                                           (if (car new-acc)
                                                               (cons #t (cons acc-class (cdr new-acc)))
                                                               (if (or (foldl (λ (zi acc) (assoc-zi (zi-speller zi) curr)) #f acc-class)
                                                                       (foldl (λ (zi acc) (assoc-zi (zi-speller zi) acc-class)) #f curr))
                                                                   (cons #t (cons (append acc-class curr) (cdr new-acc)))
                                                                   (cons #f (cons acc-class (cdr new-acc))))))
                                                         (list #f)
                                                         acc)])
                                     (if (car new-acc)
                                         (cdr new-acc)
                                         (cons curr (cdr new-acc))))
                                   (car classes) (cdr classes)))))])
    (merge (list) (car classes) (cdr classes))
    ))

;; Test case
(define zis (string->zis "東 德紅
同 徒紅
中 陟弓
終 職戎
蟲 直弓
忡 敕中
崇 鋤弓
嵩 息弓
戎 如融
弓 居戎
融 以戎
雄 羽弓
穹 去宮
窮 渠弓
馮 房戎
風 方戎
豐 敷空
充 昌終
隆 力中
空 苦紅
公 古紅
蒙 莫紅
籠 力董
洪 後公
從 徂紅
怱 蒼紅
通 他紅
蓬 薄紅
烘 呼東
㟅 五東
檧 蘇公
"))
(define classes (tongyong zis #:upper? #f))
(map (λ (zis) (map zi->string zis)) (xilian zis #:upper? #f))
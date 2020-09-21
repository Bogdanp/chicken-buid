(module buid (make-buid-factory buid)
  (import scheme
          srfi-69
          (chicken base)
          (chicken bitwise)
          (chicken format)
          (chicken random)
          (chicken time))

  (define epoch 1586026830000)
  (define epoch/centiseconds (quotient epoch 10))

  (define (centiseconds-since-epoch)
    (truncate (- (* (current-seconds) 100) epoch/centiseconds)))

  (define randomness-bytes 11)
  (define randomness-mask #x7FFFFFFFFFFFFFFFFFFFFF)

  (define (make-randomness-factory bufsize)
    (define buf (make-string bufsize))
    (define pos 0)
    (define (load!)
      (random-bytes buf bufsize))
    (define (take!)
      (define start pos)
      (set! pos (+ pos randomness-bytes))
      (when (> pos bufsize)
        (load!)
        (set! start 0)
        (set! pos randomness-bytes))
      (substring buf start pos))

    (load!)
    (lambda ()
      (define bs (take!))
      (define n
        (let loop ([n 0]
                   [i randomness-bytes])
          (cond
           [(zero? i) n]
           [else (let ([b (string-ref bs (sub1 i))])
                   (loop
                    (bitwise-ior n (arithmetic-shift (char->integer b)
                                                     (* 8 (- randomness-bytes i))))
                    (sub1 i)))])))
      (bitwise-and n randomness-mask)))


  (define (make-buid-factory)
    (define last-t 0)
    (define last-r 0)
    (define make-randomness (make-randomness-factory (* 1 1024 1024)))
    (lambda ()
      (define t (centiseconds-since-epoch))
      (cond
       [(= t last-t)
        (set! last-r (add1 last-r))
        (make-buid t last-r)]

       [else
        (set! last-t t)
        (set! last-r (make-randomness))
        (make-buid t last-r)])))

  (define buid (make-buid-factory))

  (define (make-buid t r)
    (define bs (make-string 22 #\0))
    (integer->base62 r bs 22)
    (integer->base62 t bs 7)
    bs)

  ;;; Base62

  (define (range n)
    (let loop ([n n] [xs '()])
      (cond
       [(zero? n) xs]
       [else (loop (sub1 n) (cons (sub1 n) xs))])))

  (define base62-alphabet
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

  (define base62-integer-table
    (alist->hash-table (map cons (range (string-length base62-alphabet)) (string->list base62-alphabet))))

  (define (base62-integer->char n)
    (hash-table-ref base62-integer-table n))

  (define (integer->base62 n buf bufsize)
    (let loop ([n n]
               [i (sub1 bufsize)])
      (string-set! buf i (base62-integer->char (remainder n 62)))
      (unless (or (zero? n) (zero? i))
        (loop (quotient n 62) (sub1 i))))))

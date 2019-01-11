(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
                    
(define (decode bits tree)
  (define (decode-1 bits current-branch rev)
    (if (null? bits)
        (reverse rev)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) rev))
              (decode-1 (cdr bits) next-branch rev)))))
          
  (decode-1 bits tree '()))

(define (choose-branch bit branch)
  (if (= bit 0) 
      (left-branch branch)
      (right-branch branch)))
      
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((member? symbol (symbols (left-branch tree)))
            (append '(0) (encode-symbol symbol (left-branch tree))))
        ((member? symbol (symbols (right-branch tree)))
            (append '(1) (encode-symbol symbol (right-branch tree))))))
            
(define (grow-huffman-tree pairs)
  (merge (make-leaf-set pairs)))

(define (merge ordered-leaves)
  (if (null? (cdr ordered-leaves))
    (car ordered-leaves)
    (merge (adjoin-set (make-code-tree (car ordered-leaves) (cadr ordered-leaves)) 
                       (cddr ordered-leaves)))))
                       
(define (list-of-leaves tree)
  (cond((null? tree)'())
       ((not (leaf? tree))
        (append (list-of-leaves (right-branch tree))
                (list-of-leaves (left-branch tree))))
       (else (list (list (symbol-leaf tree) (weight-leaf tree))))))
       
(define (expected-code-length tree)
  (let* ((symbol (map (lambda (x) (car x))(list-of-leaves tree)))
         (weight (map (lambda (y) (cadr y))(list-of-leaves tree)))
         (sum (apply + weight)))
        (apply + (map (lambda (z d) (*(/ z sum) (length (encode-symbol d tree))))
                         weight symbol))))

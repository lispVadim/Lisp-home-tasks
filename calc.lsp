(defstruct my-rational (numerator 0) (denominator 1))
(defvar curr-table)

(defun simplify (x)
    "Simplifies rational numbers"
    (if (my-rational-p x)
        (let* ((n (my-rational-numerator x)) (d (my-rational-denominator x)) (r (gcd n d)))
            (make-my-rational :numerator (/ n r) :denominator (/ d r)))
        x))
        
(defun to-my-rational (x)
    "Casts number to rational number"
    (if (my-rational-p x)
        x
        (make-my-rational :numerator x)))

(defun add-rational (op1 op2)
    "Adds one rational number to another"
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (+ (* m1 n2) (* m2 n1))
            :denominator (* n1 n2))))

(defun sub-rational (op1 op2)
    "Subtracts one rational number from another"
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (- (* m1 n2) (* m2 n1))
            :denominator (* n1 n2))))
            
(defun mul-rational (op1 op2)
    "Multiplies two rational numbers"
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (* m1 m2)
            :denominator (* n1 n2))))

(defun div-rational (op1 op2)
    "Divides two rational numbers"
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (* m1 n2)
            :denominator (* n1 m2))))
            
(defun do-rational (rational-func int-func &rest args)
    "Applies arithmetic function to args"
    (simplify
        (reduce
            (lambda (acc elem)
                (if (or (my-rational-p acc) (my-rational-p elem) (eq int-func #'/))
                    (funcall rational-func (to-my-rational acc) (to-my-rational elem))
                    (funcall int-func acc elem)))
            args)))

(defun my-let (expr table)
    "Processes a let expression"
    (mapcar
        (lambda (x)
            (setf (gethash (car x) table) (cadr x)))
        expr))
        
(defun lookup (key tables)
    "Searches a key in a list of hash tables"
    (if tables
        (let ((val (gethash key (car tables))))
            (if val
                val
                (lookup key (cdr tables))))))

(defun calc (expr &optional tables)
    "Performs calculations"
    (let ((op (car expr)) (tail (cdr expr)))
        (if expr
            (cond
                ((eq op '+) (apply #'do-rational #'add-rational #'+ (calc tail tables)))
                ((eq op '-) (apply #'do-rational #'sub-rational #'- (calc tail tables)))
                ((eq op '*) (apply #'do-rational #'mul-rational #'* (calc tail tables)))
                ((eq op '/) (apply #'do-rational #'div-rational #'/ (calc tail tables)))
                ((eq op 'let)
                    (setf curr-table (make-hash-table))
                    (my-let (car tail) curr-table)
                    (calc (cdr tail) (cons curr-table tables)))
                ((listp op) (cons (calc op tables) (calc tail tables)))
                ((symbolp op) (cons (lookup op tables) (calc tail tables)))
                (T (cons op (calc tail tables)))))))

;Tests
(unless (= (calc '(+ 1 2)) 3) (print "ERROR: 1 + 2 != 3"))
(unless (= (calc '(- 1 2 3)) -4) (print "ERROR: 1 - 2 - 3 != -4"))
(unless (= (calc '(* 4 5 6)) 120) (print "ERROR: 4 * 5 * 6 != 120"))
(unless (equalp (calc '(/ 5 6)) (make-my-rational :numerator 5 :denominator 6)) (print "ERROR: 5 / 6 != 5/6"))
(unless (= (calc '(+ 4 5 (* 5 2))) 19) (print "ERROR: 4 + 5 + 5*2 != 19"))
(unless (= (calc '(let ((a 4) (b 5) (c 2)) + a b (* b c))) 19) (print "ERROR: 4 + 5 + 5*2 != 19"))
(unless (equalp (calc '(let ((a 4) (b 5)) + a b (let ((a 1 ) (c 2)) / a c))) (make-my-rational :numerator 19 :denominator 2)) (print "ERROR: 4 + 5 + 1/2 != 19/2"))

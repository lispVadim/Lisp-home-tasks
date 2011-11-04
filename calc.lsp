(defstruct my-rational (numerator 0) (denominator 1))
(defvar curr-table)

(defun simplify (x)
    (if (my-rational-p x)
        (let* ((n (my-rational-numerator x)) (d (my-rational-denominator x)) (r (gcd n d)))
            (make-my-rational :numerator (/ n r) :denominator (/ d r)))
        x))
        
(defun to-my-rational (x)
    (if (my-rational-p x)
        x
        (make-my-rational :numerator x)))

(defun add-rational (op1 op2)
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (+ (* m1 n2) (* m2 n1))
            :denominator (* n1 n2))))

(defun sub-rational (op1 op2)
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (- (* m1 n2) (* m2 n1))
            :denominator (* n1 n2))))
            
(defun mul-rational (op1 op2)
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (* m1 m2)
            :denominator (* n1 n2))))

(defun div-rational (op1 op2)
    (let ((m1 (my-rational-numerator op1)) (n1 (my-rational-denominator op1))
          (m2 (my-rational-numerator op2)) (n2 (my-rational-denominator op2)))
        (make-my-rational
            :numerator (* m1 n2)
            :denominator (* n1 m2))))
            
(defun do-rational (rational-func int-func &rest args)
    (simplify
        (reduce
            (lambda (acc elem)
                (if (or (my-rational-p acc) (my-rational-p elem) (eq int-func #'/))
                    (funcall rational-func (to-my-rational acc) (to-my-rational elem))
                    (funcall int-func acc elem)))
            args)))

(defun my-let (expr table)
    (mapcar
        (lambda (x)
            (setf (gethash (car x) table) (cadr x)))
        expr))
        
(defun lookup (key tables)
    (if tables
        (let ((val (gethash key (car tables))))
            (if val
                val
                (lookup key (cdr tables))))))

(defun calc (expr &optional tables)
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

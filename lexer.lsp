(defun char-numberp (c)
    "Tests if character 'c' is number"
    (let ((code (char-code c)))
        (and (>= code (char-code #\0)) (<= code (char-code #\9)))))
        
(defun lex-num (c f &optional (acc 0))
    "Reads number, that begins by character 'c', from stream 'f'"
    (let ((num (+ (* acc 10) (- (char-code c) (char-code #\0))))
          (next (read-char f nil)))
        (if (and next (char-numberp next))
            (lex-num next f num)
            (progn (unread-char next f) num))))

(defun lex-expr (f)
    "Analyzes an expression from stream 'f'"
    (let ((c (read-char f nil)))
        (cond
            ((null c) nil)
            ((eq c #\+) (cons 'plus (lex-expr f)))
            ((eq c #\-) (cons 'minus (lex-expr f)))
            ((eq c #\*) (cons 'mul (lex-expr f)))
            ((eq c #\/) (cons 'div (lex-expr f)))
            ((eq c #\() (cons 'lpt (lex-expr f)))
            ((eq c #\)) (cons 'rpt (lex-expr f)))
            ((char-numberp c) (cons (list 'num (lex-num c f)) (lex-expr f)))
            ((or (eq c #\Space) (eq c #\Tab) (= (char-code c) 10) (= (char-code c) 13)) (lex-expr f))
            (t (cons (format nil "invalid character - ~a" c) (lex-expr f))))))

(defun lexer (file-name)
    (with-open-file (f file-name :if-does-not-exist nil)
        (lex-expr f)))

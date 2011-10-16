(defun is-palindrome (lst)
    (cond
        ((null lst) nil)
        ((null (cdr lst)) nil)
        ((equal lst (reverse lst)) T)
        (T nil)))

(defun max-right-palindrome (lst &optional (max-pal nil) (max-len 0))
    "Function that searches a maximal palindrome at the end of list 'lst'"
    (cond
        ((<= (length lst) max-len) max-pal)
        ((is-palindrome lst) lst)
        (T (max-right-palindrome (cdr lst) max-pal max-len))))
    
(defun max-palindrome (lst &optional (max-pal nil) (max-len 0))
    "Function that searches a maximal palindrome in the list 'lst'"
    (cond
        ((null lst) max-pal)
        ((<= (length lst) max-len) max-pal)
        (T
            (let ((local-max-pal (max-right-palindrome (reverse lst) max-pal max-len)))
                (max-palindrome (cdr lst) local-max-pal (length local-max-pal))))))

                
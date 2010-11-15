(defpackage :vana-utils
   (:use :common-lisp)
   (:export :args->alist
            :list->string
            :defalias
            :->
            :zero?))

(in-package :vana-utils)

;; I like pretty function names :P
(defmacro defalias (old new)
  `(defun ,new (&rest args)
     (apply #',old args)))

(defalias zerop zero?)

;; Convenience function
(defun list->string (list &optional separator)
  "A bit of a hacky way to turn a list into a string via reduce"
  (if list
      (reduce #'(lambda (str1 str2) (concatenate 'string str1 str2 separator)) list)
      ""))

(defun args->alist (&rest args)
  "Takes an even number of arguments and returns an associative list consisting of (ARG1 . ARG2) (ARG3 . ARG4), etc."
  (if (null args)
      nil
      (acons (first args)
             (second args)
             (apply #'args->alist (rest (rest args))))))

(defun -> (key list &optional (test 'string-equal))
  "Returns value associated with KEY from the associatie list ALIST. Optionally uses :TEST for key equality."
  (cdr (assoc key list :test test)))

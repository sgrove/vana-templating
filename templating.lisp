(defpackage :vana-templating
   (:use :common-lisp
         :vana-utils)
   (:export :!DOCTYPE
            :a :abbr :address :area :article :aside :audio
            :b :base :bdo :blockquote :body :br :button
            :canvas :caption :cite :code :col :colgroup :command
            :datalist :dd :del :details :dfn :div :dl :dt
            :em :embed :eventsource
            :fieldset :figcaption :figure :footer :form
            :h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :hr :html
            :i :iframe :img :input :ins
            :html-map :html-time :html-meta
            :kbd :keygen
            :label :legend :li :link
            :mark :menu :meta :meter
            :nav :noscript
            :object :ol :optgroup :option :output
            :p :param :pre :progress
            :q
            :ruby :rp :rt
            :samp :script :section :select :small :source :span :strong :style :sub :summary :sup
            :table :tbody :td :textarea :tfoot :th :thead :title :tr
            :ul :video :wbr
            :lispscript))

(in-package :vana-templating)

;; Low-level templating stuff
(defun html5-tags ()
  '(!DOCTYPE a abbr address area article aside audio b base bdo blockquote body br button canvas caption cite code col colgroup command datalist dd del details dfn div dl dt em embed eventsource fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hgroup hr html i iframe img input ins kbd keygen label legend li link mark html-map menu html-meta meta meter nav noscript object ol optgroup option output p param pre progress q ruby rp rt samp script section select small source span strong style sub summary sup table tbody td textarea tfoot th thead html-time title tr ul video wbr))

(defun self-closing-tags ()
  '(hr input img))

(defun non-closing-tags ()
  '(meta link !doctype))

(defun comment (comment)
  (format nil "<!-- ~A --!>" comment))

(defun attrs (&rest r)
  (if (evenp (length r))
      (format nil "~(~{ ~a=\"~a\"~}~)" r)
      (concatenate 'string " " (string (first r)))))

;; Example tag cases:
;;; (span)
;;; (p '(id \#body))
;;; (div '(id \#body) "Some content")
;;; (div '(id \#body) (p "some other thing"))
;;; (div "hello world")

(defun make-tag (name)
  (setf (fdefinition name)
        (lambda (&rest sub-exps)
          (let ((content (if (stringp (first sub-exps))
                             (list->string sub-exps)
                             (if (listp (first sub-exps))
                                 (list->string (rest sub-exps)))))
                (attributes (if (not (zero? (length sub-exps)))
                                (if (listp (first sub-exps))
                                    (apply #'attrs (first sub-exps))
                                    "")
                                "")))
            (get-output-stream-string
             (let ((out (make-string-output-stream)))
               (cond ((member name (self-closing-tags)) (format out "<~A~A />"
                                                                (string-downcase (string name))
                                                                attributes))
                     ((member name (non-closing-tags)) (format out "<~A~A >~A"
                                                               (string-downcase (string name))
                                                               attributes
                                                               content))
                     (t (format out "<~a~A>~A</~A>"
                                (string-downcase (string name))
                                attributes
                                content
                                (string-downcase (string name)))))
               out))))))

(map 'list #'make-tag (html5-tags))

(defmacro lispscript (&rest script)
  `(script '(type "text/javascript") (parenscript:ps ,script)))


;;; Template Utilities
(defun translate (sym)
  sym)



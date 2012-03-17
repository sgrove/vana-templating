
(asdf:defsystem #:vana-utils
  :components ((:file "utils"))
  :name "utils"
  :version "1.0.0"
  :maintainer "Sean Groves"
  :author "Sean Groves"
  :licence "MIT"
  :description "Utils for vana"
  :long-description "Utils for vana")

(asdf:defsystem #:vana-templating
  :components ((:file "templating"))
  :depends-on ( #:vana-utils #:parenscript)
  :name "templating"
  :version "1.0.0"
  :maintainer "Sean Groves"
  :author "Sean Groves"
  :licence "MIT"
  :description "An utterly sensible templating system written in Common Lisp. "
  :long-description "An utterly sensible templating system written in Common Lisp. Generates HTML")

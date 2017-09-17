;;
;;
(asdf:defsystem #:vg-tiger
  :description "set me!"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD 3-clause license"
  :serial t
  :depends-on (:lisppi-mouse :starky :cffi)
  :components ((:file "package")
;;	       (:file "mouse")
	       (:file "tiger-data-list")
	       (:file "tiger")))


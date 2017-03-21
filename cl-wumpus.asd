(asdf:defsystem #:cl-wumpus
  :name "cl-wumpus"
  :author "Soós Péter Levente"
  :licence "MIT"
  :description "Play the classic game of Hunt The Wumpus."
  :depends-on (:cl-kreg)
  :components ((:file "package")
               (:file "text" :pathname "text/text"
                      :depends-on ("english"))
               (:file "english" :pathname "text/english"
                      :depends-on ("package"))
               (:file "cl-wumpus" :depends-on ("package" "text"))))

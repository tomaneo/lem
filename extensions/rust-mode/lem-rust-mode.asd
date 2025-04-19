(defsystem "lem-rust-mode"
  :depends-on ("lem" "lem-lisp-mode" "lem-lsp-mode")
  :serial t
  :components ((:file "rust-mode")
               (:file "lsp-config")))

;;-*-coding: utf-8;-*-
(define-abbrev-table 'ada-mode-abbrev-table
  '(
    ("jd" "Ada.Text_Io.Put_Line (\"poop: \");" nil :count 0)
   ))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("jd" "std::cout << \"poop: \" << std::endl;" nil :count 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("jd" "printf(stderr, \"poop: \");" nil :count 0)
   ))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("jd" "(message \"poop: \")" nil :count 0)
   ))

(define-abbrev-table 'global-abbrev-table
  '(
    ("jT" "" (lambda nil (interactive) (insert (format-time-string "%Y%m%d%H%M%S"))) :count 0)
    ("ja" "" (lambda nil (interactive) (insert (format-time-string "%Y-%m-%d"))) :count 1)
    ("ji" "(interactive)" nil :count 1)
    ("jl" "(lambda ()" nil :count 0)
    ("jt" "" (lambda nil (interactive) (insert (format-time-string "%Y%m%d"))) :count 0)
   ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("je" "#+attr_org: :width 300px
#+attr_html: :class emacs-img" nil :count 0)
    ("jg" "#+attr_org: :width 300px
#+attr_html: :width 100%" nil :count 0)
    ("jk" "#+begin: kanban :layout (\"...\") :scope nil :range (\"TODO\" . \"DONE\") :sort \"O\" :depth 2 :compressed t
#+end:" nil :count 0)
    ("jm" "#+hugo: more" nil :count 0)
    ("jo" "---
#+TOC: headlines 1 local
---" nil :count 0)
    ("jp" "~--APT--~" nil :count 0)
   ))

(define-abbrev-table 'sh-mode-abbrev-table
  '(
    ("jd" "echo \"poop: \"" nil :count 0)
   ))

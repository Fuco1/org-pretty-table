;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-pretty-table)

(defun opt-assert-display (expected)
  (string-match-p expected (get-text-property (point) 'display)))

(defun opt-search-and-backward (str)
  (re-search-forward str)
  (backward-char 1))

(defmacro with-org-table (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (org-mode)
     (insert "|------+-----|
| name | age |
|------+-----|
| bob  |  10 |
|------+-----|")
     (org-pretty-table-mode 1)
     (jit-lock-refontify)
     (jit-lock-fontify-now)
     ,@body))

(describe "org-pretty-table"

  (before-each
    (setq org-pretty-table-charset "┌┐└┘┬┤┴├┼─│"))

  (describe "at beginning of buffer"

    (it "should prettify up-left corner"
      (with-org-table
        (goto-char (point-min))
        (expect (opt-assert-display "┌") :to-be-truthy)))

    (it "should prettify up-right corner"
      (with-org-table
        (goto-char (point-min))
        (end-of-line)
        (backward-char 1)
        (expect (opt-assert-display "┐") :to-be-truthy)))

    (it "should prettify down facing T"
      (with-org-table
        (goto-char (point-min))
        (opt-search-and-backward "+")
        (expect (opt-assert-display "┬") :to-be-truthy))))

  (describe "at the middle of buffer"

    (it "should prettify up-left corner"
      (with-org-table
        (goto-char (point-min))
        (insert "hello world \n\n")
        (opt-search-and-backward "|")
        (expect (opt-assert-display "┌") :to-be-truthy)))

    (it "should prettify up-right corner"
      (with-org-table
        (goto-char (point-min))
        (insert "hello world \n\n")
        (end-of-line)
        (backward-char 1)
        (expect (opt-assert-display "┐") :to-be-truthy)))

    (it "should prettify down facing T"
      (with-org-table
        (goto-char (point-min))
        (insert "hello world \n\n")
        (opt-search-and-backward "+")
        (expect (opt-assert-display "┬") :to-be-truthy)))))

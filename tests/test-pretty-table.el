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
  (let ((prefix (plist-get body :prefix))
        (suffix (plist-get body :suffix)))
    `(with-temp-buffer
       (org-mode)
       (insert "|------+-----|
| name | age |
|------+-----|
| bob  |  10 |
|------+-----|")
       (when ,prefix
         (goto-char (point-min))
         (insert ,prefix))
       (when ,suffix
         (goto-char (point-max))
         (insert ,suffix))

       (org-pretty-table-mode 1)
       (jit-lock-refontify)
       (jit-lock-fontify-now)
       ,@body)))

(describe "org-pretty-table"

  (before-each
    (setq org-pretty-table-charset "┌┐└┘┬┤┴├┼─│"))

  (describe "horizontal bar"

    (it "should not prettify segments not ending in + or |"
      (with-org-table
        :prefix "|------+-----|\n| ---- | --- |\n"
        (goto-char (point-min))
        (forward-line 1)
        (re-search-forward "----")
        (backward-char 4)
        (expect (get-text-property (point) 'display) :to-be nil))))

  (describe "top row"

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

    (describe "with empty line above"

      (it "should prettify up-left corner"
        (with-org-table
          :prefix "hello world \n\n"
          (goto-char (point-min))
          (opt-search-and-backward "|")
          (expect (opt-assert-display "┌") :to-be-truthy)))

      (it "should prettify up-right corner"
        (with-org-table
          :prefix "hello world \n\n"
          (goto-char (point-min))
          (re-search-forward "+")
          (end-of-line)
          (backward-char 1)
          (expect (opt-assert-display "┐") :to-be-truthy)))

      (it "should prettify down facing T"
        (with-org-table
          :prefix "hello world \n\n"
          (goto-char (point-min))
          (opt-search-and-backward "+")
          (expect (opt-assert-display "┬") :to-be-truthy))))

    (describe "with content line above"

      (it "should prettify up-left corner"
        (with-org-table
          :prefix "** headline\n"
          (goto-char (point-min))
          (opt-search-and-backward "|")
          (expect (opt-assert-display "┌") :to-be-truthy)))

      (it "should prettify up-right corner"
        (with-org-table
          :prefix "** headline\n"
          (goto-char (point-min))
          (re-search-forward "+")
          (end-of-line)
          (backward-char 1)
          (expect (opt-assert-display "┐") :to-be-truthy)))

      (it "should prettify down facing T"
        (with-org-table
          :prefix "** headline\n"
          (goto-char (point-min))
          (opt-search-and-backward "+")
          (expect (opt-assert-display "┬") :to-be-truthy)))))

  (describe "middle row"

    (it "should prettify left facing T"
      (with-org-table
        (goto-char (point-min))
        (re-search-forward "age")
        (forward-line 1)
        (end-of-line)
        (backward-char 1)
        (expect (opt-assert-display "┤") :to-be-truthy)))

    (it "should prettify right facing T"
      (with-org-table
        (goto-char (point-min))
        (re-search-forward "age")
        (forward-line 1)
        (beginning-of-line)
        (expect (opt-assert-display "├") :to-be-truthy)))

    (it "should prettify cross"
      (with-org-table
        (goto-char (point-min))
        (re-search-forward "+")
        (opt-search-and-backward "+")
        (expect (opt-assert-display "┼") :to-be-truthy))))

  (describe "bottom row"

    (describe "at the end of buffer"

      (it "should render bottom left corner"
        (with-org-table
          (goto-char (point-max))
          (re-search-backward "|")
          (beginning-of-line)
          (expect (opt-assert-display "└") :to-be-truthy)))

      (it "should render bottom right corner"
        (with-org-table
          (goto-char (point-max))
          (re-search-backward "|")
          (expect (opt-assert-display "┘") :to-be-truthy)))

      (it "should render up facing T"
        (with-org-table
          (goto-char (point-max))
          (re-search-backward "+")
          (expect (opt-assert-display "┴") :to-be-truthy))))

    (describe "with empty line below"

      (it "should render bottom left corner"
        (with-org-table
          :suffix "\n\nhello"
          (goto-char (point-max))
          (re-search-backward "|")
          (beginning-of-line)
          (expect (opt-assert-display "└") :to-be-truthy)))

      (it "should render bottom right corner"
        (with-org-table
          :suffix "\n\nhello"
          (goto-char (point-max))
          (re-search-backward "|")
          (expect (opt-assert-display "┘") :to-be-truthy)))

      (it "should render up facing T"
        (with-org-table
          :suffix "\n\nhello"
          (goto-char (point-max))
          (re-search-backward "+")
          (expect (opt-assert-display "┴") :to-be-truthy))))

    (describe "with content line below"

      (it "should render bottom left corner"
        (with-org-table
          :suffix "\n** headline\n"
          (goto-char (point-max))
          (re-search-backward "|")
          (beginning-of-line)
          (expect (opt-assert-display "└") :to-be-truthy)))

      (it "should render bottom right corner"
        (with-org-table
          :suffix "\n** headline\n"
          (goto-char (point-max))
          (re-search-backward "|")
          (expect (opt-assert-display "┘") :to-be-truthy)))

      (it "should render up facing T"
        (with-org-table
          :suffix "\n** headline\n"
          (goto-char (point-max))
          (re-search-backward "+")
          (expect (opt-assert-display "┴") :to-be-truthy))))))

;;; org-pretty-table.el --- Replace org-table characters with box-drawing unicode glyphs.

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: faces
;; Version: 0.0.1
;; Created: 29th November 2013

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This replaces the characters - | and + in `org-mode' tables with
;; appropriate unicode box-drawing glyphs, see
;; http://en.wikipedia.org/wiki/Box-drawing_character

;;; Code:

(defconst org-pretty-table-regexp (regexp-opt '("-" "+" "|")))

(defsubst org-pretty-table-is-empty-line ()
  (looking-at-p (rx bol (* blank) (or eol ?#))))

(defun org-pretty-table-propertize-region (start end)
  "Replace org-table characters with box-drawing unicode glyphs
between START and END.

Used by jit-lock for dynamic highlighting."
  (save-excursion
    (goto-char start)
    (let (table-end)
      (while (re-search-forward org-pretty-table-regexp end t)
        ;; reached the end of the current table
        (if (and table-end
                 (> (point) table-end))
            (setq table-end nil))

        ;; check if the current match is a table if we are not in a
        ;; table right now
        (unless (and (not table-end)
                     (not (save-match-data
                            (org-at-table-p))))

          ;; get the end of the table if we found a new table, so we
          ;; don't have to check (org-at-table-p) again until then
          (unless table-end
            (save-match-data
              (setq table-end (org-table-end))))

          ;; determine the context of the character
          (let ((match (match-string 0)))
            (cond
             ((equal "-" match)
              (backward-char 1)
              (re-search-forward "-+")
              (put-text-property (match-beginning 0) (match-end 0) 'display (make-string (- (match-end 0) (match-beginning 0)) ?─))
              t)
             ((equal "|" match)
              (cond
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (forward-line 1)
                       (not (org-pretty-table-is-empty-line)))
                     (save-excursion
                       (forward-line -1)
                       (not (org-pretty-table-is-empty-line))))
                (put-text-property (match-beginning 0) (match-end 0) 'display "├")
                t)
               ((and (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line 1)
                       (not (org-pretty-table-is-empty-line)))
                     (save-excursion
                       (forward-line -1)
                       (not (org-pretty-table-is-empty-line))))
                (put-text-property (match-beginning 0) (match-end 0) 'display "┤")
                t)
               ((and (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line -1)
                       (org-pretty-table-is-empty-line)))
                (put-text-property (match-beginning 0) (match-end 0) 'display "┐")
                t)
               ((and (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line 1)
                       (org-pretty-table-is-empty-line)))
                (put-text-property (match-beginning 0) (match-end 0) 'display "┘")
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (forward-line -1)
                       (org-pretty-table-is-empty-line)))
                (put-text-property (match-beginning 0) (match-end 0) 'display "┌")
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (forward-line 1)
                       (org-pretty-table-is-empty-line)))
                (put-text-property (match-beginning 0) (match-end 0) 'display "└")
                t)
               (t
                (put-text-property (match-beginning 0) (match-end 0) 'display "│")
                t)))
             ((equal "+" match)
              (cond
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (let ((char-pos (- (point) (line-beginning-position) 1)))
                         (forward-line -1)
                         (beginning-of-line)
                         (forward-char char-pos))
                       (eq (following-char) ?|))
                     (save-excursion
                       (backward-char 1)
                       (next-line)
                       (eq (following-char) ?|)))
                (put-text-property (match-beginning 0) (match-end 0) 'display "┼")
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (backward-char 1)
                       (previous-line)
                       (memq (following-char) '(? 10)))
                     (save-excursion
                       (let ((char-pos (- (point) (line-beginning-position) 1)))
                         (forward-line 1)
                         (beginning-of-line)
                         (forward-char char-pos))
                       (eq (following-char) ?|)))
                (put-text-property (match-beginning 0) (match-end 0) 'display "┬")
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (let ((char-pos (- (point) (line-beginning-position) 1)))
                         (forward-line -1)
                         (beginning-of-line)
                         (forward-char char-pos))
                       (eq (following-char) ?|))
                     (save-excursion
                       (backward-char 1)
                       (next-line)
                       (or (memq (following-char) '(? 10))
                           (eq (char-after (line-beginning-position)) ?#))))
                (put-text-property (match-beginning 0) (match-end 0) 'display "┴")
                t))))))))))

(defun org-pretty-table-unpropertize-region (start end)
  "Remove box-drawing compositions between START and END."
  (remove-text-properties start end '(display)))

;;; Minor mode:

;;;###autoload
(define-minor-mode org-pretty-table-mode
  "Replace org-table characters with box-drawing unicode glyphs."
  :lighter " OPT"
  (if org-pretty-table-mode
      (jit-lock-register 'org-pretty-table-propertize-region t)
    (jit-lock-unregister 'org-pretty-table-propertize-region)
    (org-pretty-table-unpropertize-region (point-min) (point-max))))

;;;###autoload
(defun turn-on-org-pretty-table-mode ()
  (org-pretty-table-mode 1))

;;;###autoload
(defun turn-off-org-pretty-table-mode ()
  (org-pretty-table-mode 0))

;;;###autoload
(define-globalized-minor-mode global-org-pretty-table-mode
  org-pretty-table-mode turn-on-org-pretty-table-mode)

(provide 'org-pretty-table)
;;; org-pretty-table.el ends here

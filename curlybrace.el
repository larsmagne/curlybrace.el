;;; curlybrace.el --- minor mode to write curlybrace languages faster
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: programming

;; cddb.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; cddb.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar curlybrace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta i)] 'curlybrace-finish-element)
    (define-key map [(meta r)] 'curlybrace-remove-braces)
    (define-key map [(meta p)] 'curlybrace-add-braces)
    map))

(defvar curlybrace-finishings
  '(("^function" "{\n{point}\n}\n")
    ("^[^)]+(.*function.*) *$" "{\n{point}\n});")))

(define-minor-mode curlybrace-mode
  "Toggle Curlybrace mode, a minor mode useful for doing curlybrace handling
\\{curlybrace-mode-map}"
  nil " Curly" curlybrace-mode-map)

(defun curlybrace-finish-element ()
  "Insert closing braces."
  (interactive)
  (let ((expansion (curlybrace-find-expansion))
	(start (point))
	end position)
    (unless expansion
      (error "No expansion for the current line"))
    (insert expansion)
    (setq end (point-marker))
    (when (search-backward "{point}" nil t)
      (goto-char (match-beginning 0))
      (replace-match "" t t)
      (setq position (point-marker)))
    (goto-char start)
    (while (< (point) end)
      (indent-for-tab-command)
      (forward-line 1))
    (goto-char position)
    (indent-for-tab-command)
    (set-marker end nil)
    (set-marker position nil)))

(defun curlybrace-find-expansion ()
  (loop for (regexp expansion) in curlybrace-finishings
	when (if (string-match "\\^" regexp)
		 (save-excursion
		   (beginning-of-line)
		   (looking-at regexp))
	       (looking-at regexp))
	return expansion))

(defun curlybrace-add-braces ()
  "Insert braces around the current statement."
  (interactive)
  (let ((position (point-marker))
	(goal-column (curlybrace-column)))
    (forward-line -1)
    (while (>= (curlybrace-column) goal-column)
      (forward-line -1))
    (end-of-line)
    (skip-chars-backward " \t")
    (delete-region (point) (line-end-position))
    (insert " {")
    (goto-char position)
    (forward-line 1)
    (while (>= (curlybrace-column) goal-column)
      (forward-line 1))
    (if (looking-at "[ \t]*else")
	(progn
	  (skip-chars-forward " \t")
	  (insert "} "))
      (forward-line -1)
      (end-of-line)
      (insert "\n}")
      (beginning-of-line))
    (indent-for-tab-command)
    (goto-char position)
    (set-marker position nil)))

(defun curlybrace-remove-braces ()
  "Remove braces from the current statement."
  (interactive)
  (let ((position (point-marker))
	(goal-column (curlybrace-column)))
    (forward-line -1)
    (while (>= (curlybrace-column) goal-column)
      (forward-line -1))
    (when (looking-at ".*\\( *{ *$\\)")
      (delete-region (match-beginning 1) (match-end 1)))
    (goto-char position)
    (forward-line 1)
    (while (>= (curlybrace-column) goal-column)
      (forward-line 1))
    (cond
     ((looking-at "[ \t]*}[ \t]*$")
      (delete-region (point) (progn (forward-line 1) (point))))
     ((looking-at "[ \t]*} *")
      (delete-region (match-beginning 0) (match-end 0))
      (indent-for-tab-command))
     (t
      (message "No end brace")))
    (goto-char position)
    (set-marker position nil)))

(defun curlybrace-column ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(provide 'curlybrace)

;;; curlybrace.el ends here

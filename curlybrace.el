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
    (define-key map [(super f)] 'curlybrace-finish-element)
    (define-key map [(super r)] 'curlybrace-remove-braces)
    (define-key map [(super a)] 'curlybrace-add-braceso)
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

(provide 'curlybrace)

;;; curlybrace.el ends here

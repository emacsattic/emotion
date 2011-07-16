;;; emotion --- Quick way to jump to specific characters
;;
;; Copyright (C) 2011 Rafael Sánchez-Aguilú
;;
;; Author: Rafael Sánchez-Aguilú <rafael@cornerdimension.com>
;; URL: https://github.com/rlph/emotion.el
;; Version: 0.5
;; Created: 2011-07-12
;; Keywords: movement convenience searching
;; License: GPL3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Original idea taken from EasyMotion plugin for Vim but still needs
;; a lot of work.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 0.5 - EasyMotion wannabe edition
;;
;;  - can now reuse keys if there are more candidates than keys
;;  - doesn't bother with error messages when quitting after starting jump
;;    sequence
;;
;; 0.3 - Initial release
;;
;;  - it works but needs testing and has some limitations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl)

(defgroup emotion nil "emotion customization group"
  :group 'convenience)

(defcustom emotion-keys "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Characters to use as target keys."
  :type '(string)
  :group 'emotion)

(defun emotion-get-visible-area (&optional start)
  (if start
      (if (> start (window-end))
	  ""
	(buffer-substring-no-properties start (window-end)))
  (buffer-substring-no-properties (window-start) (window-end))))

(defun emotion-get-matches (char)
  "Gets a list of each position of each occurrence of char."
  (let ((matches '())
	(start (window-start)))
    (while (string-match
	    (regexp-quote (char-to-string char))
	    (emotion-get-visible-area start))
      (setq matches (append matches (list (+ start (car (match-data))))))
      (setq start (+ 1 start (car (match-data)))))
    matches))

(defun emotion-place-overlays (keychain)
  (loop for key in keychain
	do (let ((o (make-overlay (cdr key) (+ 1 (cdr key))
				  (current-buffer) t)))
	     (overlay-put o 'category 'emotion)
	     (overlay-put o 'display (char-to-string (car key)))
	     (overlay-put o 'face '(:inverse-video t)))))

(defun emotion-remove-overlays ()
  (loop for o in (overlays-in 0 (buffer-end 1))
	if (eq (overlay-get o 'category) 'emotion)
	do (delete-overlay o)))

(defun emotion-make-keychain (keys matches)
  "Gives character from emotion-keys to each match position
reusing them if neccessary."
  (let ((keychain (make-ring (length keys))))
    (loop for k across keys do (ring-insert-at-beginning keychain k))
    (loop for m in matches
	  for k from 0 to (length matches)
	  collect (cons (ring-ref keychain k) m))))

(defun emotion-filter-keychain (key keychain)
  "Returns match positions with the given key."
  (loop for k in keychain
	if (equal key (car k))
	collect k))

(defun emotion-separate-keychain (keychain)
  "Splits the keys from the match positions.
Returns a list of the character positions."
  (loop for k in keychain
	collect (cdr k) into matches
	finally return matches))

;;;###autoload
(defun emotion-jump (char)
  "Main function for emotion. Bind to a key."
  (interactive "cJump to char")
  (let* ((char char)
	 (matches (emotion-get-matches char))
	 (keychain (emotion-make-keychain emotion-keys matches)))
    (if (= (length keychain) 0)
	(keyboard-quit))
    (if (= (length keychain) 1)
	(ignore-errors (goto-char (cdr (assoc target keychain))))
      (emotion-place-overlays keychain)
      (setq target (read-quoted-char))
      (setq keychain (emotion-filter-keychain target keychain))
      (while (< 1 (length keychain))
	(emotion-remove-overlays)
	(setq keychain (emotion-make-keychain
			emotion-keys
			(emotion-separate-keychain keychain)))
	(emotion-place-overlays keychain)
	(setq target (read-quoted-char))
	(setq keychain (emotion-filter-keychain target keychain)))
      (ignore-errors (goto-char (cdr (assoc target keychain))))
      (emotion-remove-overlays))))

(provide 'emotion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; easy-motion.el ends here

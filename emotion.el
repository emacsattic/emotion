;;; emotion --- Quick way to jump to specific characters
;;
;; Copyright (C) 2011 Rafael Sánchez-Aguilú
;;
;; Author: Rafael Sánchez-Aguilú
;; URL: https://github.com/rlph/emotion.el
;; Version: 0.3
;; Created: 2011-07-12
;; Keywords: movement
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

(defvar emotion-keys
  (string-to-list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun emotion-get-visible-area (&optional start)
  (if start
      (if (> start (window-end))
	  ""
	(buffer-substring-no-properties start (window-end)))
  (buffer-substring-no-properties (window-start) (window-end))))

(defun emotion-get-matches (char)
  "Gets a list for each position for the character given in the visible area."
  (let ((matches '())
	(start (window-start)))
    (while (string-match (regexp-quote (char-to-string char)) (emotion-get-visible-area start))
      (setq matches (append matches (list (+ start (car (match-data))))))
      (setq start (+ 1 start (car (match-data)))))
    matches))

(defun emotion-goto-match (key keys matches)
  "Goes to the specific character."
  (goto-char (nth (cdr (assoc key keys)) matches)))

(defun emotion-set-overlay (pos char)
  (let ((o (make-overlay pos (+ 1 pos) (current-buffer) t)))
    (overlay-put o 'display (char-to-string char))
    (overlay-put o 'face '(:inverse-video t))))

(defun emotion-place-overlays (char)
  (loop for pos in (emotion-get-matches char)
	for c in emotion-keys
	do (emotion-set-overlay pos c)))

(defun emotion-remove-overlays ()
  (loop for o in (overlays-in 0 (buffer-end 1))
	do (delete-overlay o)))

(defun emotion-collect-keys ()
  (loop for i from 0 to (- (length emotion-keys) 1)
	for k in emotion-keys
	collect (cons k i)))

(defun emotion-jump ()
  (interactive)
  (let ((char (read-event "Search for character: " t)))
    (emotion-place-overlays char)
    (emotion-goto-match (read-event "Target key: " t) (emotion-collect-keys) (emotion-get-matches char))
    (emotion-remove-overlays)))

(provide 'emotion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; easy-motion.el ends here

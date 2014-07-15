;;; docco-mode.el --- A quick-and-dirty documentation tool for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  David Vázquez

;; Author: David Vázquez <davazp@gmail.com>
;; Keywords: docs

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

;;

;;; Code:

(require 'cl-lib)

(defvar docco-source-buffer nil
  "In the comments buffer, it is bound to the corresponding
source buffer.")
(make-variable-buffer-local 'docco-source-buffer)

(defvar docco-comments-buffer nil
  "Buffer where the user can see the comments.")
(make-variable-buffer-local 'docco-comments-buffer)

;;; Window configuration before docco is invoked. It will be restored
;;; when we kill the comments buffer.
(defvar docco-window-configuration nil)
(make-variable-buffer-local 'docco-window-configuration)


(defmacro with-docco-comments-buffer (&rest body)
  "Execute BODY with docco-comments-buffer as the current buffer."
  (declare (indent 0))
  `(with-current-buffer docco-comments-buffer
     ,@body))

(defun docco--create-comments-buffer ()
  (let ((source-buffer (current-buffer))
        (name (concat "*" (buffer-name) "-comments*")))
    (setq docco-comments-buffer (get-buffer-create name))
    (with-docco-comments-buffer
      (erase-buffer)
      (setq docco-source-buffer source-buffer)
      (add-hook 'kill-buffer-hook 'docco--cleanup nil t))))


(defvar docco-comment-end-with-newline t
  "It is true if the closing comment delimiter is a newline.")


;; Insert the comments of chunk starting at the point in the comments
;; buffer and move the pointer to the beginning of the next chunk of
;; comments in the buffer or at the end of the buffer if there is no
;; more comments. It will return the position of the source code part
;; of the chunk.
(defun docco--insert-chunk ()
  (let ((buffer (current-buffer))
        ;;
        ;; outer start       outer end
        ;;   |                    |
        ;;   /*  Sample comment  */
        ;;     |                |
        ;;    start            end
        ;;
        outer-start start end outer-end)

    (setq end (point)
          outer-end (point))

    ;; Comments
    (while (save-excursion (comment-forward))
      ;; Find the beginning of our comment.
      ;;
      ;; Note that `comment-seach-forward' will leave the pointer in
      ;; the beginning of the comment body. However, it does not
      ;; consider as body (at least in many major modes) the
      ;; whitespace characters, which could be important for us in
      ;; some cases... by now, we just ignore them too.
      (setq outer-start (comment-search-forward (point-max) t))
      (setq start (point))

      (with-docco-comments-buffer
        (insert-buffer-substring-no-properties buffer outer-end outer-start))

      ;; Find the ending of the current commend.
      (goto-char outer-start)
      (comment-forward)
      (save-excursion
        (comment-enter-backward)
        (setq end (point)))
      (setq outer-end (point))

      ;; Insert the content of the current comment
      (with-docco-comments-buffer
        (insert-buffer-substring-no-properties buffer start end)
        (when docco-comment-end-with-newline
          (insert-char ?\n))))

    ;; Skip source code
    (prog1 (point)
      (let ((next-comment (comment-search-forward (point-max) t)))
        (goto-char (or next-comment (point-max)))))))


;; Process all the comments/code chunks in the current buffer.
(defun docco--process-buffer ()
  (goto-char (point-min))
  (let ((padding 0)
        chunk-start
        chunk-divider
        chunk-end)

    (while (not (eobp))
      (setq chunk-start (point))
      (setq chunk-divider (docco--insert-chunk))
      (setq chunk-end (point))

      ;; Hide the comments in the source buffer, setting up to
      ;; display the required number of newlines instead.
      (let ((replacement (make-string (max 0 (- padding)) ?\n)))
        (put-text-property chunk-start chunk-divider 'display replacement))


      (let ((code-lines (count-lines chunk-divider chunk-end))
            (comment-lines (count-lines chunk-start chunk-divider)))

        ;; The difference between the number of lines in the source code
        ;; and the comments. We try to make both chunks to have the same
        ;; number of lines in the display, so the number of lines match.
        (setq padding (- code-lines comment-lines))

        ;; if we need to pad the code, comments would be
        ;; contiguous. Let's add the newlines
        (if (< padding 0)
            (let ((n (save-excursion (abs (skip-chars-backward "[:space:]\n")))))
              (cl-decf padding n)
              (with-docco-comments-buffer
                (insert
                 (propertize (make-string code-lines ?\n)
                             'display (make-string n ?\n)))))

          (with-docco-comments-buffer
            (insert
             (propertize (make-string code-lines ?\n)
                         'display (make-string padding ?\n)))))))))


(defun docco--prepare-windows ()
  ;; Disable line truncating.
  ;; TODO: Restore configuration in `docco--cleanup'.
  (setq truncate-partial-width-windows nil)
  (with-docco-comments-buffer
    (setq truncate-partial-width-windows nil))
  (setq docco-window-configuration (current-window-configuration))
  (set-window-buffer (split-window-horizontally) docco-comments-buffer))


(defun docco--cleanup ()
  (when docco-source-buffer
    (with-current-buffer docco-source-buffer
      ;; TODO: We make all the text visible, but it could not to
      ;; revert the buffer properly if there was something invisible
      ;; before.
      (put-text-property (point-min) (point-max) 'display nil)
      (set-window-configuration docco-window-configuration))))


(defun docco ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (docco--create-comments-buffer)
    (docco--process-buffer)
    (docco--prepare-windows)))


(provide 'docco-mode)
;;; docco-mode.el ends here

;;; p@ck.el --- Package configuration language -*- lexical-binding: t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Homepage: https://github.com/wi11dey/p-ck
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Generate README:
;;; Commentary:

;; Ah p@ck.

;; commentary
;; Reader constructs

;;; Code:

(require 'macroexp)

;; TODO Timing functions (that don't need p@ck during runtime, only to view the % report)
;; TODO better detection of what is a function for @functions

(defgroup p@ck nil
  ""
  :group 'startup)

(defcustom p@ck-enable-timing t
  ""
  :type 'boolean)

(defun p@ck--get-body (name autoloads-ptr body)
  (let ((next (cadr body)))
    (unless (consp next)
      (user-error "Expected ( after %s" (car body)))
    (setf (cdr body) (cddr body))
    (if (listp (car next))
	(p@ck--process-body name autoloads-ptr next)
      (p@ck--process-body name autoloads-ptr (cdr next))
      (list next))))

(defun p@ck--add-autoload (autoloads-ptr func)
  (unless (memq func (car autoloads-ptr))
    (push func (car autoloads-ptr))))

;;;; @ symbols
(defun p@ck--autoload-symbol (autoloads-ptr symbol)
  (when (and (symbolp symbol)
	     (setq symbol (symbol-name symbol))
	     (> (length symbol) 1)
	     (eq (aref symbol 0) ?@))
    (setq symbol (intern (substring symbol 1)))
    (p@ck--add-autoload autoloads-ptr symbol)
    symbol))

;; ~ eval-when-compile
;; ! eval-and-compile
;; _ eval-after-load
;; ^ require
;; @' autoload quote
;; $ [package-name]
(defun p@ck--process-body (name autoloads-ptr body)
  (let ((current body))
    (while (consp current)
      (pcase (car current)
	;;;; ~
	('~
	 ;; `eval-when-compile'
	 (let ((prog (macroexp-progn (p@ck--get-body name autoloads-ptr current))))
	   (setf (car current) `',(if (bound-and-true-p byte-compile-current-buffer)
				      (byte-compile-eval prog)
				    (eval prog lexical-binding)))))
	;;;; !
	('!
	 ;; `eval-and-compile'
	 (let ((prog (macroexp-progn (p@ck--get-body name autoloads-ptr current))))
	   (eval prog lexical-binding)
	   (setf (car current) prog)))
	;;;; _
	('_
	 ;; `eval-after-load'
	 (setf (car current) `(with-eval-after-load ',name
				,@(p@ck--get-body name autoloads-ptr current))))
	;;;; @ quote
	('@
	 (let ((next (cadr current)))
	   (unless (eq (car-safe next) 'quote)
	     (user-error "Expected ' after @"))
	   (p@ck--add-autoload autoloads-ptr (cadr next))
	   (setf (car next) 'function
		 (car current) next
		 (cdr current) (cddr current))))
	;;;; ^
	('^
	 (setf (car current) `(require ',name)))
	('~^
	 (if (bound-and-true-p byte-compile-current-buffer)
	     (byte-compile-eval `(require ',name))
	   (require name))
	 (setf (car current) (cadr current)
	       (cdr current) (cddr current)
	       ;; Dummy cons so the next element (now `current') doesn't get skipped over.
	       current (cons nil current)))
	('!^
	 (require name)
	 (setf (car current) `(require ',name)))
	;;;; @ functions
	((pred consp)
	 (unless (memq (caar current) '(quote function p@ck))
	   (let ((symbol (p@ck--autoload-symbol autoloads-ptr (caar current))))
	     (when symbol
	       (setf (caar current) symbol)))
	   (setf (cdar current) (p@ck--process-body name autoloads-ptr (cdar current))))))
      (setq current (cdr current))))
  body)

(defun p@ck--replace (name-string body &optional half)
  (cond (half
	 (let ((value (funcall half body)))
	   (cond ((consp value)
		  (p@ck--replace name-string value))
		 ((symbolp value)
		  (let* ((symbol (symbol-name value))
			 (replacement (save-match-data
					(when (string-match "\\(?:\\`\\|[^[:alnum:]]\\)\\(?1:\\$\\)\\(?:[^[:alnum:]]\\|\\'\\)" symbol)
					  (intern (replace-match name-string
								 :fixedcase
								 :literal
								 symbol
								 1))))))
		    (when replacement
		      (funcall (if (eq half 'car)
				   #'setcar
				 #'setcdr)
			       body
			       replacement)))))))
	((eq (car body) 'p@ck)
	 (p@ck--replace name-string (cdr body) #'car))
	(t
	 (p@ck--replace name-string body #'car)
	 (p@ck--replace name-string body #'cdr))))

(defun p@ck--top-level-autoloads (autoloads-ptr body)
  (while body
    (if (p@ck--autoload-symbol autoloads-ptr (car body))
	(setf (car body) (cadr body)
	      (cdr body) (cddr body))
      (setq body (cdr body)))))

(defvar p@ck-timing nil
  "")

;;;###autoload
(defun p@ck-report ()
  "from `p@ck-timing'"
  (interactive)
  (if (bound-and-true-p p@ck-timing)
      (progn
	)
    (message "p@ck-report: No timing information available")))

;;;###autoload
(defmacro p@ck (name &rest body)
  ""
  (declare (indent 1))
  (let ((name-string (symbol-name name))
	(autoloads-ptr (list nil)))
    (condition-case err
	(progn
          (p@ck--replace name-string body)
	  (p@ck--top-level-autoloads autoloads-ptr body)
	  (p@ck--process-body name autoloads-ptr body)
	  (dolist (func (car autoloads-ptr))
	    ;; Effectively `eval-and-compile'.
	    (autoload func name-string)
	    (push `(declare-function ,func ,name-string) body)
	    (push `(autoload
		     ',func
		     ,name-string
		     ,@(when (fboundp func)
			 (list (documentation func :raw)
			       (commandp func :for-call-interactively)
			       (pcase func
				 ((pred keymapp) 'keymap)
				 ((pred macrop)  'macro)))))
		  body))
	  (when p@ck-enable-timing
	    (setq p@ck-timing (assq-delete-all name p@ck-timing)))
	  `(condition-case-unless-debug err
	       (progn
		 ;; `add-hook'
		 ,@(when p@ck-enable-timing
		     `((add-hook 'p@ck-timing (list ',name :start (current-time)))))
		 ,@body
		 ,@(when p@ck-enable-timing
		     `((add-hook 'p@ck-timing (list ',name :end (current-time))))))
	     (error (display-warning 'p@ck
				     (format "%s: %s"
					     ,name-string
					     (error-message-string err))
				     :error))))
      (error (display-warning 'p@ck
			      (format "%s compilation: %s"
				      name-string
				      (error-message-string err))
			      :error)))))

(provide 'p@ck)

;;; p@ck.el ends here

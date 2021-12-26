;;; ncla.el --- Non Counsel Linux App (Launcher) -*- lexical-binding: t -*-

;; Author: Linas Vidziunas <linasvidz@gmail.com>
;; Maintainer: Linas Vidziunas <linasvidz@gmail.com>
;; Created: 2021
;; Version: 0.10
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/LinasVidziunas/NCLA

;;; Code:

;; change the path to desktop file directory if necessary
;; should later consider using xdg
(defvar ncla-desktop-file-paths
  (directory-files "/usr/share/applications/" t (regexp-quote ".desktop"))
  "Contains all *.desktop files")

(defvar ncla-include-terminal-applications nil
  "If non-nil value, Terminal=true applications will be included in the list")

(defvar ncla--cached-applications nil
  "Cached list of applications. Used by ncla--get-applictions")

(defvar ncla--cached-timestamp nil
  "Time at when the last cache happened. Used by ncla--get-applications")

(defvar ncla-cache-timeout 1
  "Time in seconds for the cache timeout")


;;;###autoload
(defun ncla ()
  "Open NCLA in an interactive minibuffer"
  (interactive)
  (setq applications (ncla--get-applications ncla-desktop-file-paths))
  
  (let (name)
    (setq name
	  (let ((completion-extra-properties '(:annotation-function ncla--annotation-function)))
	    (completing-read "Prompt: " applications)))
    (start-process-shell-command name nil (ncla--get-exec-by-app-name name))))


(defun ncla--get-comment-by-app-name (application-name)
  "Returns the comment of an application from cached applications"
  (car (cdr (cdr (assoc application-name ncla--cached-applications)))))


(defun ncla--get-exec-by-app-name (application-name)
  "Returns the exec command of an application from cached applications"
  (ncla--process-exec-cmd (car (cdr (assoc application-name ncla--cached-applications)))))


(defun ncla--process-exec-cmd (preprocessed-cmd)
  "Processes cmd and returns it"

  ;; split string by white space
  (setq preprocessed-cmd
	(split-string preprocessed-cmd " " t))
  
  (let (cmd temp-cmd)
    (dolist (element preprocessed-cmd)

      ;; remove elements that start with the % symbol
      (if (not (string-match-p "%" (substring element 0 1)))
	  (if temp-cmd
	      (setq temp-cmd (cons temp-cmd element))
	    (setq temp-cmd element))))

    ;; Back at it again with some black magic
    (setq cmd temp-cmd)))


(defun ncla--get-applications (desktop-file-paths)
  "Returns list with a list of application name and command"
      (let (applications)
	(if (or (not ncla--cached-applications)
		 (and ncla--cached-timestamp
		      (time-less-p (time-add ncla--cached-timestamp ncla-cache-timeout) (current-time))))
	    (progn
	      (dolist (file-path desktop-file-paths)
		(let (name exec comment terminal)
		  (catch 'done
		    (dolist (line (split-string 
				   (with-temp-buffer
				     (insert-file-contents file-path)
				     (buffer-substring-no-properties
				      (point-min)
				      (point-max))) "\n" t))

		      (when (>= (length line) 5)

			(when (and (string-match-p "Name=" (substring line 0 5))
				   (not name))
			  (setq name (substring line 5 (length line))))
			
			(when (and (string-match-p "Exec=" (substring line 0 5))
				   (not exec))
			  (setq exec (substring line 5 (length line))))
			
			(when (>= (length line) 9)
			  (when (and (string-match-p "Terminal=" (substring line 0 9))
				     (not terminal))
			    (setq terminal (substring line 9 (length line))))
			  
			  (when (and (string-match-p "Comment=" (substring line 0 8))
				     (not comment))
			    (setq comment (substring line 8 (length line))))))))
		  
		  ;; Don't include applications with "Terminal=true",
		  ;; except when ncla-include-terminal-applications is set to a non-nil value
		  (when (or (or (not terminal)
				(string-match-p terminal "false"))
			    ncla-include-terminal-applications)
		    ;; has to have name and execuatble command
		    (when (and name exec)
		      (setq applications
			    (cons (list name exec comment) applications))))))
	      (setq ncla--cached-timestamp (current-time)))

	  (progn
	  (setq applications ncla--cached-applications)))

	(setq ncla--cached-applications applications)))


(defun ncla--annotation-function (cand-name)
  "Default function to annotate the completion choices."
  (let (comment)
    (setq comment (ncla--get-comment-by-app-name cand-name))
    (when comment
      (concat
       (propertize " " 'display '(space :align-to center))
       (propertize comment 'face '(:foreground "grey"))))))


(provide 'ncla)
;;; ncla.el ends here

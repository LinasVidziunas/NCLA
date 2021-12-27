;;; ncla.el --- Non Counsel Linux App (Launcher) -*- lexical-binding: t -*-

;; Author: Linas Vidziunas <linasvidz@gmail.com>
;; Maintainer: Linas Vidziunas <linasvidz@gmail.com>
;; Created: 2021
;; Version: 0.10
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/LinasVidziunas/NCLA

;;; Code:

;; change the path to desktop file directory if necessary
;; should consider using xdg as https://github.com/LinasVidziunas/app-launcher/blob/main/app-launcher.el
(defvar ncla-desktop-file-paths
  (directory-files "/usr/share/applications/" t (regexp-quote ".desktop"))
  "Contains all *.desktop files")

(defvar ncla-include-terminal-applications nil
  "If non-nil value, Terminal=true applications will be included in the list")

(defvar ncla--cached-applications nil
  "Cached list of applications. Used by ncla--get-applictions")

(defvar ncla--cached-timestamp nil
  "Time at when the last cache happened. Used by ncla--get-applications")

(defvar ncla-cache-timeout 5
  "Time in seconds for the cache timeout")


;;;###autoload
(defun ncla ()
  "Open NCLA in an interactive minibuffer"
  (interactive)
  (setq applications (ncla--get-applications ncla-desktop-file-paths))
  
  (let (name)
    (setq name
	  (let ((completion-extra-properties '(:annotation-function ncla--annotation-function)))
	    (completing-read "Start application: " applications)))
    (start-process-shell-command name nil (ncla--get-exec-by-app-name name))))


(defun ncla--get-comment-by-app-name (application-name)
  "Returns the comment of an application by appplication name from the cached application list"
  (cdr (assoc "comment" (assoc application-name ncla--cached-applications))))


(defun ncla--get-exec-by-app-name (application-name)
  "Returns the exec command of an application by application name from the cached application list"
  (ncla--process-exec-cmd (cdr (assoc "exec" (assoc application-name ncla--cached-applications)))))


(defun ncla--get-genericname-by-app-name (application-name)
  "Returns the GenericName of an application by application name from the cached application list"
  (let (genericname)
    (setq genericname (assoc "genericname" (assoc application-name ncla--cached-applications)))
    (when (cdr genericname)
      ;; convert list to string
      (format "%s" (cdr (assoc "genericname" (assoc application-name ncla--cached-applications)))))))


(defun ncla--process-exec-cmd (preprocessed-cmd)
  "Processes cmd and returns it"

  ;; split string by white space
  (setq preprocessed-cmd
	(split-string preprocessed-cmd " " t))
  
  (let (cmd)
    (dolist (element preprocessed-cmd cmd)
      ;; remove elements that start with the % symbol
      (if (not (string-match-p "%" (substring element 0 1)))
	  (if cmd
	      (setq cmd (cons cmd element))
	    (setq cmd element))))))


(defun ncla--parse-desktop-file (file-path)
  (let (name exec comment terminal genericname)
    (catch 'done
      (dolist (line (split-string 
		     (with-temp-buffer
		       (insert-file-contents file-path)
		       (buffer-substring-no-properties
			(point-min)
			(point-max))) "\n" t))

	(when (and (string-match-p "^Name=" line)
		   (not name))
	  (setq name (substring line 5 (length line))))

	(when (and (string-match-p "^Exec=" line)
		   (not exec))
	  (setq exec (substring line 5 (length line))))
	
	(when (and (string-match-p "^Terminal=" line)
		   (not terminal))
	  (setq terminal (substring line 9 (length line))))

	(when (and (string-match-p "^Comment=" line)
		   (not comment))
	  (setq comment (substring line 8 (length line))))
	
	(when (and (string-match-p "GenericName=" line)
		   (not genericname))
	  (setq genericname (substring line 12 (length line))))

	(when (and (and (and (and name exec) comment) terminal) genericname)
	  (throw 'done 'done))))

    ;; Don't include applications with "Terminal=true",
    ;; except when ncla-include-terminal-applications is set to a non-nil value
    (when (or (or (not terminal)
		  (string-match-p terminal "false"))
	      ncla-include-terminal-applications)
      ;; needs to have name and execuatble command
      (when (and name exec)
	(list name
	      (cons "exec" exec)
	      (cons "comment" comment)
	      (cons "genericname" genericname))))))


(defun ncla--get-applications (desktop-file-paths)
  "Returns list with a list of application name and command"
  (let (applications)
    (if (or (not ncla--cached-applications)
	 (and ncla--cached-timestamp
	      (time-less-p (time-add ncla--cached-timestamp ncla-cache-timeout) (current-time))))
	(progn
	  (dolist (file-path desktop-file-paths)
	    (let (application)
	      (setq application (ncla--parse-desktop-file file-path))
	      (when application
		(setq applications (cons (ncla--parse-desktop-file file-path) applications)))))
	  (setq ncla--cached-timestamp (current-time)))
      (setq applications ncla--cached-applications))
    (setq ncla--cached-applications applications)))


(defun ncla--annotation-function (cand-name)
  "Default function to annotate the completion choices."
  (let (comment genericname return-string)
    (setq comment (ncla--get-comment-by-app-name cand-name))
    (setq genericname (ncla--get-genericname-by-app-name cand-name))
    (when comment
      (setq return-string
	    (concat
	     return-string
	     ;; hardcoded values should be replaced
	     ;; should probably learn elisp :)
	     (propertize " " 'display '(space :align-to (+ left 40)))
	     (propertize comment 'face '(:foreground "grey")))))
    (when genericname
      (setq return-string
	    (concat
	     return-string
	     ;; hardcoded values should be replaced
	     (propertize " " 'display '(space :align-to (- right-fringe 20)))
	     (propertize genericname 'face'(:foreground "red")))))))


(provide 'ncla)
;;; ncla.el ends here

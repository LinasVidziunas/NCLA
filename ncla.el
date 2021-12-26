;; change the path to desktop file directory if necessary
;; should later consider using xdg
(defvar ncla-desktop-file-paths
  (directory-files "/usr/share/applications/" t (regexp-quote ".desktop")))

(defvar ncla-include-terminal-applications nil
  "If non-nil value, Terminal=true applications will be included in the list")

(defun ncla ()
    "Open NCLA in an interactive minibuffer"
  (interactive)
  (let (preprocessed-cmd cmd name desktop-files comment)
    (setq desktop-files (ncla--get-applications ncla-desktop-file-paths))

    (setq preprocessed-cmd
	  ;; Associate selected element in completion list with 
	  (assoc (completing-read "Start application: "
				  desktop-files nil t)
		 desktop-files))

    (setq name (car preprocessed-cmd))

    ;; fully processed command, ready to be run
    (setq cmd (ncla--process-exec-cmd (car (cdr preprocessed-cmd))))

    ;; comment
    ;; (print (cdr (cdr preprocessed-cmd)))

    (setq comment (cdr (cdr preprocessed-cmd)))

    (start-process-shell-command name nil cmd)))


(defun ncla--get-comment-by-app-name-from-app-list
    (application-name application-list)
  "Returns the comment of an application from the application list"
  (cdr (cdr (assoc application-name application-list))))


(defun ncla--get-exec-cmd-by-app-name-from-app-list
    (application-name applicatoin-list)
  "Returns the exec command of an application from the application list"
  (cdr (cdr (assoc application-name application-list))))


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
		  (setq comment (substring line 8 (length line)))))
	      
	      ;; (when comment terminal name exec
	      ;; 	    (throw 'done t))))
	      ))
	
	;; Don't include applications with "Terminal=true",
	;; except when ncla-include-terminal-applications is set to a non-nil value
	(when (or (or (not terminal) (string-match-p terminal "false")) ncla-include-terminal-applications)
	  (when (and name exec)
	    (setq applications
	     (cons (list name exec comment) applications)))))))
    
    ;; Black magic to return applications
    (push (pop applications) applications)))

(provide 'ncla)

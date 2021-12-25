;; change the path to desktop file directory if necessary
(defvar ncla-desktop-file-paths
  (directory-files "/usr/share/applications/" t (regexp-quote ".desktop")))

(defun ncla()
    "Open NCLA in an interactive minibuffer"
  (interactive)
  (let (preprocessed-cmd cmd name desktop-files)
    (setq desktop-files (ncla--get-applications ncla-desktop-file-paths))

    (setq preprocessed-cmd (assoc (completing-read "Start application: "
						   desktop-files nil t)
				  desktop-files))
    (setq name (car preprocessed-cmd))


    (setq preprocessed-cmd (split-string (car (cdr preprocessed-cmd)) " " t))

    (dolist (element preprocessed-cmd)
      (if (not (string-match-p "%" element))
	  (setq cmd (cons cmd element)))) 
    (start-process-shell-command name nil (cdr cmd))))


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
	
	(when (or (not terminal) (string-match-p terminal "false"))
	  (when (and name exec)
	    (setq applications
	     (cons (list name exec comment) applications)))))))
    
    ;; Black magic to return applications
    (push (pop applications) applications)))

(provide 'ncla)

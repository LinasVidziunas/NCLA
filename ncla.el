;; change the path to desktop file directory if necessary
(defvar ncla-desktop-file-paths (directory-files "/usr/share/applications/" t (regexp-quote ".desktop")))

(defun ncla()
    "Open NCLA in an interactive minibuffer"
  (interactive)
  (let (preprocessed-cmd cmd name desktop-files)
    (setq desktop-files (ncla--get-applications ncla-desktop-file-paths))

    (setq preprocessed-cmd (assoc (completing-read "Start application: "
						   desktop-files nil t)
				  desktop-files))

    (setq name (car preprocessed-cmd))

    (setq preprocessed-cmd (split-string (cdr preprocessed-cmd) " " t))

    (dolist (element preprocessed-cmd)
      (if (not (string-match-p "%" element))
	  (setq cmd (cons cmd element)))) 
    (start-process-shell-command name nil (cdr cmd))))

(defun ncla--get-applications (desktop-file-paths)
  "Returns list with a list of application name and command"
  (let (applications)
    (dolist (file-path desktop-file-paths)
      (let (current-application nameb execb)
	(catch 'done
	  (dolist (line (split-string 
			 (with-temp-buffer
			   (insert-file-contents file-path)
			   (buffer-substring-no-properties
			    (point-min)
			    (point-max))) "\n" t))

	    (when (and (string-match-p "Name=" line)
		       (string-match-p "Name=" (substring line 0 5)))
	      (setq current-application (substring line 5 (length line)))
	      (setq nameb 't))
	    (when (and (string-match-p "Exec=" line)
		       (string-match-p "Exec=" (substring line 0 5)))
	      (setq current-application
		    (cons current-application (substring line 5 (length line)) ))
	      (setq execb 't))
	    (when (and nameb execb)
	      (throw 'done current-application)))
	  )
	(setq applications (cons current-application applications))))

    ;; Black magic to return applications
    (push (pop applications) applications)))

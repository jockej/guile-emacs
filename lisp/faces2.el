;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Listing faces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst list-faces-sample-text
  "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Text string to display as the sample text for `list-faces-display'.")


;; The name list-faces would be more consistent, but let's avoid a
;; conflict with Lucid, which uses that name differently.

(defvar help-xref-stack)
(defun list-faces-display (&optional regexp)
  "List all faces, using the same sample text in each.
The sample text is a string that comes from the variable
`list-faces-sample-text'.

If REGEXP is non-nil, list only those faces with names matching
this regular expression.  When called interactively with a prefix
arg, prompt for a regular expression."
  (interactive (list (and current-prefix-arg
                          (read-regexp "List faces matching regexp"))))
  (let ((all-faces (zerop (length regexp)))
	(frame (selected-frame))
	(max-length 0)
	faces line-format
	disp-frame window face-name)
    ;; We filter and take the max length in one pass
    (setq faces
	  (delq nil
		(mapcar (lambda (f)
			  (let ((s (symbol-name f)))
			    (when (or all-faces (string-match-p regexp s))
			      (setq max-length (max (length s) max-length))
			      f)))
			(sort (face-list) #'string-lessp))))
    (unless faces
      (error "No faces matching \"%s\"" regexp))
    (setq max-length (1+ max-length)
	  line-format (format "%%-%ds" max-length))
    (with-help-window "*Faces*"
      (with-current-buffer standard-output
	(setq truncate-lines t)
	(insert
	 (substitute-command-keys
	  (concat
	   "\\<help-mode-map>Use "
	   (if (display-mouse-p) "\\[help-follow-mouse] or ")
	   "\\[help-follow] on a face name to customize it\n"
	   "or on its sample text for a description of the face.\n\n")))
	(setq help-xref-stack nil)
	(dolist (face faces)
	  (setq face-name (symbol-name face))
	  (insert (format line-format face-name))
	  ;; Hyperlink to a customization buffer for the face.  Using
	  ;; the help xref mechanism may not be the best way.
	  (save-excursion
	    (save-match-data
	      (search-backward face-name)
	      (setq help-xref-stack-item `(list-faces-display ,regexp))
	      (help-xref-button 0 'help-customize-face face)))
	  (let ((beg (point))
		(line-beg (line-beginning-position)))
	    (insert list-faces-sample-text)
	    ;; Hyperlink to a help buffer for the face.
	    (save-excursion
	      (save-match-data
		(search-backward list-faces-sample-text)
		(help-xref-button 0 'help-face face)))
	    (insert "\n")
	    (put-text-property beg (1- (point)) 'face face)
	    ;; Make all face commands default to the proper face
	    ;; anywhere in the line.
	    (put-text-property line-beg (1- (point)) 'read-face-name face)
	    ;; If the sample text has multiple lines, line up all of them.
	    (goto-char beg)
	    (forward-line 1)
	    (while (not (eobp))
	      (insert-char ?\s max-length)
	      (forward-line 1))))
	(goto-char (point-min))))
    ;; If the *Faces* buffer appears in a different frame,
    ;; copy all the face definitions from FRAME,
    ;; so that the display will reflect the frame that was selected.
    (setq window (get-buffer-window (get-buffer "*Faces*") t))
    (setq disp-frame (if window (window-frame window)
		       (car (frame-list))))
    (or (eq frame disp-frame)
	(dolist (face (face-list))
	  (copy-face face face frame disp-frame)))))


(defun describe-face (face &optional frame)
  "Display the properties of face FACE on FRAME.
Interactively, FACE defaults to the faces of the character after point
and FRAME defaults to the selected frame.

If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (interactive (list (read-face-name "Describe face"
                                     (or (face-at-point t) 'default)
                                     t)))
  (let* ((attrs '((:family . "Family")
		  (:foundry . "Foundry")
		  (:width . "Width")
		  (:height . "Height")
		  (:weight . "Weight")
		  (:slant . "Slant")
		  (:foreground . "Foreground")
                  (:distant-foreground . "DistantForeground")
		  (:background . "Background")
		  (:underline . "Underline")
		  (:overline . "Overline")
		  (:strike-through . "Strike-through")
		  (:box . "Box")
		  (:inverse-video . "Inverse")
		  (:stipple . "Stipple")
		  (:font . "Font")
		  (:fontset . "Fontset")
		  (:inherit . "Inherit")))
	(max-width (apply #'max (mapcar #'(lambda (x) (length (cdr x)))
					attrs))))
    (help-setup-xref (list #'describe-face face)
		     (called-interactively-p 'interactive))
    (unless face
      (setq face 'default))
    (if (not (listp face))
	(setq face (list face)))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
	(dolist (f face)
	  (if (stringp f) (setq f (intern f)))
	  ;; We may get called for anonymous faces (i.e., faces
	  ;; expressed using prop-value plists).  Those can't be
	  ;; usefully customized, so ignore them.
	  (when (symbolp f)
	    (insert "Face: " (symbol-name f))
	    (if (not (facep f))
		(insert "   undefined face.\n")
	      (let ((customize-label "customize this face")
		    file-name)
		(insert (concat " (" (propertize "sample" 'font-lock-face f) ")"))
		(princ (concat " (" customize-label ")\n"))
		;; FIXME not sure how much of this belongs here, and
		;; how much in `face-documentation'.  The latter is
		;; not used much, but needs to return nil for
		;; undocumented faces.
		(let ((alias (get f 'face-alias))
		      (face f)
		      obsolete)
		  (when alias
		    (setq face alias)
		    (insert
		     (format "\n  %s is an alias for the face `%s'.\n%s"
			     f alias
			     (if (setq obsolete (get f 'obsolete-face))
				 (format "  This face is obsolete%s; use `%s' instead.\n"
					 (if (stringp obsolete)
					     (format " since %s" obsolete)
					   "")
					 alias)
			       ""))))
		  (insert "\nDocumentation:\n"
			  (or (face-documentation face)
			      "Not documented as a face.")
			  "\n\n"))
		(with-current-buffer standard-output
		  (save-excursion
		    (re-search-backward
		     (concat "\\(" customize-label "\\)") nil t)
		    (help-xref-button 1 'help-customize-face f)))
		(setq file-name (find-lisp-object-file-name f 'defface))
		(when file-name
		  (princ "Defined in `")
		  (princ (file-name-nondirectory file-name))
		  (princ "'")
		  ;; Make a hyperlink to the library.
		  (save-excursion
		    (re-search-backward "`\\([^`']+\\)'" nil t)
		    (help-xref-button 1 'help-face-def f file-name))
		  (princ ".")
		  (terpri)
		  (terpri))
		(dolist (a attrs)
		  (let ((attr (face-attribute f (car a) frame)))
		    (insert (make-string (- max-width (length (cdr a))) ?\s)
			    (cdr a) ": " (format "%s" attr))
		    (if (and (eq (car a) :inherit)
			     (not (eq attr 'unspecified)))
			;; Make a hyperlink to the parent face.
			(save-excursion
			  (re-search-backward ": \\([^:]+\\)" nil t)
			  (help-xref-button 1 'help-face attr)))
		    (insert "\n")))))
	    (terpri)))))))

(defvar called-interactively-p-functions nil
  "Special hook called to skip special frames in `called-interactively-p'.
The functions are called with 3 arguments: (I FRAME1 FRAME2),
where FRAME1 is a \"current frame\", FRAME2 is the next frame,
I is the index of the frame after FRAME2.  It should return nil
if those frames don't seem special and otherwise, it should return
the number of frames to skip (minus 1).")

(defconst internal--funcall-interactively
  (symbol-function 'funcall-interactively))

(defun called-interactively-p (&optional kind)
  nil)

;; (defun called-interactively-p (&optional kind)
;;   "Return t if the containing function was called by `call-interactively'.
;; If KIND is `interactive', then only return t if the call was made
;; interactively by the user, i.e. not in `noninteractive' mode nor
;; when `executing-kbd-macro'.
;; If KIND is `any', on the other hand, it will return t for any kind of
;; interactive call, including being called as the binding of a key or
;; from a keyboard macro, even in `noninteractive' mode.

;; This function is very brittle, it may fail to return the intended result when
;; the code is debugged, advised, or instrumented in some form.  Some macros and
;; special forms (such as `condition-case') may also sometimes wrap their bodies
;; in a `lambda', so any call to `called-interactively-p' from those bodies will
;; indicate whether that lambda (rather than the surrounding function) was called
;; interactively.

;; Instead of using this function, it is cleaner and more reliable to give your
;; function an extra optional argument whose `interactive' spec specifies
;; non-nil unconditionally (\"p\" is a good way to do this), or via
;; \(not (or executing-kbd-macro noninteractive)).

;; The only known proper use of `interactive' for KIND is in deciding
;; whether to display a helpful message, or how to display it.  If you're
;; thinking of using it for any other purpose, it is quite likely that
;; you're making a mistake.  Think: what do you want to do when the
;; command is called from a keyboard macro?"
;;   (declare (advertised-calling-convention (kind) "23.1"))
;;   (when (not (and (eq kind 'interactive)
;;                   (or executing-kbd-macro noninteractive)))
;;     (let* ((i 1) ;; 0 is the called-interactively-p frame.
;;            frame nextframe
;;            (get-next-frame
;;             (lambda ()
;;               (setq frame nextframe)
;;               (setq nextframe (backtrace-frame i 'called-interactively-p))
;;               ;; (message "Frame %d = %S" i nextframe)
;;               (setq i (1+ i)))))
;;       (funcall get-next-frame) ;; Get the first frame.
;;       (while
;;           ;; FIXME: The edebug and advice handling should be made modular and
;;           ;; provided directly by edebug.el and nadvice.el.
;;           (progn
;;             ;; frame    =(backtrace-frame i-2)
;;             ;; nextframe=(backtrace-frame i-1)
;;             (funcall get-next-frame)
;;             ;; `pcase' would be a fairly good fit here, but it sometimes moves
;;             ;; branches within local functions, which then messes up the
;;             ;; `backtrace-frame' data we get,
;;             (or
;;              ;; Skip special forms (from non-compiled code).
;;              (and frame (null (car frame)))
;;              ;; Skip also `interactive-p' (because we don't want to know if
;;              ;; interactive-p was called interactively but if it's caller was)
;;              ;; and `byte-code' (idem; this appears in subexpressions of things
;;              ;; like condition-case, which are wrapped in a separate bytecode
;;              ;; chunk).
;;              ;; FIXME: For lexical-binding code, this is much worse,
;;              ;; because the frames look like "byte-code -> funcall -> #[...]",
;;              ;; which is not a reliable signature.
;;              (memq (nth 1 frame) '(interactive-p 'byte-code))
;;              ;; Skip package-specific stack-frames.
;;              (let ((skip (run-hook-with-args-until-success
;;                           'called-interactively-p-functions
;;                           i frame nextframe)))
;;                (pcase skip
;;                  (`nil nil)
;;                  (`0 t)
;;                  (_ (setq i (+ i skip -1)) (funcall get-next-frame)))))))
;;       ;; Now `frame' should be "the function from which we were called".
;;       (pcase (cons frame nextframe)
;;         ;; No subr calls `interactive-p', so we can rule that out.
;;         (`((,_ ,(pred (lambda (f) (subrp (indirect-function f)))) . ,_) . ,_) nil)
;;         ;; In case #<subr funcall-interactively> without going through the
;;         ;; `funcall-interactively' symbol (bug#3984).
;;         (`(,_ . (t ,(pred (lambda (f)
;;                             (eq internal--funcall-interactively
;;                                 (indirect-function f))))
;;                    . ,_))
;;          t)))))

(defun interactive-p ()
  "Return t if the containing function was run directly by user input.
This means that the function was called with `call-interactively'
\(which includes being called as the binding of a key)
and input is currently coming from the keyboard (not a keyboard macro),
and Emacs is not running in batch mode (`noninteractive' is nil).

The only known proper use of `interactive-p' is in deciding whether to
display a helpful message, or how to display it.  If you're thinking
of using it for any other purpose, it is quite likely that you're
making a mistake.  Think: what do you want to do when the command is
called from a keyboard macro or in batch mode?

To test whether your function was called with `call-interactively',
either (i) add an extra optional argument and give it an `interactive'
spec that specifies non-nil unconditionally (such as \"p\"); or (ii)
use `called-interactively-p'."
  (declare (obsolete called-interactively-p "23.2"))
  (called-interactively-p 'interactive))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar called-interactively-p-functions nil
  "Special hook called to skip special frames in `called-interactively-p'.
The functions are called with 3 arguments: (I FRAME1 FRAME2),
where FRAME1 is a \"current frame\", FRAME2 is the next frame,
I is the index of the frame after FRAME2.  It should return nil
if those frames don't seem special and otherwise, it should return
the number of frames to skip (minus 1).")

(defun interactive-p ()
  "Return t if the containing function was run directly by user input.
This means that the function was called with `call-interactively'
\(which includes being called as the binding of a key)
and input is currently coming from the keyboard (not a keyboard macro),
and Emacs is not running in batch mode (`noninteractive' is nil).

The only known proper use of `interactive-p' is in deciding whether to
display a helpful message, or how to display it.  If you're thinking
of using it for any other purpose, it is quite likely that you're
making a mistake.  Think: what do you want to do when the command is
called from a keyboard macro or in batch mode?

To test whether your function was called with `call-interactively',
either (i) add an extra optional argument and give it an `interactive'
spec that specifies non-nil unconditionally (such as \"p\"); or (ii)
use `called-interactively-p'."
  (declare (obsolete called-interactively-p "23.2"))
  (called-interactively-p 'interactive))

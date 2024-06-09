;;; pomodoro-utils.el --- Utility functions for Pomodoro timer

;;; Code:

(require 'pomodoro-custom)
(require 'cl-lib)

(defun pomodoro-ideal-state (minutes)
  (cond ((< minutes 25) (cons 'ok minutes))
        ((< minutes 30) (cons 'pause (- minutes 25)))
        ((< minutes 55) (cons 'ok (- minutes 30)))
        (t              (cons 'pause (- minutes 55)))))

(defun timestamp ()
  "Returns the timestamp as an integer."
  (string-to-number (format-time-string "%s")))

(defun play-sound-file-async (file)
  "Plays with some overhead, but at least doesn't freeze Emacs."
  (let ((command (car command-line-args)))
    (start-process "play-sound-file-async" nil command "-Q" "--batch" "--eval"
                   (format "(play-sound-file \"%s\")" file))))

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defmacro unlocking-buffer (&rest body)
  "Macro that allows safer manipulation of a read-only buffer."
  `(progn (toggle-read-only -1)
          ,@body
          (toggle-read-only 1)))

(defun pomodoro-set-events (events new-status)
  "Sets both the event history and the current status."
  (setq pomodoro-events events
        pomodoro-current new-status
        pomodoro-last   (timestamp)))

(defun pomodoro-register-event (event new-status)
  "Appends to the event list and sets the status."
  (pomodoro-set-events (append pomodoro-events (list event)) new-status))

(defun pomodoro-tubes-string (cons i)
  "Auxiliary function to display the tubes correctly."
  (let* ((type (car cons)) (amount (cdr cons))
         (length (ceiling (/ (* 1.0 amount pomodoro-bar-length) pomodoro-pomodoro-length)))
         (text (make-string length ?░))
         (text (if (not (equal type 'reset)) text
                 (concat text (make-string (- pomodoro-bar-length length) ?_))))
         (text (if (equal type 'pause) text (format "\n%d. %s" i text))))
    (propertize text 'font-lock-face
                (cl-case type
                  (ok 'pomodoro-ok-face)
		  (reset 'pomodoro-reset-face)
                  (pause 'pomodoro-pause-face)
		  (t nil)))))

(defun pomodoro-display-tubes ()
  "Displays the pomodoros done so far as a series of tubes."
  (let ((i 1))
    (dolist (item (append pomodoro-events (list pomodoro-current)))
      (insert (pomodoro-tubes-string item i))
      (unless (equal (car item) 'pause)
        (when (equal (car item) 'ok)
	  (setq i (1+ i))))))
  (insert (propertize "→\n\n" 'font-lock-face '(:weight bold)))
  (cl-loop for item in pomodoro-events
           and extra = (if (equal (car pomodoro-current) 'ok) (cdr pomodoro-current) 0)
           when (equal (car item) 'ok) sum (cdr item) into ok
           when (equal (car item) 'reset) sum (cdr item) into reset
           when (equal (car item) 'pause) sum (cdr item) into pause
           finally (insert (format "Currently using %.2f%% of your time in full pomodoros."
                                   (/ (+ ok (or extra (cdr pomodoro-current))) 0.01
                                      (+ 1e-20 ok reset pause (cdr pomodoro-current)))))))

(defun pomodoro-display-history ()
  "Displays the pomodoros done so far as a history log."
  (let ((i 0))
    (dolist (item pomodoro-events)
      (when (equal (car item) 'ok) (setq i (1+ i)))
      (let* ((type (car item)) (val (cdr item))
             (number (format "%d. " i))
             (number (if (equal type 'ok) number (make-string (length number) ? )))
             (m-ok (format "Completed a pomodoro with %d minute%s\n" val (if (> val 1) "s" "")))
             (m-reset (format "Gave up after %d minute%s\n" val (if (> val 1) "s" "")))
             (m-pause (format "Had a break of %d minute%s\n" val (if (> val 1) "s" "")))
             (message (cl-case type
                        (ok (propertize m-ok 'font-lock-face 'pomodoro-ok-face))
                        (reset (propertize m-reset 'font-lock-face 'pomodoro-reset-face))
                        (pause (propertize m-pause 'font-lock-face 'pomodoro-pause-face)))))
        (insert (concat number message)))))
  (let ((type (car pomodoro-current)) (val (cdr pomodoro-current))
        (diff (- (timestamp) pomodoro-last)))
    (insert (propertize (format "%d:%02d %s"  val diff (if (equal type 'ok) "pomodoro" "break"))
                        'font-lock-face
                        (if (equal type 'ok) 'pomodoro-current-ok-face 'pomodoro-current-pause-face)))))

(defun pomodoro-update ()
  "First updates the variables and then the buffer, if it exists."
  (let ((time (timestamp))
	      (type (car pomodoro-current))
	      (val (cdr pomodoro-current))
        (tick nil)
        (tack pomodoro-sound-tack))
    (when (>= (- time pomodoro-last) (if pomodoro-debug 0 60))
      (setq pomodoro-current (cons type (1+ val)) pomodoro-last time)
      (when (and (equal type 'ok)
		             (>= (1+ val) pomodoro-pomodoro-length))
        (setq pomodoro-events (append pomodoro-events `((ok . ,pomodoro-pomodoro-length)))
              pomodoro-current '(pause . 0)))
      (play-sound-file-async (if (equal (car pomodoro-current) 'ok) tick tack))))
  (when (get-buffer pomodoro-buffer)
    (with-current-buffer (get-buffer pomodoro-buffer)
      (unlocking-buffer
       (delete-region (point-min) (point-max))
       (setq buffer-undo-tree nil)
       (insert (propertize (format-time-string pomodoro-format)
                           'font-lock-face 'pomodoro-time-face))
       (insert "\n")
       (if pomodoro-display-tubes
	         (pomodoro-display-tubes)
	       (pomodoro-display-history))))))

(provide 'pomodoro-utils)
;;; pomodoro-utils.el ends here

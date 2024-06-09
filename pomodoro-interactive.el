;;; pomodoro-interactive.el --- Interactive functions for Pomodoro timer

;;; Code:

(require 'pomodoro-custom)
(require 'pomodoro-utils)

(defvar pomodoro-timer nil
  "Pomodoro timer.")

(defvar pomodoro-events nil
  "Pomodoro event list.")

(defvar pomodoro-current '(ok . 0)
  "Pomodoro current event. Initial value: all fine at the beginning.")

(defvar pomodoro-last 0
  "Pomodoro last timestamp value.")

(defvar pomodoro-debug nil
  "Pomodoro debugging switch.")

(defvar pomodoro-display-tubes t
  "Pomodoro displaying mode, tubes rather than text.")

(defvar pomodoro-mode-line-string "")
(defvar pomodoro-time-remaining)
(defvar pomodoros 0)
(defvar pomodoro-current-cycle nil)

(defun pomodoro-interactive-deliberate-pause ()
  "Pause deliberately."
  (interactive)
  (let ((event (if (equal (car pomodoro-current) 'pause) pomodoro-current
                 (cons 'reset (cdr pomodoro-current)))))
    (pomodoro-register-event event '(pause . 0)))
  (play-sound-file-async pomodoro-sound-tick))

(defun pomodoro-interactive-kill-buffer ()
  "Kills the buffer."
  (interactive)
  (kill-current-buffer))

(defun pomodoro-interactive-new-pomodoro ()
  "Forgoes the current pomodoro or leaves a break."
  (interactive)
  (let ((event (if (equal (car pomodoro-current) 'pause) pomodoro-current
                 (cons 'reset (cdr pomodoro-current)))))
    (pomodoro-register-event event '(ok . 0)))
  (play-sound-file-async pomodoro-sound-tick))

(defun pomodoro-interactive-reset ()
  "Resets the timer."
  (interactive)
  (if (y-or-n-p "Are you sure you want to reset the timer? ")
      (progn (setq pomodoro-current '(ok . 0)
                   pomodoro-events nil
                   pomodoro-last (timestamp))
             (play-sound-file-async pomodoro-sound-tick))
    (message "Pfew! That was close!")))

(defun pomodoro-interactive-toggle-display ()
  "Toggles between display modes."
  (interactive)
  (setq pomodoro-display-tubes (not pomodoro-display-tubes))
  (pomodoro-update))

(defun pomodoro-interactive-quit ()
  "Turns off Pomodoro."
  (interactive)
  (if (y-or-n-p "Are you sure you want to turn off Pomodoro? ")
      (progn (cancel-timer pomodoro-timer)
             (kill-current-buffer)
             (pomodoro-set-events nil '(ok . 0)))
    (message "Pfew! That was close!")))

(defun pomodoro-stick-to-the-grid ()
  (interactive)
  (setq pomodoro-current
        (pomodoro-ideal-state
         (string-to-number (format-time-string "%M") 10))))

(defun pomodoro-start (arg)
  (interactive "P")
  (let* ((timer (or (if (listp arg)
                        (car arg))
                    arg
                    pomodoro-pomodoro-length)))
    (setq pomodoro-current-cycle "w")
    (when pomodoro-timer
      (cancel-timer pomodoro-timer))
    (setq pomodoro-pomodoro-length timer)
    (pomodoro-set-end-time pomodoro-pomodoro-length)
    (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-tick))
    (run-hooks 'pomodoro-start-work-hook))
  (if (not pomodoro-inhibit-prompting-messages)
      (message "Pomodoro started")))

(defun pomodoro-pause ()
  (interactive)
  (cancel-timer pomodoro-timer)
  (setq pomodoro-time-remaining (round (float-time (time-subtract pomodoro-end-time (current-time)))))
  (force-mode-line-update)
  (if (not pomodoro-inhibit-prompting-messages)
      (message "Pomodoro paused")))

(defun pomodoro-resume ()
  (interactive)
  (setq pomodoro-end-time (time-add (current-time) (seconds-to-time pomodoro-time-remaining)))
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-tick))
  (if (not pomodoro-inhibit-prompting-messages)
      (message "Pomodoro resumed")))

(defun pomodoro-stop ()
  (interactive)
  (cancel-timer pomodoro-timer)
  (setq pomodoro-mode-line-string "")
  (setq pomodoro-current-cycle "w")
  (force-mode-line-update)
  (if (not pomodoro-inhibit-prompting-messages)
      (message "Pomodoro stopped")))

(defun pomodoro-tick ()
  (let ((time (round (float-time (time-subtract pomodoro-end-time (current-time))))))
    (if (<= time 0)
        (if (string= pomodoro-current-cycle "w")
            (progn
              (cl-incf pomodoros)
              (let ((p (if (= 0 (mod pomodoros pomodoro-nth-for-longer-break))
                           (cons pomodoro-long-break-time
                                 pomodoro-long-break-start-message)
                         (cons pomodoro-break-time
                               pomodoro-break-start-message))))
                (if pomodoro-play-sounds
                    (play-pomodoro-break-sound))
                (if pomodoro-desktop-notification
                    (notifications-notify :body (cdr p)))
                (cond ((yes-or-no-p (cdr p))
                       (setq pomodoro-current-cycle "b")
                       (pomodoro-set-end-time (car p))
                       (run-hooks 'pomodoro-start-break-hook))
                      (t
                       (cl-decf pomodoros)
                       (pomodoro-set-end-time pomodoro-extra-time)))))
          (if pomodoro-play-sounds
              (play-pomodoro-work-sound))
          (if pomodoro-desktop-notification
              (notifications-notify :body pomodoro-work-start-message))
          (if (not (yes-or-no-p pomodoro-work-start-message))
              (pomodoro-set-end-time pomodoro-extra-time)
            (setq pomodoro-current-cycle "w")
            (pomodoro-set-end-time pomodoro-pomodoro-length)
            (run-hooks 'pomodoro-start-work-hook))))
    (setq pomodoro-mode-line-string
          (format (concat "%s"
                          (when pomodoro-show-number
                            (format "%s-" (+ 1 (mod pomodoros pomodoro-nth-for-longer-break))))
                          (format-seconds pomodoro-time-format time))
                  pomodoro-current-cycle))
    (force-mode-line-update)))

(provide 'pomodoro-interactive)
;;; pomodoro-interactive.el ends here

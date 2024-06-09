;;; pomodoro-custom.el --- Custom settings for Pomodoro timer

;;; Code:

(require 'cl-lib)

(defgroup pomodoro nil
  "Custom settings for `pomodoro`."
  :group 'productivity)

(defcustom pomodoro-buffer-name "Pomodoro!"
  "Name of the Pomodoro buffer."
  :type 'string :group 'pomodoro)

(defvaralias 'pomodoro-buffer 'pomodoro-buffer-name)

(defcustom pomodoro-bar-length 25
  "Length of a pomodoro bar in tubes mode."
  :type 'integer :group 'pomodoro)

(defcustom pomodoro-pomodoro-length 25
  "Time length of a Pomodoro round."
  :type 'integer :group 'pomodoro)

(defvar pomodoro-format "%H:%M:%S"
  "Time format for pomodoro clock.")

(defvar pomodoro-dir (file-name-directory (or load-file-name buffer-file-name))
  "Pomodoro directory in which sounds are stored.")

(defvar pomodoro-sound-tick (expand-file-name (concat pomodoro-dir "tick.wav"))
  "Tick sound during a pomodoro run.")

(defvar pomodoro-sound-tack (expand-file-name (concat pomodoro-dir "tack.wav"))
  "Tack sound during a break.")

;;; Faces
(defface pomodoro-time-face
  '((t (:family "DejaVu Sans" :height 6.0 :width semi-condensed)))
  "Pomodoro face for Clock."
  :group 'pomodoro)

(defface pomodoro-ok-face
  '((t (:foreground "#ff0000")))
  "Pomodoro face for valid pomodoro run."
  :group 'pomodoro)

(defface pomodoro-pause-face
  '((t (:foreground "#00ff00")))
  "Pomodoro face for paused pomodoro."
  :group 'pomodoro)

(defface pomodoro-reset-face
  '((t (:foreground "#333333")))
  "Pomodoro face for reset pomodoro."
  :group 'pomodoro)

(defface pomodoro-current-ok-face
  '((t (:height 2.5 :inherit pomodoro-ok-face)))
  "Pomodoro face for current pomodoro."
  :group 'pomodoro)

(defface pomodoro-current-pause-face
  '((t (:height 2.5 :inherit pomodoro-pause-face)))
  "Pomodoro face for current pause."
  :group 'pomodoro)

(provide 'pomodoro-custom)
;;; pomodoro-custom.el ends here

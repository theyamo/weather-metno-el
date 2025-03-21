;;; weather-metno-tabulated-view.el --- Weather in the mode line


;;; Commentary:
;;

;;; Code:

(require 'weather-metno)
(require 'weather-metno-query)
(require 'org-weather-metno)
(require 'calendar)

(require 'cl-lib)
(require 'vtable)

(defcustom weather-metno--condensed-view-query
  '(
    :get temperature :name temperature-max :select value :each string-to-number :max
    :get temperature :name temperature-min :select value :each string-to-number :min    
    :get temperature :name temperature-max :select value :each string-to-number :max
    :get temperature :name temperature-avg :select value :each string-to-number :reduce org-weather-metno~q-avg
    :get minTemperature :name min-temperature :select value :each string-to-number :min
    :get maxTemperature :name max-temperature :select value :each string-to-number :max
    :get precipitation :name precipitation-max :select value :each string-to-number :max
    :get precipitation :name precipitation-min :select value :each string-to-number :min    
    :get symbol :select code :name symbol :reduce weather-metno--most-frequent-element
    :get windSpeed :name wind-speed :select mps :each string-to-number :max
    :get windGust :name wind-gust :select mps :each string-to-number :max
    :get windDirection :name wind-direction-symbol :select name :each weather-metno-wind-direction :reduce weather-metno--most-frequent-element
    )
  "The query used in condensed weather forecast.
See `weather-metno-query' for more information."
  :group 'weather-metno)

(defun attach-date-range (x)
  (list x date-range))

(defun attach-date-range-v2 (x)
  (debug)
  (list date-range x))

(defun attach-date-range-convert-to-string (x)
  (list (string-to-number x) date-range))

(defun weather-metno--most-frequent-element (lst)
  (let ((hash-table (make-hash-table :test 'equal))
        most-frequent
        max-count)
    ;; Count occurrences
    (dolist (item lst)
      (puthash item (1+ (gethash item hash-table 0)) hash-table))
    ;; Find the most frequent element
    (maphash (lambda (key value)
               (when (or (not max-count) (> value max-count))
                 (setq max-count value
                       most-frequent key)))
             hash-table)
    most-frequent))

;; "C-x 8 RET 2b07"
;; DOWNWARDS BLACK ARROW
;; (insert-char #x2b06)⬆
;; (insert-char #x2b07)⬇
;; (insert-char #x2b08)⬈
;; (insert-char #x2b09)⬉
;; (insert-char #x2b0a)⬊
;; (insert-char #x2b0b)⬋
;; (insert-char #x2b05)⬅
;; (insert-char #x2b95)⮕

(defconst weather-metno--wind-direction-map
  '((S . #x2b06) (N . #x2b07) (SW . #x2b08) (SE . #x2b09) (NW . #x2b0a) (NE . #x2b0b) (E . #x2b05) (W . #x2b95)))

(defun weather-metno-wind-direction (str)
  (char-to-string (cdr (assq (intern str) weather-metno--wind-direction-map))))

(defconst weather-metno--condensed-forecast-format
  "   {symbol|:symbol}={min-temperature} – {max-temperature} ℃={precipitation-min} – {precipitation-max} ㎜={wind-speed} ({wind-gust}) m/s={wind-direction-symbol}")

;; (defconst weather-metno--side-column
;;   "{time-range|:times}")

(defun transpose-2d-list (matrix)
  (apply 'mapcar* 'list matrix))

(defun time-less-or-equal-p (time1 time2)
  "Return non-nil if TIME1 is less than or equal to TIME2."
  (or (time-less-p time1 time2)
      (time-equal-p time1 time2)))

(defun time-more-or-equal-p (time1 time2)
  "Return non-nil if TIME1 is equal to or greater than TIME2."
  (or (time-equal-p time1 time2)
      (not (time-less-p time1 time2))))

(defun calendar-to-emacs-time (date)
  "Return the current time with seconds and minutes set to zero."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun add-hours-to-time (date hours)
  (encode-time (decoded-time-add
                (decode-time date)
                (make-decoded-time :hour hours))))

(defun get-forecast-for-day (date)
  (let ((ret nil))
    (dotimes (day-segment 4)
      (let* ((emacs-time (calendar-to-emacs-time date))
             (hour (+ (* 0 24) (* day-segment 6)))
             (start-time (add-hours-to-time emacs-time hour))
             (end-time (add-hours-to-time start-time 6))
             (data weather-metno--data))

        (when-let ((query-data (eval `(weather-metno-query-v2
                                       (weather-metno--data nil start-time end-time)
                                       ,@weather-metno--condensed-view-query))))
          
          (let ((time-string (format "%s – %s"
                                     (format-time-string "%H:%M" start-time)
                                     (format-time-string "%H:%M" end-time)))
                (formatted (split-string (weather-metno-query-format weather-metno--condensed-forecast-format query-data nil "org-weather-metno--f-" "?") "=")))
            ;;(edebug start-time end-time time-string (assq 'symbol query-data))
            (push time-string formatted)
            (push formatted ret))
          )))
    (nreverse ret)))

(defconst weather-metno-forecast--tabulated-view-field-descriptions
  '("Time" "" "Temperature" "Precipitation" "Wind speed/gusts" "Wind direction"))

(defun weather-metno-forecast-condensed-view (&optional no-switch)
  (interactive)
  (unless weather-metno--data
    (weather-metno-update))

  (with-current-buffer (get-buffer-create weather-metno-buffer-name)
    (let ((inhibit-read-only t))
      (weather-metno-forecast-mode)
      (display-line-numbers-mode 0)
      (erase-buffer)
      (goto-char (point-min))
      ;; (apply 'weather-metno--location-format (caar weather-metno--data))
      (let ((cur-point (point)))
        (weather-metno--insert 'weather-metno-header
                               (concat "Forecast for "
                                       (apply 'weather-metno--location-format (caar weather-metno--data))) "\n"))      
      (dotimes (day 10)
        (let* ((current-day (calendar-current-date day))
               (data (get-forecast-for-day current-day)))
          (let ((calendar-latitude (string-to-number (nth 0 (caar weather-metno--data))))
                (calendar-longitude (string-to-number (nth 1 (caar weather-metno--data)))))
            (weather-metno--insert 'weather-metno-entry
                                   (format "\n%s: %s\n\n"
                                           (format-time-string
                                            weather-metno-format-date-string
                                            (calendar-to-emacs-time current-day))
                                           (solar-sunrise-sunset-string current-day t))))
          (push weather-metno-forecast--tabulated-view-field-descriptions data)
          (setq data (transpose-2d-list data))
          (goto-char (point-max))
          (make-vtable
           :objects data
           :separator-width 8
           :keymap (define-keymap
                     "g" #'weather-metno-update
                     "s" #'weather-metno-forecast-search-location
                     "q" #'quit-window))
          (goto-char (point-max))
          )))
    (goto-char (point-min)))
  (setq weather-metno--display-function #'weather-metno-forecast-condensed-view)
  (weather-metno--switch-to-forecast-buffer))


(provide 'weather-metno-tabulated-view)

;;; weather-metno-mode-tabulated-view.el ends here

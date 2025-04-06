;;; weather-metno-tabular-view.el --- Weather as a tabular view


;;; Commentary:
;;

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'vtable)
(require 'solar)
(require 'lunar)

(require 'weather-metno-query)

(defcustom weather-metno--table-view-query
  '(:get minTemperature :name min-temperature :select value :each string-to-number :min
         :get maxTemperature :name max-temperature :select value :each string-to-number :max
         :get precipitation :name precipitation-max :select value :each string-to-number :max
         :get precipitation :name precipitation-min :select value :each string-to-number :min
         :get symbol :select code :name symbol :reduce weather-metno--most-frequent-element
         :get windSpeed :name wind-speed :select mps :each string-to-number :max
         :get windGust :name wind-gust :select mps :each string-to-number :max
         :get windDirection :name wind-direction-symbol :select name :each weather-metno--wind-direction
         :reduce weather-metno--most-frequent-element)
  "The query used in condensed weather forecast.
See `weather-metno-query' for more information."
  :group 'weather-metno)

(defconst weather-metno-forecast--table-view-field-descriptions
  '("Time" "" "Temperature" "Precipitation" "Wind speed/gusts" "Wind direction"))


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


(defconst weather-metno--condensed-forecast-format
  "{symbol|:symbol}={min-temperature} – {max-temperature} ℃={precipitation-min} – {precipitation-max} ㎜={wind-speed} ({wind-gust}) m/s={wind-direction-symbol}")

(defun weather-metno-tabular-view--f-symbol (code)
  "Format symbol."
  (let* ((image (if (and (stringp weather-metno-weathericons-directory)
                         (display-images-p))
                    (weather-metno-get-weathericon code)
                  nil)))
    (if image
        (propertize "icon"
                    'display (append image '(:ascent center :margin 4))
                    'rear-nonsticky '(display))
      "")))

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

(defun weather-metno--wind-direction (str)
  (char-to-string (cdr (assq (intern str) weather-metno--wind-direction-map))))

(defun weather-metno--transpose-2d-list (matrix)
  (apply 'cl-mapcar 'list matrix))

(defun weather-metno--calendar-to-emacs-time (date)
  "Return the current time with seconds and minutes set to zero."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun weather-metno--add-hours-to-time (date hours)
  (encode-time (decoded-time-add
                (decode-time date)
                (make-decoded-time :hour hours))))

(defun weather-metno--get-forecast-for-day (date)
  (let ((ret nil))
    (dotimes (day-segment 4)
      (let* ((emacs-time (weather-metno--calendar-to-emacs-time date))
             (hour (+ (* 0 24) (* day-segment 6)))
             (start-time (weather-metno--add-hours-to-time emacs-time hour))
             (end-time (weather-metno--add-hours-to-time start-time 6))
             (day-data weather-metno--data))
        (when-let* ((query-data (eval `(weather-metno-query-v2
                                        (weather-metno--data nil start-time end-time)
                                        ,@weather-metno--table-view-query))))
          (let ((time-string (format "%s – %s"
                                     (format-time-string "%H:%M" start-time)
                                     (format-time-string "%H:%M" end-time)))
                (formatted (split-string (weather-metno-query-format weather-metno--condensed-forecast-format
                                                                     query-data
                                                                     nil
                                                                     "weather-metno-tabular-view--f-" "?")
                                         "=")))
            (push time-string formatted)
            (push formatted ret)))))
    (nreverse ret)))

(defun weather-metno--insert-lunar-phase-info (day)
  (let* ((month-year (list (car day)
                           (nth 2 day)))
         (phases (apply 'lunar-phase-list month-year)))
    (dolist (phase phases)
      (if (equal (car phase) day)
          (let ((eclipse (nth 3 phase)))
            (weather-metno--insert 'weather-metno-lunar-phase
                                   (format "%s\n"
                                           (concat (lunar-phase-name (nth 2 phase)) " "
                                                   (cadr phase) (unless (string-empty-p eclipse) " ")
                                                   eclipse))))))))

;;;###autoload
(defun weather-metno-forecast-tabular-view (&optional no-switch)
  "Display weather forecast, sunrise and sunset times, and lunar phases in tabular format.
If NO-SWITCH is non-nil then do not switch to weather forecast buffer."
  (interactive)
  (setq weather-metno-display-function #'weather-metno-forecast-tabular-view)
  (if (not weather-metno--data)
      (weather-metno-update)
    (with-current-buffer (get-buffer-create (weather-metno-buffer-name))
      (let ((inhibit-read-only t))
        (remove-images (point-min) (point-max))

        (weather-metno-forecast-mode)
        (erase-buffer)
        (goto-char (point-min))
        (weather-metno--insert 'weather-metno-header
                               (concat "Forecast for "
                                       (apply 'weather-metno--location-format (caar weather-metno--data))) "\n")
        (dotimes (day 10)
          (let* ((current-day (calendar-current-date day))
                 (day-data (weather-metno--get-forecast-for-day current-day)))
            (let ((calendar-latitude (string-to-number (nth 0 (caar weather-metno--data))))
                  (calendar-longitude (string-to-number (nth 1 (caar weather-metno--data))))
                  (time-string (format-time-string
                                weather-metno-format-date-string
                                (weather-metno--calendar-to-emacs-time current-day))))
              (weather-metno--insert 'weather-metno-entry
                                     (format "\n%s: %s\n%s"
                                             time-string
                                             (solar-sunrise-sunset-string current-day t)
                                             (make-string (+ (string-width time-string) 2) ? )))
              (weather-metno--insert-lunar-phase-info current-day)
              (insert "\n"))
            (push weather-metno-forecast--table-view-field-descriptions day-data)
            ;; Remove any empty strings that remain in data when icons are disabled
            (when (or (not (stringp weather-metno-weathericons-directory))
                      (not (display-images-p)))
              (setq day-data (mapcar (lambda (sublist)
                                       (cl-remove-if (lambda (s) (string= s "")) sublist))
                                     day-data)))
            (make-vtable
             :objects (weather-metno--transpose-2d-list day-data)
             :separator-width 8
             :keymap (define-keymap
                       "g" #'weather-metno-update
                       "s" #'weather-metno-forecast-search-location
                       "l" #'weather-metno-forecast-list-view
                       "q" #'weather-metno-kill-forecast-buffer))
            (goto-char (point-max))
            ))
        (insert "\n")
        (weather-metno--insert
         'weather-metno-footer
         "Weather data from The Norwegian Meteorological Institute (CC BY 3.0)\n" ;; TODO link!
         ))
      (goto-char (point-min))))
  (unless no-switch
    (weather-metno--switch-to-forecast-buffer)))


(provide 'weather-metno-tabular-view)

;;; weather-metno-mode-tabular-view.el ends here

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
  '(:get temperature :name temperature-max :select value :each string-to-number :max
         :get temperature :name temperature-min :select value :each string-to-number :min
         :get temperature :name temperature-avg :select value :each string-to-number :reduce org-weather-metno~q-avg
         :get precipitation :name precipitation-max :select value :each string-to-number :max
         :get precipitation :name precipitation-min :select value :each string-to-number :min
         :get windSpeed :name wind-speed :select mps :each string-to-number :max
         :get windGust :name wind-gust :select mps :each string-to-number :max
         :get windDirection :name wind-direction :select deg :each string-to-number :reduce weather-metno--most-frequent-element
         :get windDirection :name wind-direction-symbol :select name :each weather-metno-wind-direction :reduce weather-metno--most-frequent-element
         :get symbol :select code :reduce weather-metno--most-frequent-element)
  "The query used in condensed weather forecast.
See `weather-metno-query' for more information."
  :group 'weather-metno)

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
  "{symbol|:symbol} {precipitation-min}–{precipitation-max} ({precipitation-min-time|:time}–{precipitation-max-time|:time}) {temperature-min}–{temperature-max} ({temperature-min-time|:time}–{temperature-max-time|:time}) {wind-speed} {wind-gust} {wind-direction} {wind-direction-symbol}")

(defun weather-metno--format-forecast-items-list (date)
  (let ((query-data (eval `(weather-metno-query
                            (weather-metno--data nil date)
                            ,@weather-metno--condensed-view-query))))
    (when query-data
      (split-string (weather-metno-query-format weather-metno--condensed-forecast-format query-data nil "org-weather-metno--f-" "?")))))

(defun weather-metno-forecast-condensed-view (&optional no-switch)
  (interactive)
  (unless weather-metno--data
    (weather-metno-update))

  (let ((data nil))
    (dotimes (i 10)
      (let* ((current-time (current-time))
             (days-in-seconds (* i 24 60 60)) ;; N days in seconds
             (new-time (time-add current-time (seconds-to-time days-in-seconds)))
             (new-entry `(,(format-time-string "%A %m-%d" new-time)
                          ,@(weather-metno--format-forecast-items-list
                             (calendar-current-date i)))))
        (setq data (append data (list new-entry)))))
    (with-current-buffer (get-buffer-create weather-metno-buffer-name)
      (let ((inhibit-read-only t))
        (weather-metno-forecast-mode)
        (erase-buffer)
        (goto-char (point-min))
        (apply 'weather-metno--location-format (caar weather-metno--data))
        (weather-metno--insert 'weather-metno-header
                               (concat "Forecast for "
                                       (apply 'weather-metno--location-format (caar weather-metno--data))) "\n")
        (add-text-properties (point-min) (point) '(face weather-metno-header))
        (goto-char (point-max))
        (make-vtable
         :columns '("Date" "Symbol" "Precipitation" "Hours" "℃ min-max" "T" "WS" "WG" "WD" "WDS")
         :objects data
         :separator-width 5
         :keymap (define-keymap
                   "s" #'weather-metno-forecast-search-location
                   "q" #'quit-window))
        (goto-char (point-max))
        (let ((calendar-latitude (string-to-number (nth 0 (caar weather-metno--data))))
              (calendar-longitude (string-to-number (nth 1 (caar weather-metno--data)))))
          (insert (solar-sunrise-sunset-string (calendar-current-date) t))))
      (goto-char (point-min))
      (setq weather-metno--display-function #'weather-metno-forecast-condensed-view))
    ;;TODO: unless no-switch ...
    (weather-metno--switch-to-forecast-buffer)))

(provide 'weather-metno-tabulated-view)

;;; weather-metno-mode-tabulated-view.el ends here

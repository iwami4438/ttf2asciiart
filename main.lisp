(ql:quickload :zpb-ttf)

(defun make-canvas (height width)
  (make-array (list height width) :initial-element #\Space))

(defstruct glyph-data
  xmin ymin xmax ymax
  segments)

(defstruct segment
  start-x start-y
  control-x control-y
  end-x end-y)

(defun collect-glyph-segments (glyph)
  (let ((segments nil))
    (zpb-ttf:do-contours (contour glyph)
      (zpb-ttf:do-contour-segments (start control end)
        contour
        (when start
          (push (make-segment :start-x (zpb-ttf:x start)
                              :start-y (zpb-ttf:y start)
                              :control-x (when control (zpb-ttf:x control))
                              :control-y (when control (zpb-ttf:y control))
                              :end-x (zpb-ttf:x end)
                              :end-y (zpb-ttf:y end))
                segments))))
    (nreverse segments)))

(defun load-glyph-to-struct (font-path char)
  (zpb-ttf:with-font-loader (loader font-path)
    (let* ((glyph (zpb-ttf:find-glyph char loader))
           (box (zpb-ttf:bounding-box glyph)))
      (make-glyph-data
       :xmin (zpb-ttf:xmin box)
       :ymin (zpb-ttf:ymin box)
       :xmax (zpb-ttf:xmax box)
       :ymax (zpb-ttf:ymax box)
       :segments (collect-glyph-segments glyph)))))

(defun calculate-scale (width height margin glyph)
  (let ((avail-w (- width (* 2.0 margin)))
        (avail-h (- height (* 2.0 margin)))
        (glyph-width (- (glyph-data-xmax glyph)
                        (glyph-data-xmin glyph)))
        (glyph-height (- (glyph-data-ymax glyph)
                         (glyph-data-ymin glyph))))
    (min (/ avail-w (if (zerop glyph-width) 1.0 glyph-width))
         (/ avail-h (if (zerop glyph-height) 1.0 glyph-height)))))

(defun transform-glyph (glyph-data scale margin)
  (let ((xmin (glyph-data-xmin glyph-data))
        (ymin (glyph-data-ymin glyph-data))
        (xmax (glyph-data-xmax glyph-data))
        (ymax (glyph-data-ymax glyph-data)))
    (let ((glyph-height (- ymax ymin)))
      (flet ((transform-coord (x y)
               (values (+ (* (- x xmin) scale)
                          margin)
                       (+ (* (- glyph-height
                                (- y ymin))
                             scale)
                          margin)))
             (to-float (val)
               (and val (float val 0.0d0))))
        (let ((transformed-segments
                (mapcar (lambda (seg)
                          (let ((sx (to-float (segment-start-x seg)))
                                (sy (to-float (segment-start-y seg)))
                                (ex (to-float (segment-end-x seg)))
                                (ey (to-float (segment-end-y seg)))
                                (cx (to-float (segment-control-x seg)))
                                (cy (to-float (segment-control-y seg))))
                            (multiple-value-bind (nsx nsy) (transform-coord sx sy)
                              (multiple-value-bind (nex ney) (transform-coord ex ey)
                                (multiple-value-bind (ncx ncy)
                                    (if (and cx cy)
                                        (transform-coord cx cy)
                                        (values nil nil))
                                  (make-segment :start-x nsx
                                                :start-y nsy
                                                :control-x ncx
                                                :control-y ncy
                                                :end-x nex
                                                :end-y ney))))))
                        (glyph-data-segments glyph-data))))
          (make-glyph-data
           :xmin (* 0.0 scale)
           :ymin (* 0.0 scale)
           :xmax (* (- xmax xmin) scale)
           :ymax (* glyph-height scale)
           :segments transformed-segments))))))

(defun draw-line-to-grid (grid width height x0 y0 x1 y1 char)
  (let* ((dx (abs (- x1 x0)))
         (dy (abs (- y1 y0)))
         (sx (if (< x0 x1) 1 -1))
         (sy (if (< y0 y1) 1 -1))
         (err (- dx dy)))
    (loop
      (when (and (>= x0 0)
                 (< x0 width)
                 (>= y0 0)
                 (< y0 height))
        (setf (aref grid y0 x0)
              char))
      (when (and (= x0 x1)
                 (= y0 y1))
        (return))
      (let ((e2 (* 2 err)))
        (when (> e2 (- dy))
          (setq err (- err dy))
          (setq x0 (+ x0 sx)))
        (when (< e2 dx)
          (setq err (+ err dx))
          (setq y0 (+ y0 sy)))))))

(defun make-grid (glyph-data &key (width 80) (height 40) (ch #\#))
  (let ((grid (make-canvas height width)))
    (dolist (seg (glyph-data-segments glyph-data))
      (let ((sx (round (segment-start-x seg)))
            (sy (round (segment-start-y seg)))
            (ex (round (segment-end-x seg)))
            (ey (round (segment-end-y seg)))
            (cx (segment-control-x seg))
            (cy (segment-control-y seg)))
        (draw-line-to-grid grid width height sx sy ex ey ch)
        (when (and cx cy)
          (let ((icx (round cx))
                (icy (round cy)))
            (draw-line-to-grid grid width height sx sy icx icy ch)
            (draw-line-to-grid grid width height icx icy ex ey ch)))))
    grid))

(defun render-horizontal (grids width height)
  (loop :for y :below height
        :do (progn
              (dolist (gri grids)
                (loop :for x :below width
                      :do (write-char (aref gri y x))))
              (terpri))))

(defun render-vertical (grids width height)
  (dolist (gri grids)
    (loop :for y :below height
          :do (progn
                (loop :for x :below width
                      :do (write-char (aref gri y x)))
                (terpri)))))

(defun usage ()
  (write-line "Usage: ttf2asciiart [options] text ...")
  (write-line "Options:")
  (write-line "  -f, --font <path>    Set the font the path")
  (write-line "  -v, --vertical       Output in vertical text")
  (write-line "  -w, --width <val>    Specify output width (default: 48.0)")
  (write-line "  -h, --height <val>   Specify output height (default: 24.0)")
  (write-line "  -c, --char <char>    Specify the text to be drawn. (default: #)")
  (write-line "      --help           Display this."))

(defun parse-cli-args (args)
  (when (null args)
    (error "At least one argument is required."))
  (let ((f-path "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf")
        (vertical-flg nil)
        (texts nil)
        (canvas-width 48.0)
        (canvas-height 24.0)
        (opt-flg nil)
        (describe-char #\#))
    (dolist (arg args)
      (cond
        (opt-flg
         (when (and (> (length arg) 0)
                    (char= (char arg 0) #\-))
           (error "Option ~a expects a value, but got another option: ~a" opt-flg arg))
         (ecase opt-flg
           (:font (setq f-path arg))
           (:width (setq canvas-width (read-from-string arg)))
           (:height (setq canvas-height (read-from-string arg)))
           (:ch (setq describe-char (char arg 0))))
         (setq opt-flg nil))
        ((or (string= arg "-w")
             (string= arg "--width"))
         (setq opt-flg :width))
        ((or (string= arg "-h")
             (string= arg "--height"))
         (setq opt-flg :height))
        ((or (string= arg "-v")
             (string= arg "--vertical"))
         (setq vertical-flg t))
        ((or (string= arg "-f")
             (string= arg "--font"))
         (setq opt-flg :font))
        ((or (string= arg "-c")
             (string= arg "--char"))
         (setq opt-flg :ch))
        ((string= arg "--help")
         (usage)
         (uiop:quit 0))
        (t
         (push arg texts))))
    (values f-path (nreverse texts) vertical-flg canvas-width canvas-height
            describe-char)))

(defun main ()
  (handler-case
      (multiple-value-bind (f-path texts vertical-flg canvas-width canvas-height
                            describe-char)
          (parse-cli-args (uiop:command-line-arguments))
        (when (null texts)
          (error "No text provided"))
        (when (or (<= canvas-width 0)
                  (<= canvas-height 0))
          (error "Width or height must be positive"))
        (let* ((margin 1.0)
               (concatted (mapcan (lambda (te)
                                    (coerce te 'list))
                                  texts))
               (grids (mapcar (lambda (ch)
                                (let* ((raw-glyph (load-glyph-to-struct f-path ch))
                                       (scale (calculate-scale canvas-width
                                                               canvas-height
                                                               margin
                                                               raw-glyph))
                                       (transformed (transform-glyph raw-glyph scale margin)))
                                  (make-grid transformed
                                             :width (round canvas-width)
                                             :height (round canvas-height)
                                             :ch describe-char)))
                              concatted)))
          (if vertical-flg
              (render-vertical grids
                               canvas-width
                               canvas-height)
              (render-horizontal grids
                                 canvas-width
                                 canvas-height))))
    (error (e)
      (format *error-output* "Error: ~a~%" e)
      (usage)
      (uiop:quit 1))))

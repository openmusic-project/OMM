
(in-package :om)


;=========================================
;TOOLS
;=========================================
(defun  point_in_triangle_p (s a b c)
    (setf as_x (- (first s) (first a)))
    (setf as_y (- (second s) (second a)))

    (setf s_ab (> (- (* as_y (- (first b) (first a)))
                     (* as_x (- (second b) (second a)))) 0))
    
    (setf test1 (> (- (* as_y (- (first c) (first a)))
                     (* as_x (- (second c) (second a)))) 0))

    (setf test2 (> (- (* (- (first c) (first b)) (- (second s) (second b)))
                     (* (- (second c) (second b)) (- (first s) (first b)))) 0))
    (cond
     ((equal s_ab test1) nil)
     ((not (equal s_ab test2)) nil)
     (t t)
))

(defun  point_in_rec_p (s tl br)
  (and (>= (first s) (first tl)) (<= (first s) (first br))
       (>= (second s) (second tl)) (<= (second s) (second br))))

(defun  trans_by_p (x p plus)
  (loop for item in x collect
        (funcall plus item p)))

(defun get-space-dim (list)
  (let ((min (copy-list  (car list))) 
        (max (copy-list  (car list)))
        (len (length (car list))))
    (loop for item in list do
          (loop for i from 0 to (- len 1) do
                (setf (nth i min) (min (nth i min) (nth i item)))
                (setf (nth i max) (max (nth i max) (nth i item)))))
    (list min max)))

(defun points2pict (list)
  (let* ((size (get-space-dim list))
         (min2D (car size))
         (max2D (second size)))
    (loop for j from (second min2D) to (second max2D) collect
          (loop for i from (car min2D) to (car max2D) collect
                (if (member (list i j) list :test 'equal)
                    '(0 0 0) '(1 1 1))))))

(defun points2pictws (list x0 y0 x y)
    (loop for j from y0 to y collect
          (loop for i from x0 to x collect
                (if (member (list i j) list :test 'equal)
                    '(0 0 0) '(1 1 1)))))


(defun getpointsfrompict (list)
  (let (rep)
    (loop for line in list
          for i = 0 then (+ i 1) do
          (loop for item in line
                for j = 0 then (+ j 1) do
                (unless (= (apply '+ item) 0)
                  (push (list i j) rep))))
    rep))

;=========================================
;GENERIC FUNCTIONS
;=========================================
(defmethod! dilation ((object t) (structuring t))
  :icon 435
  :doc "Constraint, on numbers only.
It states that the inputs have to be equal, for numbers only.
The associated cost-function measures the distance between the inputs.

Inputs :
numbers
"
  "ok")


(defmethod! dilation ((object list) (structuring list))
 (let (rep)
   (loop for item in structuring do
         (let ((new (translate object item)))
           (setf rep (supremum new rep))))
   rep))


(defmethod! erosion ((object t) (structuring t))
  :icon 437
  :doc "Constraint, on numbers only.
It states that the inputs have to be equal, for numbers only.
The associated cost-function measures the distance between the inputs.

Inputs :
numbers
"
  "ok")

(defmethod! erosion ((object list) (structuring list))
  (let ((rep (trans_by_p object (car structuring) 'om-)))
   (loop for elem in (cdr structuring) do
         (setf rep (x-intersect rep (trans_by_p object elem 'om-))))
   rep))

(defmethod! erosion ((object list) (structuring list))
  (let ((rep (translate object (symmetrical (car structuring)))))
   (loop for item in (cdr structuring) do
           (let ((new (translate object (symmetrical item))))
           (setf rep (infimum rep new))))
   rep))

(defmethod! opening ((object t) (structuring t))
  :icon 438
  :doc ""
  (dilation (erosion object structuring) structuring)
 )

(defmethod! closing ((object t) (structuring t))
  :icon 439
  :doc ""
  (erosion  (dilation object structuring) structuring))


(defmethod! symetrical ((object t))
  :icon 440
  :doc ""
  (loop for item in object collect (om* -1 item)))

(defmethod! quotient ((object t) (set t))
  :icon 441
  :doc ""
  (let (rep)
    (loop for item in object do
          (unless (member item set :test 'equal)
            (push item rep)))
    rep))


(defmethod! hit-or-miss ((object t) (fstruct t) (bstruct t))
  :icon 442
  :doc ""
  (quotient  (erosion object fstruct) (dilation object (symetrical bstruct))))


;========================================
;SETS
;========================================
(defmethod translate ((object t) (x t))
  (loop for item in object collect (om+ item x)))


(defmethod supremum ((object t) list)
  (x-union object list))

(defmethod infimum ((object t) list)
 (x-intersect object list ))

(defmethod symmetrical ((object t))
  (om* -1 object))



;========================================
;PICTURE
;========================================
(defmethod! dilation ((object picture) (structuring picture))
            (points2pictws (dilation  (getpointsfrompict (background object)) (getpointsfrompict (background structuring))) 0 0 100 100)
)

(defmethod translate ((object list) (x t))
  (loop for item in object collect (om+ item x)))


(defmethod supremum ((object list) list)
  (x-union object list))

(defmethod infimum ((object list) list)
 (x-intersect object list ))

(defmethod symmetrical ((object list))
  (om* -1 object))

;========================================
;BPF
;========================================

(defmethod remove-y-dup ((object bpf))
  (let ((new-bpf (make-instance 'bpf :from-file t))
        (points (point-list object))
        lasty rep)
    (loop for item in (butlast points) do
          (unless (equal lasty (om-point-v item))
            (push item rep)
            (setf lasty (om-point-v item))))
    (push (car (last points)) rep)
    (cons-bpf new-bpf rep) new-bpf))



(defmethod! dilation ((object bpf) (structuring bpf))
 (let (rep)
   (loop for item in (point-list structuring) do
         (let ((new (translate object item)))
           (setf rep (supremum rep new))
           ))
   rep
   ;(remove-y-dup rep)
))

(defmethod! erosion ((object bpf) (structuring bpf))
  (let* ((struct-points  (point-list structuring))
         (rep (translate object (symmetrical (car struct-points)))))
   (loop for item in (cdr struct-points) do
           (let ((new (translate object (symmetrical item))))
             (setf rep (infimum rep new ))))
   ;(remove-y-dup rep)
   rep
))

(defmethod translate ((object bpf) (p om-api::ompoint))
  (let* ((points (point-list object))
         (new-bpf (make-instance 'bpf :from-file t))
         (newpoints (loop for y in points collect (om-make-point (+ (om-point-h y) (om-point-h p)) (+ (om-point-v y) (om-point-v p))))))
    (print newpoints)
    (cons-bpf new-bpf newpoints)
    new-bpf))

(defmethod symmetrical ((p om-api::ompoint))
  (om-make-point (* -1 (om-point-h p)) (* -1 (om-point-v p))))

(defmethod supremum ((object null) (object1 bpf))
  object1)

(defmethod supremum ((object bpf) (object1 bpf))
  (suplist1  object object1))

(defun suplist1 (bpf1 bpf2)
  (let ((new-bpf (make-instance 'bpf :from-file t))
         points1 rep min1 min2 max1 max2)
    (setf points1  (point-list bpf1))
    (setf points2  (point-list bpf2))
    (setf min1 (om-point-h (car points1)))
    (setf min2 (om-point-h (car points2)))
    (setf max1 (om-point-h (car (last points1))))
    (setf max2 (om-point-h (car (last points2))))
    (loop for point in points1 do
          (let* ((t0 (om-point-h point))
                 (max (om-point-v point)))
            (when (and (<= t0 max2) (>= t0 min2))
              (setf max (max max (x-transfer bpf2 t0))))
            (push (om-make-point t0 max) rep)))
     (loop for point in points2 do
          (let* ((t0 (om-point-h point))
                 (max (om-point-v point)))
            (unless (member t0 points1 :key 'om-point-h)
              (when (and (<= t0 max1) (>= t0 min1))
                (setf max (max max (x-transfer bpf1 t0))))
              (push (om-make-point t0 max) rep))))
     (setf rep (sort rep '< :key 'om-point-h))
     (cons-bpf new-bpf (reverse rep))
     new-bpf))
    
(defun suplist2 (bpf1 bpf2)
  (let ((new-bpf (make-instance 'bpf :from-file t))
         points1 rep lasty1 lasty2 lastmax)
    (setf points1  (copy-list (point-list bpf1)))
    (setf points2  (copy-list (point-list bpf2)))
    (loop while (and points1 points2 ) do 
            (let (newpoint newpoint2)
         (cond 
            ( (< (om-point-h (car points1)) (om-point-h (car points2)))
             (progn
              (setf newpoint (pop points1))
              (if lasty2 (setf lastmax (max   lasty2 (om-point-v newpoint) )) (setf lastmax  (om-point-v newpoint) ))
              (push (om-make-point (om-point-h newpoint) lastmax) rep)
              (setf lasty1 (om-point-v newpoint))))

             ( (> (om-point-h (car points1)) (om-point-h (car points2)))
              (progn
              (setf newpoint (pop points2))
              (if lasty1 (setf lastmax (max   lasty1 (om-point-v newpoint) )) (setf lastmax  (om-point-v newpoint) ))
              (push (om-make-point (om-point-h newpoint) lastmax) rep)
              (setf lasty2 (om-point-v newpoint))))
              (t
                (progn
                  (setf newpoint (pop points1))
                  (setf newpoint2 (pop points2))
                  (setf lastmax (max   (om-point-v newpoint) (om-point-v newpoint2) ))
                  (push (om-make-point (om-point-h newpoint) lastmax) rep)
                  (setf lasty1 (om-point-v newpoint))
                  (setf lasty2 (om-point-v newpoint2)))))))
    (loop while points1 do 
            (push (pop points1) rep))
    (loop while points2 do 
            (push (pop points2) rep))
    
     (cons-bpf new-bpf (reverse rep))
     new-bpf))      
 
(defmethod infimum ((object bpf) (object1 bpf))
  (inflist1  object object1))

(defun inflist1 (bpf1 bpf2)
  (let ((new-bpf (make-instance 'bpf :from-file t))
         points1 rep min1 min2 max1 max2)
    (setf points1  (point-list bpf1))
    (setf points2  (point-list bpf2))
    (setf min1 (om-point-h (car points1)))
    (setf min2 (om-point-h (car points2)))
    (setf max1 (om-point-h (car (last points1))))
    (setf max2 (om-point-h (car (last points2))))
    (loop for point in points1 do
          (let* ((t0 (om-point-h point))
                 (min (om-point-v point)))
            (when (and (<= t0 max2) (>= t0 min2))
              (setf min (min min (x-transfer bpf2 t0)))
              (push (om-make-point t0 min) rep))))
     (loop for point in points2 do
          (let* ((t0 (om-point-h point))
                 (min (om-point-v point)))
            (unless (member t0 points1 :key 'om-point-h)
              (when (and (<= t0 max1) (>= t0 min1))
                (setf min (min min (x-transfer bpf1 t0)))
                (push (om-make-point t0 min) rep)))))
     (setf rep (sort rep '< :key 'om-point-h))
     (cons-bpf new-bpf (reverse rep))
     new-bpf))

(defun inflist2 (bpf1 bpf2)
  (let ((new-bpf (make-instance 'bpf :from-file t))
         points1 rep lasty1 lasty2 lastmin)
    (setf points1  (copy-list (point-list bpf1)))
    (setf points2  (copy-list (point-list bpf2)))
    (loop while (and points1 points2 ) do 
            (let (newpoint newpoint2)
         (cond 
            ( (< (om-point-h (car points1)) (om-point-h (car points2)))
             (progn
              (setf newpoint (pop points1))
              (if lasty2 (progn
                          (setf lastmin (min   lasty2 (om-point-v newpoint) ))
                          (push (om-make-point (om-point-h newpoint) lastmin) rep)
                          ))
              (setf lasty1 (om-point-v newpoint))))

             ((> (om-point-h (car points1)) (om-point-h (car points2)))
              (progn
              (setf newpoint (pop points2))
              (if lasty1 (progn
                          (setf lastmin (min   lasty1 (om-point-v newpoint) ))
                          (push (om-make-point (om-point-h newpoint) lastmin) rep)
                          ))
              (setf lasty2 (om-point-v newpoint))))
               (t
                (progn
                  (setf newpoint (pop points1))
                  (setf newpoint2 (pop points2))
                  (setf lastmin (min   (om-point-v newpoint) (om-point-v newpoint2) ))
                  (push (om-make-point (om-point-h newpoint) lastmin) rep)
                  (setf lasty1 (om-point-v newpoint))
                  (setf lasty2 (om-point-v newpoint2)))))))
     (cons-bpf new-bpf (reverse rep))
     new-bpf))
    

;========================================
;chord-seq
;========================================
(defmethod! dilation ((object chord-seq) (structuring chord-seq))
 (bpf2cs (dilation (ch2bpf object) (ch2bpf structuring))))

(defmethod! erosion ((object chord-seq) (structuring chord-seq))
 (bpf2cs (erosion (ch2bpf object) (ch2bpf structuring))))


(defun ch2bpf (cs1)
   (let ((new-bpf (make-instance 'bpf :from-file t))
         (points (loop for note in (lmidic cs1)
                       for onset in (lonset cs1) collect (om-make-point onset (- (car note) 6000) ))))
     (cons-bpf new-bpf points)
    new-bpf))

(defun bpf2cs (bpf)
   (make-instance 'chord-seq :Lmidic (om+ 6000 (y-points bpf)) :Lonset (x-points bpf) :legato 100))



;========================================
;voice
;========================================
(defmethod! dilation ((object voice) (structuring voice))
 (list2voice (sort (dilation (voice2lista object) (voice2lista structuring)) '<)))

(defmethod! erosion ((object voice) (structuring voice))
  (list2voice (sort (erosion (voice2lista object) (voice2lista structuring)) '<)))

(defmethod voice2lista ((self voice))
  (let* ((points (second (car (second (tree self)))))
         rep)
    (loop for item in points
          for i = 0 then (+ i 1) do
          (when (= item 1) (push i rep)))
    (reverse rep)))

(defun list2voice (list)
  (let* ((last (+ 1 (car (last list))))
         (signature (list last 8))
         (attacks (loop for i from 0 to (- last 1) collect (if (member i list) 1 -1)))
         (points (list (list signature attacks))))
    (make-instance 'voice
      :tree points)))


;=========================================
;GENERIC FUNCTIONS RT
;=========================================
;========================================
;voice
;========================================

(defmethod! erosion ((object t) (structuring t))
  :icon 437
  :doc "Constraint, on numbers only.
It states that the inputs have to be equal, for numbers only.
The associated cost-function measures the distance between the inputs.

Inputs :
numbers
"
  "ok")

(defmethod! dilation_rt ((object voice) (structuring voice))
            :icon 435
            :doc "Constraint, on numbers only.
It states that the inputs have to be equal, for numbers only.
The associated cost-function measures the distance between the inputs.

Inputs :
numbers
"
            (let* ((rt1 (car (second (tree object))))
                   (rt2 (car (second (tree structuring)))))
              (make-instance 'voice :tree (list '? (list (dilation2 rt1 rt2)))))
            )
             

(defmethod! erosion_rt ((object voice) (structuring voice))
            :icon 437
            :doc "Constraint, on numbers only.
It states that the inputs have to be equal, for numbers only.
The associated cost-function measures the distance between the inputs.

Inputs :
numbers
"
            (let* ((rt1 (car (second (tree object))))
                   (rt2 (car (second (tree structuring)))))
              (make-instance 'voice :tree (list '? (list (erosion2 rt1 rt2)))))
            )

(defmethod! opening_rt ((object t) (structuring t))
  :icon 438
  :doc ""
  (dilation_rt (erosion_rt object structuring) structuring)
 )

(defmethod! closing_rt ((object t) (structuring t))
  :icon 439
  :doc ""
  (erosion_rt  (dilation_rt object structuring) structuring))

;========================================
;RT
;========================================

(defmethod! dilation2 ((rt1 list) (rt2 list))
            (let* ((trans_list (reemplazar-nodos rt1 rt2 -1))
                   (rep (car trans_list))
                   ev)
              (print (list "lista " (list '? trans_list)))
              (loop for item in (cdr trans_list) do
                      (setf rep (suppremum_rt rep item))
                      (push rep ev))
              (print (list "evolution " (list '? (reverse ev))))
              rep)
            )


(defmethod! erosion2 ((rt1 list) (rt2 list))
            (let* ((trans_list (reemplazar-nodos rt1 rt2 1))
                   (rep (car trans_list))
                   ev)
              (print (list "lista " (list '? trans_list)))
              (loop for item in (cdr trans_list) do
                      (setf rep (infimum_rt rep item ))
                      (push rep ev))
              (print (list "evolution " (list '? (reverse ev))))
              (print rep)))


(defun suppremum_rt (rt1 rt2)
  (let* ((r1 (/ (caar rt1) (second (car rt1)))) (r2 (/ (caar rt2) (second (car rt2)))) (prop1 (second rt1)) (prop2 (second rt2))
         (nr (max r1 r2)))
    (list (list (numerator nr) (denominator nr)) (union_prop prop1 prop2))))

(defun infimum_rt (rt1 rt2)
  (let* ((r1 (/ (caar rt1) (second (car rt1)))) (r2 (/ (caar rt2) (second (car rt2)))) (prop1 (second rt1)) (prop2 (second rt2))
         (nr (min r1 r2)))
    (list (list (numerator nr) (denominator nr)) (intersec_prop prop1 prop2))))

 
(defun union_prop (prop1 prop2)
  (cond 
   ( (null  prop1) prop2)
   ( (null  prop2) prop1)
   (t (let ((alist1 (make-attack-list prop1))
            (alist2 (make-attack-list prop2))
            (c1 0)
            (c2 0)
            minus1 minus2 rep)
        (loop while (and alist1 alist2 ) do 
                (cond (
                       (<  (car alist1)  (car alist2))
                       (progn
                         (setf minus1 (< (getroot (nth c1 prop1)) 0))
                         (push (list (pop alist1) (getprop (nth c1 prop1)) (and minus1 minus2)) rep)
                         (setf c1 (+ c1 1))
                         ))
                      ((> (car alist1)  (car alist2))
                       (progn
                         (setf minus2 (< (getroot (nth c2 prop2)) 0))
                         (push (list (pop alist2) (getprop (nth c2 prop2)) (and minus1 minus2)) rep)
                         (setf c2 (+ c2 1))
                         ))
                      (t
                       (progn
                         (setf minus2 (< (getroot (nth c2 prop2)) 0))
                         (setf minus1 (< (getroot (nth c1 prop1)) 0))
                         (setf val1 (pop alist1))
                         (setf val2 (pop alist2))
                         (if (and minus1 minus2)
                             (push (list val1  nil (and minus1 minus2)) rep)
                             (push (list val1  (union_prop (getprop (nth c1 prop1)) (getprop (nth c2 prop2))) (and minus1 minus2)) rep))
                         (setf c1 (+ c1 1))
                         (setf c2 (+ c2 1))))))
        (setf rep  (reverse rep))
        (setf ratios (x->dx (cons 0 (normalize-ratios (mapcar #'car rep)))))
        (setf rep (loop for i from 0 to (- (length rep) 1)
                        collect  (progn
                                   (setf val (if (third (nth i rep)) (* -1 (nth i ratios)) (nth i ratios)))
                                   (if (second (nth i rep)) (list val (second (nth i rep))) val))))
        rep ))))

(defun union_prop (prop1 prop2)
  (cond 
   ( (null  prop1) prop2)
   ( (null  prop2) prop1)
   (t (let ((alist1 (make-attack-list prop1))
            (alist2 (make-attack-list prop2))
            (c1 0)
            (c2 0)
            minus1 minus2 rep)
        (loop while (and alist1 alist2 ) do 
                (cond (
                       (<  (car alist1)  (car alist2))
                       (progn
                         (setf minus1 (< (getroot (nth c1 prop1)) 0))
                         (push (list (pop alist1) (union_prop (getprop (nth c1 prop1)) (getprop (nth c2 prop2))) (and minus1 minus2)) rep)
                         (setf c1 (+ c1 1))
                         ))
                      ((> (car alist1)  (car alist2))
                       (progn
                         (setf minus2 (< (getroot (nth c2 prop2)) 0))
                         (push (list (pop alist2) (union_prop (getprop (nth c1 prop1)) (getprop (nth c2 prop2))) (and minus1 minus2)) rep)
                         (setf c2 (+ c2 1))
                         ))
                      (t
                       (progn
                         (setf minus2 (< (getroot (nth c2 prop2)) 0))
                         (setf minus1 (< (getroot (nth c1 prop1)) 0))
                         (setf val1 (pop alist1))
                         (setf val2 (pop alist2))
                         (if (and minus1 minus2)
                             (push (list val1  nil (and minus1 minus2)) rep)
                             (push (list val1  (union_prop (getprop (nth c1 prop1)) (getprop (nth c2 prop2))) (and minus1 minus2)) rep))
                         (setf c1 (+ c1 1))
                         (setf c2 (+ c2 1))))))
        (setf rep  (reverse rep))
        (setf ratios (x->dx (cons 0 (normalize-ratios (mapcar #'car rep)))))
        (setf rep (loop for i from 0 to (- (length rep) 1)
                        collect  (progn
                                   (setf val (if (third (nth i rep)) (* -1 (nth i ratios)) (nth i ratios)))
                                   (if (second (nth i rep)) (list val (second (nth i rep))) val))))
        rep ))))

(defun intersec_prop (prop1 prop2)
  (cond 
   ( (null  prop1) nil)
   ( (null  prop2) nil)
   (t (let ((alist1 (make-attack-list prop1))
            (alist2 (make-attack-list prop2))
            (c1 0)
            (c2 0)
            minus1 minus2 rep)
        (loop while (and alist1 alist2 ) do 
                (cond (
                       (<  (car alist1)  (car alist2))
                       (progn
                         (setf minus1 (< (getroot (nth c1 prop1)) 0))
                         (push (list (pop alist1) (intersec_prop (getprop (nth c1 prop1)) (getprop (nth c2 prop2))) (or minus1 minus2)) rep)
                         (setf c1 (+ c1 1))
                         ))
                      ((> (car alist1)  (car alist2))
                       (progn
                         (setf minus2 (< (getroot (nth c2 prop2)) 0))
                         (push (list (pop alist2) (intersec_prop (getprop (nth c1 prop1)) (getprop (nth c2 prop2))) (or minus1 minus2)) rep)
                         (setf c2 (+ c2 1))
                         ))
                      (t
                       (progn
                         (setf minus2 (< (getroot (nth c2 prop2)) 0))
                         (setf minus1 (< (getroot (nth c1 prop1)) 0))
                         (setf val1 (pop alist1))
                         (setf val2 (pop alist2))
                         (if (or minus1 minus2)
                             (push (list val1  nil (or minus1 minus2)) rep)
                             (push (list val1  (intersec_prop (getprop (nth c1 prop1)) (getprop (nth c2 prop2))) (or minus1 minus2)) rep))
                         (setf c1 (+ c1 1))
                         (setf c2 (+ c2 1))))))
        (setf rep  (reverse rep))
        (setf ratios (x->dx (cons 0 (normalize-ratios (mapcar #'car rep)))))
        (setf rep (loop for i from 0 to (- (length rep) 1)
                        collect  (progn
                                   (setf val (if (third (nth i rep)) (* -1 (nth i ratios)) (nth i ratios)))
                                   (if (second (nth i rep)) (list val (second (nth i rep))) val))))
        rep ))))

(defun least-common-denominator (ratios)
  (reduce #'lcm (mapcar #'denominator ratios)))

(defun normalize-ratios (ratios)
  (let ((lcd (least-common-denominator ratios)))
    (mapcar (lambda (r)
              (* r lcd))
            ratios)))

(defun getroot (rt)
  (if (atom rt) rt (first rt)))

(defun getprop (rt)
  (if (atom rt) nil (second rt)))

(defun make-attack-list (prop)
  (let* ((somme (reduce #'+ (mapcar (lambda (x) (abs (getroot x))) prop)))
         (points (mapcar (lambda (x) (/ (getroot x) somme)) prop))
         (attacks (mapcar (lambda (x) (/ (getroot x) somme)) prop)))
    (loop for dx in attacks
                    sum (abs dx) into thesum
                    collect  thesum)))


(defun mk-new-rt (a1 a2 path neg)
  (list (list (first a2) (remplace-path a1 (cdr path) (second a2) neg))))

(defun remplace-path (a1 path prop neg)
  (setf index (car path))
  (setf rep 
        (if (equal (length path) 1)
            (loop for item in prop
                  for i = 0 then (+ i 1) collect
                    ( if (equal i index) (list (getroot item) (second a1)) neg))
          (loop for item in prop
                for i = 0 then (+ i 1) collect
                  (if (equal i index) (if (listp item) (list (first item) (remplace-path a1 (cdr path) (second item) neg)) a1) (* (getroot (nth i prop)) neg)))))
 rep)
 

(defun reemplazar-nodos-rec (a1 a2 a2o path rep neg)
      (let ((d2 (first a2))
        (r2 (second a2)))
        (setf rep (copy-list rep))
        (loop for item in r2
                 for i = 0 then (+ i 1) do
                (setf newpath (append path (list i)))
                (setf rep (append rep (mk-new-rt a1 a2o newpath neg)))
                (if (listp item)
                    (setf rep (reemplazar-nodos-rec a1 item a2o newpath rep neg))))
        rep))

(defun reemplazar-nodos (a1 a2 neg)
  (reemplazar-nodos-rec a1 a2 a2 '(0) (list (list (car a2) (second a1))) neg))





;;; OMM  LIBRARY


(in-package :om)

(defvar  *mathmorph-files* nil)

(setf *mathmorph-files* 
      '(
        "sources;mathmorph"
        ))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *mathmorph-files*))

(defvar *subpackages-omm-list* nil)
(setf *subpackages-omm-list*
      '( ("general" nil nil (dilation erosion opening closing) nil)
         ("RTs" nil nil (dilation_rt erosion_rt opening_rt closing_rt) nil)
         ))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-omm-list*)

(in-package #:cloud-prog)

(def-feature fuel-feature (state action)
  (ws-fuel env-state))

(def-feature fuel-critical-p (state action)
  (let ((env (ws-env env-state)))
    ;; We're bingo fuel...
    (if (<= (ws-fuel env-state) (fuel-amount-per-step env))
        t
        nil)))

(def-feature loc (state action)
  (ws-robot-loc env-state))

(def-feature have-cloud? (state action)
  (eql (ws-cloud-status env-state) :on-robot))

(def-feature cloud-source-feature (state action)
  (ws-cloud-source env-state))

(def-feature cloud-target-feature (state action)
  (ws-cloud-target env-state))

(def-feature robot-dest (state action)
  (stack-var-val 'loc t t))

(def-feature navigating-to-cloud (state action)
  (cond ((stack-contains-frame 'pickup-cloud)
         #+ (or)
         (assert (not (stack-contains-frame 'drop-cloud)))
         t)
        (t nil)))

;;; Superfluous, since it currently is always the negation of NAVIGATING-TO-CLOUD
(def-feature navigating-to-dropoff (state action)
  (stack-contains-frame 'drop-cloud))

(def-feature target-loc (state action)
  (if (navigating-to-cloud state action)
      (cloud-source-feature state action)
      (cloud-target-feature state action)))

(def-feature cloud-dist (state action)
  (grid-world:shortest-path-dist (ws-env env-state)
                                 (ws-cloud-source env-state)
                                 (ws-cloud-target env-state)))

(def-feature act-dist (state action)
  (grid-world:shortest-path-dist (ws-env env-state)
                                 (ws-robot-loc env-state)
                                 (ws-cloud-target env-state)))

(def-feature shortest-path-distance (state action)
  (let* ((robot-loc (ws-robot-loc env-state))
         (target-loc (stack-var-val 'loc t t)))
    (grid-world:shortest-path-dist (ws-env env-state)
                                   robot-loc
                                   target-loc)))

(defun direction-from-to (from to)
  (destructuring-bind (from-x from-y) from
    (destructuring-bind (to-x to-y) to
      (cond ((< from-x to-x) 's)
            ((> from-x to-x) 'n)
            ((< from-y to-y) 'e)
            ((> from-y to-y) 'w)
            (t 'rest)))))

(def-feature target-direction (state action)
  (direction-from-to (loc state action) (target-loc state action)))

(def-feature shortest-path-direction (state action)
  (let* ((robot-loc (ws-robot-loc env-state))
         (target-loc (stack-var-val 'loc t t))
         (target-path (grid-world:shortest-path (ws-env env-state)
                                               robot-loc
                                               target-loc)))
    (assert target-loc (target-loc) "No target location?")
    (cond ((equal robot-loc target-loc) 'rest)
          (t
           (assert (>= (length target-path) 2))
           (direction-from-to robot-loc (second target-path))))))

(defun make-bucket-fun (features)
  (lambda (state)
    (mapcar (lambda (fun)
              (funcall fun state nil))
            features)))

(defparameter *cloud-featurizer-0*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc target-direction choice)
    (:qc-depends loc target-direction choice)
    (:qe-depends loc target-loc choice))
   (navigate-to-cloud
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-cloud-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))))

(defparameter *cloud-bucket-function-0*
  (make-bucket-fun '(loc target-direction target-loc)))

(defparameter *cloud-featurizer-1*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc target-loc choice)
    (:qc-depends loc target-loc choice)
    (:qe-depends loc target-loc choice))
   (navigate-to-cloud
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-cloud-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-dependes))))

(defparameter *cloud-bucket-function-1*
  (make-bucket-fun '(loc target-loc)))

(defparameter *cloud-featurizer-2*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc choice navigating-to-cloud)
    (:qc-depends choice navigating-to-cloud
                 shortest-path-distance shortest-path-direction)
    (:qe-depends choice navigating-to-cloud
                 shortest-path-distance shortest-path-direction))
   (navigate-to-cloud
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-cloud-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-dependes))))

(defparameter *cloud-bucket-function-2*
  (make-bucket-fun '(loc navigating-to-cloud shortest-path-distance shortest-path-direction)))

(defparameter *cloud-featurizer-3*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc choice navigating-to-cloud)
    (:qc-depends choice navigating-to-cloud
                 shortest-path-distance shortest-path-direction)
    (:qe-depends choice shortest-path-direction))
   (navigate-to-cloud
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-cloud-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-dependes))))

(defparameter *cloud-bucket-function-3*
  (make-bucket-fun '(loc navigating-to-cloud shortest-path-distance shortest-path-direction)))

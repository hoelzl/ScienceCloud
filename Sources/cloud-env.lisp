(in-package #:cloud-env)

;;; A grid environment in which a robot should move from a source field to the location of a
;;; cloud item, collect the cloud and then drop it in a target area.  Modeled loosely after the
;;; td-taxi world.

(defstruct (cloud-state (:conc-name #:ws-))
  robot-loc
  (cloud-status :at-source) ; :on-robot :at-source :at-dest
  cloud-source
  cloud-target
  fuel
  env)

(defmethod clone ((state cloud-state))
  (make-cloud-state
   :robot-loc (ws-robot-loc state)
   :cloud-status (ws-cloud-status state)
   :cloud-source (ws-cloud-source state)
   :cloud-target (ws-cloud-target state)
   :fuel (ws-fuel state)
   :env (ws-env state)))

(defmethod same ((s1 cloud-state) (s2 cloud-state))
  (and (equal (ws-robot-loc s1) (ws-robot-loc s2))
       (eql (ws-cloud-status s1) (ws-cloud-status s2))
       (equal (ws-cloud-source s1) (ws-cloud-source s2))
       (equal (ws-cloud-target s1) (ws-cloud-target s2))
       (= (ws-fuel s1) (ws-fuel s2))
       (eql (ws-env s1) (ws-env s2))))

(defmethod canonicalize ((state cloud-state))
  (list 'robot-loc (ws-robot-loc state)
        'cloud-status (ws-cloud-status state)
        'cloud-source (ws-cloud-source state)
        'cloud-target (ws-cloud-target state)
        'fuel (ws-fuel state)))

(defvar *print-graphically* nil)
(defmethod print-object ((state cloud-state) stream)
  (if *print-graphically*
      (loop
        with env = (ws-env state)
        with d = (dimensions env)
        for i from -1 to (first d)
        do (terpri stream)
        do (loop
             for j from -1 to (second d)
             do (cond ((or (= i -1) (= i (first d)))
                       (if (or (= j -1) (= j (second d)))
                           (format stream "XX")
                           (format stream "~AX" (mod j 10))))
                      ((or (= j -1) (= j (second d)))
                       (format stream "~AX" (mod i 10)))
                      ((eq (loc-value env (list i j)) 'wall) 
                       (format stream "XX"))
                      ((equal (ws-robot-loc state) (list i j))
                       (if (eq (ws-cloud-status state) :on-robot)
                           (format stream "RR")
                           (format stream "rr")))
                      ((equal (ws-cloud-source state) (list i j))
                       (if (eq (ws-cloud-status state) :at-source)
                           (format stream "WW")
                           (format stream "ww")))
                      (t (format stream "  "))))
        finally (format stream "~&Cloud Targets: ~A  Fuel: ~A"
                        (cloud-targets env)  (ws-fuel state)))
      (call-next-method)))


(defclass <cloud-env> (<fully-observable-env> <grid-world>)
  ((move-success-prob :type float
                      :initarg :move-success-prob :initform 0.95
                      :accessor move-success-prob)
   (wall-collision-cost :type float
                        :initarg :wall-collision-cost :initform 0.5
                        :accessor wall-collision-cost)
   (cost-of-living :type float
                   :initarg :cost-of-living :initform 0.1
                   :accessor cost-of-living)
   (init-fuel :type number
              :initarg :init-fuel :initform 100.0
              :accessor init-fuel)
   (fuel-decrease-prob :type float
                       :initarg :fuel-decrease-prob :initform 0.8
                       :accessor fuel-decrease-prob)
   (fuel-amount-per-step :type float
                         :initarg :fuel-amount-per-step :initform 1.0
                         :accessor fuel-amount-per-step)
   (no-fuel-cost :type float
                 :initarg :no-fuel-cost :initform 100.0
                 :accessor no-fuel-cost)
   (refuel-success-prob :type float
                        :initarg :refuel-success-prob :initform 0.95
                        :accessor refuel-success-prob)
   (cloud-sources :initarg :cloud-sources
                  :accessor cloud-sources)
   (cloud-targets :type list
                  :initarg :cloud-targets :initform '((0 0))
                  :accessor cloud-targets)
   (cloud-delivery-reward :type float
                          :initarg :cloud-delivery-reward :initform 50.0
                          :accessor cloud-delivery-reward))
  (:default-initargs :legality-test (lambda (val)
                                      (not (eq val 'wall)))))

(defmethod initialize-instance ((env <cloud-env>)
                                &rest initargs &key cloud-sources world-map)
  (declare (ignorable initargs))
  ;; TODO: We have to try repeatedly to find an initial cloud source because we don't take into
  ;; account inaccessible grid fields here.  Maybe we should just generate a list of valid
  ;; fields and sample from this list?
  (unless cloud-sources
    (let ((sources (make-instance '<prod-set>
                     :sets (array-dimensions world-map)
                     :alist-keys '(0 1))))
      (setf (cloud-sources env) sources)))
  (call-next-method))

(defvar *available-actions* '(n e s w pickup drop refuel))

(defmethod avail-actions ((env <cloud-env>) state)
  (declare (ignore state))
  ;; All actions are possible in every state.
  *available-actions*)

(defmethod is-terminal-state ((env <cloud-env>) state)
  "is-terminal-state CLOUD-ENV STATE
A state is terminal if we have unloaded the cloud at one of the cloud targets or if the robot
has run out of fuel."
  (or (eq (ws-cloud-status state) :at-dest)
      #+ (or)
      (zerop (ws-fuel state))))

(defun move-would-hit-wall-p (env state action)
  (destructuring-bind (robot-x robot-y) (ws-robot-loc state)
    (case action
      ((n) (= robot-x 0))
      ((w) (= robot-y 0))
      ((s) (destructuring-bind (dim-x dim-y) (dimensions env)
             (declare (ignore dim-y))
             (= (1+ robot-x) dim-x)))
      ((e) (destructuring-bind (dim-x dim-y) (dimensions env)
             (declare (ignore dim-x))
             (= (1+ robot-y) dim-y))))))

(defun reward (env state action new-state)
  (let* ((cloud-at-dest? (eq (ws-cloud-status new-state) :at-dest))
         (cloud-reward (if cloud-at-dest?
                           (cloud-delivery-reward env)
                           0.0))
         (hit-wall? (move-would-hit-wall-p env state action))
         (wall-cost (if hit-wall? (wall-collision-cost env) 0.0))
         (no-fuel? (zerop (ws-fuel state)))
         (no-fuel-cost (if no-fuel? (no-fuel-cost env) 0.0)))
    (if (is-terminal-state env state)
        0
        (- cloud-reward
           (cost-of-living env)
           wall-cost
           no-fuel-cost))))

(defun move-action-p (action)
  (case action
    ((n e s w) t)
    (otherwise nil)))

(defun compute-next-loc (env state action)
  (if (and (move-action-p action)
           (> (ws-fuel state) 0))
      (let* ((loc (ws-robot-loc state))
             (succ-prob (move-success-prob env))
             (slip-prob (/ (- 1.0 succ-prob) 2.0))
             (forward-loc (result loc action))
             (forward-prob (if (is-legal-loc env forward-loc) succ-prob 0.0))
             (left-loc (result loc (rot-counterclockwise action)))
             (left-prob (if (is-legal-loc env left-loc) slip-prob 0.0))
             (right-loc (result loc (rot-clockwise action)))
             (right-prob (if (is-legal-loc env right-loc) slip-prob 0.0))
             (stay-prob (- 1.0 (+ forward-prob left-prob right-prob))))
        (sample-multinomial (list forward-loc right-loc left-loc loc)
                            forward-prob right-prob left-prob stay-prob))
      (ws-robot-loc state)))

(defun compute-cloud-status (env state action)
  (let ((cloud-status (ws-cloud-status state)))
    (case action
      ((pickup)
       (if (equal (ws-robot-loc state) (ws-cloud-source state))
           :on-robot
           cloud-status))
      ((drop)
       (if (and (eq (ws-cloud-status state) :on-robot)
                (member (ws-robot-loc state) (cloud-targets env) :test 'equal))
           :at-dest
           cloud-status))
      (otherwise
       cloud-status))))

(defun compute-fuel (env state action)
  (cond ((move-action-p action)
         (let ((fuel (ws-fuel state))
               (fuel-prob (fuel-decrease-prob env)))
           (sample-multinomial (list (max 0.0 (- fuel (fuel-amount-per-step env))) fuel)
                               fuel-prob (- 1.0 fuel-prob))))
        ((eq action 'refuel)
         (let ((refuel-prob (refuel-success-prob env)))
           (sample-multinomial (list (ws-fuel state) (init-fuel env))
                               (- 1.0 refuel-prob) refuel-prob)))
        (t (ws-fuel state))))

(defmethod sample-next ((env <cloud-env>) state action)
  (assert (member action (avail-actions env state)) (action)
          "Action ~A is not possible in state ~A." action state)
  (let* ((next-loc (compute-next-loc env state action))
         (cloud-status (compute-cloud-status env state action))
         (fuel (compute-fuel env state action))
         (new-state (make-cloud-state
                     :robot-loc next-loc
                     :cloud-status cloud-status
                     :cloud-source (ws-cloud-source state)
                     :cloud-target (ws-cloud-target state)
                     :fuel fuel
                     :env (ws-env state))))
    (values new-state (reward env state action new-state))))

(defvar *max-initial-cloud-source-tries* 100)

(defun compute-intial-cloud-source (env)
  (loop for i from 0 to *max-initial-cloud-source-tries*
        do (let ((loc (mapcar #'cdr (sample-uniformly (cloud-sources env)))))
             (when (is-legal-loc env loc)
               (return-from compute-intial-cloud-source loc))))
  (error "Could not find an initial position in ~A." env))

(defmethod sample-init ((env <cloud-env>))
  (make-cloud-state
   :robot-loc (funcall (unif-grid-dist-sampler env))
   :cloud-status :at-source
   :cloud-source (compute-intial-cloud-source env)
   :cloud-target (first (cloud-targets env))
   :fuel (init-fuel env)
   :env env))

(defun set-up-exploration ()
  (format t "~&Welcome to the robotic cloud collection example.

This environment demonstrates a robot that moves around on a rectangular grid, picks up cloud
and delivers it to a drop-off zone.  X's on the map represent walls, blank spaces are roads.
The robot is represented by 'r' as long as it does not carry cloud, by 'R' as soon as it has
picked up cloud.  You can move by entering N, E, S, W; if the robot is on the same grid field as
the cloud you can pick it up by entering PICKUP, if you are in a drop-off zone you can drop the
cloud and thereby end the episode (and collect the reward) by entering DROP.  To quit the
environment, enter NIL.  (All input can be in lower or upper case.)")
  (setf *print-graphically* t))

(defmethod io-interface :before ((env <cloud-env>))
  (set-up-exploration))

(defun make-cloud-env-0 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (apply #'make-instance '<cloud-env>
           :world-map world
           initargs)))

(defun make-cloud-env-1 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (setf (aref world 0 2) 'wall
          (aref world 1 2) 'wall)
    (apply #'make-instance '<cloud-env>
           :world-map world 
           initargs)))

(defun make-cloud-env-2 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(8 8) :initial-element 'road)))
    (apply #'make-instance '<cloud-env>
           :world-map world
           initargs)))

(defun make-cloud-env-3 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(8 8) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<cloud-env>
           :world-map world
           initargs)))

(defun make-cloud-env-4 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(20 20) :initial-element 'road)))
    (apply #'make-instance '<cloud-env>
           :world-map world
           initargs)))

(defun make-cloud-env-5 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(20 20) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<cloud-env>
           :world-map world
           initargs)))

(defun make-cloud-env-6 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(10 10) :initial-element 'road))
        (walls '((0 3)
                 (1 3) (1 4) (1 5) (1 6) (1 7) (1 9)
                 (2 0) (2 1) (2 3) (2 5)
                 (3 3) (3 5) (3 6) (3 7) (3 8)
                 (4 1) (4 2) (4 3) (4 5)
                 (5 5) (5 7) (5 8) (5 9)
                 (6 0) (6 1) (6 2) (6 3) (6 5)
                 (7 1) (7 5) (7 6)
                 (8 1) (8 3) (8 5) (8 7)
                 (9 3))))
    (mapc (lambda (wall)
            (setf (aref world (first wall) (second wall)) 'wall))
          walls)
    (apply #'make-instance '<cloud-env>
           :world-map world
           initargs)))

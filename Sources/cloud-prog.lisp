(in-package #:cloud-prog)

;;; Accessors for the current state.

(def-env-accessor robot-loc ws-robot-loc
    "The current location of the robot.")
(def-env-accessor cloud-status ws-cloud-status
    "The current status of the cloud: either 'at-source, 'on-robot or 'at-dest.")
(def-env-accessor cloud-source ws-cloud-source
    "The source of the cloud for the current state (this will never change during an episode).")
(def-env-accessor cloud-target ws-cloud-target
    "The target for the cloud in the current state (will never change during an episode).")
(def-env-accessor fuel ws-fuel
    "The amount of fuel the robot currently has.")
(def-env-accessor env ws-env
    "The environemnt for a state")

(defun* nav (loc)
  "nav LOC 
Navigate to location LOC.  Repeatedly choose among the N, E, S, W, and REFUEL actions until the
robot reaches LOC."
  (until (equal (robot-loc) loc)
    (with-choice navigate-choice (dir '(N E S W refuel))
      (action navigate-move dir))))

(defun* nav-directly (loc)
  "nav-directly LOC
navigate to LOC using the solution computed from Floyd's algorithm."
  (until (equal (robot-loc) loc)
    (let ((next-loc (second (grid-world:shortest-path (env) (robot-loc) loc))))
      (with-choice navigate-choice
          (act (list 'refuel (direction-from-to (robot-loc) next-loc)))
        (action nav-direct-action act)))))

(defparameter *refuel-counter* 0)
(defparameter *move-counter* 0)

(defun* refuel-and-nav (loc)
  "refuel-and-navigate LOC 
Navigate to location LOC.  Repeatedly choose among the N, E, S and W actions until the robot
reaches LOC."
  (until (equal (robot-loc) loc)
    (if (<= (fuel) 1.0)
        ;; Always refuel when fuel is low
        (progn
          (incf *refuel-counter*)
          (action refuel-nav 'refuel))
        ;; But still allow refueling when fuel is high
        (progn
          (incf *move-counter*)
          (with-choice navigate-choice (dir '(N E S W))
            (action navigate-move dir))))))

(defun* refuel-and-nav-directly (loc)
  "refuel-and-nav-directly LOC 
Navigate to location LOC.  Repeatedly choose among the N, E, S and W actions until the robot
reaches LOC."
  (until (equal (robot-loc) loc)
    (if (<= (fuel) 1.0)
        ;; Always refuel when fuel is low
        (progn
          (incf *refuel-counter*)
          (action refuel-nav 'refuel))
        ;; But still allow refueling when fuel is high
        (progn
          (incf *move-counter*)
          (let ((next-loc (second (grid-world:shortest-path (env) (robot-loc) loc))))
            (action navigate-move (direction-from-to (robot-loc) next-loc)))))))

(defun* pickup-cloud ()
  "pickup-cloud
Navigate to the location of the cloud and pick it up."
  (call navigate-to-cloud (refuel-and-nav (cloud-source)))
  (action pickup-action 'pickup))

(defun* drop-cloud ()
  "drop-cloud
Navigate to the dropoff location and drop off the cloud."
  (call navigate-to-dropoff (refuel-and-nav (cloud-target)))
  (action drop-action 'drop))

(defun* cloud-robot-prog ()
  "cloud-robot-prog
Repeadedly pick up cloud and drop it off."
  (loop
    (progn
      (call choose-cloud-removal-action (pickup-cloud))
      (call choose-cloud-removal-action (drop-cloud)))))

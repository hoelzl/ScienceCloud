(in-package :cloud-prog)

(defun extract-q-function-table (partial-q-designator &optional (algorithm (current-algorithm)))
  (let ((q-fn (ecase partial-q-designator
                ((:qr) #'ahq::qr)
                ((:qc) #'ahq::qc)
                ((:qe) #'ahq::qe))))
    (fn-approx:params (q-function:fn-approx (funcall q-fn algorithm)))))

(defun extract-q-function-tables (&optional (algorithm (current-algorithm)))
  (values (extract-q-function-table :qr algorithm)
          (extract-q-function-table :qc algorithm)
          (extract-q-function-table :qe algorithm)))

(defun extract-q-function-values (item &optional (algorithm (current-algorithm)))
  (multiple-value-bind (qr-table qc-table qe-table)
      (extract-q-function-tables algorithm)
    (values (gethash item qr-table)
            (gethash item qc-table)
            (gethash item qe-table))))

;;; This presupposes that QR, QC and QE all have the same featurizers and equality function.  In
;;; addition, the second element of each entry (i.e., the first feature from the featurizer)
;;; should be CHOICE.

(defun build-combined-table (&optional (algorithm (current-algorithm)))
  (multiple-value-bind (qr qc qe) (extract-q-function-tables algorithm)
    (let ((result (make-hash-table :test (hash-table-test qr) :size (hash-table-size qr))))
      (maphash (lambda (k v)
                 (setf (gethash k result)
                       (+ v (gethash k qc 0) (gethash k qe 0))))
               qr)
      result)))

(defun build-state-table ()
  (build-combined-table (current-algorithm)))

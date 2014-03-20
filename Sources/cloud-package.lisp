(in-package #:common-lisp-user)

(defpackage #:cloud-env
  (:use #:common-lisp
        ;; From ALisp
        #:utils
        #:direct-product-set
        #:prob
        #:create-env
        #:grid-world)
  (:export 
   ;; The Environment
   #:<cloud-env>
   #:*print-graphically*
   #:set-up-exploration
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W
   #:pickup 
   #:drop
   #:refuel

   ;; Accessors for <cloud-env>
   #:init-fuel
   #:fuel-decrease-prob
   #:fuel-amount-per-step
   #:no-fuel-cost
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:cloud-sources
   #:cloud-targets
   #:cloud-delivery-reward

   #:make-cloud-env-0
   #:make-cloud-env-1
   #:make-cloud-env-2
   #:make-cloud-env-3
   #:make-cloud-env-4
   #:make-cloud-env-5
   #:make-cloud-env-6

   ;; States (Not sure whether this should really be exposed, but we need the functions
   ;; to define state accessors)
   #:cloud-state
   #:ws-robot-loc
   #:ws-cloud-status
   #:ws-cloud-source
   #:ws-cloud-target
   #:ws-fuel
   #:ws-env

   ;; The cloud status 
   #:on-robot
   #:at-source
   #:at-dest))

(defpackage #:cloud-prog
  (:use #:common-lisp
        #:cloud-utils
        ;; From ALisp
        #:utils
        #:alisp-prog
        #:alisp-features
        #:alisp-user
        #:cloud-env)
  (:export
   ;; The Top-Level Program
   #:cloud-robot-prog
   
   ;; Partial Programs
   #:navigate
   #:pickup-cloud
   #:drop-wate
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W
   #:pickup 
   #:drop
   #:refuel
   
   ;; Environment
   #:<cloud-env>
   #:init-fuel
   #:fuel-decrease-prob
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:cloud-sources
   #:cloud-targets
   #:cloud-delivery-reward))

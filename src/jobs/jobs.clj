(ns jobs.jobs
  "Namesspace holding the declarative definitions of ml jobs.")

(defmacro define-job [job-name & {input-def :input
                                  tasks :tasks
                                  output-def :output}]
  (let [platform-sym (gensym "platform_")]
    `(defn ~job-name [~platform-sym input#]
       (let [~input-def input#]
         (let ~(vec (mapcat (fn [[f input-map output-map]]
                              [output-map `(~f ~platform-sym ~input-map)])
                            tasks))
           ~output-def)))))

(define-job abstract-learning
  :input
  {d :data, k :model-kind, a :attribute-roles}

  :tasks
  [[concrete-learning
    {:data d, :model-kind k, :attribute-roles a}
    {t :target-model, o :optimization}]
   [learning
    {:data d, :target-model t, :optimization o}
    {m :model}]]

  :output
  {:model m})

(def apply-in-sample
  {:input
   '{:data d
     :target-model t
     :optimization o}

   :tasks
   [[`learning
     '{:data d, :target-model t, :optimization o}
     '{:model m}]
    [`apply-model
     '{:data d, :model m}
     '{:data result-d}]]

   :output
   '{:data result-d}})

(def input {:data 1, :target-model 2, :optimization 3})

;; job, input -> code
(def apply-in-sample-after-macro
  `(let [{d# :data
          t# :target-model
          o# :optimization} input]))

(defmacro add-sym [v]
  (symbol (str (name v) "#")))

(macroexpand-1 '(add-sym x))

(defmacro map-destrue [v]
  (let [])
  `(let [(add-sym v) 5]
     v#))

(macroexpand-1 '(map-destrue c))

(e {})

(def apply-in-sample-2
  `(learning
    {:data (:data ~'input)
      :target-model (:target-model ~'input)
     :optimization (:optimization ~'input)}))
;; [a# ~(eval job)]

(def apply-in-sample-3
  {:input '{d# :data
            t# :target-model
            o# :optimization}
   :tasks [[`learning
            '{:data d#
              :target-model t#
              :optimization o#}]]})

(defn learning [{:keys [data target-model optimization]}]
  {:model (str "trained" data target-model optimization)})



(defmacro code [job input]
  `(let [~(:input (eval job)) input]
     d#))



(code apply-in-sample-3 input)
(macroexpand-1 '(code apply-in-sample-3 input))

(macroexpand '(code '{:a 5}))

(code '{:a 5})

'{:a 5}

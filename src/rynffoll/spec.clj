(ns rynffoll.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.instant :refer [read-instant-date]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.spec.test.alpha :refer [instrument]]
            [expound.alpha :as expound]
            [spec-tools.json-schema :as json-schema]
            [spec-tools.swagger.core :as swagger]
            [cheshire.core :as json])
  (:import (java.util Date)))

;; https://grishaev.me/clj-book-spec/

(s/def ::string string?)

(s/get-spec ::string)

(s/valid? ::string 1) ;; => false
(s/valid? ::string "test") ;; => true

(s/def ::ne-string ;; ne - non-empty
  (fn [val]
    (and (string? val)
         (not (empty? val)))))

(s/valid? ::ne-string "") ;; => false
(s/valid? ::ne-string "test") ;; => true

(s/def ::ne-string
  (every-pred string? not-empty))

(s/valid? ::ne-string "") ;; => false
(s/valid? ::ne-string "test") ;; => true

(s/def ::ne-string
  (s/and ::string not-empty))

(s/valid? ::ne-string "") ;; => false
(s/valid? ::ne-string "test") ;; => true

(s/def ::url
  (partial re-matches #"(?i)^http(s?)://.*"))

(s/valid? ::url "test") ;; => false
(s/valid? ::url "http://test.com") ;; => true
(try
  (s/valid? ::url nil)
  (catch Exception e
    (str (.toString e))))
;; => "java.lang.NullPointerException"

(s/def ::url
  (s/and
   ::ne-string
   (partial re-matches #"(?i)^http(s?)://.*")))

(s/valid? ::url nil) ;; => false

(s/def ::age
  (s/and int? #(<= 0 % 150)))

(s/valid? ::age nil) ;; => false
(s/valid? ::age 0) ;; => true
(s/valid? ::age 150) ;; => true
(s/valid? ::age 25) ;; => true

(s/def ::url-list
  (s/coll-of ::url))

(s/valid? ::url-list []) ;; => true
(s/valid? ::url-list nil) ;; => false
(s/valid? ::url-list [nil "http://test.com"]) ;; => false
(s/valid? ::url-list ["http://test.com" "https://test.com"]) ;; => true

(s/def ::params
  (s/map-of keyword? string?))

(s/valid? ::params nil) ;; => false
(s/valid? ::params {}) ;; => true
(s/valid? ::params {:key "test"}) ;; => true
(s/valid? ::params {1 nil}) ;; => false

(s/def :page/address ::url)
(s/def :page/description ::ne-string)

(s/def ::page
  (s/keys :req-un [:page/address ;; un - unqualified
                   :page/description]))

;; :req — необходимые полные ключи,
;; :req-un — необходимые краткие ключи,
;; :opt — опциональные полные ключи,
;; :opt-un — опциональные краткие ключи.

(s/valid? ::page {:address "test"
                  :description "test"})
;; => false
(s/valid? ::page {:address "http://test.com"
                  :description "test"})
;; => true
(s/valid? ::page {:page/address "https://clojure.org/"
                  :page/description "Clojure Language"})
;; => false

(s/def ::page-fq
  (s/keys :req [:page/address
                :page/description]))

(s/valid? ::page-fq ;; fq - fully qualified
          {:page/address "https://clojure.org/"
           :page/description "Clojure Language"})
;; => true

(s/def :page/status int?)

(s/def ::page-status
  (s/keys :req-un [:page/address
                   :page/description]
          :opt-un [:page/status]))

(s/valid? ::page-status
          {:address "https://clojure.org/"
           :description "Clojure Language"})
;; => true
(s/valid? ::page-status
          {:address "https://clojure.org/"
           :description "Clojure Language"
           :status 200})
;; => true

(s/valid? ::page-status
          {:address "https://clojure.org/"
           :description "Clojure Language"
           :status nil}) ;; => false

(s/def ::->int
  (s/conformer
   (fn [value]
     (try
       (Integer/parseInt value)
       (catch Exception e
         ::s/invalid)))))

(s/conform ::->int "42") ;; => 42
(s/conform ::->int "test") ;; => :clojure.spec.alpha/invalid

(s/def ::->int+
  (s/and ::ne-string
         ::->int))

(s/conform ::->int+ nil) ;; => :clojure.spec.alpha/invalid
(read-instant-date "2019") ;; => #inst "2019-01-01T00:00:00.000-00:00"

(s/def ::->date
  (s/and
   ::ne-string
   (s/conformer
    (fn [value]
      (try
        (read-instant-date value)
        (catch Exception e
          ::s/invalid))))))

(s/conform ::->date "2019-12-31") ;; => #inst "2019-12-31T00:00:00.000-00:00"
(s/conform ::->date "2019-12-31T23:59:59") ;; => #inst "2019-12-31T23:59:59.000-00:00"

(s/def ::->bits
  (s/conformer
   (fn [value]
     (case value
       "32" 32
       "64" 64
       ::s/invalid))))

(s/conform ::->bits "32") ;; => 32
(s/conform ::->bits "64") ;; => 64
(s/conform ::->bits "128") ;; => :clojure.spec.alpha/invalid

(def bits-map {"32" 32 "64" 64})

(s/def ::->bits
  (s/conformer
   (fn [value]
     (get bits-map value ::s/invalid))))

(s/conform ::->bits "32") ;; => 32
(s/conform ::->bits "64") ;; => 64
(s/conform ::->bits "128") ;; => :clojure.spec.alpha/invalid

(s/def ::->bool
  (s/and
   ::ne-string
   (s/conformer clojure.string/lower-case)
   (s/conformer
    (fn [value]
      (case value
        ("true" "1" "on" "yes") true
        ("false" "0" "off" "no") false
        ::s/invalid)))))

(s/conform ::->bool nil) ;; => :clojure.spec.alpha/invalid
(s/conform ::->bool "0") ;; => false
(s/conform ::->bool "yes") ;; => true

(s/def ::status #{"todo" "in_progres" "done"})

(s/valid? ::status "todo") ;; => true
(s/valid? ::status "hold") ;; => false

(defn enum [& args]
  (let [arg-set (set args)]
    (fn [value]
      (contains? arg-set value))))

(s/def ::status
  (enum "todo" "in_progres" "done"))

(s/valid? ::status "todo") ;; => true
(s/valid? ::status "hold") ;; => false

(defmacro with-conformer
  [bind & body]
  `(s/conformer
    (fn [~bind]
      (try
        ~@body
        (catch Exception e#
          ::s/invalid)))))

(s/def ::->int
  (s/and
   ::ne-string
   (with-conformer val
     (Integer/parseInt val))))

(s/conform ::->int nil) ;; => :clojure.spec.alpha/invalid

(def ->lower
  (s/and
    string?
    (s/conformer clojure.string/lower-case)))

(s/def ::->bool
  (s/and
   ->lower
   (with-conformer val
     (case val
       ("true"  "1" "on"  "yes") true
       ("false" "0" "off" "no" ) false))))

(s/conform ::->bool "off") ;; => false

(s/def ::smart-port
  (s/or :string ::->int :num int?))

(s/conform ::smart-port 8080) ;; => [:num 8080]

(s/conform ::smart-port "8080") ;; => [:string 8080]

(s/def :conn/port ::smart-port)

(s/def ::conn
  (s/keys :req-un [:conn/port]))

(s/conform ::conn {:port 8080}) ;; => {:port [:num 8080]}
(s/conform ::conn {:port "8080"}) ;; => {:port [:string 8080]}

;; s/explain - печатает текст ошибки в стандартный поток (на экран);
;; s/explain-str - возвращает эту же информацию в виде строки;
;; s/explain-data - возвращает словарь с данными. Это самый полный отчет об ошибке.
(s/def :sample/username ::ne-string)

(s/def ::sample
  (s/keys :req-un [:sample/username]))

(s/explain ::sample {:username "test"}) ;; stdout: Success!
(s/explain ::sample {:username ""}) ;; stdout: "" - failed: not-empty in: [:username] at: [:username] spec: :rynffoll.spec/ne-string
(s/explain-str ::sample {:username ""}) ;; => "\"\" - failed: not-empty in: [:username] at: [:username] spec: :rynffoll.spec/ne-string\n"
(s/explain-data ::sample {:username "test"}) ;; => nil
(s/explain-data ::sample {:username ""})
;; => #:clojure.spec.alpha{:problems ({:path [:username], :pred clojure.core/not-empty, :val "", :via [:rynffoll.spec/sample :rynffoll.spec/ne-string], :in [:username]}), :spec :rynffoll.spec/sample, :value {:username ""}}
(s/explain-data ::sample {:username 0})
;; => #:clojure.spec.alpha{:problems ({:path [:username],
;;                                     :pred clojure.core/string?,
;;                                     :val 0,
;;                                     :via [:rynffoll.spec/sample
;;                                           :rynffoll.spec/ne-string
;;                                           :rynffoll.spec/string],
;;                                     :in [:username]}),
;;                         :spec :rynffoll.spec/sample,
;;                         :value {:username 0}}

;; :path – Логический путь валидации. Вектор ключей, где спеки чередуются с тегами-развилками. Условные спеки типа s/or записывают в этот вектор метки дочерних спек.
;; :pred – полный символ предиката, например clojure.core/string?.
;; :val – конкретное значение, которое не прошло проверку на предикат. Например, один из элементов исходного словаря.
;; :via – цепочка спек, по которым успело пройти значение от верхнего уровня к нижнему.
;; :in – физический путь к значению. Вектор ключей и индексов, который передают в функцию get-in. Если выполнить (get-in <исходные-данные> <путь>), то получим значение, которое вызвало ошибку.

(s/def ::email
  (s/and
   ::ne-string
   (partial re-matches #"(.+?)@(.+?)\.(.+?)")))

(s/def :sample/email ::email)

(s/def ::sample
  (s/keys :req-un [:sample/username
                   :sample/email]))

(s/explain-data ::sample {:username "test" :email ""})
;; => #:clojure.spec.alpha{:problems ({:path [:email],
;;                                     :pred clojure.core/not-empty,
;;                                     :val "",
;;                                     :via [:rynffoll.spec/sample
;;                                           :rynffoll.spec/email
;;                                           :rynffoll.spec/ne-string],
;;                                     :in [:email]}),
;;                         :spec :rynffoll.spec/sample,
;;                         :value {:username "test", :email ""}}

(s/explain-data ::sample {:username "test" :email "test"})
;; => #:clojure.spec.alpha{:problems ({:path [:email],
;;                                     :pred (clojure.core/partial
;;                                            clojure.core/re-matches #"(.+?)@(.+?)\.(.+?)"),
;;                                     :val "test",
;;                                     :via [:rynffoll.spec/sample
;;                                           :rynffoll.spec/email],
;;                                     :in [:email]}),
;;                         :spec :rynffoll.spec/sample,
;;                         :value {:username "test", :email "test"}}

(def spec-errors
  {::ne-string "Строка не должна быть пустой"
   ::email "Введите правильный почтовый адрес"})

(defn get-message
  [problem]
  (let [{:keys [via]} problem
        spec (last via)]
    (get spec-errors spec)))

(get-message {:via [::sample ::ne-string]})
;; => "Строка не должна быть пустой"
(get-message {:via [:rynffoll.spec/sample
                    :rynffoll.spec/email]})
;; => "Введите правильный почтовый адрес"

(def spec-errors
  {::ne-string "Строка не должна быть пустой"
   :email "Введите правильный почтовый адрес"
   :account/email "Особое сообщение для адреса отправителя"})

(def default-message
  "Исправьте ошибки в поле")

(defn get-better-message
  [problem]
  (let [{:keys [via]} problem
        spec (last via)]
    (or
     (get spec-errors spec)
     (get spec-errors
          (-> spec name keyword))
     default-message)))

(get-better-message {:via [:rynffoll.spec/sample
                           :rynffoll.spec/email]})
;; => "Введите правильный почтовый адрес"
(get-better-message {:via [:rynffoll.spec/sample
                           :rynffoll.spec/account/email]})
;; => "Особое сообщение для адреса отправителя"

(s/def :user/status
  (s/and
   ->lower
   (with-conformer val
     (case val
       "active" :USER_ACTIVE
       "pending" :USER_PENDING))))

(s/def ::user
  (s/cat :id ::->int
         :email ::email
         :status :user/status))

(s/conform ::user ["1" "test@test.com" "active"])
;; => {:id 1, :email "test@test.com", :status :USER_ACTIVE}

(s/def ::blocked
  (s/and
   ->lower
   (s/conformer
    #(= % "blocked"))))

(s/def ::user
  (s/cat :blocked (s/? ::blocked)
         :id ::->int
         :email ::email
         :status :user/status))

(s/conform ::user ["1" "test@test.com" "active"])
;; => {:id 1, :email "test@test.com", :status :USER_ACTIVE}

(s/conform ::user ["Blocked" "1" "test@test.com" "active"])
;; => {:blocked true, :id 1, :email "test@test.com", :status :USER_ACTIVE}

(s/def ::users
  (s/coll-of ::user))

(def user-data
  [["1" "test@test.com" "active"]
   ["Blocked" "2" "joe@doe.com" "pending"]])

(s/conform ::users user-data)
;; => [{:id 1, :email "test@test.com", :status :USER_ACTIVE} {:blocked true, :id 2, :email "joe@doe.com", :status :USER_PENDING}]

(defn get-ini-lines
  [path]
  (with-open [src (io/reader path)]
    (doall (line-seq src))))

(defn comment?
  [line]
  (str/starts-with? line "#"))

(defn clear-ini-lines
  [lines]
  (->> lines
       (filter (complement str/blank?))
       (filter (complement comment?))))

(s/def :ini/title
  (s/and
   #(str/starts-with? % "[")
   #(str/ends-with? % "]")
   (with-conformer val
     (subs val 1 (dec (count val))))))

(s/def :ini/field
  (with-conformer val
    (let [[key val :as pair] (str/split val #"=" 2)]
      (if (and key val)
        pair
        ::s/invalid))))

(comment

  (str/split "a=b=c" #"=" 2)

  )

(s/def ::->ini-config
  (s/and
   (s/conformer clear-ini-lines)
   (s/* (s/cat :title :ini/title :fields (s/* :ini/field)))))

(defn parse
  [path]
  (let [lines (get-ini-lines path)]
    (s/conform ::->ini-config lines)))

(parse "resources/spec/config.ini")
;; => [{:title "db", :fields [["url" "jdbc:postgresql://localhost:5432/postgres"] ["user" "postgres"] ["password" "postgres"]]} {:title "server", :fields [["port" "3000"]]}]

(defn remap-ini-data
  [data-old]
  (reduce
   (fn [data-new entry]
     (let [{:keys [title fields]} entry]
       (assoc data-new title (into {} fields))))
   {}
   data-old))

(s/def :db/url ::ne-string)
(s/def :db/user ::ne-string)
(s/def :db/password ::ne-string)

(s/def ::db
  (s/keys :req-un [:db/url
                   :db/user
                   :db/password]))

(s/def :server/port ::->int)

(s/def ::server
  (s/keys :req-un [:server/port]))

(s/def ::ini-config
  (s/keys :req-un [::db
                   ::server]))

(s/def ::->ini-config
  (s/and
   (s/conformer clear-ini-lines)
   (s/* (s/cat :title :ini/title :fields (s/* :ini/field)))
   (s/conformer remap-ini-data)
   (s/conformer walk/keywordize-keys)
   ::ini-config))

(parse "resources/spec/config.ini")
;; => {:db {:url "jdbc:postgresql://localhost:5432/postgres", :user "postgres", :password "postgres"}, :server {:port 3000}}

(s/def ::date-range-args
  (s/and
   (s/cat :start inst? :end inst?)
   (fn [{:keys [start end]}]
     (<= (compare start end) 0))))

(s/valid? ::date-range-args [#inst "2019" #inst "2020"])
;; => true
(s/valid? ::date-range-args [#inst "2020" #inst "2019"])
;; => false

(defn date-range-sec
  "Return a difference between two dates in seconds."
  [^Date date1 ^Date date2]
  (quot (- (.getTime date2)
           (.getTime date1))
        1000))

(date-range-sec #inst "2019-01-01" #inst "2019-01-02")
;; => 86400

(s/fdef date-range-sec
  :args (s/cat :start inst? :end inst?)
  :ret int?)

(instrument `date-range-sec)

(time
 (dotimes [n 10000]
   (date-range-sec #inst "2019" #inst "2020")))
;; "Elapsed time: 91.079423 msecs"

(defn date-range-sec-orig
  "Return a difference between two dates in seconds."
  [^Date date1 ^Date date2]
  (quot (- (.getTime date2)
           (.getTime date1))
        1000))

(time
 (dotimes [n 10000]
   (date-range-sec-orig #inst "2019" #inst "2020")))
;; "Elapsed time: 2.733342 msecs"

(expound/expound string? 1)
;; -- Spec failed --------------------
;;
;;   1
;;
;; should satisfy
;;
;;   string?
;;
;; -------------------------
;; Detected 1 error

(s/def ::id string?)
(s/def ::name string?)
(s/def ::street string?)
(s/def ::city #{:tre :hki})
(s/def ::address (s/keys :req-un [::street ::city]))
(s/def ::user (s/keys :req-un [::id ::name ::address]))

(json-schema/transform ::user)
;; {:type "object",
;;  :properties {"id" {:type "string"},
;;               "name" {:type "string"},
;;               "address" {:type "object",
;;                          :properties {"street" {:type "string"},
;;                                       "city" {:enum [:tre :hki]}},
;;                          :required ["street" "city"],
;;                          :title "rynffoll.spec/address"}},
;;  :required ["id" "name" "address"],
;;  :title "rynffoll.spec/user"}

(s/def ::id string?)
(s/def ::name string?)
(s/def ::street string?)
(s/def ::city #{:tre :hki})
(s/def ::address (s/keys :req-un [::street ::city]))
(s/def ::user (s/keys :req-un [::id ::name ::address]))

(def swagger-spec
  (swagger/swagger-spec
   {:swagger "2.0"
    :info {:version "1.0.0"
           :title "Sausages"
           :description "Sausage description"
           :termsOfService "http://helloreverb.com/terms/"
           :contact {:name "My API Team"
                     :email "foo@example.com"
                     :url "http://www.metosin.fi"}
           :license {:name "Eclipse Public License"
                     :url "http://www.eclipse.org/legal/epl-v10.html"}}
    :tags [{:name "user"
            :description "User stuff"}]
    :paths {"/api/ping" {:get {:responses {:default {:description ""}}}}
            "/user/:id" {:post {:summary "User Api"
                                :description "User Api description"
                                :tags ["user"]
                                ::swagger/parameters {:path (s/keys :req [::id])
                                                      :body ::user}
                                ::swagger/responses {200 {:schema ::user
                                                          :description "Found it!"}
                                                     404 {:description "Ohnoes."}}}}}}))

(def swagger-spec-json (json/generate-string swagger-spec))

(println swagger-spec-json)

;; https://editor.swagger.io/


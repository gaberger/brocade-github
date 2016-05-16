(ns github.core
  (:require [clojure.pprint :refer [pprint]]
            [tentacles.repos :as repos]
            [tentacles.users :as users]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [clojure.data.json :as json]
            [environ.core :refer [env]]
            [taoensso.timbre :as timbre]
            [clojure.tools.cli :refer [parse-opts]]))


(timbre/refer-timbre)

(def sites [{:user "brocade" :repo "brocade"}
            {:user "BRCDcomm" :repo "BRCDcomm"}])

(def cli-options
  [["-h" "--help"]])

(defn exit [exit text]
  (fatal text)
  (System/exit exit))


(defn edn->json
  "Convert EDN to JSON"
  [coll]
  (map #(json/pprint %) coll)
  )

(defn get-repos
  "Return collection of repos based on user/org"
  [user token]
  (info "Get Repositories for use" user)
  (let [repos (repos/user-repos user {:oauth-token token})]
    ;(if (contains? repos :status)
    (if (instance? clojure.lang.PersistentArrayMap repos)
      (cond
        (= (:status repos) 401) (exit 1 (str "Bad API call, check token" (:status repos)))
        (= (:status repos) 404) (exit 1 (str "Repo Not Found Error:" (:status repos)))
        )
      (pmap #(select-keys % [:name :html_url :forks :description])
            repos))))

(defn get-lastcommits
  "Return a collection containing the sha, date, name and url of lastcommit for user and a collection of repos"
  [user repo token]
  (info "Get Commits" repo)
  (let [lastcommit (first (repos/commits user repo {:oauth-token token}))
        sha (:sha lastcommit)
        date (get-in lastcommit [:commit :author :date])
        author (get-in lastcommit [:commit :author :name])
        email (get-in lastcommit [:commit :author :email])
        ]
    {:lastcommit sha :date date :author author :email email}
    ))

(defn get-repo-contributors
  "Return a list of repo contributors"
  [user repo token]
  (map #(:login %) (repos/contributors user repo {:oauth-token token})
       ))

(defn get-contributors-email
  "Take a list of users and return a map of user names and emails"
  [users token]
  (reduce (fn [output user]
            (let [record (users/user user {:oauth-token token})
                  email (:email record)
                  company (:company record)
                  ]
              (conj output {:email email :name user :company company})
              )
            )
          []
          users
          ))

(defn get-members
  "Return a list of collaborators"
  [user repos token]
  (info "Get Collaborators")
  (map #(repos/collaborators user (:name %) {:oauth-token token}) repos)
  )

(defn assemble-report
  [repo contribs commits]
  (let [a (assoc {} :commiters contribs)
        b (conj repo a commits)
        r (assoc {} :repo b)]
    r
    )
  )

(def repo-filter (vec ["pybvc-archive-DO-NOT-USE", "brocade-github", "brocade.github.io"]))

(defn re-remove
  [f-coll coll]
  (if (empty? f-coll)
    coll
    (let [lazy-res (remove #(= (:name %) (first f-coll)) coll)]
      (recur (rest f-coll) lazy-res)))
  )


(defn filter-repos
  [repo token]
  (let [repos (into [] (get-repos repo token))]
    (re-remove repo-filter repos)
    ))

(defn git-report
  "Return a collection of repo commits and contributors"
  [site token]
  ;(let [repos (get-repos site token)
  (info "Starting github repo collection on" site)
  (let [repos (filter-repos site token)
        report
        (map (fn [repo]
               (let [name (:name repo)
                     c (get-repo-contributors site name token)
                     contribs (get-contributors-email c token)
                     commits (get-lastcommits site name token)

                     ]

                 (assemble-report repo contribs commits))
               )
             repos
             )]
    ; (assoc {} :app/repo (into [] report))
    (into [] report)

    ))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (clojure.tools.cli/parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 "help: lein run <repo>")
      (not= (count arguments) 1) (exit 1 "args: lein run <repo>")
      errors (exit 1 "error: lein run <repo>"))

    (let [token (env :gittoken)
          repo (first arguments)]
      (if token
        (spit "resources/public/app/app.edn" (git-report repo token))
        (println "Please set env variable gittoken"))
      ))

  )
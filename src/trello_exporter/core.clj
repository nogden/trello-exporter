(ns trello-exporter.core
  (:require [cheshire.core :as json]
            [clojure.string :as string]
            [clojure.data.csv :as csv]))

(defn columns [board]
  (into {} (map (juxt :id :name)) (:lists board)))

(defn members [board]
  (into {} (map (juxt :id :fullName)) (:members board)))

(defn checklists [board]
  (letfn [(complete-incomplete [checklist]
            (->> checklist
                 :checkItems
                 (map #(select-keys % [:name :state]))
                 (group-by :state)))]
    (into {} (map (juxt :id complete-incomplete)) (:checklists board))))

(defn comments [board]
  (let [parse-time #(java.time.ZonedDateTime/parse %)
        comments (comp
                  (filter #(= "commentCard" (:type %)))
                  (map #(hash-map
                         :card (get-in % [:data :card :id])
                         :comment {:author (get-in % [:memberCreator :fullName])
                                   :time   (-> % :date parse-time)
                                   :text   (get-in % [:data :text])})))]
    (->> board
         :actions
         (sequence comments)
         (group-by :card))))

(defn created-at [board]
  (-> board
      :id
      (subs 0 8)
      (Integer/parseInt 16)
      java.time.Instant/ofEpochSecond
      (java.time.ZonedDateTime/ofInstant (java.time.ZoneId/of "Europe/London"))
      (.format java.time.format.DateTimeFormatter/ISO_LOCAL_DATE)))

(defn merged-card-checklists [card all-checklists]
  (let [lists-on-card (:idChecklists card)
        merged-list (reduce
                     (fn [acc c] (merge-with concat acc (all-checklists c)))
                     {"complete" [] "incomplete" []}
                     lists-on-card)]
    {:complete (string/join \newline (map :name (get merged-list "complete")))
     :incomplete (string/join \newline (map :name (get merged-list "incomplete")))}))

(defn formatted-card-comments [card all-comments]
  (letfn [(human-friendly-time [t]
            (.format (:time t)
                     java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME))]
    (->> card
         :id
         all-comments
         (map :comment)
         (sort-by :time)
         (map #(string/join \newline [(human-friendly-time %)
                                      (:author %)
                                      (:text %)])))))

(defn cards [board {:keys [checklists comments columns members]}]
  (into []
        (comp
         (map #(assoc % :list (get columns (:idList %))))
         (map #(update % :labels (fn [ls] (string/join ", " (map :name ls)))))
         (map #(assoc % :members (->> % :idMembers (map members) (string/join ", "))))
         (map #(assoc % :checklist (merged-card-checklists % checklists)))
         (map #(assoc % :comments (formatted-card-comments % comments)))
         (map #(assoc % :created (created-at %)))
         (map #(assoc % :state ({true "Archived" false "Open"} (:closed %))))
         (map #(select-keys % [:id :list :name :state :desc :members :labels
                               :shortUrl :checklist :comments :actions :created])))
        (:cards board)))

(defn table [cards]
  (let [max-comments   (reduce
                        (fn [acc card] (max acc (count (:comments card))))
                        0
                        cards)
        comment-fields (map #(str "Comment " (inc %)) (range max-comments))
        header         (concat ["State" "List" "Card" "Created" "Labels"
                                "Members" "Description" "URL"
                                "Incomlete Items" "Complete Items"]
                               comment-fields)
        rows (map #(let [comments (:comments %)
                         empties (repeat (- max-comments (count comments)) "")]
                     (concat ((juxt :state :list :name :created :labels :members
                                    :desc :shortUrl (comp :incomplete :checklist)
                                    (comp :complete :checklist)) %)
                             comments
                             empties))
                  cards)]
    (concat [header] rows)))

(defn board->table [board]
  (let [board-columns    (columns board)
        board-checklists (checklists board)
        board-members    (members board)
        board-comments   (comments board)
        board-cards      (cards board {:checklists board-checklists
                                       :comments   board-comments
                                       :columns    board-columns
                                       :members    board-members})]
    (table board-cards)))

(defn export-to-csv [board-path csv-path]
  (let [board (json/parse-stream
               (clojure.java.io/reader board-path)
               #(keyword %))
        table (board->table board)]
    (with-open [file (clojure.java.io/writer csv-path)]
      (csv/write-csv file table))))

(comment
  (export-to-csv "board.json" "sectoral.csv")

  (def trello-board
    (json/parse-stream (clojure.java.io/reader "board.json") #(keyword %)))
  )

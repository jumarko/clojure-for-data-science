(ns clojure-for-data-science.ch01.data
  (:require [clojure.java.io :as io]
            [incanter.core :as i]
            [incanter.excel :as xls]
            [incanter.charts :as c]
            [incanter.distributions :as d]
            [incanter.stats :as s]))

(def uk-data-path "UK2010.xls")

(defmulti load-data identity)

(defmethod load-data :uk [_]
  (-> (io/resource uk-data-path)
    (str)
    (xls/read-xls)))

(def uk-data (load-data :uk))

(defn ex-1-1 []
  (i/col-names uk-data))

;; let's examine the column names
(ex-1-1)

;; The first six columns are:
;;   Press Association Reference: This is a number identifying the constituency (voting district, represented by one MP)
;;   Constituency Name; This is the common name given to the voting district
;;   Region: This is the geographic region of the UK where the constituency is based
;;   Election Year: This is the year in which the election was held
;;   Electorate: This is the total number of people eligible to vote in constituency
;;   Votes: This is the total number of votes cast

;; let's select only one column
(defn select-election-year []
  (i/$ "Election Year" uk-data))
(def ex-1-2 select-election-year)
(ex-1-2)

;; output is too long -> show only distinct values
(defn ex-1-3 []
  (distinct (select-election-year)))
(ex-1-3)

;; notice the "nil" value in result -> this is unexpected and may indicate a problem with our data
;; let's count the nil values
(defn ex-1-4 []
  (frequencies (select-election-year)))
(ex-1-4)

;;; Data scrubbing

;; let's select only row with "nil" Election year
;; Notice that this is very similar to a WHERE sql clause
;; Supported query operators
;;   :$gt
;;   :$lt
;;   :$gte
;;   :$lte
;;   :$eq
;;   :$ne
;;   :$in
;;   :$nin
;;   :$fn
(i/query-dataset uk-data
  {"Election Year" {:$eq nil}})

;; we can return row as a map
(defn ex-1-5 []
  (->> (i/query-dataset uk-data
         {"Election Year" {:$eq nil}})
    i/to-map))
(ex-1-5)

;; notice that all columns in row were nil except the one
;; That is "summary total" and should be removed from data
(defmethod load-data :uk-scrubbed [_]
  (i/$where {"Election Year" {:$ne nil}} uk-data))
(def uk-data-scrubbed (load-data :uk-scrubbed))

;;; Descriptive statistics
;;; - numbers to summarize and describe data
;;; Two of the most basic statistics are "mean" and "variance"
(def uk-electorate
  (->> uk-data-scrubbed
    (i/$ "Electorate")))
(defn ex-1-6 []
  (count uk-electorate))
(ex-1-6)

;; mean
(defn mean [xs]
  (/ (reduce + xs)
    (count xs)))
(defn ex-1-7 []
  (mean uk-electorate))
(ex-1-7)

;; In fact, Incanter already includes a function "mean" in "incanter.stats" namespace

;; median
;; - middle value in ordered sequence (or mean of two middle values)
;; Notice, that incanter also has function for median in stats package
(defn median [xs]
  (let [n (count xs)
        mid (int (/ n 2))]
    (if (odd? n)
      (nth (sort xs) mid)
      (->> (sort xs)
        (drop (dec mid))
        (take 2)
        (mean)))))
(defn ex-1-8 []
  (median uk-electorate))
(ex-1-8)

;; Variance
;; - "spread" about the mean of the sequence
(defn variance [xs]
  (let [x-bar (mean xs)
        n (count xs)
        square-deviation-fn (fn [x]
                              (i/sq (- x x-bar)))]
    (mean (map square-deviation-fn xs))))

;; notice that units of variance are also "squared"
;; -> so the units of the variance of the UK electorate are "people squared"
;; we can make it more natural by taking a square root of variance
;; -> standard deviation
(defn standard-deviation [xs]
  (i/sqrt (variance xs)))
(defn ex-1-9 []
  (standard-deviation uk-electorate))
(ex-1-9)

;;; Quantiles

;; to get a richer picture of a sequence of numbers let's calculate five-number summary:
;; - 0, 0.25, 0.5, 0.75, and 1.0 quantiles
(defn quantile [q xs]
  (let [n (dec (count xs))
        i (-> (* n q)
            (+ 1/2)
            int)]
    (nth (sort xs) i)))
(defn ex-1-10 []
  (let [xs uk-electorate
        f (fn [q]
            (quantile q xs))]
    (map f [0 1/4 1/2 3/4 1])))
(ex-1-10)

;;; Binning data
;;; => divide the range of values into a number of consecutive, equally-sized, smaller bins
;;;    to get a broad sense of the structure of the continuous data

(defn bin [n-bins xs]
  (let [min-x (apply min xs)
        max-x (apply max xs)
        range-x (- max-x min-x)
        bin-fn (fn [x]
                 (-> x
                   (- min-x)
                   (/ range-x)
                   (* n-bins)
                   int
                   (min (dec n-bins))))]
    (map bin-fn xs)))
(bin 5 (range 15))

;; once we've binned the values we can then use the frequencies function once again
;; to count the number of points in each bin
;; the count seem to rise up towards the median and then down again
(defn ex-1-11 []
  (->> uk-electorate
    (bin 10)
    frequencies))
(ex-1-11)

;;; Histograms
;;; incanter.charts contains histogram function

(defn ex-1-12 []
  (-> uk-electorate
    (c/histogram)
    (i/view)))
(ex-1-12)

;; we can configure the number of bins data is segmented into by passing the keyword :nbins
(defn ex-1-13 []
  (-> uk-electorate
    (c/histogram :nbins 200)
    i/view))
(ex-1-13)

(defn ex-1-14 []
  (-> uk-electorate
    (c/histogram :x-label "UK electorate"
      :nbins 20)
    i/view))
(ex-1-14)

;;; Normal Distribution

;; Clojure's random function generates numbers with "uniform" distribution
(defn ex-1-15 []
  (let [xs (->> (repeatedly rand)
             (take 10000))]
    (-> (c/histogram xs
          :x-label "Uniform distribution"
          :nbins 20)
      i/view)))
(ex-1-15)

;; If we generate a histogram of the means of sequences of numbers, distribution looks rather different
;; => Central limit theorem
(defn ex-1-16 []
  (let [xs (->> (repeatedly rand)
             (partition 10)
             (map mean)
             (take 10000))]
    (-> (c/histogram xs
          :x-label "Distribution of means"
          :nbins 20)
      i/view)))
(ex-1-16)

;; Incanter’s distribution namespace provides functions for generating sampels efficiently from a variety of distributions
(defn ex-1-17 []
  (let [distribution (d/normal-distribution)
        ;; there is also incanter.stats/sample-normal function
        xs (->> (repeatedly #(d/draw distribution))
             (take 10000))]
    (-> (c/histogram xs
          :x-label "Normal distribution")
      i/view)))
(ex-1-17)

;;; Poincare's baker
;;; Let's model a honest and dishonest baker

(defn honest-baker [mean sd]
  (let [distribution (d/normal-distribution mean sd)]
    (repeatedly #(d/draw distribution))))
(defn ex-1-18 []
  (-> (take 10000 (honest-baker 1000 30))
    (c/histogram :x-label "Honest baker"
      :nbins 25)
    i/view))
(ex-1-18)

;; dishonest baker sells only the heaviest of his loaves
;; the histogram indicates a skewed normal distribution
(defn dishonest-baker [mean sd]
  (let [distribution (d/normal-distribution mean sd)]
    (->> (repeatedly #(d/draw distribution))
      (partition 13)
      (map (partial apply max)))))
(defn ex-1-19 []
  (-> (take 10000 (dishonest-baker 950 30))
    (c/histogram :x-label "Dishonest baker"
      :nbins 25)
    i/view))
(ex-1-19)

;;; Skewness

(defn ex-1-20 []
  (let [weights (take 10000 (dishonest-baker 950 30))]
    {:mean (mean weights)
     :median (median weights)
     :skewness (s/skewness weights)}))
(ex-1-20)

;; QQ-plot for honest & dishonest baker
(defn ex-1-21 []
  (->> (honest-baker 1000 30)
    (take 10000)
    c/qq-plot
    i/view)
  (->> (dishonest-baker 950 30)
    (take 10000)
    c/qq-plot
    i/view))

(ex-1-21)

;;; Comparative visualizations
;;; We can use Incanter’s Q-Q plot chars if we want to compare two or more empirical distributions with each other

;; box plots (box and whisker plots)
(defn ex-1-22 []
  (->
    (c/box-plot (->> (honest-baker 1000 30)
                  (take 10000))
      :legend true
      :y-label "Loaf weight (g)"
      :series-label "Honest baker")
    (c/add-box-plot (->> (dishonest-baker 950 30)
                      (take 10000))
      :series-label "Dishonest baker")
    (i/view)))
(ex-1-22)

;; CDF - comparative distribution functions
(defn ex-1-23 []
  (let [sample-honest (->> (honest-baker 1000 30)
                        (take 1000))
        sample-dishonest (->> (dishonest-baker 950 30)
                           (take 1000))
        ecdf-honest (s/cdf-empirical sample-honest)
        ecdf-dishonest (s/cdf-empirical sample-dishonest)]
    (-> (c/xy-plot sample-honest (map ecdf-honest sample-honest)
          :x-label "Loaf Weight"
          :y-label "Probability"
          :legend true
          :series-label "Honest baker")
      (c/add-lines sample-dishonest
        (map ecdf-dishonest sample-dishonest)
        :series-label "Dishonest baker")
      i/view)))
(ex-1-23)

;;; Visualizing electorate data
;;; Comparing electorate sequence against theoretical normal distribution CDF (we can use Incanter' s/cdf-normal function)
;;; Notice we need to supply measured mean and standard deviation from the electorate data (default mean is 0, default stand. dev. is 1)


(defn ex-1-24 []
  (let [ecdf (s/cdf-empirical uk-electorate)
        fitted (s/cdf-normal uk-electorate
                 :mean (s/mean uk-electorate)
                 :sd (s/sd uk-electorate))]
    (-> (c/xy-plot uk-electorate fitted
          :x-label "Electorate"

          :y-label "Probability"
          :seriels-label "Fitted"
          :legend true)
      (c/add-lines uk-electorate (map ecdf uk-electorate)
        :series-label "Empirical")
      i/view)))

(ex-1-24)

;; let's use Q-Q plot for comparing our data against theoretical normal distribution too
(defn ex-1-25 []
  (->> uk-electorate
    c/qq-plot
    i/view))
(ex-1-25)

;;; Adding derived columns
;;; - adding new columns while supplying the function to compute data
;;; - we want to find out percentage of the electorate voted for either the Conservative or Liberal Democrat parties

(defn ex-1-26 []
  (i/add-derived-column :victors [:Con :LD] + uk-data-scrubbed))
;; if we try to evalute following we'll get ClassCastException java.lang.String cannot be cast to java.lang.Number
;; because some values in "Con" or "LD" columns are strings
#_(ex-1-26)

;; let's find the String data
(->> uk-data-scrubbed
  (i/$ "Con")
  (map type)
  frequencies)

(->> uk-data-scrubbed
  (i/$ "LD")
  (map type)
  frequencies)

;; let's use i/$where function to find corrupted data
(defn ex-1-27 []
  (->> uk-data-scrubbed
    (i/$where #(not-any? number? [(% "Con") (% "LD")]))
    (i/$ [:Region :Electorate :Con :LD])))
(ex-1-27)
;; => it looks like that reason for these fields being blank is that candidates were not put forward in the corresponding constituencies
;; Let's filter these rows out (instead of setting them zero), since it wasn't even possible for voters
;; to choose a Liberal Democrat or Conservative candidate in these constituencies.
(defmethod load-data :uk-victors [_]
  (->> uk-data-scrubbed
    (i/$where {:Con {:$fn number?} :LD {:$fn number?}})
    (i/add-derived-column :victors [:Con :LD] +)
    (i/add-derived-column :victors-share [:victors :Votes] /)
    (i/add-derived-column :turnout [:Votes :Electorate] /)))

(def uk-victors-data (load-data :uk-victors))

(defn ex-1-28 []
  (->> uk-victors-data
    (i/$ :victors-share)
    c/qq-plot
    i/view))

;;; Comparative visualizations of electorate data - p. 43
(defmethod load-data :ru [_]
  (i/conj-rows
    (-> (io/resource "Russia2011_1of2.xls")
      str
      xls/read-xls)
    (-> (io/resource "Russia2011_2of2.xls")
      str
      xls/read-xls)))

(def ru-data (load-data :ru))

;; Let's see what the Russia data column names are
(defn ex-1-29 []
  (i/col-names ru-data))
(ex-1-29)
;; => ["Code for district" "Number of the polling district (unique to state, not overall)" "Name of district" "Number of voters included in voters list" "The number of ballots received by the precinct election commission" "The number of ballots issued to voters who voted early" "The number of ballots issued to voters at the polling" "The number of ballots issued to voters outside the polling station" "The number of canceled ballots" "The number of ballots in mobile ballot boxes" "The number of ballots in the stationary ballot boxes" "Number of invalid ballots" "Number of valid ballots" "The number of absentee ballots received by the precinct election commission" "The number of absentee ballots issued to voters at a polling station" "The number of voters who voted with absentee ballots at a polling station" "The number of the unused absentee ballots" "The number of absentee ballots issued to voters of the territorial election commission" "Number of lost absentee ballots" "The number of lost ballots" "The number of ballots not recorded after being obtained" "A Just Russia" "Liberal Democratic Party of Russia" "Patriots of Russia" "Communist Party" "Russian United Democratic Party \"Yabloko\"" "United Russia" "Right Cause"]

;; Notice that column names are descriptive but long than we might want to type
;; We also want to label the columns that represent the same attributes as we've seen in UK data to be labeled the same
(defmethod load-data :ru-victors [_]
  (->> ru-data
    (i/rename-cols
      {"Number of voters included in voters list" :electorate
       "Number of valid ballots" :valid-ballots
       "United Russia" :victors})
    (i/add-derived-column :victors-share [:victors :valid-ballots] i/safe-div)
    (i/add-derived-column :turnout [:valid-ballots :electorate] /)))
(def ru-victors-data (load-data :ru-victors))

;; let's plot the Russian election data to see how it compares to UK data
(defn ex-1-30 []
  (-> (i/$ :turnout ru-victors-data)
    (c/histogram :x-label "Russia turnout"
      :nbins 20)
    i/view))
(ex-1-30)

;; and QQ-plot
(defn ex-1-31 []
  (->> ru-victors-data
    (i/$ :turnout)
    c/qq-plot
    i/view))
(ex-1-31)


;;; Let's compare the UK and Russia data side by side
;;; We need to use probability mass function and normalization
;;; to compensate for the facts that sizes and number of voting districts are very different in UK and Russia

(defn as-pmf [bins]
  (let [histogram (frequencies bins)
        total (reduce + (vals histogram))]
    (->> histogram
      (map (fn [[k v]]
             [k (/ v total)]))
      (into {}))))

(defn ex-1-32 []
  (let [n-bins 40
        uk (->> uk-victors-data
             (i/$ :turnout)
             (bin n-bins)
             (as-pmf))
        ru (->> ru-victors-data
             (i/$ :turnout)
             (bin n-bins)
             (as-pmf))]
    (-> (c/xy-plot (keys uk) (vals uk)
          :series-label "UK"
          :legend true
          :x-label "Turnout Bins"
          :y-label "Probability")
      (c/add-lines (keys ru) (vals ru)
        :series-label "Russia")
      i/view)))

(ex-1-32)


;;; Scatter plots
;;; Let's visualize the relationship between turnout and the proportion of votes for the winning party
(defn ex-1-33 []
  (-> (c/scatter-plot (i/$ :turnout uk-victors-data) (i/$ :victors-share uk-victors-data)
        :x-label "Turnout"
        :y-label "Victor's share")
    i/view))
(ex-1-33)

;; The same scatter plot for Russia
(defn ex-1-34 []
  (-> (c/scatter-plot (i/$ :turnout ru-victors-data) (i/$ :victors-share ru-victors-data)
        :x-label "Turnout"
        :y-label "Victor's share")
    i/view))
(ex-1-34)

;; Scatter transparency
;; Since sheer volume of data obscures the scatter plot for Russia
;; we want to use semi-transparent points
(defn ex-1-35 []
  (let [data (s/sample ru-victors-data :size 10000)]
    (-> (c/scatter-plot (i/$ :turnout data) (i/$ :victors-share data)
          :x-label "Turnout"
          :y-label "Victor's Share")
      (c/set-alpha 0.05)
      (i/view))))
(ex-1-35)



;;; Finnaly, for a comparison let's visualize the results of Slovak election 2016 data
;;; This is not an example from the book.
;;; The data has can be found at https://sites.google.com/site/marekhlavac/slovak_election_data_project.
;;; [Hlavac, Marek. 2016. "Results of the 2016 Parliamentary Election in the Slovak Republic - Data Set." Slovak Election Data Project, 27 March 2016.]

(defmethod load-data :sk [_](-> (io/resource "slovakia_parl_election_2016.xlsx")
                              str
                              xls/read-xls))

(def sk-data (load-data :sk))

(i/col-names sk-data)
;; => ["region_code" "region" "ward_code" "ward" "district_code" "district" "municipality_code" "municipality" "precinct" "voters_eligible" "ballots_given_out" "voter_turnout_pct" "voted_in_person" "voted_in_person_pct" "voted_by_mail" "voted_by_mail_pct" "ballots_valid" "ballots_valid_pct" "ballots_valid_tip" "ballots_valid_pct_tip" "ballots_preferential_tip" "ballots_preferential_pct_tip" "ballots_valid_sms" "ballots_valid_pct_sms" "ballots_preferential_sms" "ballots_preferential_pct_sms" "ballots_valid_olano" "ballots_valid_pct_olano" "ballots_preferential_olano" "ballots_preferential_pct_olano" "ballots_valid_kanik" "ballots_valid_pct_kanik" "ballots_preferential_kanik" "ballots_preferential_pct_kanik" "ballots_valid_sanca" "ballots_valid_pct_sanca" "ballots_preferential_sanca" "ballots_preferential_pct_sanca" "ballots_valid_kollar" "ballots_valid_pct_kollar" "ballots_preferential_kollar" "ballots_preferential_pct_kollar" "ballots_valid_szs" "ballots_valid_pct_szs" "ballots_preferential_szs" "ballots_preferential_pct_szs" "ballots_valid_spolocne" "ballots_valid_pct_spolocne" "ballots_preferential_spolocne" "ballots_preferential_pct_spolocn" "ballots_valid_mkda" "ballots_valid_pct_mkda" "ballots_preferential_mkda" "ballots_preferential_pct_mkds" "ballots_valid_vzdor" "ballots_valid_pct_vzdor" "ballots_preferential_vzdor" "ballots_preferential_pct_vzdor" "ballots_valid_most" "ballots_valid_pct_most" "ballots_preferential_most" "ballots_preferential_pct_most" "ballots_valid_sns" "ballots_valid_pct_sns" "ballots_preferential_sns" "ballots_preferential_pct_sns" "ballots_valid_odvaha" "ballots_valid_pct_odvaha" "ballots_preferential_odvaha" "ballots_preferential_pct_odvaha" "ballots_valid_kss" "ballots_valid_pct_kss" "ballots_preferential_kss" "ballots_preferential_pct_kss" "ballots_valid_sdku" "ballots_valid_pct_sdku" "ballots_preferential_sdku" "ballots_preferential_pct_sdku" "ballots_valid_smer" "ballots_valid_pct_smer" "ballots_preferential_smer" "ballots_preferential_pct_smer" "ballots_valid_kdh" "ballots_valid_pct_kdh" "ballots_preferential_kdh" "ballots_preferential_pct_kdh" "ballots_valid_skok" "ballots_valid_pct_skok" "ballots_preferential_skok" "ballots_preferential_pct_skok" "ballots_valid_lsns" "ballots_valid_pct_lsns" "ballots_preferential_lsns" "ballots_preferential_pct_lsns" "ballots_valid_siet" "ballots_valid_pct_siet" "ballots_preferential_siet" "ballots_preferential_pct_siet" "ballots_valid_smk" "ballots_valid_pct_smk" "ballots_preferential_smk" "ballots_preferential_pct_smk" "ballots_valid_pd" "ballots_valid_pct_pd" "ballots_preferential_pd" "ballots_preferential_pct_pd" "ballots_valid_sas" "ballots_valid_pct_sas" "ballots_preferential_sas" "ballots_preferential_pct_sas"]

;; find the place with zero eligible voters
#_(->> sk-data
    (i/$where #(zero? (% "voters_eligible")))
    (i/$ "precinct"))

(defmethod load-data :sk-victors [_]
  (->> sk-data
    
    (i/rename-cols
      {"voters_eligible" :electorate
       "ballots_valid" :valid-ballots
       "ballots_valid_smer" :victors})
    (i/add-derived-column :victors-share [:victors :valid-ballots] i/safe-div)
    ;; filter zero voters ("Borovce 01")
    (i/$where #(pos? (% :electorate)))
    (i/add-derived-column :turnout [:valid-ballots :electorate] i/safe-div)))

(def sk-victors-data (load-data :sk-victors))


;; let's plot the Slovak election data to see how it compares to other data
(defn plot-sk-turnout []
  (-> (i/$ :turnout sk-victors-data)
    (c/histogram :x-label "Slovakia turnout"
      :nbins 20)
    i/view))
(plot-sk-turnout)


;; and QQ-plot
(defn sk-qq-plot []
  (->> sk-victors-data
    (i/$ :turnout)
    c/qq-plot
    i/view))
(sk-qq-plot)


;;; Let's compare the UK and Russia data side by side
;;; We need to use probability mass function and normalization
;;; to compensate for the facts that sizes and number of voting districts are very different in UK and Russia

(defn uk-vs-sk []
  (let [n-bins 40
        uk (->> uk-victors-data
             (i/$ :turnout)
             (bin n-bins)
             (as-pmf))
        sk (->> sk-victors-data
             (i/$ :turnout)
             (bin n-bins)
             (as-pmf))]
    (-> (c/xy-plot (keys uk) (vals uk)
          :series-label "UK"
          :legend true
          :x-label "Turnout Bins"
          :y-label "Probability")
      (c/add-lines (keys sk) (vals sk)
        :series-label "Slovakia")
      i/view)))

(->> sk-victors-data
  (i/$ :turnout)
  (bin 40)
  )

(uk-vs-sk)

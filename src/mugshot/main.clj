(ns mugshot.main
  (:require [stylo.svg :as svg]
            [stylo.shape :as shape]
            [stylo.style.mu :as mu]
            [forge.proto :as f]
            [hiccup.core :refer [html]]
            [hiccup.page :as page]
            [hickory.core :as hickory]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]
            [clojure.java.shell :refer [sh]]))

(defn seconds->timestamp [s]
  (let [hh (format "%02d" (int (/ s 3600)))
        mm (format "%02d" (int (/ (rem s 3600) 60)))
        ss (format "%02d" (rem s 60))]
    (apply str (interpose ":" [hh mm ss]))))

(defn clean-name [name]
  (-> name
      (s/lower-case)
      (s/replace #"!" "")
      (s/replace #"'" "")
      (s/replace #"," "")
      (s/replace #"/" "-")
      (s/replace #"\|" "-")
      (s/replace #"\\" "-")
      (s/replace #"&" "and")
      (s/replace #" " "-")))

(defn delete [fname]
  (sh "rm" fname))

(defn yt-playlist [url start end]
  (let [output (-> (sh "youtube-dl" "-e" "-g" 
                       "--playlist-start" (str start)
                       "--playlist-end" (str end) url)
                   (:out)
                   (s/split-lines))]
    (mapv #(zipmap [:title :video-url] (drop-last %))
          (partition 3 output))))

(defn yt-url->video-data [url]
  (let [[title video-url _ descr]
        (-> (sh "youtube-dl" "-e" "-g" "--get-description" url)
            (:out)
            (s/split-lines))]
    {:title title
     :descr descr
     :video-url video-url}))

(defn parse-url [url]
  (let [[url & rest] (s/split url #"[\?&]")
        xr (map #(s/split % #"=") rest)
        keys (map keyword (into [:url] (map first xr)))
        vals (into [url] (map second xr))]
    (zipmap keys vals)))

(defn save-image [vid-url time fname]
  (sh "ffmpeg" 
      "-ss" time
      "-i" vid-url
      "-vframes" "1" 
      "-s" "1920x1080" 
      "-f" "image2" fname))

(defn img->svg-old [fname]
  (sh "./potrace" 
      "--tight"
      "--unit" "0.5"
      "--svg"
      "--flat" fname)
  (delete fname)
  (str (first (s/split fname #"\.")) ".svg"))

(defn img->svg [fname & color?]
  (let [new-fname (str (first (s/split fname #"\.")) ".svg")
        settings (if (first color?) 
                   ["color" "-p" "7" "-f" "16" "-g" "36"] 
                   ["bw"])]
    (apply sh (concat ["./vtracer"
                       "--mode" "polygon"
                       "--colormode"]
                      settings
                      ["--input" fname
                       "--output" new-fname]))
    (delete fname)
    new-fname))

(defn screenshot! [url]
  (let [urlp (parse-url url)
        data (yt-url->video-data (:url urlp))
        vid-url (:video-url data)
        name (clean-name (:title data))
        time (seconds->timestamp (read-string (:t urlp)))
        fname (str "output/" name ".png")]
    (save-image vid-url time fname)
    fname))

(defn get-paths
  [hiccup]
  (->> hiccup
       (filter vector?)
       (first)
       (tree-seq vector? rest)
       (filter vector?)
       (filter #(= :path (first %)))))

(defn split-path
  [string]
  (-> string
      (s/replace #"[M]" #(str "\n" %))
      (s/replace #"[Zz]" #(str % "\n"))
      (s/trim)
      (s/split-lines)))

(defn svg->paths
  [svg]
  (->> svg
       (hickory/parse)
       (hickory/as-hiccup)
       (get-paths)
       (mapv #(get-in % [1 :d]))
       (mapcat split-path)))

(defn get-numbers
  [l]
  (-> l
      (s/split #"[\s,ML]")
      (rest)
      (#(mapv read-string %))))

(defn add-z
  [pt]
  (conj pt 0))

(defn drop-z
  [pt]
  (into [] (take 2 pt)))

(defn ->pts
  [string]
  (-> string
      (s/trim)
      (s/replace #"[A-DF-Za-df-z]" #(str "\n" %))
      (s/triml)
      (s/split-lines)
      #_(linify)
      (#(map get-numbers %))
      (#(filter (complement empty?) %))
      (#(mapv add-z %))))

(defn bb-area
  [pts]
  (let [pts (when (= 3 (count (first pts))) (mapv drop-z pts))
        [ca cb] (f/bb-corners-2d (mapv drop-z pts))]
    (reduce * (f/v- ca cb))))

(defn along-sides?
  [pts]
  (let [[x y] (f/midpoint pts)]
    (or (< 0 x 175) (< 1500 x 1920)
        (< 0 y 175) (< 930 y 1080))))

(defn large?
  [pts]
  (> 1200000 (bb-area pts)))

(defn filter-paths
  [paths]
  (->> paths
       (filterv large?)
       (filterv (complement along-sides?))))

(defn move-to-origin
  [paths]
  (let [[mx my _] (map float (f/midpoint (apply concat paths)))]
    (into []
          (for [path paths]
            (mapv #(f/v- [mx my 0] %) path)))))

(defn resize
  [max-dim paths]
  (let [pts (apply concat paths)
        pts (if (= 3 (count (first pts))) (mapv drop-z pts) pts)
        [ca cb] (f/bb-corners-2d pts)
        [w h] (f/v- ca cb)
        sc (/ max-dim (max w h))]
    (into []
          (for [path paths]
            (mapv #(f/v* [sc sc sc] %) path)))))

(def mug-a (url->svg "https://youtu.be/IPAr7YazehQ?t=209"))
(def mug-b (url->svg "https://youtu.be/u6B0tXrIpLY?t=495"))
(def mug-c (url->svg "https://youtu.be/SgAXQRXmWMk?t=239"))

;; as of 2020-11-04
(def drawfee-extra-mug-urls
  ["https://youtu.be/IPAr7YazehQ?t=209"
   "https://youtu.be/u6B0tXrIpLY?t=495"
   "https://youtu.be/SgAXQRXmWMk?t=239"
   "https://youtu.be/2q219S-odkQ?t=1011"
   "https://youtu.be/q39D7rwH308?t=424"
   "https://youtu.be/oRlIqf0V9EE?t=305"
   "https://youtu.be/6xO63fbXhDY?t=461"
   "https://youtu.be/jnLoT9koWsw?t=401"
   "https://youtu.be/jPCT63Rj3vA?t=455"
   "https://youtu.be/QXOiN7IYAUk?t=446"
   "https://youtu.be/i4clQyKVdoc?t=310"
   "https://youtu.be/qPFW6B0OmG0?t=356"
   "https://youtu.be/qPFW6B0OmG0?t=838"])

;; as of 2020-11-04
(def old-drawfee-streams-mug-urls
  ["https://youtu.be/4G7saDnPK1Q?t=354"
   "https://youtu.be/syR9Ritno_k?t=238"
   "https://youtu.be/d1am7JDMJZc?t=572"
   "https://youtu.be/SkuFbcIIBCI?t=529"
   "https://youtu.be/RyEqSUKAuFs?t=251"
   "https://youtu.be/mi4jBM4VO2I?t=332"
   "https://youtu.be/A87_voIuo6o?t=390"
   "https://youtu.be/xWTyfvAL_kU?t=446"
   "https://youtu.be/tnMtRZ3zRkg?t=247"
   "https://youtu.be/ozmOzZ2tfMc?t=342"
   "https://youtu.be/GI5vpRHMQLM?t=339"
   "https://youtu.be/QKxERLsrVlI?t=394"
   "https://youtu.be/BTbb7Qa5xH4?t=132"
   "https://youtu.be/87nUl98pTrU?t=303"
   "https://youtu.be/ADvr13cZw90?t=336"
   "https://youtu.be/qGnsMMOOPKo?t=209"
   "https://youtu.be/4kcIZDUb4fo?t=173"])

(defn url->svg
  [url]
  (-> url
      (screenshot!)
      (img->svg)
      (slurp)))

(defn svg->mug-pts
  [svg]
  (->> svg
       (svg->paths)
       (mapv ->pts)
       (filter-paths)
       (move-to-origin)))

(defn mug-pts->shape
  [pts]
  (->> pts
       (apply f/polygon2)))

(defn svg!
  [name & content]
  (let [fname (str name ".svg")]
    (spit fname (html content))
    fname))

(defn mug-pts->polygon2d-front
  [pts]
  (->> pts
       (mapv #(mapv drop-z %))
       (#(mapv svg/path-polygon %))
       (#(apply svg/merge-paths %))))

(defn simple-translate
  [[x y z] pts]
  (mapv #(f/v+ [x y z] %) pts))

(defn mug-pts->polygon2d-iso
  [pts]
  (let [[mx my _] (mapv float (f/midpoint (apply concat pts)))]
    (->> pts
         (mapv #(simple-translate [(- mx) (- my) 0] %))
         (mapv #(shape/isometric-xf %))
         (mapv #(simple-translate [mx my 0] %))
         (mapv #(mapv drop-z %))
         (#(mapv svg/path-polygon %))
         (#(apply svg/merge-paths %)))))

(defn mug-pts->polygon2d-right
  [pts]
  (let [[mx my _] (mapv float (f/midpoint (apply concat pts)))]
    (->> pts
         (mapv #(simple-translate [(- mx) (- my) 0] %))
         (mapv #(shape/rotate-points % [0 90 0]))
         (mapv #(simple-translate [mx my 0] %))
         (mapv #(mapv drop-z %))
         (#(mapv svg/path-polygon %))
         (#(apply svg/merge-paths %)))))

(defn mug-pts->polygon2d-back
  [pts]
  (let [[mx my _] (mapv float (f/midpoint (apply concat pts)))]
    (->> pts
         (mapv #(simple-translate [(- mx) (- my) 0] %))
         (mapv #(shape/rotate-points % [0 180 0]))
         (mapv #(simple-translate [mx my 0] %))
         (mapv #(mapv drop-z %))
         (#(mapv svg/path-polygon %))
         (#(apply svg/merge-paths %)))))

(def style-str 
"
@import url('https://fonts.googleapis.com/css2?family=Amatic+SC:wght@700&display=swap');
html {
  font-family: 'Amatic SC', cursive;
  font-size: 16pt;
}
.card {
  background-color: #ddd;
  border: 3px solid #aaa;
  padding: 10px;
  margin: 20px;
  width: 200px;
  height: 310px;
  border-radius: 12px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
  transition: all 0.3s cubic-bezier(.25,.8,.25,1);
}
.card:hover {
  box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22);
  transform: scale(1.5);
}
.card svg {
  width: 93%;
  height: auto;
  margin: 2%;
  border: 4px solid rgb(197, 3, 38);
  background-color: #eee;
}
.title {
  padding: 0px 10px;
  margin: 0;
}
.container {
  width: 1200px;
  margin: 0 auto;
  display: flex;
  flex-flow: wrap;
  justify-content: center;
}
")

(defn html! [content]
  (let [style [:style style-str]]
    (spit "index.html"
          (hiccup.page/html5 
           (list
            [:div {:style {:width "100%"
                           :text-align "center"}}
             [:h1 "The Many Mugs of Drawfee"]
             [:h3 "a dumb fan page."]]
            style
            content)))))

(defn url->card
  [url]
  (let [data (yt-url->video-data url)
        drw (->> url
                 (url->svg)
                 (svg->mug-pts)
                 (resize 225)
                 (mug-pts->polygon2d-front)
                 (svg/style-element {:fill "black"})
                 (svg/dwg-2d [350 350 1]))]
    [:div.card
     [:a {:href url} drw]
     [:h4.title (hiccup.util/escape-html (:title data))]
     #_[:p (:descr data)]]))

(def mug-cache
  (into 
   []
   (for [url (concat drawfee-extra-mug-urls
                     old-drawfee-streams-mug-urls)]
     (try
       (url->card url)
       (catch Exception e
         nil)))))

(defn save-cache! []
  (spit "cache.edn" mug-cache))

(defn save-index! []
  (->> mug-cache
       (into [:div.container])
       (html!)))

(defn drawfee-demo!
  [url]
  (let [pts (->> url
                 (url->svg)
                 (svg->mug-pts)
                 (move-to-origin)
                 (resize 200))
        mug1 (->> pts (mug-pts->polygon2d-front))
        mug2 (->> pts (mug-pts->polygon2d-back))
        p1 (get-in mug1 [1 :d])
        p2 (get-in mug2 [1 :d])
        anim [:path {:d p1}
              [:animate {:attributeName "d"
                         :values (str "\n" p1 ";\n" p2 ";\n" p1)
                         :dur "4s"
                         :repeatCount "indefinite"}]]]
    (svg! "output/asdf"
          (svg/dwg-2d
           [300 420 1]
           (svg/g
            (->> anim
                 (svg/style-element {:stroke "none"
                                     :fill "hotpink"})))))))

(defn pad-path
  [pts size]
  (let [osize (count pts)
        diff (- size osize)
        xpts (concat
              (interleave pts (take diff pts))
              (drop diff pts))]
    (if (not= (count xpts) size)
      (recur xpts size)
      (into [] xpts))))

(defn pad-smaller-path
  [path1 path2]
  (let [p1-count (count path1)
        p2-count (count path2)
        size (max p1-count p2-count)
        diff (Math/abs (- p1-count p2-count))]
    (if (= size p1-count)
      [path1 (pad-path path2 size)]
      [(pad-path path1 size) path2])))

(defn pad-paths-for-morph
  [paths1 paths2]
  (let [[sp lp] (sort-by count [paths1
                                paths2])
        size (count lp)
        diff (- size (count sp))
        padpt (last (last sp))
        xsp (concat sp (take diff (repeat [padpt])))
        padded (map pad-smaller-path xsp lp)]
    [(mapv first padded)
     (mapv second padded)]))

(defn drawfee-demo2!
  [svg1 svg2 svg3]
  (let [pts1 (->> svg1 (svg->mug-pts))
        pts2 (->> svg2 (svg->mug-pts))
        pts3 (->> svg3 (svg->mug-pts))
        [s m l] (sort-by count [pts1 pts2 pts3])
        [xs xma] (pad-paths-for-morph s m)
        [xmb xl] (pad-paths-for-morph m l)
        mug1 (->> xs (mug-pts->polygon2d-front))
        mug2a (->> xma (mug-pts->polygon2d-front))
        mug2b (->> xmb (mug-pts->polygon2d-front))
        mug3 (->> xl (mug-pts->polygon2d-front))
        p1 (get-in mug1 [1 :d])
        p2a (get-in mug2a [1 :d])
        p2b (get-in mug2b [1 :d])
        p3 (get-in mug3 [1 :d])
        anim [:path {:d p1}
              [:animate {:attributeName "d"
                         :values (str "\n"
                                      p1 ";\n"
                                      p1 ";\n"
                                      p2a ";\n"
                                      p2b ";\n"
                                      p3 ";\n"
                                      p3 ";\n"
                                      p2b ";\n"
                                      p2a ";\n"
                                      p1 ";\n"
                                      p1)
                         :dur "12s"
                         :repeatCount "indefinite"}]]]
    (svg! "output/asdf"
          (svg/svg
           [1920 1080 0.5]
           (svg/g
            (->> anim
                 (svg/style-element {:stroke "none"
                                     :fill "hotpink"})))))))

(def mug-url "https://youtu.be/IPAr7YazehQ?t=209")

;; a large drawing:
;; Https://youtu.be/2q219S-odkQ?t=7873

;; a mug:
;; https://youtu.be/IPAr7YazehQ?t=209

(defn bb-pts
  [pts]
  (let [pts (if (= 3 (count (first pts))) (mapv drop-z pts) pts)
        [[ax ay] [bx by]] (f/bb-corners-2d pts)]
    [[ax ay]
     [bx ay]
     [bx by]
     [ax by]]))

(defn bb-wh
  [pts]
  (let [pts (if (= 3 (count (first pts))) (mapv drop-z pts) pts)
        [[ax ay] [bx by]] (f/bb-corners-2d pts)]
    [(Math/abs (- bx ax)) (Math/abs (- by ay))]))

(defn minimize-bb-h
  [shape]
  (let [ [mx my _] (mapv float (f/midpoint (:vertices shape)))
        rs (for [angle (range 0 180 0.5)]
             (let [xshape (-> shape
                              (f/translate [(- mx) (- my) 0])
                              (f/rotate [0 0 angle])
                              (f/translate [mx my 0]))
                   [nmx nmy _] (f/midpoint (:vertices xshape))
                   [bbw bbh] (bb-wh (:vertices xshape))]
               {:angle angle
                :height bbh
                :width bbw
                :midpoint [nmx nmy]
                :vertices [[(- mx (/ bbw 2.0)) my 0] 
                           [(+ mx (/ bbw 2.0)) my 0]]}))]
    (first (sort-by :height rs))))

(defn idealize-endpoints-2d
  [shape]
  (let [xpts (sort-by first (:vertices shape))
        ypts (sort-by second (:vertices shape))
        pts [(first xpts)
             (first ypts)
             (first (reverse xpts)) 
             (first (reverse ypts))]
        pairs (combo/combinations pts 2)]
    (mapv #(mapv float (f/midpoint %)) 
          (take 2 (sort-by #(apply f/distance %) pairs)))))

(defn ipad!
  []
  (->> "output/out.png"
       (img->svg)
       (slurp)
       (svg->paths)
       (mapcat split-path)
       (mapv ->pts)))

(defn ipad->forge
  []
  (->> "output/out.png"
       (img->svg)
       (slurp)
       (svg->paths)
       (mapcat split-path)
       (mapv ->pts)
       (#(apply f/polygon2 %))))

(let [pts (ipad!)
      [mx my _] (mapv float (f/midpoint (apply concat pts)))
      xpts (-> pts
               (#(apply f/polygon2 %))
               (f/translate [(- mx) (- my) 0])
               (f/rotate [0 0 0])
               (f/translate [mx my 0]) 
               (:vertices))
      bb (svg/path-polygon (bb-pts xpts))
      obj (->> [xpts]
               (mapv #(mapv drop-z %))
               (#(map svg/path-polygon %)) 
               (#(apply svg/merge-paths %)))
      data (minimize-bb-h (apply f/polygon2 pts))
      ipts (mapv drop-z 
                 (idealize-endpoints-2d 
                  (apply f/polygon2 pts)))]
  (svg! "output/test"
        (svg/svg
         [1024 1024 0.25]
         (svg/style-element {:stroke "red"
                             :stroke-width 5
                             :fill "none"}
                            bb)
         (svg/style-element {:stroke "none"
                             :fill "green"}
                            (svg/g obj))
         (svg/style-element {:stroke "hotpink"
                             :stroke-width 15
                             :fill "none"}
                            (svg/path-polygon ipts))
         (for [pt ipts]
           (svg/translate 
            pt
            (svg/style-element {:fill "red"
                                :opacity 0.5} (svg/circle 25)))))))
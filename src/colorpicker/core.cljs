(ns ^:figwheel-hooks colorpicker.core
  (:require [clojure.string :as cs]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [goog.string :as gstring]
            [goog.string.format]))

(defonce global-state (r/atom {:colors []
                               :fixed-dimension :hue}))

(defn round [x]
  (.round js/Math x))

(defn fixed [s]
  (let [dimension (:fixed-dimension s)
        color ((:colors s) (:current s))]
    [dimension
     (color (case dimension
              :hue 0
              :saturation 1
              :lightness 2))]))

(defn format-hsl [h s l]
  (gstring/format "hsl(%d, %f%%, %f%%)" h s l))

(defn format-url-fragment [colors]
  (str \# (cs/join \, (flatten colors))))

(defn parse-url-fragment [fragment]
  (when (seq fragment)
    (as-> fragment x
      (subs x 1)
      (cs/split x \,)
      (map int x)
      (partition 3 3 [0 0] x)
      (mapv vec x))))

(defn format-color [i j steps [fixed-dimension fixed-value]]
  (case fixed-dimension
    :hue (format-hsl fixed-value (/ (* 100 i) steps) (/ (* 100 j) steps))
    :saturation (format-hsl (/ (* 360 i) steps) fixed-value (/ (* 100 j) steps))
    :lightness (format-hsl (/ (* 360 i) steps) (/ (* 100 j) steps) fixed-value)))

(defn render-background []
  (let [canvas (js/document.getElementById "background")
        ctx (.getContext canvas "2d")
        bounds (.getBoundingClientRect canvas)
        width (.-width bounds)
        height (.-height bounds)
        steps 64
        fixed (fixed @global-state)]
    (doseq [i (range steps)
            j (range steps)]
      (let [x (/ (* i width) steps)
            y (/ (* j height) steps)
            color (format-color i j steps fixed)]
        (aset ctx "fillStyle" color)
        (.fillRect ctx (round x) (round y) (round (+ x (/ width steps))) (round (+ y (/ height steps))))))))

(defn position [[h s l] fixed-dimension]
  (case fixed-dimension
    :hue [(/ s 100) (/ l 100)]
    :saturation [(/ h 360) (/ l 100)]
    :lightness [(/ h 360) (/ s 100)]))

(defn render-foreground []
  (let [s @global-state
        canvas (js/document.getElementById "foreground")
        bounds (.getBoundingClientRect canvas)
        width (.-width bounds)
        height (.-height bounds)
        color ((:colors s) (:current s))
        pos-pct (position color (:fixed-dimension s))
        pos [(+ 0.5 (round (* width (first pos-pct))))
             (+ 0.5 (round (* height (second pos-pct))))]
        s-x (fn [p d] [(+ (first p) d) (second p)])
        s-y (fn [p d] [(first p) (+ (second p) d)])
        north (s-y pos -10)
        south (s-y pos +10)
        west (s-x pos -10)
        east (s-x pos +10)
        ctx (.getContext canvas "2d")
        line (fn [[x-1 y-1] [x-2 y-2]]
               (.beginPath ctx)
               (.moveTo ctx x-1 y-1)
               (.lineTo ctx x-2 y-2)
               (.stroke ctx))]
    (.clearRect ctx 0 0 width height)
    (aset ctx "strokeStyle" "white")
    (line (s-x north -1) (s-x south -1))
    (line (s-x north +1) (s-x south +1))
    (line (s-y west -1) (s-y east -1))
    (line (s-y west +1) (s-y east +1))
    (aset ctx "strokeStyle" "black")
    (line north south)
    (line west east)))

(defn render-canvas []
  (render-background)
  (render-foreground))

(defn fix-dimension [dimension]
  (swap! global-state assoc :fixed-dimension dimension)
  (render-canvas))

(defn updated-color [[h s l] fixed-dimension x-pct y-pct]
  (case fixed-dimension
    :hue [h (round (* 100 x-pct)) (round (* 100 y-pct))]
    :saturation [(round (* 360 x-pct)) s (round (* 100 y-pct))]
    :lightness [(round (* 360 x-pct)) (round (* 100 y-pct)) l]))

(defn pick [e]
  (let [rect (.getBoundingClientRect (.-target e))
        left (.-left rect)
        top (.-top rect)
        width (.-width rect)
        height (.-height rect)
        x (- (.-clientX e) (.-left rect))
        y (- (.-clientY e) (.-top rect))
        x-pct (/ x width)
        y-pct (/ y height)
        s @global-state
        current (:current s)]
    (swap! global-state assoc-in [:colors current] (updated-color ((:colors s) current) (:fixed-dimension s) x-pct y-pct))
    (render-foreground)))

(defn add-color [s c]
  (let [current (count (:colors s))]
    (-> s
        (update :colors conj c)
        (assoc :current current))))

(defn select-current [s index]
  (assoc s :current index))

(defn remove-color [s i]
  (let [colors (:colors s)
        n (count colors)
        new-current (if (= i (dec n)) (dec i) (:current s))]
    (-> s
        (assoc :colors (vec (concat (subvec colors 0 i) (subvec colors (inc i)))))
        (assoc :current new-current))))

(defn handle-select [index e]
  (.stopPropagation e)
  (swap! global-state select-current index)
  (render-canvas))

(defn handle-add []
  (let [c [(round (* 360 (.random js/Math))) 50 50]]
    (swap! global-state add-color c)
    (render-canvas)))

(defn handle-delete [i e]
  (.stopPropagation e)
  (swap! global-state remove-color i)
  (render-canvas))

(defn handle-export []
  (swap! global-state update :export? not))

(defn matrix [colors]
  (let [n (count colors)]
    [:div.matrix
     (for [i (range n)
           j (range n)]
       [:div {:key (str i \- j)
              :class "swatch"
              :on-click (partial handle-select i)
              :style {:grid-row (inc i)
                      :grid-column (inc j)
                      :background-color (apply format-hsl (colors i))
                      :color (apply format-hsl (colors j))
                      :border-color (apply format-hsl (colors j))}}
        [:span {:on-click (partial handle-select j)} "Text"]])]))

(defn export [url colors]
  [:div.export
   [:pre (str
           ":root {\n"
           "  /* view/edit at " url (format-url-fragment colors) " */\n"
           "  /* use these colors with var(--color-1) etc. */\n"
           (apply str (for [[c i] (map vector colors (range))]
                        (str "  --color-" (inc i) ": " (apply format-hsl c) ";\n")))
           "}\n")]
   [:button {:on-click handle-export} "Back"]])

(defn root []
  (let [s @global-state
        colors (:colors s)
        n (count colors)
        fixed-dimension (:fixed-dimension s)
        url js/window.location.href]
    [:div.container
     [:div
      [:button {:on-click #(fix-dimension :lightness)
                :disabled (= fixed-dimension :lightness)} "HS"]
      [:button {:on-click #(fix-dimension :saturation)
                :disabled (= fixed-dimension :saturation)} "HL"]
      [:button {:on-click #(fix-dimension :hue)
                :disabled (= fixed-dimension :hue)} "SL"]
      [:div#canvas
       [:canvas#background {:width 256 :height 256}]
       [:canvas#foreground {:width 256 :height 256 :on-click pick}]]
      [:ul.colors
       (for [i (range n)]
         [:li
          {:key i :on-click (partial handle-select i) :class (if (= i (:current s)) "selected" nil)}
          [:span
           [:span {:style {:color (apply format-hsl ((:colors s) i))}} "⬤◯ "]
           (apply format-hsl (colors i))]
          (when (> n 1) [:span.delete {:on-click (partial handle-delete i)} "✖"])])
       (when (< n 5)
         [:li.add {:on-click handle-add} "Add color"])
       [:li.add {:on-click handle-export} "Export"]]]
     (if (:export? s)
       (export url colors)
       (matrix colors))]))

(defn ^:export run []
  (when-let [colors (parse-url-fragment js/window.location.hash)]
    (swap! global-state assoc :colors colors :current 0))
  (js/history.replaceState nil nil " ") ; clear URL fragment
  (rdom/render [root] (js/document.getElementById "app"))
  (when (empty? (:colors @global-state)) (handle-add))
  (render-canvas))

(run)

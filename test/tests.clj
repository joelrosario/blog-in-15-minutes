(ns tests
  (:use [clojure test]))

(deftype blog-post [title content])

(defprotocol posts-store
  (add [store post])
  (posts [store]))

(deftype posts-list [db]
  posts-store
  (add [post] (swap! db conj post))
  (posts [] @db))

(defn new-posts-list [] (posts-list (atom [])))

(defn simple-post-template [post]
  (str (:title post) ": " (:content post)))

(defn simple-posts-template [post-template posts]
  (apply str (interpose "<br>" (map post-template posts))))

(def test-posts-as-list "test post: hello world<br>test post2: hello world2")

(defn simple-page-template [posts-template post-template posts]
  (str "<html><body>" (posts-template post-template posts) "</body></html>"))

(def test-post (blog-post "test post" "hello world"))
(defn test-post? [post]
  (and
   (= (:title post) "test post")
   (= (:content post) "hello world")))

(def test-posts-datastore (new-posts-list))
(add test-posts-datastore test-post)
(add test-posts-datastore (blog-post "test post2" "hello world2"))


(defmacro define-template [template-name & template-params]
  (let [template-params (vec (concat [{}] (mapcat (fn [[k v]]
                                                [(keyword k) (list 'var-get (resolve v))])
                                              (partition 2 2 template-params))))]
    `(def ~template-name (apply assoc ~template-params))))

(defn replace-in-str [str-val old-char new-char]
  (apply str
         (map (fn [ch] (if (= ch old-char) new-char ch)) str-val)))

(defmacro expected-method? [method name]
  (let [name-s (str name)]
    `(do
       (>= (.indexOf (str ~method)
                     (replace-in-str ~name-s \- \_))
           0))))

(defn render-posts-list-page [template posts]
  (let [{page-template :page posts-template :posts post-template :post} template]
    (page-template posts-template post-template posts)))

(deftest create-new-blog-post
  (is (= "test post" (:title test-post)))
  (is (= "hello world" (:content test-post))))

(deftest add-new-post-to-datastore
  (let [datastore (new-posts-list)]
    (add datastore test-post)
    (is (= test-post (first (posts datastore))))))

(deftest render-post-as-html
  (is (= "test post: hello world" (simple-post-template test-post))))

(deftest render-posts-as-html
  (is (= test-posts-as-list
         (simple-posts-template simple-post-template (posts test-posts-datastore)))))

(deftest render-page-of-posts-as-html
  (is (= (str "<html><body>" test-posts-as-list "</body></html>")
         (simple-page-template
          simple-posts-template
          simple-post-template
          (posts test-posts-datastore)))))

(deftest define-a-page-template
  (define-template simple-template
            page simple-page-template
            posts simple-posts-template
            post simple-post-template)
  (is (expected-method? (:page simple-template) simple-page-template))
  (is (expected-method? (:posts simple-template) simple-posts-template))
  (is (expected-method? (:post simple-template) simple-post-template)))

(deftest render-a-page-with-a-template
  (define-template simple-blog-template
    page simple-page-template
    posts simple-posts-template
    post simple-post-template)
  (is (= (str "<html><body>" test-posts-as-list "</body></html>"))
      (render-posts-list-page simple-blog-template (posts test-posts-datastore))))

(run-tests)

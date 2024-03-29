(ns file-utils.test.core
  (:use [file-utils.core])
  (:use [clojure.test]))


(deftest exists
  (is (= true (exists? "project.clj")))
  (is (= false (exists? "this-should-not-exist"))))

(deftest is-dir?
  (is (= true (dir? "./src")))
  (is (= false (dir? "project.clj"))))

(deftest is-file
  (is (= true (file? "project.clj")))
  (is (= false (file? "./src"))))

(deftest list-file
  (is (seq? (list-files ".")))
  (is (= nil (list-files "./tmp/does/not/exist"))))

(deftest has-children
  (is (= true (children? "./src")))
  (is (= false (children? "./README"))))

(deftest is-same-file?
  (is (= true (same-file? "project.clj" "project.clj")))
  (is (= false (same-file? "project.clj" "README"))))

(deftest size
  (is (> (file-size "project.clj") 0)))

(deftest modified
  (is (instance? org.joda.time.DateTime (date-modified "project.clj"))))

(deftest is-executable
  (is (= true (executable? "./src")))
  (is (= false (executable? "project.clj"))))

(deftest is-readable
  (is (= true (readable? "project.clj")))
  (is (= false (readable? "i-dont-exist"))))

(deftest is-writable
  (is (= true (writable? "project.clj")))
  (is (= false (writable? "i-dont-exist"))))

(testing "Make and delete direcoties"
  (is (= true (mkdir "./tmp/test/1/2")))
  (is (= true (exists? "./tmp/test/1/2")))
  (is (= true (rm "./tmp/test/1/2")))
  (is (= false (exists? "./tmp/test/1/2")))
  ;(is (= true (rm "./tmp/test/1")))
  ;(is (= false (exists? "./tmp/test/1")))
  ;(is (= true (rm "./tmp/test")))
  ;(is (= false (exists? "./tmp/test")))
  (is (= true (rm-r "./tmp")))
  (is (= false (exists? "./tmp"))))

(deftest temp-file
  (let [tmp (create-temp)]
    (is (= true (exists? tmp)))
    (is (= true (rm tmp)))))

(deftest test-name
  (is (= "test.txt" (filename "/home/joe/Documents/test.txt"))))

(deftest file-extension
  (is (= "txt" (extension "/home/joe/Documents/test.txt")))
  (is (= nil (extension "/etc/passwd"))))

(deftest test-base-filename
  (is (= "test" (base-filename "/home/joe/Documents/test.txt")))
  (is (= "test" (base-filename "/home/joe/Documents/test"))))

(testing "File search"
   (testing "with regex"
     (testing "recursively"
        (let [files (find-file "." :file-filter #".*\.clj" :recursive true)]
           (is (> (count files) 0))
           (is (= () (filter (fn [f] (not= "clj" (extension f))) files)))))          
     (testing "not recursively"
        (let [files (find-file "." :file-filter #".*\.clj" :recursive false)]
           ; only the project.clj is expected. 
           (is (= (count files) 1))    
           (is (= "project.clj" (filename (first files)))))))
  (testing "with string"
    (testing "recursively"
      (let [files (find-file "." :file-filter "core.clj" :recursive true)]
        (is (= (count files) 2)
        (is (= (filename (first files)) "core.clj")))))
    (testing "not recusively"
      (let [files (find-file "." :file-filter "core.clj" :recursive false)]
        (is (= (count files) 0)))))
  (testing "with IFn"
    (testing "recursively"
      (let [files (find-file "." :file-filter #(= "clj" (extension %)) :recursive true)]
         (is (> (count files) 0))
         (is (= () (filter (fn [f] (not= "clj" (extension f))) files)))))))

(deftest test-file-str
  (is (exists? (file-str "." "src" "file_utils" "core.clj")))
  (is (exists? (file-str ".\\src" "file_utils")))
  (is (exists? (file-str "." "/project.clj"))))


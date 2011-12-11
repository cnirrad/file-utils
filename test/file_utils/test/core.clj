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

(comment deftest mkdir-and-rmdir
  (is (= true (mkdir "./tmp/test/1/2")))
  (is (= true (exists? "./tmp/test/1/2")))
  (is (= true (rm "./tmp/test/1/2")))
  (is (= false (exists? "./tmp/test/1/2")))
  (is (= true (rm "./tmp")))
  (is (= false (exists? "./tmp"))))

(deftest temp-file
  (let [tmp (create-temp)]
    (is (= true (exists? tmp)))
    (is (= true (rm tmp)))))

(deftest test-name
  (is (= "test.txt" (name "/home/joe/Documents/test.txt"))))

(deftest file-extension
  (is (= "txt" (extension "/home/joe/Documents/test.txt")))
  (is (= nil (extension "/etc/passwd"))))


(testing "File search"
   (testing "with regex"
     (testing "recursively"
        (let [files (find-file "." :file-pattern #".*\.clj" :recursive true)]
           (is (> (count files) 0))
           (is (= () (filter (fn [f] (not= "clj" (extension f))) files)))))          
     (testing "not recursively"
        (let [files (find-file "." :file-pattern #".*\.clj" :recursive false)]
           ; only the project.clj is expected. 
           (is (= (count files) 1))    
           (is (= "project.clj" (name (first files)))))))
  (testing "with string"
    (testing "recursively"
      (let [files (find-file "." :file-pattern "core.clj" :recursive true)]
        (is (= (count files) 2)
        (is (= (name (first files)) "core.clj")))))
    (testing "not recusively"
      (let [files (find-file "." :file-pattern "core.clj" :recursive false)]
        (is (= (count files) 0))))))



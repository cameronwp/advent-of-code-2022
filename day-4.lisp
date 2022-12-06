(uiop:define-package #:aoc-2022-day-4
    (:use #:cl
          #:alexandria
          #:iterate))

(in-package #:aoc-2022-day-4)

(defun get-range (elf)
  "Go from a string of l-u to a list of ints from l to u"
  (destructuring-bind (lower upper)
      (uiop:split-string elf :separator "-")
    (iter (for section from (parse-integer lower) to (parse-integer upper))
      (collecting section))))

(defun fully-contained (pair)
  (destructuring-bind (elf-1 elf-2)
      (uiop:split-string pair :separator ",")
    (let* ((range-1 (get-range elf-1))
           (range-2 (get-range elf-2))
           ;; sort the intersection just in case
           (overlap (sort (intersection range-1 range-2) '<)))
      (or (equal overlap range-1)
          (equal overlap range-2)))))

(defun part1 (filepath)
  (let ((pairs (uiop:read-file-lines filepath)))
    (iter (for pair in pairs)
      (counting (fully-contained pair)))))

(print (part1 "day-4-data.txt"))

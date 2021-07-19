(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) 

val test_remove_duplicates = remove_duplicates_months([1,2,3,4,5,4,3,6,7]) = [1,2,5,4,3,6,7]

val reasonable_date_test1 = reasonable_date(~1, 2, 12) = false
val reasonable_date_test2 = reasonable_date(1, 2, 12) = true
val reasonable_date_test3 = reasonable_date(1, 0, 12) = false
val reasonable_date_test4 = reasonable_date(1, 13, 12) = false
val reasonable_date_test5 = reasonable_date(1, 1, 32) = false
val reasonable_date_test6 = reasonable_date(2001, 2, 29) = false
val reasonable_date_test7 = reasonable_date(2001, 2, 28) = true
val reasonable_date_test8 = reasonable_date(1996, 2, 29) = true
val reasonable_date_test9 = reasonable_date(2000, 2, 29) = true
val reasonable_date_test10 = reasonable_date(2000, 4, 31) = false
val reasonable_date_test11 = reasonable_date(2000, 4, 30) = true
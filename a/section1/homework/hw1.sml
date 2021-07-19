(* takes two dates and evaluates to true or false.  
It evaluates to true if the first argument is a date that comes before the second argument.  
(If the two dates are the same,the result is false.) *)
fun is_older(d1: int * int * int, d2: int * int * int): bool =
	if #1 d1 <> #1 d2
	then #1 d1 < #1 d2
	else
		if #2 d1 <> #2 d2
		then #2 d1 < #2 d2
		else #3 d1 < #3 d2

(* takes a list of dates and a month and returns how many dates in the list are in the given month *)
fun number_in_month(dates: (int * int * int) list, month: int): int =
	if null dates
	then 0
	else 
		if #2 (hd dates) = month
		then 1 + number_in_month(tl dates, month)
		else number_in_month(tl dates, month)

(* takes a list of dates and a list of months and returns the number of dates in the list of dates that are in any of the months in the list of months. *)
fun number_in_months(dates: (int * int * int) list, months: int list): int =
	if null months
	then 0
	else 
		if number_in_month(dates, hd months) > 0
		then 1 + number_in_months(dates, tl months)
		else number_in_months(dates, tl months)
		
(* takes a list of dates and a month and returns a list holding the dates from the argument list of dates that are in the month.  The returned list should contain dates in the order they were originally given *)
fun dates_in_month(dates: (int * int * int) list, month: int) =
	if null dates
	then []
	else 
		if #2 (hd dates) = month
		then (hd dates)::dates_in_month(tl dates, month)
		else dates_in_month(tl dates, month)

(* akes a list of dates and a list of months and returns a list holding the dates from the argument list of dates that are in any of the months inthe list of months. *)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* takes a list of strings and anintnand returns thenthelement of thelist where the head of the list is 1st *)
fun get_nth(strs: string list, n: int): string =
	if n = 1
	then hd strs
	else get_nth(tl strs, n -1)

(* takes a date and returns astringof the formJanuary 20, 2013(for example). *)
fun date_to_string(date: (int * int * int)): string =
	let
	  val months = [
		  "January",
		  "February",
		  "March",
		  "April",
		  "May",
		  "June",
		  "July",
		  "August",
		  "September",
		  "October",
		  "November",
		  "December"
	  ]
	in
		get_nth(months, #2 date) ^ " " ^ 
		Int.toString(#3 date) ^ ", " ^ 
		Int.toString(#1 date)
	end

(* takes anintcalledsum, which you can assumeis positive, and anint list, which you can assume contains all positive numbers, and returns anint.
You should return an intnsuch that the firstnelements of the list add to less than sum, but the firstn+ 1 elements of the list add tosumor more.  *)
fun number_before_reaching_sum(sum: int, integers: int list): int =
	let
		fun sum_up(total: int, nums: int list, n: int) =
			if  total + hd nums >= sum
			then n
			else sum_up(total + hd nums, tl  nums, n + 1)
	in
		sum_up(0, integers, 0)
	end

(* takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.).  *)
fun what_month(day: int): int =
	let
	  val days_of_month = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
	in
	  number_before_reaching_sum(day, days_of_month) + 1
	end

(* takes two days of the year day1 and day2 and returns an int list[m1,m2,...,mn] where m1is the month of day1,m2 is the month of day1+1, ..., and mn is the month of day day2.  Note the result will have length day2 - day1 + 1 or length 0 if day1>day2 *)
fun month_range(day1: int, day2: int): int list =
	if day1 > day2
	then []
	else what_month(day1)::month_range(day1 + 1, day2)

(* takes a list of dates and evaluates to an(int*int*int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(dates: (int * int * int) list) =
	if null dates
	then NONE
	else
		let
			val date_ans = oldest(tl dates)
		in
			if isSome date_ans andalso is_older(valOf date_ans, hd dates)
			then date_ans
			else SOME(hd dates)
		end

(* takes a int and a int list, return true if it in the list *)
fun within(item: int, items: int list): bool =
	if null items
	then false
	else
		if item = (hd items)
		then true
		else within(item, tl items)

(* takes a list of month, remove the duplicate month *)
fun remove_duplicates_months(months: int list) =
	if null months
	then []
	else
		if within(hd months, tl months)
		then remove_duplicates_months(tl months)
		else (hd months)::remove_duplicates_months(tl months)

(* Write functions number_in_months_challenge and dates_in_months_challengethat are like your solutions to problems 3 and 5 except having a month in the second argument multiple times has no more effect than having it once *)
fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
	number_in_months(dates, remove_duplicates_months(months))

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
	dates_in_months(dates, remove_duplicates_months(months))

(* takes a date and determines if itdescribes a real date in the common era.  A “real date” has a positive year (year 0 did not exist), amonth between 1 and 12, and a day appropriate for the month.  Solutions should properly handle leapyears. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)
fun reasonable_date(date: (int * int * int)): bool = 
	if 
		#1 date <= 0  orelse 
		#2 date < 1 orelse 
		#2 date > 12 orelse
		#3 date < 1
	then false
	else
		let
			val year = #1 date
			val month = #2 date
			val day = #3 date
			val days_of_month = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
			val is_leap_year = (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0))

			fun get_nth_int(items: int list, n: int): int =
				if n = 1
				then hd items
				else get_nth_int(tl items, n -1)

			val max_days = get_nth_int(days_of_month, month)

		in
			if 
			(is_leap_year andalso month = 2 andalso day > 29) orelse
			(is_leap_year andalso month <> 2 andalso day > max_days) orelse
			((not is_leap_year) andalso day > max_days)
			then false
			else true
		end
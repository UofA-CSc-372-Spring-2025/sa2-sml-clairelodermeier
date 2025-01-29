(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Claire Lodermeier *)
(* Time spent on HW6: 7 hours
*)

(* Collaborators and references:
    ChatGpt
    ML for the Working Programmer, 2nd Edition
    Class Slides
    https://www.cs.cornell.edu/courses/cs312/2008sp/recitations/rec05.html 
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true


(**** Problem B ****)

fun firstVowel (#"a" :: _)  = true
  | firstVowel (#"e" :: _)  = true
  | firstVowel (#"i" :: _)  = true
  | firstVowel (#"o" :: _)  = true
  | firstVowel (#"u" :: _)  = true
  | firstVowel _            = false

val () =
  Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
  (fn () => firstVowel [#"a",#"c",#"k"])
  true

val () =
  Unit.checkExpectWith Bool.toString "firstVowel 'claire' should be false"
  (fn () => firstVowel [#"c",#"l",#"a",#"i",#"r",#"e"])
  false

val () =
  Unit.checkExpectWith Bool.toString "firstVowel 'ee' should be true"
  (fn () => firstVowel [#"e", #"e"])
  true


(**** Problem C ****)

fun reverse []   = []
  | reverse list = foldl (fn (x, acc) => x :: acc) [] list;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2,3,4,5] should be [5,4,3,2,1]"
  (fn () => reverse [1,2,3,4,5])
  [5,4,3,2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,1,1,1] should be [1,1,1,1]"
  (fn () => reverse [1,1,1,1])
  [1,1,1,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [9] should be [9]"
  (fn () => reverse [9])
  [9]

(**** Problem D ****)

fun minlist []        = raise Match
  | minlist (x :: xs) = foldl (fn (cur, min) => Int.min(cur, min)) x xs;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [0,1,2,5] should be 0"
  (fn () => minlist [0,1,2,5])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [0,-1,2,5] should be -1"
  (fn () => minlist [0,~1,2,5])
  ~1

val () =
  Unit.checkExpectWith Int.toString
  "minlist [9,9,9] should be 9"
  (fn () => minlist [9,9,9])
  9

(**** Problem E ****)

exception Mismatch

fun zip (x::xs, y::ys)  = (x, y) :: zip (xs, ys)
  | zip ([], [])        = []
  | zip (_, _)          = raise Mismatch; 

val () = 
  Unit.checkExpectWith 
  (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([],[]) should be []"
  (fn () => zip ([],[]))
  []

val () = 
  Unit.checkExpectWith 
  (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([1,2,3],[3,2,1]) should be [(1,3),(2,2),(3,1)]"
  (fn () => zip ([1,2,3],[3,2,1]))
  [(1,3),(2,2),(3,1)]

val () = 
  Unit.checkExpectWith 
  (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([9,9,9],[9,9,9]) should be [(9,9),(9,9),(9,9)]"
  (fn () => zip ([9,9,9],[9,9,9]))
  [(9,9),(9,9),(9,9)]

val () = 
  Unit.checkExpectWith 
  (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([1],[10]) should be [(1,10)]"
  (fn () => zip ([1],[10]))
  [(1,10)]

val () =
  Unit.checkExnWith
  (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([1,2,3],[1,2]) should raise an exception"
  (fn () => zip ([1,2,3],[1,2]))
  
val () =
  Unit.checkExnWith
  (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([],[0]) should raise an exception"
  (fn () => zip ([],[0]))


(**** Problem F ****)

fun concat []      = []
  | concat (x::xs) = x @ concat xs;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[],[1,2],[8,1]] should be [1,2,8,1]"
  (fn () => concat [[],[1,2],[8,1]])
  [1,2,8,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[]] should be []"
  (fn () => concat [[]])
  []
  
val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1,2,3]] should be [1,2,3]"
  (fn () => concat [[1,2,3]])
  [1,2,3]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1],[2],[3]] should be [1,2,3]"
  (fn () => concat [[1],[2],[3]])
  [1,2,3]


(**** Problem G ****)

fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _ = false

val () =
  Unit.checkExpectWith Bool.toString 
  "isDigit '0' should be true"
  (fn () => isDigit #"0")
  true
val () =
  Unit.checkExpectWith Bool.toString 
  "isDigit '0' should be true"
  (fn () => isDigit #"6")
  true
val () =
  Unit.checkExpectWith Bool.toString 
  "isDigit 'b' should be false"
  (fn () => isDigit #"b")
  false
val () =
  Unit.checkExpectWith Bool.toString 
  "isDigit ' ' should be false"
  (fn () => isDigit #" ")
  false


(**** Problem H ****)

fun isAlpha c = 
  let val code = Char.ord c
  in 
    (code <= Char.ord #"z" andalso code >= Char.ord #"a") orelse
    (code <= Char.ord #"Z" andalso code >= Char.ord #"A")
  end
  
val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha ' ' should be false"
  (fn () => isAlpha #" ")
  false

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 'a' should be true"
  (fn () => isAlpha #"a")
  true

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 'b' should be true"
  (fn () => isAlpha #"b")
  true

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 'c' should be true"
  (fn () => isAlpha #"c")
  true

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 'z' should be true"
  (fn () => isAlpha #"z")
  true

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 'A' should be true"
  (fn () => isAlpha #"A")
  true

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 'B' should be true"
  (fn () => isAlpha #"B")
  true

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha 'Z' should be true"
  (fn () => isAlpha #"Z")
  true

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha '1' should be false"
  (fn () => isAlpha #"1")
  false
  
val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha '[' should be false"
  (fn () => isAlpha #"[")
  false

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha '?' should be false"
  (fn () => isAlpha #"?")
  false

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha '@' should be false"
  (fn () => isAlpha #"@")
  false

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha '`' should be false"
  (fn () => isAlpha #"`")
  false

val () =
  Unit.checkExpectWith Bool.toString 
  "isAlpha '{' should be false"
  (fn () => isAlpha #"{")
  false

(**** Problem I ****)

fun svgCircle (cx, cy, r, fill) = 
  "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^ "\" r=\"" 
  ^ Int.toString r ^ "\" fill=\"" ^ fill  ^ "\" />";

val () =
  Unit.checkExpectWith (fn x => x)
  ("svgCircle (200, 300, 100, \"red\") should return " ^
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />")
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

val () =
  Unit.checkExpectWith (fn x => x)
  ("svgCircle (120, 150, 60, \"white\") should return " ^
  "<circle cx=\"120\" cy=\"150\" r=\"60\" fill=\"white\" />")
  (fn () => svgCircle (120, 150, 60, "white"))
  "<circle cx=\"120\" cy=\"150\" r=\"60\" fill=\"white\" />";


(**** Problem J ****)

fun partition p [] = ([],[])
  | partition p (x :: xs) = 
      let 
        val (list1, list2) = partition p xs
      in 
        if p x then (x ::list1, list2)
        else (list1, x::list2)
      end;

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString 
  l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  ("partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return " ^
  "([2, 4], [1, 3, 5])")
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);


val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString 
  l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  ("partition (fn x => x > 0) [~3, ~2, 0, 1, 2] should return " ^
  "([1, 2], [~3, ~2, 0])")
  (fn () => partition (fn x => x > 0) [~3, ~2, 0, 1, 2])
  ([1, 2], [~3, ~2, 0]);


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)

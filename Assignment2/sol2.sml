(* 1. Simple Eval *)

datatype expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr * expr

datatype formula = TRUE
                 | FALSE
                 | NOT of formula
                 | ANDALSO of formula * formula
                 | ORELSE of formula * formula
                 | IMPLY of formula * formula
                 | LESS of expr * expr

fun eval (e : formula) : bool = 
  case e of
      TRUE => true
    | FALSE => false
    | NOT e2 => not(eval e2)
    | ANDALSO (e1, e2) => eval(e1) andalso eval(e2)
    | ORELSE (e1, e2) => eval(e1) orelse eval(e2)
    | IMPLY (e1, e2) => if eval(e1) then eval(e2) else true
    | LESS (e1, e2) =>
                      let fun evalExp (x : expr) : int =
                        case x of
                            NUM n => n
                          | PLUS (n, m) => evalExp(n) + evalExp(m)
                          | MINUS (n, m) => evalExp(n) - evalExp(m)
                      in
                          evalExp(e1) < evalExp(e2)
                      end

(* 2. Check MetroMap *)

type name = string
datatype metro = STATION of name
               | AREA of name * metro
               | CONNECT of metro * metro

fun checkMetro (e : metro) = 
  let 
    fun checkMetroFunc (mt : metro, nameList : name list) =
      case mt of
        STATION e2 => let 
                        fun existList (n : name, xs : name list) = 
                          case xs of
                                [] => false
                              |  x::xs' => if x = n then true else existList(n, xs')
                      in 
                        existList(e2, nameList)
                      end
      | AREA (e1, e2) => checkMetroFunc (e2, e1::nameList)
      | CONNECT (e1, e2) => checkMetroFunc (e1, nameList) andalso checkMetroFunc (e2, nameList)
  in 
    checkMetroFunc (e, [])
  end

(* 3. Lazy List *)

(* Datatype *)
datatype 'a lazyList = nullList 
                     | cons of 'a * (unit -> 'a lazyList) 

(* seq(first, last) *)
fun seq(first : int, second : int) =
  let
    fun makingSeq (now : int, second : int) =
      if now > second
        then nullList
      else
        cons(now, fn () => makingSeq(now+1, second))
  in
    makingSeq(first, second)
  end

(* infSeq(first) *)
fun infSeq(first : int) =
  let 
    fun makingInfSeq (now : int) = 
      cons(now, fn () => makingInfSeq(now + 1))
  in
    makingInfSeq(first)
  end

(* firstN(lazyListVal, n) *)
fun firstN(lazyListVal, n) = 
  let
    fun makingFirstN(xs, n, lst) = 
      if n = 0
      then lst
      else
        case xs of
          nullList => lst
        | cons(x, xs') => makingFirstN(xs'(), n-1, x::lst)
    fun reverse(xs, ys) =
      case xs of
         [] => ys
       | x::xs' => reverse(xs', x::ys)
  in
    reverse(makingFirstN(lazyListVal, n, []), [])
  end

(* Nth(lazyListVal, n) *)
fun Nth(lazyListVal, n) =
  let
    fun makingNth(xs, n) = 
      case xs of
         nullList => NONE
       | cons(x, xs') => if n = 1 then SOME x else makingNth(xs'(), n-1)
  in
    if n = 0 then NONE else makingNth(lazyListVal, n)
  end

(* filterMultiples(lazyListVal, n) *)
fun filterMultiples(lazyListVal, n) = 
  let 
    fun makingFilterMultiples(xs, n) =
      case xs of
        nullList => nullList
      | cons(x, xs') => 
          if x mod n = 0 
          then makingFilterMultiples(xs'(), n) 
          else cons(x, fn() => makingFilterMultiples(xs'(), n))
  in
    if n = 0 then lazyListVal else makingFilterMultiples(lazyListVal, n)
  end

(* Sieve of Eratosthenes *)

fun primes() = 
  let
    fun sieve(lazyListVal) = 
      case lazyListVal of
         nullList => nullList
       | cons(x, xs') => cons(x, fn() => sieve(filterMultiples(xs'(), x)))
  in
    sieve(infSeq(2))
  end
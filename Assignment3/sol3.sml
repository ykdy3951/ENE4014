datatype pattern = Wildcard | Variable of string | UnitP
                     | ConstP of int | TupleP of pattern list
                     | ConstructorP of string * pattern
datatype valu = Const of int | Unit | Tuple of valu list
                | Constructor of string * valu

(* 1. check_pat *)
fun check_pat pat = 
    let 
        fun has_duplicate [] = false
        |   has_duplicate (x::xs) = List.exists(fn y => x = y) xs orelse has_duplicate(xs)

        fun var_list (Wildcard, acc) = acc
        |   var_list (Variable v, acc) = v::acc
        |   var_list (UnitP, acc) = acc
        |   var_list (ConstP _, acc) = acc
        |   var_list (TupleP tp, acc) = foldl var_list acc tp 
        |   var_list (ConstructorP (_, cp), acc) = var_list(cp, acc)
    in
        not (has_duplicate (var_list (pat, [])))
    end

(* 2. match *)
fun match(v, p) = 
    case (v, p) of
        (_, Wildcard) => SOME []
    |   (v, Variable x) => SOME [(x, v)]
    |   (Unit, UnitP) => SOME []
    |   (Const c, ConstP cp) => if c = cp then SOME [] else NONE
    |   (Tuple t, TupleP tp) => if length(t) = length(tp) 
                                then 
                                    let
                                        val temp = ListPair.zip(t, tp) 
                                        val ans = List.filter (fn(x, y) => isSome(match(x, y))) temp
                                    in
                                        if length(ans) = length(tp) 
                                        then
                                            let
                                              fun ret(xs) =
                                                case xs of
                                                    x::[] => match(x)
                                                |   x::xs' => SOME (valOf(match(x)) @ valOf(ret(xs')))
                                            in
                                              ret temp
                                            end
                                        else NONE
                                    end
                                else NONE
    |   (Constructor (s1, c), ConstructorP (s2, cp)) => if s1 = s2 then match(c, cp) else NONE
    |   (_, _) => NONE

(* 3. RSP *)
type name = string
datatype RSP =
    ROCK
  | SCISSORS
  | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
    PLAYER of name * (RSP strategy ref)
  | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))
val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val pr = alterTwo(PAPER, ROCK)
val sr = alterTwo(SCISSORS, ROCK)
val rs = alterTwo(ROCK, SCISSORS)
val ps = alterTwo(PAPER, SCISSORS)
val sp = alterTwo(SCISSORS, PAPER)
val rsp = alterThree(ROCK, SCISSORS, PAPER)
val rps = alterThree(ROCK, PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)
val spr = alterThree(SCISSORS, PAPER, ROCK)
val prs = alterThree(PAPER, ROCK, SCISSORS)
val psr = alterThree(PAPER, SCISSORS, ROCK)

fun next(strategyRef) =
    let val Cons(rsp, func) = !strategyRef 
    in
        strategyRef := func();
        rsp
    end

fun whosWinner(t) = 
    let
        fun rsp_winner(PLAYER(n1, s1), PLAYER(n2, s2)) =
            let
                val rsp1 = next(s1)
                val rsp2 = next(s2)
            in
                case (rsp1, rsp2) of
                    (ROCK, SCISSORS) => PLAYER(n1, s1)
                |   (ROCK, PAPER) => PLAYER(n2, s2)
                |   (SCISSORS, PAPER) => PLAYER(n1, s1)
                |   (SCISSORS, ROCK) => PLAYER(n2, s2)
                |   (PAPER, ROCK) => PLAYER(n1, s1)
                |   (PAPER, SCISSORS) => PLAYER(n2, s2)
                |   _ => rsp_winner(PLAYER(n1, s1), PLAYER(n2, s2))
            end
    in
        case t of
            PLAYER(n, s) => PLAYER(n, s)
        |   MATCH(t1, t2) => rsp_winner(whosWinner t1, whosWinner t2)
    end


val winner = whosWinner(MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))));
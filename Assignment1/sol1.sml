fun merge (xs : int list, ys : int list) : int list =
  if null xs
  then 
    if null ys
    then []
    else hd(ys)::merge(xs, tl(ys))
  else
    if null ys
    then hd(xs)::merge(tl(xs), ys)
    else 
      if hd(xs) < hd(ys)
      then hd(xs)::merge(tl(xs), ys)
      else hd(ys)::merge(xs, tl(ys))

fun reverse(xs : int list) : int list = 
  let fun makingReverse(xs : int list, res : int list) =
        if null (tl xs)
        then hd xs::res
        else
          makingReverse(tl xs, hd xs::res)
  in makingReverse(xs, [])
  end

fun pi(a : int, b : int, f : int -> int) : int = 
  if a > b
  then 1
  else f(a) * pi(a+1, b, f)

fun digits(x : int) : int list = 
  let fun makingDigits(x : int, res : int list) = 
        if x = 0
        then res
        else makingDigits(x div 10, x mod 10::res)
  in makingDigits(x, [])
  end

fun sumList(xs : int list) : int =
  if null xs
  then 0
  else hd(xs) + sumList(tl(xs))

fun additivePersistence(x : int) : int = 
  let fun makingAdditivePersistence(x : int, y : int) =
        if x < 10
        then y
        else makingAdditivePersistence(sumList(digits x), y + 1)
  in makingAdditivePersistence(x, 0)
  end

fun digitalRoot(x : int) : int =
  if x < 10
  then x
  else digitalRoot(sumList(digits(x)))

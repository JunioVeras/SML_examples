(* Pattern Matching *)

fun ffib n =
    let 
        fun fib 0 = [0]
          | fib 1 = [1,0]
          | fib n = 
            let 
                fun sum ([a, b]) = [a+b, a]
                  | sum (l) = []
            in
                sum (fib(n-1))
            end
    in
        hd (fib(n))
    end
;

ffib(40);

(* ADT *)

datatype btree = L of int | Node of btree * int * btree;

fun size (Node(b1, n, b2)) = 1 + size(b1) + size(b2)
  | size (L _) = 0;

val e = L 1
val t3 = Node(e,3,e)
val t5 = Node (e, 5, e)
val t9 = Node (t3,9,t5)
val t4 = Node(t9,4,e);

size(t4);
size(t9);
size (Node(Node(L(0),1,L(2)), 3, Node(L(4),5,Node(L(6),7,L(8)))));

fun mirror (Node(b1, n, b2)) = Node(mirror(b2), n, mirror(b1))
  | mirror (L n) = L n;

mirror (Node(Node (L 0, 1, L 1), 3, Node (L 0, 4, Node (L 1, 7, L 2))));

(* Polymorphism *)

datatype ilist = E | L of int * ilist;

fun first (L(h, _)) = h
  | first (E) = raise Match;

fun rest (L(_, r)) = r
  | rest (E) = raise Match;

fun last (E) = raise Match
  | last (L(h, E)) = h
  | last (L(_, r)) = last r;

val L0 = L(1, (L(2, L(3, E))));

first(L0);

rest(L0);

last(L0);

datatype 'a plist = E | L of 'a * 'a plist;

fun first (L(h, _)) = h
  | first (E) = raise Match;

fun rest (L(_, r)) = r
  | rest (E) = raise Match;

fun last (E) = raise Match
  | last (L(h, E)) = h
  | last (L(_, r)) = last r;

val L1 = L("1", L("2", L("3", E)));

first(L1);

rest(L1);

last(L1);

fun max (E) = raise Match
  | max (L(h, E)) = h
  | max (L(h, r)) = 
    let 
      val m = max(r)
    in
      if m > h then m else h
    end;

fun min (E) = raise Match
  | min (L(h, E)) = h
  | min (L(h, r)) = 
    let 
      val m = min(r)
    in
      if m < h then m else h
    end;
  
max(L0);

min(L0);

(* Rascunho Lista 1 *)

fun cube (num: real) = num * num * num;

fun pow (n, 0) = 1
  | pow (n : int, e : int) = n * pow(n, e - 1);

fun sumLists([], []) = []
  | sumLists(((h1::t1) : int list), ((h2::t2) : int list)) = (h1 + h2)::(sumLists(t1,t2));

fun max ([]) = 0
  | max (h::[]) = h
  | max ((h::t) : int list) = 
    let 
      val m = max(t)
    in
      if m > h then m else h
    end;

fun cumSum([]) = []
  | cumSum(h1::[]) = [h1]
  | cumSum(h1::h2::t) = h1::cumSum((h1+h2)::t);

fun greet("") = "Hello nobody"
  | greet(s : string) = "Hello " ^ s;

fun split(s : string) = 
  let
    fun isDelimiter(c : char) = c = #" " orelse c = #"-" orelse c = #"," orelse c = #".";
  in 
    String.tokens isDelimiter s
  end;

fun allTrue([h]) = if h = true then true else false
  | allTrue([]) = false
  | allTrue((h::t) : bool list) = if h = true then allTrue(t) else false;

datatype dinheiro = Centavos of int | Reais of real | Pessoa_Dinheiro of (string * real);

fun amount(Centavos c) = c
  | amount(Reais r) = ceil(r * 100.0)
  | amount(Pessoa_Dinheiro (p, r)) = ceil(r * 100.0);

datatype Planeta = Mercurio | Venus | Terra | Marte | Jupiter | Saturno | Urano | Netuno;

fun planetAge(m, Mercurio) = (m * 88) div 12
  | planetAge(m, Venus) = (m * 225) div 12
  | planetAge(m, Terra) = (m * 365) div 12
  | planetAge(m, Marte) = (m * 687) div 12
  | planetAge(m, Jupiter) = (m * 4332) div 12
  | planetAge(m, Saturno) = (m * 10760) div 12
  | planetAge(m, Urano) = (m * 30681) div 12
  | planetAge(m, Netuno) = (m * 60190) div 12;

datatype btree = Leaf | Node of (btree * int * btree);

fun sumAll(Leaf) = 0
  | sumAll(Node(e1,v,e2)) = sumAll(e1) + v + sumAll(e2);

fun multiPairs([], []) = [] | multiPairs(((h1::t1) : int list), ((h2::t2) : int list)) = (h1 * h2)::(sumLists(t1,t2));

fun square (num : int) = num * num;
fun sum ((num1 : int), (num2 : int)) = num1 + num2;
fun compose(square,sum,num1,num2) = square(sum(num1, num2));

(*  *)
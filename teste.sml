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
  ;

fun min (E) = raise Match
  | min (L(h, E)) = h
  | min (L(h, r)) = 
    let 
      val m = min(r)
    in
      if m < h then m else h
    end;
  ;

val L0 = L(1, (L(2, L(3, E))));

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
  ;

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

fun multiPairs([], []) = [] | multiPairs(((h1::t1) : int list), ((h2::t2) : int list)) = (h1 * h2)::(multiPairs(t1,t2));

fun square (num : int) = num * num;
fun sum ((num1 : int), (num2 : int)) = num1 + num2;
fun compose(square,sum,num1,num2) = square(sum(num1, num2));

(*  *)

datatype academico = Estudante | Professor | Coordenador;
fun salario (Estudante) = Reais 100.0
  | salario (Professor) = Reais 1000.0
  | salario (Coordenador) = Reais 10000.0;

fun menos2(Centavos a, Centavos b) = real(a - b)
  | menos2(Reais a, Reais b) = a - b
  | menos2(Pessoa_Dinheiro (a,b), Pessoa_Dinheiro (c,d)) = b - d
  | menos2(_, _) = 0.0;

fun menos(a, b) = menos2 (salario (a), salario (b));
menos (Coordenador, Estudante);

(*  *)

fun filter _ [] = []
  | filter p (h::t) = if p h then h::(filter p t) else filter p t;

fun pos x = x > 0;
pos 1;
pos ~1;

filter pos [1,~2,3,~4];

foldr (fn (x,y) => if pos x then x::y else y) [] [1,~2,3,~4];

fun otherfilter p l = foldr (fn (x,y) => if p x then x::y else y) [] l;
otherfilter pos [1,~2,3,~4];

(* Sintaxe e Semantica / Binding variables *)

fun isIn (x : string) (h::t) = h = x orelse (isIn x t)
  | isIn x [] = false;

fun union l [] = l
  | union l (h::t) = if isIn h l then union l t else union (h::l) t;

fun insert x l = if isIn x l then l else (x::l);

union (["1","2","3","4"]) (["2","4","5","6"]);

type mem = (string * int) list;
datatype bexpr = BConst of bool | And of bexpr * bexpr | Or of bexpr * bexpr | Not of bexpr;
datatype iexpr = IConst of int | Plus of iexpr * iexpr | Minus of iexpr * iexpr | Ite of bexpr * iexpr * iexpr | Var of string | Let of string * iexpr * iexpr;

fun beval (BConst e) = e
  | beval (And(e1, e2)) = beval(e1) andalso beval(e2)
  | beval (Or(e1, e2)) = beval(e1) orelse beval(e2)
  | beval (Not(e)) = not(beval(e));

val e1 = And(Not(Or(BConst true, BConst false)), BConst true);
beval(e1);

fun ieval (IConst e) (m) = e 
  | ieval (Plus(e1, e2)) (m) = ieval e1 m + ieval e2 m
  | ieval (Minus(e1, e2)) (m) = ieval e1 m - ieval e2 m
  | ieval (Ite(b, e1, e2)) (m) = if beval(b) then ieval e1 m else ieval e2 m
  | ieval (Var x) ((s,i)::t) = if s = x then i else ieval (Var x) t
  | ieval (Let(x, v, e)) (m) = ieval e (((x, (ieval v m)))::m);

val e2 = Minus(Plus(IConst 1, IConst 2), Let("x", IConst 111, Ite(e1, IConst 1, Var "x")));

fun freeVars (IConst e) (m) = []
  | freeVars (Plus(e1, e2)) (m) = union (freeVars e1 m) (freeVars e2 m)
  | freeVars (Minus(e1, e2)) (m) = union (freeVars e1 m) (freeVars e2 m)
  | freeVars (Ite(b, e1, e2)) (m) = union (freeVars e1 m) (freeVars e2 m)
  | freeVars (Var x) (m) = if isIn x m then [] else [x]
  | freeVars (Let(x, v, e)) (m) = union (freeVars v m) (freeVars e (x::m));

fun closed (e : iexpr) = (freeVars e [] = []);
exception NonClosed;

fun run (e : iexpr) = if closed e then ieval e [] else raise NonClosed;

run e2;

run (Plus(Minus(Let("x", IConst 10, Let("y", IConst 20, Plus(Var "x", Var "y"))), e2), IConst 2));

(* BubbleSort *)

fun bubbleSort(l) = 
  let
    fun sortAux (h1::h2::t) = if h1 < h2 then h1::(sortAux(h2::t)) else h2::(sortAux(h1::t))
      | sortAux (l) = l
    fun size (h::t) = 1 + size(t)
      | size ([]) = 0
  in
    let 
      fun for (0) (l) =l
        | for (n) (h::t) = for (n-1) (sortAux(h::t))
    in
      for (size(l)) (l)
    end
  end;

bubbleSort([1,85,83,36,6,995,31,30,5]);

(* MergeSort *)

fun mergeSort(l) =
  let
    fun fori (l) (0) = l
      | fori (h::t) (n) = fori (t) (n-1)
    fun forj (l) (0) = []
      | forj (h::t) (n) = h::(forj (t) (n-1))
  in
    let 
      fun ordena (h1::t1) ([]) = h1::t1
        | ordena ([]) (h2::t2) = h2::t2
        | ordena (h1::t1) (h2::t2) = if h1 < h2 then h1::(ordena (t1) (h2::t2)) else h2::(ordena (h1::t1) (t2))
      fun size (h::t) = 1 + size(t)
        | size ([]) = 0
    in
      let
        fun merge ([n1,n2]) = if n1 > n2 then [n2,n1] else [n1,n2]
          | merge ([n]) = [n]
          | merge (h::t) = 
          let  
            val l1 = merge (forj (h::t) (size(h::t) div 2))
            val l2 = merge (fori (h::t) (size(h::t) div 2))
          in
            ordena l1 l2
          end
      in
        merge(l)
      end
    end
  end;

val test = [18,765,574,3891,309,932,682,626,72];
mergeSort(test);

(* Graph

type grafo = (int * int list) list;

fun BFS (g) (init) = 
  let
    fun for ([]) (n) = []
      | for (((id, l)::t) : grafo) (n) = if id = n then l else for (t) (n);
    fun size (h::t) = 1 + size(t)
      | size ([]) = 0
    fun l (0) = []
      | l (size) = (l (size-1))@[(size, 0)]
    fun remove ([]) (n) = []
      | remove (((id, l)::t) : grafo) (n) = if id = n then t else (id, l)::(remove t n)
    fun addAux ([]) (n) (r:int) = []
      | addAux ((i1, i2)::t) (n : int) r = if i1 = n then (i1, r)::t else (i1, i2)::(addAux (t) (n) r)
    (* fun add (l) ([]) (r:int) = l
      | add (l : (int * int) list) ((h::t) : int list) r : (int * int) list = add (addAux l h r) t r *)
    fun union [] [] = []
      | union (l : (int * int) list) [] = l
      | union (((i1,j1)::t1) : (int * int) list) (((i2,j2)::t2) : (int * int) list) = if j1 < j2 then (i1, j1)::(union t1 t2) else (i2, j2)::(union t1 t2)
  in
    let
      fun percorre (g:grafo) (i) (l:(int*int)list) (r:int) = 
        let
          val p1 = for g i
          val p2 = remove g i
          val p3 = r+1
          val p4 = addAux l i r
        in
          let
            fun percorreAux (p2) ([]) (p4) (p3) = []
              | percorreAux (p2) (p1::t) (p4) (p3) = union (percorre p2 p1 p4 p3) (percorreAux p2 t p4 p3)
          in 
            percorreAux p2 p1 p4 p3
          end
        end
    in
      percorre g init (l (size g)) 0
    end
  end;

BFS [(1,[2,3]),(2,[]),(3,[2]),(4,[1])] 1

val g = [(1,[2,3]),(2,[]),(3,[2]),(4,[1])];
val i = 1;
val r = 0;
val l1 = l (size g);
val p1 = for g i;
val p2 = remove g i;
val p3 = r+1;
val p4 = addAux l1 i r;

percorre g i (l1) 0 *)
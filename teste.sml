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

(* Sintaxe e Semantica / Binding variables / Closures*)

fun isIn (x : string) (h::t) = h = x orelse (isIn x t)
  | isIn x [] = false;

fun union l [] = l
  | union l (h::t) = if isIn h l then union l t else union (h::l) t;

fun insert x l = if isIn x l then l else (x::l);

union (["1","2","3","4"]) (["2","4","5","6"]);

type mem = (string * int) list;
datatype bexpr = BConst of bool | And of bexpr * bexpr | Or of bexpr * bexpr | Not of bexpr;

fun beval (BConst e) = e
  | beval (And(e1, e2)) = beval(e1) andalso beval(e2)
  | beval (Or(e1, e2)) = beval(e1) orelse beval(e2)
  | beval (Not(e)) = not(beval(e));

datatype iexpr = IConst of int | BConst of bool | Prim2 of string * iexpr * iexpr | Prim1 of string * iexpr | Ite of iexpr * iexpr * iexpr 
               | Var of string | Let of string * iexpr * iexpr | LetFun of string * string * iexpr * iexpr | Call of string * iexpr;

type 'v env = (string * 'v) list;

datatype value = Int of int | Closure of string * string * iexpr * value env;

fun intToBool 1 = true
  | intToBool 0 = false
  | intToBool _ = raise Match;

fun boolToInt true = 1
  | boolToInt false = 0;

fun lookup x [] = raise Match
  | lookup x (((f, v)::t) : value env) = if x = f then v else lookup x t;

fun eval (IConst e) (m : value env) = e 
  | eval (BConst e) (m) = boolToInt e 
  | eval (Prim2(a, e1, e2)) (m) =
  (
    case a of
      "+" => (eval e1 m) + (eval e2 m)
    | "-" => (eval e1 m) - (eval e2 m)
    | "*" => (eval e1 m) * (eval e2 m)
    | "/" => (eval e1 m) div (eval e2 m)
    | "=" => if (eval e1 m) = (eval e2 m) then 1 else 0
    | ">" => if (eval e1 m) > (eval e2 m) then 1 else 0
    | "and" => boolToInt ((intToBool (eval e1 m)) andalso (intToBool (eval e2 m)))
    | "or" => boolToInt ((intToBool (eval e1 m)) orelse (intToBool (eval e2 m)))
    | _ => raise Match
  )
  | eval (Prim1(a, e)) (m) =
  (
    case a of
      "-" => ~(eval e m)
    | "not" => boolToInt (not (intToBool (eval e m)))
    | _ => raise Match
  )
  | eval (Ite(c, e1, e2)) (m) = if intToBool(eval c m) then eval e1 m else eval e2 m
  | eval (Var x) (m) = 
    let
      val v = lookup x m
    in
      case v of
        (Int i) => i
      | (Closure _) => raise Match
    end
  | eval (Let(x, v, e)) (m) = eval e (((x, Int (eval v m)))::m)
  | eval (Call(f, x)) (m) =
    let
      val v = lookup f m
    in
      case v of
        (Int _) => raise Match
      | (Closure(f, arg, e, fSt)) => eval e (((arg, eval x m))::(f, v)::m)
    end

val e7 = Let("y", IConst 49, Prim2("*", Var "x", Prim2("+", IConst 3, Var "y")));
eval e7 [("x", 1)];

fun freeVars (IConst e) (m) = []
  | freeVars (Plus(e1, e2)) (m) = union (freeVars e1 m) (freeVars e2 m)
  | freeVars (Minus(e1, e2)) (m) = union (freeVars e1 m) (freeVars e2 m)
  | freeVars (Ite(b, e1, e2)) (m) = union (freeVars e1 m) (freeVars e2 m)
  | freeVars (Var x) (m) = if isIn x m then [] else [x]
  | freeVars (Let(x, v, e)) (m) = union (freeVars v m) (freeVars e (x::m));

fun closed (e : iexpr) = (freeVars e [] = []);
exception NonClosed;

fun run (e : iexpr) = if closed e then eval e [] else raise NonClosed;

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

(* Lista 2 *)

datatype expr = IConst of int | Plus of expr * expr | Minus of expr * expr |
                Multi of expr * expr | Div of expr * expr | Max of expr * expr |
                Min of expr * expr | Eq of expr * expr | Gt of expr * expr;

fun eval (IConst i) = i
  | eval (Plus(e1, e2)) = (eval e1) + (eval e2)
  | eval (Minus(e1, e2)) = (eval e1) - (eval e2)
  | eval (Multi(e1, e2)) = (eval e1) * (eval e2)
  | eval (Div(e1, e2)) = if (eval e2) = 0 then 0 else (eval e1) div (eval e2)
  | eval (Max(e1, e2)) = if (eval e1) > (eval e2) then eval e1 else eval e2
  | eval (Min(e1, e2)) = if (eval e1) < (eval e2) then eval e1 else eval e2
  | eval (Eq(e1, e2)) = if (eval e1) = (eval e2) then 1 else 0
  | eval (Gt(e1, e2)) = if (eval e1) > (eval e2) then 1 else 0;


datatype area = RConst of real | AQuadrado of area | ARetangulo of area * area | ACirculo of area;

fun eval (RConst r) = r
  | eval (AQuadrado a) = (eval a) * (eval a)
  | eval (ARetangulo(a1, a2)) = (eval a1) * (eval a2)
  | eval (ACirculo(a)) = 3.14 * (eval a) * (eval a);


datatype perimetro = RConst of real | PQuadrado of perimetro | PRetangulo of perimetro * perimetro | PCirculo of perimetro | PTriangulo of perimetro * perimetro * perimetro;

fun eval (RConst r) = r
  | eval (PQuadrado p) = 4.0 * (eval p)
  | eval (PRetangulo(p1, p2)) = 2.0 * (eval p1) + 2.0 * (eval p2)
  | eval (PCirculo(p)) = 2.0 * 3.14 * (eval p)
  | eval (PTriangulo(p1, p2, p3)) = (eval p1) + (eval p2) + (eval p3);

eval(PTriangulo(RConst 9.978, RConst 8.008, RConst 0.600))


datatype UnOp = Not;
datatype BinOp = Add | Sub | Mul | Gt | Eq | Or;
datatype Sexpr = IConst of int | Op1 of UnOp * Sexpr | Op2 of BinOp * Sexpr * Sexpr;

fun simplify (IConst i) = IConst i
  | simplify (Op2(Add, e1, e2)) = if (simplify e1) = IConst 0 then simplify e2 else if (simplify e2) = IConst 0 then simplify e1 else Op2(Add, simplify e1, simplify e2)
  | simplify (Op2(Sub, e1, e2)) = if (simplify e2) = IConst 0 then simplify e1 else if (simplify e1) = (simplify e2) then IConst 0 else Op2(Sub, simplify e1, simplify e2)
  | simplify (Op2(Mul, e1, e2)) = if (simplify e1) = IConst 0 orelse (simplify e2) = IConst 0 then IConst 0 else if (simplify e1) = IConst 1 then simplify e2 else if (simplify e2) = IConst 1 then simplify e1 else Op2(Mul, simplify e1, simplify e2)
  | simplify (Op2(Gt, e1, e2)) = Op2(Gt, (simplify e1), (simplify e2))
  | simplify (Op2(Eq, e1, e2)) = Op2(Eq, (simplify e1), (simplify e2))
  | simplify (Op2(Or, e1, e2)) = if (simplify e1) = (simplify e2) then simplify e1 else Op2(Or, simplify e1, simplify e2)
  | simplify (Op1(Not, e)) =
    let
      fun aux (Op1(Not, s))  = (s, true)
        | aux (_) = (IConst 0 , false)
      val simp = simplify e
    in
      if (#2 (aux simp)) = true then (#1 (aux simp)) else (Op1(Not, simp))
    end;


fun count_main(n) =
  let
    fun count(i) = if i = n then [n] else i::count(i+1)
  in
    count(1)
  end;


fun pow(n) =
  let
    fun calculePow(n) = n * n
  in
    calculePow(n)
  end;


fun split(l) = 
  let
    fun sp1 (h1::h2::t) = h1::(sp1 t)
      | sp1 (h::t) = [h]
      | sp1 ([]) = []
    fun sp2 (h1::h2::t) = h2::(sp2 t)
      | sp2 (h::t) = sp2 t
      | sp2 ([]) = []
  in
    (sp1 l, sp2 l)
  end;


fun bad_max ( xs : int list ) =
  if null xs
    then 0
  else if null ( tl xs )
    then hd xs
  else if hd xs > bad_max ( tl xs )
    then hd xs
  else bad_max ( tl xs ) ;

fun bad_max ( xs : int list ) =
  if null xs
    then 0
  else if hd xs > bad_max ( tl xs )
    then hd xs
  else bad_max ( tl xs ) ;

fun good_max([]) = 0
  | good_max(h::t) = if h > good_max(t) then h else good_max(t);

bad_max([1,5,7,9,3,5,6]);

good_max([1,5,7,9,3,5,6]);

fun expr () =
  let
    val x = 1
  in
    let
      val y = x + 2
      val x = 2
    in
      (x + 1) + (y + 1)
    end
  end;

expr()

(* (x = 2 , x + 1) + (y = x + 2 , y + 1) *)

type Num = int ;
type Var = string ;

datatype Aexpr =
N of Num
| V of Var
| Plus of Aexpr * Aexpr
| Mult of Aexpr * Aexpr
| Minus of Aexpr * Aexpr ;

datatype Bexpr =
True
| False
| Eq of Aexpr * Aexpr
| Leq of Aexpr * Aexpr
| Not of Bexpr
| And of Bexpr * Bexpr ;

datatype Stm =
Assign of Var * Aexpr
| Skip
| Comp of Stm * Stm
| If of Bexpr * Stm * Stm
| While of Bexpr * Stm 
| Repeat of Stm * Bexpr;

fun evalN n : Num = n

exception FreeVar ;
fun lookup [] id = raise FreeVar
| lookup (( k : string , v ) :: l ) id = if id = k then v else lookup l id ;

fun evalA ( N n ) _ = evalN n
| evalA ( V x ) s = lookup s x
| evalA ( Plus ( e1 , e2 ) ) s = ( evalA e1 s ) + ( evalA e2 s )
| evalA ( Mult ( e1 , e2 ) ) s = ( evalA e1 s ) * ( evalA e2 s )
| evalA ( Minus ( e1 , e2 ) ) s = ( evalA e1 s ) - ( evalA e2 s ) ;

fun evalB True _ = true
| evalB False _ = false
| evalB ( Eq ( a1 , a2 ) ) s = ( evalA a1 s ) = ( evalA a2 s )
| evalB ( Leq ( a1 , a2 ) ) s = ( evalA a1 s ) <= ( evalA a2 s )
| evalB ( Not b ) s = not ( evalB b s )
| evalB ( And ( b1 , b2 ) ) s = ( evalB b1 s ) andalso ( evalB b2 s ) ;

fun evalStm ( stm : Stm ) ( s : ( string * int ) list ) : ( string * int ) list =
  case stm of
  ( Assign (x , a ) ) => (x , evalA a s ) :: s
  | Skip => s
  | ( Comp ( stm1 , stm2 ) ) => evalStm stm2 ( evalStm stm1 s )
  | ( If (b , stm1 , stm2 ) ) =>
  if ( evalB b s ) then evalStm stm1 s else evalStm stm2 s
  | While (b, stm ) => if (evalB b s) then 
  let 
    val w2 = While(b, stm)
    val s2 = evalStm stm s
  in
    evalStm w2 s2
  end
  else s
  | Repeat (stm, b) =>
  let 
    val w2 = Repeat(stm, b)
    val s2 = evalStm stm s
  in
    if (evalB b s2) then s2 else evalStm w2 s2
  end;
  (* | _ => raise Match ; *)

val teste = While(Leq(V "x", N 10), Assign("x", Plus(V "x", N 1)));

val s = [("x", 1)];

evalStm teste s;

val teste2 = Repeat(Assign("x", Minus(V "x", N 1)), Leq(V "x", N 10));

val s2 = [("x", 20)];

evalStm teste2 s2;

(* Prova 1 *)
(*3*)
fun f ((h::t) : int list) = (h mod 2) = 0 orelse f(t)
  | f ([]) = false;


f ([1,3,5,7,2])

(*4*)
fun vconc (l: (string * string) list) =
  case l of
    nil => ("","")
    | (h::t) =>
      let 
        val headValue1 = #1 h;
        val headValue2 = #2 h;
        val recValue = vconc t;
        val recValue1 = #1 recValue;
        val recValue2 = #2 recValue
      in
        (headValue1 ^ recValue1, headValue2 ^ recValue2)
      end;

fun aux (l1 : (string * string), l2 : (string * string)) = ((#1 l1) ^ (#1 l2), (#2 l1) ^ (#2 l2))

foldr (fn (l1 : (string * string), l2 : (string * string)) => ((#1 l1) ^ (#1 l2), (#2 l1) ^ (#2 l2))) ("","") [("a","b"),("c","d"),("e","f")];

fun filter2 _ [] = []
  | filter2 f (h::t) = if f(h) then h::(filter f t) else (filter f t);

fun map2 _ [] = []
  | map2 f (h::t) = f(h)::(map f t);

fun foldl2 _ acc [] = acc
  | foldl2 f acc (h::t) = foldl2 f (f(h,acc)) t;

filter2 (fn x => (x mod 2) = 0) [1,2,3,4,5,6];

map2 (fn x => x) [1,2,3,4,5];

foldl2 (fn (l1 : (string * string), l2 : (string * string)) => ((#1 l1) ^ (#1 l2), (#2 l1) ^ (#2 l2))) ("","") [("a","b"),("c","d"),("e","f")];


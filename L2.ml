(*  T ::= int | bool | T1 --> T2 |  T1 * T2  *)

type tipo = 
    TyInt 
  | TyBool
  | TyFn of tipo * tipo
  | TyPair of tipo * tipo 
  | TyRef of tipo
  | TyUnit
              
type ident = string
  
type bop = Sum | Sub | Mult  | Gt | Lt | Geq | Leq | Eq
  
   (* e ::= n | x | b | e1 op e2 
| (e1,e2) | fst e | snd e
| if e1 then e2 else e3
| fn x:T => e | e1 e2 | let x:T = e1 in e2
| let rec f: T1--> T2 = fn x: T1 => e1 in e2 *)
    
type expr  = 
    Num of int  
  | Var of ident 
  | True
  | False
  | Binop of bop * expr * expr
  | Pair of expr * expr 
  | Fst of expr
  | Snd of expr 
  | If of expr * expr * expr
  | Fn of ident * tipo * expr  
  | App of expr * expr
  | Let of ident * tipo * expr * expr
  | LetRec of ident * tipo * expr  * expr
  | Asg of expr * expr
  | Dref of expr
  | New of expr
  | Seq of expr * expr
  | Whl of expr * expr
  | Skip 
              
type amb = (ident * tipo) list 
  
    
type valor =
  | NumV of int
  | TrueV
  | FalseV
  | ClosV  of ident * expr * bsamb
  | RclosV of ident * ident * expr * bsamb
  | SkipV
  | IdentV of ident
and
  bsamb = (ident * valor) list
    
type mem = (int * valor) list  
    
let empty_gamma : amb = []
  
    
let rec lookup gamma x = 
  match gamma with
    []          -> None
  | (y,t) :: tl -> if (y = x) then Some t else lookup tl x
  
let rec update gamma x t = 
  (x,t) :: gamma
  
  
let empty_mem : mem = []

(* TypeError é ativada se programador L1 escreveu expressão mal tipada *) 

exception TypeError of string 
                              
(* BugParser ativada se parser deixou passar expressão c/ erro de sintaxe *)

exception BugParser 
  
                                
(* NotInMemory ativada se tenta acessar endereço que não está na memória *)

exception NotInMemory
  
  
  
let find_max (mem: (int * valor) list) = 
  let custom_max (n: int) (p: int * valor )= if n > fst(p) then n else fst(p) in
  List.fold_left custom_max (-1) mem 
    
    
let rec read_memory (mem: (int * valor) list) (v: int) : valor = 
  match mem with
    [] -> raise NotInMemory
  | (address, value) :: xs -> if address = v then value else read_memory xs v
    
let rec typeinfer (gamma: amb) (e:expr) : tipo  = 
  match e with
  
  | Num _ -> TyInt 
    
  | Var x -> 
      (match lookup gamma x with
         Some t -> t
       | None   -> raise (TypeError ("variavel nao declarada:" ^ x)))
      
  | True  -> TyBool
  | False -> TyBool 
  
    (*  G |-- e1:int    G |-- e2:int     bop in {+,-,*}
------------------------------------------------
G |-- e1  bop  e2 : int 
                 
G |-- e1:int    G |-- e2:int     bop in {=, <, >, >=, <=,*}
----------------------------------------------------------
G |-- e1  bop  e2 : bool
                
*) 
    
  | Binop(oper,e1,e2) -> 
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      if t1 = TyInt && t2 = TyInt then 
        (match oper with
           Sum | Sub | Mult -> TyInt
         | Eq | Lt | Gt | Geq | Leq -> TyBool) 
      else raise (TypeError "operando nao é do tipo int")
  
      
  | Pair(e1,e2) -> TyPair(typeinfer gamma e1, typeinfer gamma e2) 
  | Fst e1 -> 
      (match typeinfer gamma e1 with
         TyPair(t1,_) -> t1
       | _ -> raise (TypeError "fst espera tipo par"))
  | Snd e1 -> 
      (match typeinfer gamma e1 with
         TyPair(_,t2) -> t2
       | _ -> raise (TypeError "fst espera tipo par"))
    
  | If(e1,e2,e3) -> 
      ( match typeinfer gamma e1 with 
          TyBool -> 
            let t2 = typeinfer gamma e2 in
            let t3 = typeinfer gamma e3
            in if t2 = t3 then t2 
            else raise (TypeError "then e else com tipos diferentes")
        | _ -> raise (TypeError "condição de IF não é do tipo bool")) 
      
  | Fn(x,t,e1) -> 
      let t1 = typeinfer (update gamma x t) e1
      in TyFn(t,t1)
        
  | App(e1,e2) -> 
      (match typeinfer gamma e1 with
         TyFn(t, t') ->  if (typeinfer gamma e2) = t then t' 
           else raise (TypeError "tipo argumento errado" )
       | _ -> raise (TypeError "tipo função era esperado"))
           
  | Let(x,t,e1,e2) -> 
      if (typeinfer gamma e1) = t then typeinfer (update gamma x t) e2
      else raise (TypeError "expr nao é do tipo declarado em Let" )
  
          
  | LetRec(f,(TyFn(t1,t2) as tf), Fn(x,tx,e1), e2) -> 
      let gamma_tf = update gamma f tf in
      let gamma_tf_tx = update gamma_tf x tx in
      if (typeinfer gamma_tf_tx e1) = t2 then typeinfer gamma_tf e2
      else raise (TypeError "tipo da funcao diferente do tipo declarado" )
  
   (* se não colocarmos essa ultimo pattern teremos warning:
pattern matching non exhaustive *)  

  | LetRec _ -> raise BugParser 
                  
  | Skip -> TyUnit
    
  | Whl(e1,e2) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      if t1 = TyBool && t2 = TyUnit then TyUnit
      else raise (TypeError "e1 esperava tipo bool e e2 esperava tipo unit")
          
  | Seq(e1,e2) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      if t1 = TyUnit then t2
      else raise(TypeError "e1 esperava tipo unit")
          
  | New(e1) -> TyRef(typeinfer gamma e1)
                  
  | Asg(e1,e2) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      (match t1 with
         TyRef(t) ->
           if t2 = t then TyUnit
           else raise (TypeError "o tipo T de T ref de e1 deve ser igual ao tipo T de e2")
       | _ -> raise (TypeError "e1 esperava tipo T ref"))
      
  | Dref (e1) -> 
      ((match typeinfer gamma e1 with
            TyRef(t) -> t
          | _ -> raise (TypeError "e1 esperava tipo T ref")))
      
  
      

(* função auxiliar que convert tipo para string *)

let rec ttos (t:tipo) : string =
  match t with 
    TyInt  -> "int" 
  | TyBool -> "bool"
  | TyFn(t1,t2)   ->  "("  ^ (ttos t1) ^ " --> " ^ (ttos t2) ^ ")"
  | TyPair(t1,t2) ->  "("  ^ (ttos t1) ^ " * "   ^ (ttos t2) ^ ")" 
  | TyRef t1 -> (ttos t1) ^ " ref"
  | TyUnit -> "unit"
   
                                                                                                    
   (* ========================================= *)
   (*    Avaliador                              *)
   (* ========================================= *) 

exception BugTypeInfer
  
exception  NoRuleApplies 
let compute(oper: bop) (v1: valor) (v2:valor) = match (oper,v1,v2) with
    (Sum, NumV(n1),  NumV(n2)) -> NumV(n1+n2) 
  | (Sub, NumV(n1),  NumV(n2)) -> NumV(n1-n2)
  | (Mult, NumV(n1), NumV(n2)) -> NumV(n1*n2)
  | (Eq, NumV(n1), NumV(n2)) -> if (n1 = n2) then TrueV else FalseV
  | (Gt, NumV(n1), NumV(n2)) -> if (n1 > n2) then TrueV else FalseV
  | (Lt, NumV(n1), NumV(n2)) -> if (n1 < n2) then TrueV else FalseV
  | (Geq, NumV(n1), NumV(n2)) -> if (n1 >= n2) then TrueV else FalseV
  | (Leq, NumV(n1), NumV(n2)) -> if (n1 <= n2) then TrueV else FalseV
  | _ -> raise NoRuleApplies

           
let rec atualiza_mem mem address value =
  match mem with
  | [] -> raise NotInMemory
  | (addr, _) :: tl when addr = address -> (address, value) :: tl
  | entry :: tl -> entry :: atualiza_mem tl address value           
           
let rec avalia(amb:bsamb) (mem:mem) (e:expr): (valor * mem) =
  match e with
  
  | Num n -> (NumV n, mem)
             
  | True -> (TrueV, mem)
            
  | False -> (FalseV, mem) 
             
  | Skip -> (SkipV, mem)
             
  | Var x ->
      (match lookup amb x with
         Some v -> (v, mem)
       | None -> raise BugParser)
             
  | Binop(bop, e1, e2) -> (
      let v1, mem = avalia amb mem e1 in
      let v2, mem = avalia amb mem e2 in
      (compute bop v1 v2,mem)
    ) 
    
  | If(e1, e2, e3) -> (
      let (v1, mem) = avalia amb mem e1 in
      let (v2, mem) = avalia amb mem e2 in
      
      match avalia amb mem e1 with
        TrueV, mem  -> avalia amb mem e2     
      | FalseV, mem -> avalia amb mem e3 
      | _ -> raise BugParser
    )
    
  | Fn(id, ty, ex) -> (ClosV(id,ex,amb),mem)
                      
  | Let(id, ty, e1, e2) -> 
      let v1, mem = avalia amb mem e1 in 
      avalia (update amb id v1) mem e2 
                      
  | App (e1, e2) ->(
      let v1, _ = avalia amb mem e1 in
      let v2, _ = avalia amb mem e2 in
      match v1 with
        ClosV(x,eb,amb') -> 
          let amb'' = update amb' x v2 in
          avalia amb'' mem eb
              
            
      |RclosV(f,x,eb,amb') -> 
          let amb'' = update amb' x v2 in
          let amb''' = update amb'' f v1 in
          avalia amb''' mem eb 
      |_ -> raise BugParser)
              
  | New e -> (
      let v1, mem = avalia amb mem e in 
      match find_max mem with
        (-1) -> (NumV 0), [(0,v1)]
      | n -> (NumV (n+1)), (n+1, v1) :: mem )
    
  | Dref e -> 
      let v1 = avalia amb mem e in 
      let n = (match v1 with 
            ((NumV x), mem) -> x
          | _ -> raise BugParser) in
      (read_memory mem n, mem)
  
  | Whl(e1, e2) ->
      let (v1, mem') = avalia amb mem e1 in
      (match v1 with
       | TrueV -> 
           let (v2, mem'') = avalia amb mem' e2 in
           avalia amb mem'' (Whl(e1, e2))
       | FalseV -> (SkipV, mem')
       | _ -> raise (TypeError "A condição do loop não é do tipo bool.")
      ) 

  | Asg(e1, e2) ->
      let v1, mem' = avalia amb mem e1 in
      let v2, mem'' = avalia amb mem' e2 in
      (match v1 with
       | NumV(address) ->
           let mem''' = atualiza_mem mem'' address v2 in
           (SkipV, mem''')
       | _ -> raise (TypeError "Erro: tentativa de atribuição em endereço não-inteiro da memória.")
      )
                      
  | Seq(e1,e2) ->
      let (v1, mem) = avalia amb mem e1 in
      (match v1 with
         SkipV -> avalia amb mem e2
       | _ -> raise BugTypeInfer)
      
  | LetRec(f,TyFn(t1,t2),Fn(x,tx,e1), e2) when t1 = tx ->
      let amb'= update amb f (RclosV(f,x,e1,amb))
      in avalia amb' mem e2
          
  |_ -> raise BugParser
      
  
let rec vtos (v:valor) : string = match v with
    NumV n1 -> string_of_int n1
  | TrueV -> "true"
  | FalseV -> "false" 
  | ClosV _ -> "<fn>"
  | RclosV _ -> "<fn>"
  | SkipV -> "skip"
  | IdentV _ -> "ident"
                  (*| _ ->  raise (Invalid_argument "not a vlue")*)
            
   
(* Função auxiliar para imprimir mem*)        
let rec mem_to_string mem =
  match mem with
  | [] -> ""
  | (key, value) :: rest ->
      "l" ^ string_of_int key ^ ": " ^ (vtos value) ^ "\n" ^mem_to_string rest
     
            
let int_st (e:expr)  = 
  try
    let t = typeinfer empty_gamma e in
    let (v, mem) = avalia [] empty_mem e
    in  print_endline ("valor = " ^ (vtos v) ^ " : tipo " ^ (ttos t));
    print_endline ("Memoria:\n" ^ (mem_to_string mem))
  with 
    TypeError msg -> print_string ("erro de tipo: " ^ msg) 
  | BugParser -> print_string "corrigir bug no typeinfer"
  | BugTypeInfer ->  print_string "corrigir bug do parser para let rec" 
                       

(* ---------------------------- TESTES  ---------------------------- *) 
  
let teste1 = Let("x", TyRef TyInt, New (Num 3),
                 Let("y", TyInt, Dref (Var "x"), 
                     Seq(Asg(Var "x", Binop(Sum, Dref(Var "x"), Num 1)), 
                         Binop(Sum, Var "y",  Dref (Var "x"))))) 
     
 
    
let teste2 = Let("x", TyRef TyInt, New (Num 0),
                 Let("y", TyRef TyInt, Var "x", 
                     Seq(Asg(Var "x", Num 1), 
                         Dref (Var "y"))))
       
let counter1 = Let("counter", TyRef TyInt, New (Num 0),
                   Let("next_val", TyFn(TyUnit, TyInt),
                       Fn("w", TyUnit, 
                          Seq(Asg(Var "counter",Binop(Sum, Dref(Var "counter"), Num 1)),
                              Dref (Var "counter"))),
                       Binop(Sum, App (Var "next_val", Skip), 
                             App (Var "next_val", Skip))))
    
      
let whilefat = Whl(Binop(Gt, Dref (Var "z"), Num 0), 
                   Seq( Asg(Var "y", Binop(Mult, Dref (Var "y"), Dref (Var "z"))), 
                        Asg(Var "z", Binop(Sub,  Dref (Var "z"), Num 1)))                       
                  ) 
                               
                             
let bodyfat = Let("z", 
                  TyRef TyInt, 
                  New (Var "x"),
                  Let("y", 
                      TyRef TyInt, 
                      New (Num 1), 
                      Seq (whilefat, Dref (Var "y"))))
    
let impfat = Let("fat", 
                 TyFn(TyInt,TyInt), 
                 Fn("x", TyInt, bodyfat), 
                 App(Var "fat", Num 5))
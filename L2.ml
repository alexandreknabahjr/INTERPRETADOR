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
  | ClosV  of ident * expr * amb
  | RclosV of ident * ident * expr * amb
  | SkipV
  | IdentV of ident
    
type mem = (ident * valor) list  
    
let empty_gamma : amb = []
  
    
let rec lookup (gamma: amb) (x:ident) : tipo option = 
  match gamma with
    []          -> None
  | (y,t) :: tl -> if (y = x) then Some t else lookup tl x
  
let rec update (gamma: amb) (x:ident) (t:tipo) : amb = 
  (x,t) :: gamma
  
  
let empty_mem : mem = []

(* TypeError é ativada se programador L1 escreveu expressão mal tipada *) 

exception TypeError of string 
                              
(* BugParser ativada se parser deixou passar expressão c/ erro de sintaxe *)

exception BugParser
    
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
           
           
let rec avalia(amb:amb) (mem:mem) (e:expr): (valor * mem) =
  match e with
  
  | Num n -> (NumV n, mem)
             
  | True -> (TrueV, mem)
            
  | False -> (FalseV, mem)
             
  |_ -> raise BugParser
  
           
exception BugTypeInfer 
          
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
      key ^ ": " ^ (vtos value) ^ "\n" ^ mem_to_string rest
     
            
let int_st (e:expr)  = 
  try
    let t = typeinfer empty_gamma e in
    let (v, mem) = avalia empty_gamma empty_mem e
    in  print_endline ((vtos v) ^ " : " ^ (ttos t));
    print_endline ("Memory:\n" ^ (mem_to_string mem))
  with 
    TypeError msg -> print_string ("erro de tipo: " ^ msg) 
  | BugParser -> print_string "corrigir bug no typeinfer"
  | BugTypeInfer ->  print_string "corrigir bug do parser para let rec"      
            
            (*let int_st (e:expr)  = 
try
let t = typeinfer empty_gamma e in
let v = evalst e  
in  print_string ((vtos v) ^ " : " ^ (ttos t))
with 
TypeError msg -> print_string ("erro de tipo: " ^ msg)
      
| BugParser -> print_string "corrigir bug no typeinfer"
| BugTypeInfer ->  print_string "corrigir bug do parser para let rec" *)
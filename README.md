# Avaliador Big Step

Extensão da Linguagem L1 com instruções imperativas.

## Sintaxe Abstrata

 e ∈ Expr
 e ::= n | b | e1ope2 | if e1 then e2 else e3 | x | e1 e2 | fn x :T ⇒ e | let x:T = e1 in e2 | 
       let rec f:T1 →T2 = (fn x:T1 ⇒ e1) in e2 | e1:= e2 | ! e | new e | skip | while e1 do e2 | 
       e1; e2 | l

## Progresso

- TypeInfer ⭕
  - Asg ✅ 
  - Dref ❌
  - New ✅
  - Seq ✅
  - Whl ✅
  - Skip ✅
- Avaliador ⭕
  - Asg ❌ 
  - Dref ❌
  - New ❌
  - Seq ❌
  - Whl ❌
  - Skip ❌

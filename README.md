# Avaliador Big Step

Extensão da Linguagem L1 com instruções imperativas.

## Sugestões de testes para o TypeInfer:

- Asg: int_st (Asg(New(If(True, Num 10, Num 20)), Num 10)) deve imprimir unit ✅
- Dref: int_st (Dref(New(If(True, Num 10, Num 20)))) deve imprimir int ✅
- New: int_st (New(If(True, Num 10, Num 20))) deve imprimir int ref ✅
- Seq: int_st (Seq(Skip, If(True, Num 10, Num 20))) deve imprimir int ✅
- Whl: int_st (Whl(True, Skip)) deve imprimir unit ✅
- Skip: int_st Skip deve imprimir unit ✅

## Progresso

- TypeInfer ✅
  - Asg ✅ 
  - Dref ✅
  - New ✅
  - Seq ✅
  - Whl ✅
  - Skip ✅
- Avaliador ⭕
  - Num ✅
  - True ✅
  - False ✅
  - Var ✅
  - Binop ✅
  - If ✅
  - Fn ✅
  - Let ✅
  - LetRec ✅
  - App ✅
  - Asg ❌ 
  - Dref ✅
  - New ✅
  - Seq ✅
  - Whl ✅
  - Skip ✅

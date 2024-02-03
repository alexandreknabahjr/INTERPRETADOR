# Avaliador Big Step

Extensão da Linguagem L1 com instruções imperativas.

## Testes para o TypeInfer:

- Asg: teste_typeinfer (Asg(New(If(True, Num 10, Num 20)), Num 10)) deve imprimir unit ✅
- Dref: teste_typeinfer (Dref(New(If(True, Num 10, Num 20)))) deve imprimir int ✅
- New: teste_typeinfer (New(If(True, Num 10, Num 20))) deve imprimir int ref ✅
- Seq: teste_typeinfer (Seq(Skip, If(True, Num 10, Num 20))) deve imprimir int ✅
- Whl: teste_typeinfer (Whl(True, Skip)) deve imprimir unit ✅
- Skip: teste_typeinfer Skip deve imprimir unit ✅

## Testes para o Intepretador:

- int_bse (teste1) deve imprimir valor = 7 e memória 4 em l0 ✅
- int_bse (teste2) deve imprimir valor = 1 e memória 1 em l0 ✅
- int_bse (counter1) deve imprimir valor = 3 e memória 2 em l0 ✅
- int_bse (impfat) deve imprimir valor = 120, memória 0 em l0 e memória 120 em l1✅

## Progresso

- TypeInfer ✅
  - Asg ✅ 
  - Dref ✅
  - New ✅
  - Seq ✅
  - Whl ✅
  - Skip ✅
- Avaliador ✅
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
  - Asg ✅ 
  - Dref ✅
  - New ✅
  - Seq ✅
  - Whl ✅
  - Skip ✅

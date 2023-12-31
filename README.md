# Avaliador Big Step

Extensão da Linguagem L1 com instruções imperativas.

## Sugestões de testes para o TypeInfer:

- Asg: ⭕
- Dref: int_st (Dref(New(If(True, Num 10, Num 20)))) deve retornar int ✅
- New: int_st (New(If(True, Num 10, Num 20))) deve retornar intref ✅
- Seq: int_st (Seq(Skip, If(True, Num 10, Num 20))) deve retornar int ✅
- Whl: int_st (Whl(True, Skip)) deve retornar unit ✅
- Skip: int_st Skip deve retornar unit ✅

## Progresso

- TypeInfer ✅
  - Asg ✅ 
  - Dref ✅
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


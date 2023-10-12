module MiniHs.Test where
import MiniHs.Lexer
import MiniHs.Parser
import MiniHs.Expr
import MiniHs.Inter as S
import MiniHs.InterEnv as E
import MiniHs.Expr as Value

mugiCheck :: IO Bool  -> IO ()
mugiCheck a = do a <- a;
                 if a
                 then putStrLn "Respiracion de Haskell... Primera postura, Acierto en el test!"
                 else putStrLn  "Mal! Donificaste a Rengoku :C"

s_step_n :: Int -> Expr -> Expr
s_step_n n e = if n == 0
            then e
            else s_step_n  (n-1) (S.step e)

e_step_n :: Int -> State -> State
e_step_n n e = if n == 0
            then e
            else e_step_n  (n-1) (E.step e)

test_parser :: FilePath -> IO Expr
test_parser file = parser . lexer <$> readFile file

pgm1 = test_parser "MiniHs/mypgm/simple_fun.minihs"
pgm2 = test_parser "MiniHs/mypgm/alcance.minihs"
pgm3 = test_parser "MiniHs/mypgm/fact.minihs"
pgm4 = test_parser "MiniHs/mypgm/fact_tail.minihs"
pgm5 = test_parser "MiniHs/mypgm/reverse.minihs"
pgm6 = test_parser "MiniHs/mypgm/hard_pgm.minihs"

test_inter :: (Expr -> Value) -> IO Expr -> IO Value
test_inter inter e = inter <$> e

test_infer :: (Expr -> Type) -> IO Expr -> IO Type
test_infer infer e = infer <$> e

expected_s1 = NumV 8
expected_s2 = NumV 7
expected_s3 = NumV 120
expected_s4 = NumV 720
expected_s5 = ConsV (NumV 10) (ConsV (NumV 9) (ConsV (NumV 8) (ConsV (NumV 7) (ConsV (NumV 6) (ConsV (NumV 5) (ConsV (NumV 4) (ConsV (NumV 3) (ConsV (NumV 2) (ConsV (NumV 1) NilV)))))))))
expected_s6 = LamV "z" (App (Fix "identity" (Lambda "x" Nat (V "x"))) (V "z")) -- este es el unico test que pueden cambiar

expected_it1 = Nat
expected_it2 = Nat
expected_it3 = Nat
expected_it4 = Nat
expected_it5 = List Nat
expected_it6 = Arrow Nat Nat

test_s1 = (== expected_s1) <$> test_inter S.inter pgm1
test_s2 = (== expected_s2) <$> test_inter S.inter pgm2
test_s3 = (== expected_s3) <$> test_inter S.inter pgm3
test_s4 = (== expected_s4) <$> test_inter S.inter pgm4
test_s5 = (== expected_s5) <$> test_inter S.inter pgm5
test_s6 = (== expected_s6) <$> test_inter S.inter pgm6

test_e1 = (== expected_s1) <$> test_inter E.inter pgm1
test_e2 = (== expected_s2) <$> test_inter E.inter pgm2
test_e3 = (== expected_s3) <$> test_inter E.inter pgm3
test_e4 = (== expected_s4) <$> test_inter E.inter pgm4
test_e5 = (== expected_s5) <$> test_inter E.inter pgm5
test_e6 = (== expected_s6) <$> test_inter E.inter pgm6

test_it1 = (== expected_it1) <$> test_infer infer pgm1
test_it2 = (== expected_it2) <$> test_infer infer pgm2
test_it3 = (== expected_it3) <$> test_infer infer pgm3
test_it4 = (== expected_it4) <$> test_infer infer pgm4
test_it5 = (== expected_it5) <$> test_infer infer pgm5
test_it6 = (== expected_it6) <$> test_infer infer pgm6

main = do
    putStrLn " ======== Test Interprete Substitucion ======== "
    putStr "Funcion Simple: "
    mugiCheck test_s1
    putStr "Alcance Estatico: "
    mugiCheck test_s2
    putStr "Funcion Recursiva 1 (Factorial): "
    mugiCheck test_s3
    putStr "Funcion Recursiva 2 (Factorial_Tail): "
    mugiCheck test_s4
    putStr "Funcion Lista (Reversa): "
    mugiCheck test_s5
    putStr "Funcion como Valor:  "
    mugiCheck test_s6
    putStrLn ""
    putStrLn " ======== Test Interprete Ambientes ======== "
    putStr "Funcion Simple: "
    mugiCheck test_e1
    putStr "Alcance Estatico: "
    mugiCheck test_e2
    putStr "Funcion Recursiva 1 (Factorial): "
    mugiCheck test_e3
    putStr "Funcion Recursiva 2 (Factorial_Tail): "
    mugiCheck test_e4
    putStr "Funcion Lista (Reversa): "
    mugiCheck test_e5
    putStr "Funcion como Valor:  "
    mugiCheck test_e6
    putStrLn ""
    putStrLn " ======== Test Inferencia de Tipos ======== "
    putStr "Funcion Simple: "
    mugiCheck test_it1
    putStr "Alcance Estatico: "
    mugiCheck test_it2
    putStr "Funcion Recursiva 1 (Factorial): "
    mugiCheck test_it3
    putStr "Funcion Recursiva 2 (Factorial_Tail): "
    mugiCheck test_it4
    putStr "Funcion Lista (Reversa): "
    mugiCheck test_it5
    putStr "Funcion como Valor:  "
    mugiCheck test_it6


module Main (main) where


import ArbitragemFunctions
import Test.HUnit


-- Dados de teste
campA = Campeonato (Just 1) "A" "2025-03-15" "L" "M" "Q"
campB = Campeonato (Just 2) "B" "2025-03-20" "L" "M" "Q"
campC = Campeonato (Just 3) "C" "2025-02-10" "L" "M" "Q"
campJan = Campeonato (Just 4) "Jan" "2025-01-05" "L" "M" "Q"
campFeb = Campeonato (Just 5) "Feb" "2025-02-27" "L" "M" "Q"
campeonatosTeste = [campB, campC, campA]

pra2 :: Int -> String
pra2 n = if n < 10 then '0':show n else show n




test1 = TestCase (assertEqual "ordenarPorData" [campC, campA, campB] (ordenarPorData campeonatosTeste))
test2 = TestCase (assertEqual "splitData válido" (Just ("2025","03","15")) (splitData "2025-03-15"))
test3 = TestCase (assertEqual "splitData inválido" Nothing (splitData "2025-03"))
test4 = TestCase (assertEqual "pra2 menor que 10" "05" (pra2 5))
test5 = TestCase (assertEqual "pra2 maior que 10" "12" (pra2 12))
test6 = TestCase (assertEqual "intervaloMes março" True (intervaloMes "3" campA))
test7 = TestCase (assertEqual "intervaloMes abril" False (intervaloMes "4" campA))
test8 = TestCase (assertEqual "intervaloMes fevereiro" True (intervaloMes "2" campFeb))
test9 = TestCase (assertEqual "intervaloMes janeiro" True (intervaloMes "1" campJan))

test10 = TestCase (assertEqual "teste propositalmente errado" "XX" (pra2 5))
test11 = TestCase (assertEqual "teste propositalmente errado" True (intervaloMes "4" campA))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8, TestLabel "test9" test9, TestLabel "test10" test10, TestLabel "test11" test11]



main :: IO ()
main = do
  counts <- runTestTT tests
  print counts

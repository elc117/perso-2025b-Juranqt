{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import Data.List (sort, sortOn)
import Data.List.Split (splitOn)
import System.Exit (exitFailure, exitSuccess)

-- Função pura para filtrar campeonatos por intervalo de datas (usada no frontend)
filtrarPorMes :: String -> [(String, String)] -> [(String, String)]
filtrarPorMes mes campeonatos = filter dentroIntervalo campeonatos
  where
    -- campeonatos :: [(dataCampo, nome)]
    dentroIntervalo (dataCampo, _) =
      case splitData dataCampo of
        Just (ano, mesCampo, dia) ->
          let mesNum = read mes :: Int
              mesAnt = if mesNum == 1 then 12 else mesNum - 1
              anoAnt = if mesNum == 1 then show (read ano - 1) else ano
              dataInicio = anoAnt ++ "-" ++ pad2 mesAnt ++ "-28"
              dataFim = ano ++ "-" ++ pad2 mesNum ++ "-27"
              dataAtual = ano ++ "-" ++ mesCampo ++ "-" ++ dia
          in dataAtual >= dataInicio && dataAtual <= dataFim
        _ -> False
    splitData s = case wordsWhen (=='-') s of
      [a,m,d] -> Just (a,m,d)
      _ -> Nothing
    pad2 n = if n < 10 then '0':show n else show n
    wordsWhen p s = case dropWhile p s of
      "" -> []
      s' -> w : wordsWhen p s'' where (w, s'') = break p s'

-- Função para ordenar campeonatos por data (YYYY-MM-DD)
ordenarPorData :: [(String, String)] -> [(String, String)]
ordenarPorData = reverse . sortOn fst

-- Função auxiliar para extrair nomes dos campeonatos
nomes :: [(String, String)] -> [String]
nomes = map snd

-- Testes para a função splitData
testSplitData :: Test
testSplitData = TestList
  [ "splitData válido" ~: splitData "2025-03-15" ~?= Just ("2025", "03", "15")
  , "splitData inválido" ~: splitData "2025-03" ~?= Nothing
  , "splitData formato errado" ~: splitData "15/03/2025" ~?= Nothing
  , "splitData vazio" ~: splitData "" ~?= Nothing
  ]
  where
    splitData s = case wordsWhen (=='-') s of
      [a,m,d] -> Just (a,m,d)
      _ -> Nothing
    wordsWhen p s = case dropWhile p s of
      "" -> []
      s' -> w : wordsWhen p s'' where (w, s'') = break p s'

-- Testes para a função pad2
testPad2 :: Test
testPad2 = TestList
  [ "pad2 menor que 10" ~: pad2 5 ~?= "05"
  , "pad2 maior que 10" ~: pad2 12 ~?= "12"
  , "pad2 igual a 10" ~: pad2 10 ~?= "10"
  , "pad2 zero" ~: pad2 0 ~?= "00"
  ]
  where
    pad2 n = if n < 10 then '0':show n else show n

-- Testes para a função filtrarPorMes
testFiltrarPorMes :: Test
testFiltrarPorMes = TestList
  [ "Fevereiro: dentro do intervalo (pagamento Jan-Fev)" ~:
      filtrarPorMes "02" [("2025-02-27", "Teste")] ~?= [("2025-02-27", "Teste")]
  , "Fevereiro: fora do intervalo (após dia 27)" ~:
      filtrarPorMes "02" [("2025-02-28", "Teste")] ~?= []
  , "Março: dentro do intervalo (pagamento Fev-Mar)" ~:
      filtrarPorMes "03" [("2025-03-01", "Teste")] ~?= [("2025-03-01", "Teste")]
  , "Março: dentro do intervalo (meio do mês)" ~:
      filtrarPorMes "03" [("2025-03-15", "Teste")] ~?= [("2025-03-15", "Teste")]
  , "Março: limite superior (dia 27)" ~:
      filtrarPorMes "03" [("2025-03-27", "Teste")] ~?= [("2025-03-27", "Teste")]
  , "Março: fora do intervalo (após dia 27)" ~:
      filtrarPorMes "03" [("2025-03-28", "Teste")] ~?= []
  , "Abril: dentro do intervalo (pagamento Mar-Abr)" ~:
      filtrarPorMes "04" [("2025-04-01", "Teste")] ~?= [("2025-04-01", "Teste")]
  , "Janeiro: teste virada de ano" ~:
      filtrarPorMes "01" [("2025-01-15", "Teste")] ~?= [("2025-01-15", "Teste")]
  , "Data inválida" ~:
      filtrarPorMes "03" [("2025-03", "Teste")] ~?= []
  ]

-- Testes para a função ordenarPorData
testOrdenarPorData :: Test
testOrdenarPorData = TestCase $ do
  let campeonatos = 
        [ ("2025-03-15", "C")
        , ("2025-02-10", "A")
        , ("2025-03-01", "B")
        ]
  
  let ordenados = ordenarPorData campeonatos
  let datas = map fst ordenados
  
  assertEqual "Deve ordenar por data decrescente" 
    ["2025-03-15", "2025-03-01", "2025-02-10"] datas

-- Teste de integração: filtragem e ordenação combinadas
testFiltragemEOrdenacao :: Test
testFiltragemEOrdenacao = TestCase $ do
  let todosCampeonatos =
        [ ("2025-02-27", "Fevereiro Final")
        , ("2025-03-01", "Março Início")
        , ("2025-03-15", "Março Meio")
        , ("2025-03-27", "Março Final")
        , ("2025-04-01", "Abril Início")
        , ("2025-04-28", "Abril Fora")
        ]
  
  -- Filtra campeonatos de março
  let marcos = filtrarPorMes "03" todosCampeonatos
  let marcosOrdenados = ordenarPorData marcos
  let nomesMarcos = nomes marcosOrdenados
  
  assertEqual "Deve filtrar e ordenar campeonatos de março corretamente"
    ["Março Final", "Março Meio", "Março Início", "Fevereiro Final"] nomesMarcos
  
  -- Filtra campeonatos de abril
  let abris = filtrarPorMes "04" todosCampeonatos
  let abrisOrdenados = ordenarPorData abris
  let nomesAbris = nomes abrisOrdenados
  
  assertEqual "Deve filtrar e ordenar campeonatos de abril corretamente"
    ["Abril Início", "Março Final"] nomesAbris

-- Testes originais do exemplo
testExemplosOriginais :: Test
testExemplosOriginais = TestList
  [ TestCase (assertEqual "Filtro mês 3" (sort ["B","C","D","E"]) (sort $ nomes $ filtrarPorMes "03" campeonatos))
  , TestCase (assertEqual "Filtro mês 4" (sort ["F","G"]) (sort $ nomes $ filtrarPorMes "04" campeonatos))
  , TestCase (assertEqual "Ordenação decrescente por data" 
        [ ("2025-04-01", "G"), ("2025-03-28", "F"), ("2025-03-27", "E")
        , ("2025-03-15", "D"), ("2025-03-01", "C"), ("2025-02-28", "B")
        , ("2025-02-27", "A") ] 
        (ordenarPorData campeonatos))
  ]
  where
    campeonatos = [ ("2025-02-27", "A"), ("2025-02-28", "B"), ("2025-03-01", "C")
                  , ("2025-03-15", "D"), ("2025-03-27", "E"), ("2025-03-28", "F")
                  , ("2025-04-01", "G") ]

-- Teste de borda: virada de ano
testViradaAno :: Test
testViradaAno = TestList
  [ "Dezembro para Janeiro" ~: 
      filtrarPorMes "01" [("2025-01-15", "Teste")] ~?= [("2025-01-15", "Teste")]
  , "Fevereiro ano bissexto" ~:
      filtrarPorMes "02" [("2024-02-28", "Teste")] ~?= [("2024-02-28", "Teste")]
  ]

-- Suite principal de testes
testSuite :: Test
testSuite = TestList
  [ TestLabel "splitData" testSplitData
  , TestLabel "pad2" testPad2
  , TestLabel "filtrarPorMes" testFiltrarPorMes
  , TestLabel "ordenarPorData" testOrdenarPorData
  , TestLabel "filtragemEOrdenacao" testFiltragemEOrdenacao
  , TestLabel "exemplosOriginais" testExemplosOriginais
  , TestLabel "viradaAno" testViradaAno
  ]

-- Função principal para executar os testes
main :: IO ()
main = do
  counts <- runTestTT testSuite
  if errors counts > 0 || failures counts > 0
    then exitFailure
    else exitSuccess

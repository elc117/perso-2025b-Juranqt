module ArbitragemFunctions (
    Campeonato(..),
    ordenarPorData,
    splitData,
    pad2,
    intervaloMes
) where

import Data.List (sortOn)
import Data.List.Split (splitOn)

-- Tipo principal

data Campeonato = Campeonato
  { campeonatoId :: Maybe Int
  , campeonato   :: String
  , dataCampo    :: String
  , local        :: String
  , mesa         :: String
  , quadra       :: String
  } deriving (Show, Eq)

ordenarPorData :: [Campeonato] -> [Campeonato]
ordenarPorData = sortOn dataCampo

splitData :: String -> Maybe (String, String, String)
splitData s = case splitOn "-" s of
  [a,m,d] -> Just (a,m,d)
  _ -> Nothing

pad2 :: Int -> String
pad2 n = if n < 10 then '0':show n else show n

intervaloMes :: String -> Campeonato -> Bool
intervaloMes mes c =
  case splitData (dataCampo c) of
    Just (ano, mesCampo, dia) ->
      let mesNum = read mes :: Int
          mesAnt = if mesNum == 1 then 12 else mesNum - 1
          anoAnt = if mesNum == 1 then show (read ano - 1) else ano
          dataInicio = anoAnt ++ "-" ++ pad2 mesAnt ++ "-28"
          dataFim = ano ++ "-" ++ pad2 mesNum ++ "-27"
          dataAtual = ano ++ "-" ++ mesCampo ++ "-" ++ dia
      in dataAtual >= dataInicio && dataAtual <= dataFim
    _ -> False

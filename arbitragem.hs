{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import System.Environment (lookupEnv)
import Data.List (sortOn)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import Data.List.Split (splitOn)


-- Modelo para Campeonato
-- Função para ordenar campeonatos por data (YYYY-MM-DD)
-- sorOn: função de alta ordem que aplica dataCampo a cada elemento antes de ordenar
ordenarPorData :: [Campeonato] -> [Campeonato]
ordenarPorData = sortOn dataCampo

-- Função de filtro por mes com base nos dias de pagamento
intervaloMes :: String -> Campeonato -> Bool
intervaloMes mes c =
  case splitData (dataCampo c) of
    Just (ano, mesCampo, dia) ->
      let mesNum = read mes :: Int
          mesAnt = if mesNum == 1 then 12 else mesNum - 1
          anoAnt = if mesNum == 1 then show (read ano - 1) else ano
          dataInicio = "28" ++ "-" ++ pra2 mesAnt ++ "-"  ++ anoAnt
          dataFim = "27" ++ "-" ++ pra2 mesNum ++ "-" ++ ano
          dataAtual = dia ++ "-" ++ mesCampo ++ "-" ++ ano  
      in dataAtual >= dataInicio && dataAtual <= dataFim
    _ -> False

-- Divide a string da data em tupla (ano, mes, dia)
splitData :: String -> Maybe (String, String, String)
splitData s = case splitOn "-" s of
  [a,m,d] -> Just (a,m,d)
  _ -> Nothing

-- essa função tinha sido gerada pela ia mas ao compreender ela vi que não era necessária, que dava pra usar o splitOn 
  --  wordsWhen p s = case dropWhile p s of
    --   "" -> []
    --   s' -> w : wordsWhen p s'' where (w, s'') = break p s'

-- Preenche zeros à esquerda para não ocorrer erro na comparação de strings de data
pra2 :: Int -> String
pra2 n = if n < 10 then '0':show n else show n


data Campeonato = Campeonato
  { campeonatoId :: Maybe Int
  , campeonato   :: String
  , dataCampo    :: String
  , local        :: String
  , mesa         :: String
  , quadra       :: String
  } deriving (Show, Generic)

instance ToJSON Campeonato
instance FromJSON Campeonato

instance FromRow Campeonato where
  fromRow = Campeonato <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Campeonato where
  toRow (Campeonato _ campeonato_ data_ local_ mesa_ quadra_) = toRow (campeonato_, data_, local_, mesa_, quadra_)


hostAny :: HostPreference
hostAny = "*"


-- Inicializa o banco de dados
initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS campeonatos (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ campeonato TEXT,\
  \ dataCampo TEXT,\
  \ local TEXT,\
  \ mesa TEXT,\
  \ quadra TEXT)"

-- Main entry point
main :: IO ()
main = do
  conn <- open "arbitragem.db"
  initDB conn

  mPort <- lookupEnv "PORT"
  let port = maybe 3000 id (mPort >>= readMaybe)

  putStrLn $ "Server running on port:" ++ show port
  let opts = Options
        { verbose  = 1
        , settings = setHost hostAny $ setPort port defaultSettings
        }

  scottyOpts opts $ do
    middleware logStdoutDev

    -- Faz o index.html aparecer na pagina inicial
    get "/" $ file "index.html"

    -- GET /healthz
    get "/healthz" $ text "ok"

    -- lista todos os jogos cadastrados ordenados por data
    -- GET /campeonatos
    get "/campeonatos" $ do 
      campeonatos <- liftIO $ query_ conn "SELECT id, campeonato, dataCampo, local, mesa, quadra FROM campeonatos" :: ActionM [Campeonato]
      json (ordenarPorData campeonatos)

    -- filtra por mes todos os jogos cadastrados
    -- GET /campeonatos/mes/:mes
    get "/campeonatos/mes/:mes" $ do
      mesParam <- param "mes"
      campeonatos <- liftIO $ query_ conn "SELECT id, campeonato, dataCampo, local, mesa, quadra FROM campeonatos" :: ActionM [Campeonato]
      let filtrados = ordenarPorData $ filter (intervaloMes mesParam) campeonatos
      json filtrados

    -- POST /campeonatos - cria um novo campeonato
    post "/campeonatos" $ do 
      c <- jsonData :: ActionM Campeonato
      liftIO $ execute conn "INSERT INTO campeonatos (campeonato, dataCampo, local, mesa, quadra) VALUES (?, ?, ?, ?, ?)" (campeonato c, dataCampo c, local c, mesa c, quadra c)
      rowId <- liftIO $ lastInsertRowId conn
      json ("Campeonato criado com id " ++ show rowId)

    -- PUT /campeonatos/:id - atualiza um campeonato ja existente
    put "/campeonatos/:id" $ do 
      idParam <- pathParam "id" :: ActionM Int
      c <- jsonData :: ActionM Campeonato
      let updatedC = c { campeonatoId = Just idParam }
      liftIO $ execute conn "UPDATE campeonatos SET campeonato = ?, dataCampo = ?, local = ?, mesa = ?, quadra = ? WHERE id = ?" (campeonato updatedC, dataCampo updatedC, local updatedC, mesa updatedC, quadra updatedC, campeonatoId updatedC)
      json ("Campeonato atualizado" :: String)

    -- DELETE /campeonatos/:id - deleta um campeonato
    delete "/campeonatos/:id" $ do 
      idParam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM campeonatos WHERE id = ?" (Only idParam)
      json ("Campeonato excluído" :: String)

    -- GET /campeonatos/:id - busca campeonato especifico
    get "/campeonatos/:id" $ do 
      idParam <- pathParam "id" :: ActionM Int
      result <- liftIO $ query conn "SELECT id, campeonato, dataCampo, local, mesa, quadra FROM campeonatos WHERE id = ?" (Only idParam) :: ActionM [Campeonato]
      case listToMaybe result of
        Just c -> json c
        Nothing -> status status404 >> json ("Campeonato não encontrado" :: String)
    

  -- fim do último endpoint

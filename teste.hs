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
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)


-- Modelo para Campeonato
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


-- Inicializa banco de dados
initDB :: Connection -> IO ()
initDB conn = do
  execute_ conn
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

    -- Servir o index.html na raiz
    get "/" $ file "index.html"
    -- GET /healthz
    get "/healthz" $ text "ok"

    -- ENDPOINTS CAMPEONATOS
    -- GET /campeonatos
    get "/campeonatos" $ do
      campeonatos <- liftIO $ query_ conn "SELECT id, campeonato, dataCampo, local, mesa, quadra FROM campeonatos" :: ActionM [Campeonato]
      json campeonatos
    
  -- fim do bloco scottyOpts

    -- POST /campeonatos
    post "/campeonatos" $ do
      c <- jsonData :: ActionM Campeonato
      liftIO $ execute conn "INSERT INTO campeonatos (campeonato, dataCampo, local, mesa, quadra) VALUES (?, ?, ?, ?, ?)" (campeonato c, dataCampo c, local c, mesa c, quadra c)
      rowId <- liftIO $ lastInsertRowId conn
      json ("Campeonato criado com id " ++ show rowId)
    

    -- PUT /campeonatos/:id
    put "/campeonatos/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      c <- jsonData :: ActionM Campeonato
      let updatedC = c { campeonatoId = Just idParam }
      liftIO $ execute conn "UPDATE campeonatos SET campeonato = ?, dataCampo = ?, local = ?, mesa = ?, quadra = ? WHERE id = ?" (campeonato updatedC, dataCampo updatedC, local updatedC, mesa updatedC, quadra updatedC, campeonatoId updatedC)
      json ("Campeonato atualizado" :: String)
    

    -- DELETE /campeonatos/:id
    delete "/campeonatos/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM campeonatos WHERE id = ?" (Only idParam)
      json ("Campeonato excluído" :: String)
    

    -- GET /campeonatos/:id
    get "/campeonatos/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      result <- liftIO $ query conn "SELECT id, campeonato, dataCampo, local, mesa, quadra FROM campeonatos WHERE id = ?" (Only idParam) :: ActionM [Campeonato]
      case listToMaybe result of
        Just c -> json c
        Nothing -> status status404 >> json ("Campeonato não encontrado" :: String)
    
  -- fim do último endpoint

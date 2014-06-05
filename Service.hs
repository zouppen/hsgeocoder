{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString,fromChunks)
import Data.Functor
import Data.Conduit
import Data.Conduit.List (foldMap)
import Network.HTTP.Types (ok200,badRequest400)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Data.List
import Data.Aeson
import Data.Maybe
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Conduit.Text as CT
import Text.HTML.TagSoup

import Nominatim
import AddressExtractor
import DeclinationReader

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    declinations <- readExampleDeclinations
    streets <- (lines.(map toLower)) <$> readFile "examples/streets_in_jyvaskyla.txt"
    run port $ app (declinations,streets)

app :: ([(String, [String])], [String]) -> Application
app (inflections,streets) req = case (requestMethod req,pathInfo req) of
  ("GET",["nominative",area,street]) -> do
    locations <- liftIO $ search "Finland" "Jyväskylä" $ T.unpack street
    jsonResponse $ encode locations
  ("GET",["inflected",area,street]) -> do
    let nominative = toNominative inflections $ T.unpack street
    locations <- liftIO $ search "Finland" "Jyväskylä" nominative
    jsonResponse $ encode locations
  ("POST",["document","text",area]) -> do
    doc <- requestBody req =$= CT.decode CT.utf8 $$ foldMap T.unpack
    docSearch doc
  ("POST",["document","html",area]) -> do
    -- FIXME encoding issues if HTML has not UTF-8 encoding
    doc <- requestBody req =$= CT.decode CT.utf8 $$ foldMap T.unpack
    docSearch $ innerText $ parseTags doc
  e -> do
    liftIO $ print e
    bad "Unknown command"
  where 
    docSearch s = do
      let candidates = listOfCandidates streets inflections s
      answers <- liftIO $ mapM (search "Finland" "Jyväskylä") candidates
      jsonResponse $ encode $ nub $ catMaybes answers

bad,good :: Monad m => ByteString -> m Response
bad  = textualResponse badRequest400 
good = textualResponse ok200

textualResponse code text = return $
                            responseLBS code
                            [("Content-Type", "text/plain")]
                            text

jsonResponse x = return $
                 responseLBS ok200
                 [("Content-Type", "application/json")]
                 x

searchJson country city street = do
  locations <- search country city street
  return $ encode locations

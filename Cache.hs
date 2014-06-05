module Cache where

import Data.Functor
import Data.Binary
import Data.Digest.Pure.SHA
import System.IO.Error

-- FIXME hardcoded cache directory

-- |Performs IO action via cache. If key is cached, then it is
-- returned from there. Otherwise the action is performed for
-- real. The cache is a flat directory where older objects may freely
-- be purged by hand.
viaCache :: (Show a, Binary a, Binary b) => (a -> IO b) -> a -> IO b
viaCache act key = do 
  mbHit <- lookup
  putStrLn $ (maybe "MISS" (const "HIT") mbHit) ++ " " ++ show key
  case mbHit of
    Nothing -> do
      value <- act key
      store value
      return value
    Just hit -> return hit
  where
    hexKey = showDigest $ sha1 $ encode key
    lookup = do 
      stored <- tryIOError $ decodeFile $ "cache/" ++ hexKey
      case stored of
        Left e -> if isDoesNotExistError e
                  then return Nothing
                  else ioError e
        Right a -> return $ Just a
    store value = encodeFile ("cache/" ++ hexKey) value

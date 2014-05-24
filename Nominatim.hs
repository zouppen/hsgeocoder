{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |Interface to Nominatim
-- (http://wiki.openstreetmap.org/wiki/Nominatim) and contains caches
-- for avoiding flooding of OSM servers.
module Nominatim where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Network.URL
import Network.Curl.Aeson

data Location = Location { name      :: String
                         , coordType :: String
                         , coords    :: [[[Double]]]
                         } deriving (Show)

instance FromJSON Location where
  parseJSON (Object o) = do
    name <- o .: "display_name"
    geo <- o .: "geojson"
    coordType <- geo .: "type"
    coords <- geo .: "coordinates"
    return Location{..}
  parseJSON _ = mzero

Just baseUrl = importURL "http://nominatim.openstreetmap.org/search?format=jsonv2&polygon_geojson=1"

search :: String -> String -> String -> IO [Location]
search country city street = curlAesonGet url
  where
    url = exportURL $ baseUrl &>
          ("country",country) &> ("city",city) &> ("street",street)


-- |Infix version of add_param
(&>) = add_param

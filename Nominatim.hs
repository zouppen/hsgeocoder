{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |Interface to Nominatim
-- (http://wiki.openstreetmap.org/wiki/Nominatim) and contains caches
-- for avoiding flooding of OSM servers.
module Nominatim where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Maybe
import Network.URL
import Network.Curl.Aeson

data Location = Location { name      :: String
                         , coordType :: String
                         , coords    :: Value
                         } deriving (Show,Eq)

-- JSON parsers and producers of Location. FromJSON and ToJSON
-- are not compatible with each other, meaning that ToJSON does not
-- produce suitable input for FromJSON instance. TODO In future, toJSON
-- should be replaced with a Parser to avoid mystification.
instance FromJSON Location where
  parseJSON (Object o) = do
    name <- o .: "display_name"
    geo <- o .: "geojson"
    coordType <- geo .: "type"
    coords <- geo .: "coordinates"
    return Location{..}
  parseJSON _ = mzero

instance ToJSON Location where
  toJSON Location{..} = object ["category" .= ("address" :: String)
                               ,"coordinates" .= coords
                               ,"name" .= name
                               ,"type" .= coordType
                               ]

Just baseUrl = importURL "http://nominatim.openstreetmap.org/search?format=jsonv2&polygon_geojson=1&limit=1"

search :: String -> String -> String -> IO (Maybe Location)
search country city street = listToMaybe <$> curlAesonGet url
  where
    url = exportURL $ baseUrl &>
          ("country",country) &> ("city",city) &> ("street",street)

-- |Infix version of add_param
(&>) = add_param

-- |Interface to Nominatim
-- (http://wiki.openstreetmap.org/wiki/Nominatim) and contains caches
-- for avoiding flooding of OSM servers.
module Nominatim where

import Network.URL
import Network.Curl.Aeson

Just baseUrl = importURL "http://nominatim.openstreetmap.org/search?format=jsonv2&polygon_geojson=1"

search country city street = curlAesonGet url
  where
    url = exportURL $ baseUrl &>
          ("country",country) &> ("city",city) &> ("street",street)


-- |Infix version of add_param
(&>) = add_param

module AddressExtractor where

import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe

-- |Takes list of streets, inflection table and a document. Returns
-- sist of possible addresses with itself are lists of possible
-- parses, and should be tried from left to right.
listOfCandidates :: [String] -> [(String, [String])] -> String -> [[String]]
listOfCandidates streets cases s = nub $ 
                                   map toAlternatives $
                                   matchStreet streets $ 
                                   map (toNominative cases) $
                                   splitter $
                                   map toLower s

-- |Breaks given string into words
splitter :: String -> [String]
splitter x = case break isDelimiter x of
  ([],[])    -> []
  ([],(_:b)) -> splitter b
  (a,[])     -> [a]
  (a,(_:b))  -> a:splitter b

-- |Returns true if it is a word delimiter.
isDelimiter :: Char -> Bool
isDelimiter x = isSpace x || x `elem` ".,"

-- |Takes a word (possibly a compound word), nominative ending and an
-- inflected case of that word. It returns Just x if the word was
-- inflected to nominative form. Otherwise, Nothing is returned.
tryNominative :: String -> (String,String) -> Maybe String
tryNominative word (nom,inf) = case isSuffixOf inf word of
  True -> Just $ take (length word - length inf) word ++ nom
  False -> Nothing

-- |Takes a case list and a (compound) word and tries to inflect it to
-- nominative case. If no inflection seems to be possible, keep the
-- word intact.
toNominative cases word = maybe word id $
                          foldl1 (<|>) [ tryNominative word (nom,inf)
                                       | (nom,infs) <- cases
                                       , inf <- infs
                                       ]

-- |Produces list of matching streets. The returned list of pairs
-- contain the street name and list of following words.
matchStreet :: [String] -> [String] -> [(String, [String])]
matchStreet streets words = [ (street,end)
                            | phrase <- tails words
                            , street <- streets
                            , let parts = splitter street
                            , parts `isPrefixOf` phrase
                            , let end = drop (length parts) phrase
                            ]

-- |Produces multiple interpretations of given street address. One
-- with street address, with building number (eg. Survontie 46 A),
-- second using only street address (Survontie 15) and the last with
-- street name only (Survontie)
toAlternatives :: (String, [String]) -> [String]
toAlternatives (street,end) = catMaybes [Just street +> number +> house
                                        ,Just street +> number
                                        ,Just street
                                        ]
  where
    number = case end of
      (a:_) -> justIf (all isDigit) a
      _     -> Nothing
    house = case end of
      (_:a:_) -> justIf (all isAlpha) a
      _       -> Nothing

-- |Checks if the function given as first argument returns true when
-- applied to the second argument. In that case the value is returned
-- as wrapped in Just.
justIf :: (a -> Bool) -> a -> Maybe a
justIf f a = if f a
             then Just a
             else Nothing

-- |Concatenate strings and delimite them by a space. If a or b is
-- Nothing, then this function returns Nothing.
(+>) :: Maybe String -> Maybe String -> Maybe String
(+>) a b = case (a,b) of
  (Just a,Just b) -> Just $ showString a $ showChar ' ' $ b
  _ -> Nothing

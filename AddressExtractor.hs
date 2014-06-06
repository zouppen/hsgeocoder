module AddressExtractor where

import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe

-- |Takes list of streets, inflection table and a document. Returns
-- sist of possible addresses with itself are lists of possible
-- parses, and should be tried from left to right.
listOfCandidates :: [String] -> [(String, [String])] -> String -> [String]
listOfCandidates streets cases s = nub $
                                   map toAlternatives $
                                   matchStreet streets $
                                   map (toNominative cases) $
                                   splitter $
                                   map toLower s

-- |Breaks given string into words
splitter :: String -> [String]
splitter = unfoldr f
  where f [] = Nothing
        f x  = Just $ break isDelimiter $ dropWhile isDelimiter x

-- |Returns true if it is a word delimiter.
isDelimiter :: Char -> Bool
isDelimiter x = case x of 
  '.'    -> True
  ','    -> True
  ' '    -> True
  '\160' -> True -- NBSP
  '\t'   -> True
  '\n'   -> True 
  '\r'   -> True
  '\f'   -> True
  '\v'   -> True
  _      -> False

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
toNominative :: [(String, [String])] -> String -> String
toNominative cases word = maybe word id $
                          foldl1 (<|>) [ tryNominative word (nom,inf)
                                       | (nom,infs) <- cases
                                       , inf <- infs
                                       ]

-- |Produces list of matching streets. The returned list of pairs
-- contain the street name and the next word (perhaps the street number).
matchStreet :: [String] -> [String] -> [(String, Maybe String)]
matchStreet streets words = [ (street,maybeIndex (length parts) phrase)
                            | phrase <- tails words
                            , street <- streets
                            , let parts = splitter street
                            , parts `isPrefixOf` phrase
                            ]

-- |Produces an interpretations of given street address. If house
-- number exists (eg. Survontie 15), use that. Otherwise, return just
-- the street name (eg. Survontie).
toAlternatives :: (String, Maybe String) -> String
toAlternatives (street,end) = case end of
  Nothing  -> street
  Just num -> if all isDigit num
              then street ++ " " ++ num
              else street

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

-- |Like (!!) but returns Nothing instead of an error if the list is
-- too short.
maybeIndex :: Int -> [a] -> Maybe a
maybeIndex i xs = listToMaybe $ drop i xs

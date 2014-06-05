module DeclinationReader where

import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Text.Pandoc
import Data.Default (Default)
import Data.Set (Set)

type Declination = (String,Set String)

data Types = BaseForm String
           | Declinations [String]
           deriving (Show)

-- |Unwrap set and remove empty lists
unSet :: [(a, Set b)] -> [(a, [b])]
unSet xs = [ (a,S.toList b)
           | (a,b) <- xs
           , not (S.null b)
           ]

readExampleDeclinations :: IO [(String,[String])]
readExampleDeclinations = do
  s <- readFile "examples/vartalot.md"
  return $ unSet $ extractDeclinations readMarkdown s

-- |Reads declinations from given data and with given reader
extractDeclinations :: Default d => (d -> String -> Pandoc) -> String -> [Declination]

extractDeclinations parser = groupTypes . catMaybes . map extractor . takeBlocks . parsed
  where
    parsed = parser def
    takeBlocks (Pandoc _ bs) = bs

extractor :: Block -> Maybe Types
extractor (Header 2 _ xs) = Just $ BaseForm $ inlineToText xs
extractor (BulletList xs) = Just $ Declinations $ map blockToText xs
extractor _ = Nothing

-- |Groups heading-list pairs. Adds the base form of a word to the
-- list if not there already.
groupTypes :: [Types] -> [Declination]
groupTypes ((BaseForm base):(Declinations ds):xs) = (base,S.fromList ds):groupTypes xs
groupTypes ((BaseForm base):xs) = (base,S.empty):groupTypes xs -- No definitions given yet.
groupTypes [] = []
groupTypes _ = error "Invalid input, contains wrong ordering of lists and headers"

blockToText :: [Block] -> String
blockToText bs = writePlain def $ Pandoc (Meta [] [] []) bs

inlineToText :: [Inline] -> String
inlineToText xs = blockToText [Plain xs]

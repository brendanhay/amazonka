{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Documentation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Documentation where

import           Data.Foldable     (foldl')
import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Text.HTML.TagSoup

highlightType :: Text -> Text
highlightType = Text.unwords . map start . Text.words
  where
    start t
        | Text.null t                     = t
        | Just ('[', xs) <- Text.uncons t = "['" <> end xs
        | Just ('(', xs) <- Text.uncons t = "('" <> end xs
        | otherwise                       = "'"  <> end t

    end t
        | Text.null t        = t
        | ']' <- Text.last t = Text.init t <> "']"
        | ')' <- Text.last t = Text.init t <> "')"
        | otherwise          = t <> "'"

wrapDescription :: Text -> Text
wrapDescription t
    | x:xs <- wrapped = Text.intercalate "\n" (x:xs)
    | otherwise       = ""
  where
    wrapped = formatTags 78 t

wrapHaddock :: Text -> Int -> Int -> Text -> Text
wrapHaddock start n i txt
    | x:xs <- wrapped = Text.intercalate "\n" . map indent $ start <> x:xs
    | otherwise       = ""
  where
    indent x
        | Text.null x = pre <> "--"
        | otherwise   = pre <> "-- " <> x
      where
        pre = Text.replicate i (Text.singleton ' ')

    wrapped = formatTags n txt

endWith :: Text -> Text -> Text
endWith x y
    | x `Text.isSuffixOf` y = y
    | otherwise             = y <> x

data Mode
    = Ref
    | Link
    | Code
    | Em
    | Text

formatTags :: Int -> Text -> [Text]
formatTags n = map Text.strip . parse
  where
    parse = Text.lines
        . fst
        . foldl' (uncurry go) (mempty, Text)
        . splitTags n
        . parseTags

    go x Link (TagText  t)                 = (x <> t,    Link)
    go x Link (TagClose "a")               = (x <> ">",  Text)

    go x Ref  (TagText  t)                 = (x <> t,    Ref)
    go x Ref  (TagClose "a")               = (x <> "'",  Text)

    go x Em   (TagText  t)                 = (x <> t,    Em)
    go x Em   (TagClose "i")               = (x <> "/",  Text)

    go x Code (TagText  t)                 = (x <> t,    Code)
    go x Code (TagClose "code")            = (x <> "'",  Text)

    go x _    (TagOpen  "a" [("href", a)]) = (x <> "<" <> a <> " ", Link)
    go x _    (TagOpen  "a" [])            = (x <> "'",  Ref)
    go x _    (TagOpen  "i"    _)          = (x <> "/",  Em)
    go x _    (TagOpen  "code" _)          = (x <> "'",  Code)
    go x Text (TagOpen  "br"   _)          = (x <> "\n", Text)

    go x m    (TagText  t)                 = (x <> t,    m)

    go x m    _                            = (x, m)

splitTags :: Int -> [Tag Text] -> [Tag Text]
splitTags n = strip . fst . foldl' go ([], 0)
  where
    strip (TagOpen "br" _ : xs) = strip xs
    strip xs                    = reverse xs

    go :: ([Tag Text], Int) -> Tag Text -> ([Tag Text], Int)
    go (xs, col) = \case
         TagOpen  "p" _      -> (xs, col)
         TagClose "p"        -> (line : line : xs, 0)
         TagText t
             | col >= n      -> go (line : xs, 0) (TagText t)
             | len + col > n ->
                 let hd     = Text.take diff t
                     (k, _) = Text.breakOnEnd " " hd
                     lst    = Text.drop (Text.length k) t
                  in if Text.null k
                         then    (line : TagText t : xs, 0)
                         else go (line : TagText (Text.stripEnd k) : xs, 0) (TagText lst)

             | otherwise     -> (TagText t : xs, col + len)
           where
             len  = Text.length t
             diff = n - col

         x                   -> (x : xs, col)

    line = TagOpen "br" []

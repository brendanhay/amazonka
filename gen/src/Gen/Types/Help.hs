-- Module      : Gen.Types.Help
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Help
  ( Help (..),
    renderHaddock,
  )
where

import Data.Aeson
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Gen.Text (replaceHead)
import System.IO.Unsafe (unsafePerformIO)
import Text.Pandoc qualified as Pandoc

newtype Help = Help {fromHelp :: Text}
  deriving (Eq, Semigroup, Monoid)

-- | Empty Show instance to avoid verbose debugging output.
instance Show Help where
  show = const mempty

instance IsString Help where
  fromString = Help . fromString

instance FromJSON Help where
  parseJSON = withText "help" (pure . Help . convertHaddock)

instance ToJSON Help where
  -- Note the wierd templating behaviour of first line is unprefixed,
  -- with the remainder being haddock prefixed with "--".
  toJSON = toJSON . LText.strip . LText.drop 3 . renderHaddock False 0

renderHaddock :: Bool -> Int -> Help -> LText.Text
renderHaddock topLevel indent (Help text) =
  let start =
        LText.replicate (fromIntegral indent) " "

      block =
        replaceHead $ \case
          '$' -> Just "\\$"
          _c -> Nothing

      cat sep =
        mappend (start <> sep) . LText.fromStrict . block
   in case Text.lines text of
        [] -> mempty
        x : xs ->
          LText.unlines $
            cat (if topLevel then "-- | " else "-- ") x : map (cat "-- ") xs

convertHaddock :: Text -> Text
convertHaddock text =
  unsafePerformIO . Pandoc.runIOorExplode $ do
    html <- Pandoc.readHtml Pandoc.def text
    help <- Pandoc.writeHaddock Pandoc.def html

    pure (Text.strip help)

-- go . XML.documentRoot . DOM.parseLBS . LBS.fromStrict . Text.encodeUtf8
-- where
--   go x =
--     case name x of
--       "html" -> nodes x
--       "p" -> nodes x <> "\n\n"
--       "ul" -> nodes x <> "\n\n"
--       "ol" -> nodes x <> "\n\n"
--       "dl" -> nodes x <> "\n\n"
--       "li" -> list (nodes x)
--       "dt" -> list (nodes x)
--       "dd" -> list (nodes x)
--       "br" -> "\n" <> nodes x
--       "i" -> emphasis (nodes x)
--       "u" -> emphasis (nodes x)
--       "important" -> "/Important:/ " <> nodes x
--       "title" -> title (nodes x)
--       "fullname" -> title (nodes x)
--       "caution" -> title (nodes x)
--       "h1" -> title (nodes x)
--       "h2" -> title (nodes x)
--       "h3" -> title (nodes x)
--       "h4" -> title (nodes x)
--       "h5" -> title (nodes x)
--       "b" -> bold (nodes x)
--       "strong" -> bold (nodes x)
--       "a"
--         | Just link <- attribute "href" x -> "<" <> link <> " " <> nodes x <> "> "
--         | otherwise -> code (nodes x)
--       "code"
--         | null (XML.elementNodes x) -> mempty
--         | otherwise -> mono (nodes x)
--       "pre"
--         | null (XML.elementNodes x) -> mempty
--         | otherwise -> mono (nodes x)
--       "note" -> mempty
--       "div" -> nodes x
--       "literal" -> mono (nodes x)
--       y -> "<" <> y <> ">" <> nodes x

--   title x = bold x <> "\n"
--   bold x = "__" <> x <> "__ "
--   emphasis x = "/" <> escape x <> "/ "
--   code x = "'" <> x <> "' "
--   mono x = "@" <> x <> "@ "
--   list x = "    * " <> x

--   escape = Text.concatMap f
--     where
--       f '/' = "\\/"
--       f c = Text.singleton c

--   nodes = foldMap f . XML.elementNodes
--     where
--       f = \case
--         NodeElement e -> go e
--         NodeContent t
--           | Text.null x -> mempty
--           | Text.head x == '$' -> "> " <> Text.tail x
--           | Text.head x == '>' -> x
--           | otherwise -> Text.dropWhile Char.isSeparator t
--           where
--             x = Text.stripStart t
--         _ -> mempty

--   attribute key x =
--     Map.lookup
--       (XML.Name key Nothing Nothing)
--       (XML.elementAttributes x)

--   name = XML.nameLocalName . XML.elementName

-- wrap :: Text -> Text -> Text
-- wrap sep = go . Text.lines
--   where
--     go [] = mempty
--     go (x : []) = x
--     go (x : xs) = x <> start <> foldMap prefix xs

--     prefix x
--       | Text.null x = start
--       | otherwise = start <> " " <> x

--     start = "\n" <> sep

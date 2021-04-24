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
  )
where

import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Text.HTML.DOM qualified as DOM
import Text.XML (Node (..))
import Text.XML qualified as XML

newtype Help = Help Text
  deriving (Eq, Semigroup, Monoid)

-- | Empty Show instance to avoid verbose debugging output.
instance Show Help where
  show = const mempty

instance IsString Help where
  fromString = Help . fromString

instance FromJSON Help where
  parseJSON =
    withText "help" $
      pure . Help . wrap "--" . convertHaddock

instance ToJSON Help where
  toJSON (Help h) = toJSON h

convertHaddock :: Text -> Text
convertHaddock =
  go . XML.documentRoot . DOM.parseLBS . LBS.fromStrict . Text.encodeUtf8
  where
    go x =
      case name x of
        "html" -> nodes x
        "p" -> nodes x <> "\n\n"
        "ul" -> nodes x <> "\n\n"
        "ol" -> nodes x <> "\n\n"
        "dl" -> nodes x <> "\n\n"
        "li" -> list (nodes x)
        "dt" -> list (nodes x)
        "dd" -> list (nodes x)
        "br" -> "\n" <> nodes x
        "i" -> emphasis (nodes x)
        "u" -> emphasis (nodes x)
        "important" -> "/Important:/ " <> nodes x
        "title" -> title (nodes x)
        "fullname" -> title (nodes x)
        "caution" -> title (nodes x)
        "h1" -> title (nodes x)
        "h2" -> title (nodes x)
        "h3" -> title (nodes x)
        "h4" -> title (nodes x)
        "h5" -> title (nodes x)
        "b" -> bold (nodes x)
        "strong" -> bold (nodes x)
        "a"
          | Just link <- attribute "href" x -> "<" <> link <> " " <> nodes x <> "> "
          | otherwise -> code (nodes x)
        "code"
          | null (XML.elementNodes x) -> mempty
          | otherwise -> mono (nodes x)
        "pre"
          | null (XML.elementNodes x) -> mempty
          | otherwise -> mono (nodes x)
        "note" -> mempty
        "div" -> nodes x
        "literal" -> mono (nodes x)
        y -> "<" <> y <> ">" <> nodes x

    title x = bold x <> "\n"
    bold x = "__" <> x <> "__ "
    emphasis x = "/" <> escape x <> "/ "
    code x = "'" <> x <> "' "
    mono x = "@" <> x <> "@ "
    list x = "    * " <> x

    escape = Text.concatMap f
      where
        f '/' = "\\/"
        f c = Text.singleton c

    nodes = foldMap f . XML.elementNodes
      where
        f = \case
          NodeElement e -> go e
          NodeContent t
            | Text.null x -> mempty
            | Text.head x == '$' -> "> " <> Text.tail x
            | Text.head x == '>' -> x
            | otherwise -> Text.dropWhile Char.isSeparator t
            where
              x = Text.stripStart t
          _ -> mempty

    attribute key x =
      Map.lookup
        (XML.Name key Nothing Nothing)
        (XML.elementAttributes x)

    name = XML.nameLocalName . XML.elementName

wrap :: Text -> Text -> Text
wrap sep = go . Text.lines
  where
    go [] = mempty
    go (x : []) = x
    go (x : xs) = x <> start <> foldMap prefix xs

    prefix x
      | Text.null x = start
      | otherwise = start <> " " <> x

    start = "\n" <> sep

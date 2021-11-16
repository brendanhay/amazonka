module Gen.Types.Help
  ( Help (..),
    renderHaddock,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Gen.Prelude
import Gen.Text (replaceHead)
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Pandoc as Pandoc

newtype Help = Help {fromHelp :: Text}
  deriving (Eq, Semigroup, Monoid)

-- | Empty Show instance to avoid verbose debugging output.
instance Show Help where
  show = const mempty

instance IsString Help where
  fromString = Help . fromString

instance FromJSON Help where
  parseJSON = Aeson.withText "Help" (pure . Help . convertHaddock)

instance ToJSON Help where
  -- Note the wierd templating behaviour of first line is unprefixed,
  -- with the remainder being haddock prefixed with "--".
  toJSON =
    Aeson.toJSON
      . Text.Lazy.strip
      . Text.Lazy.drop 3
      . renderHaddock False 0

renderHaddock :: Bool -> Int -> Help -> Text.Lazy.Text
renderHaddock topLevel indent (Help text) =
  let start =
        Text.Lazy.replicate (fromIntegral indent) " "

      block =
        replaceHead $ \case
          '$' -> Just "\\$"
          _c -> Nothing

      cat sep =
        mappend (start <> sep) . Text.Lazy.fromStrict . block
   in case Text.lines text of
        [] -> mempty
        x : xs ->
          Text.Lazy.unlines $
            cat (if topLevel then "-- | " else "-- ") x : map (cat "-- ") xs

convertHaddock :: Text -> Text
convertHaddock text =
  unsafePerformIO . Pandoc.runIOorExplode $ do
    html <- Pandoc.readHtml Pandoc.def text
    help <- Pandoc.writeHaddock Pandoc.def html

    pure (Text.strip help)

-- go . XML.documentRoot . DOM.parseByteString.Lazy . LBS.fromStrict . Text.encodeUtf8
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

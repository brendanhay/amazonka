module Gen.Text
  ( stripLens,
    stripTilUpper,
    stripPrefix,
    stripSuffix,
    renameOperation,
    renameServiceFunction,
    renameService,
    renameEnumValue,
    renameReserved,
    lowerHead,
    upperHead,
    toPascalCase,
    toCamelCase,
    replaceHead,
  )
where

import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Gen.Prelude

stripLens :: Text -> Text
stripLens t
  | "_" `Text.isPrefixOf` t = lowerHead (stripTilUpper t)
  | otherwise = t

stripTilUpper :: Text -> Text
stripTilUpper = Text.dropWhile (not . Char.isUpper)

stripPrefix :: Text -> Text -> Text
stripPrefix p t = Text.strip . fromMaybe t $ p `Text.stripPrefix` t

stripSuffix :: Text -> Text -> Text
stripSuffix s t = Text.strip . fromMaybe t $ s `Text.stripSuffix` t

renameOperation :: Text -> Text
renameOperation t
  | "S3" `Text.isSuffixOf` t = t
  | "EC2" `Text.isSuffixOf` t = t
  | "V2" `Text.isSuffixOf` t = t -- e.g. "ListObjectsV2"
  | otherwise = Text.dropWhileEnd (not . Char.isAlpha) t

renameServiceFunction :: Text -> Text
renameServiceFunction _n = "defaultService"

renameService :: Text -> Text
renameService =
  mappend "Amazon "
    . flip mappend " SDK"
    . stripPrefix "Amazon"
    . stripPrefix "AWS"
    . stripPrefix "Service"
    . stripSuffix "SDK"

renameEnumValue :: Text -> (Text, Text)
renameEnumValue v = (go v, v)
  where
    go = Text.map (\c -> if Char.isAlphaNum c then c else '_') . upperHead

renameReserved :: Text -> Text
renameReserved x
  | Text.isPrefixOf "new" x = x <> "'"
  | HashSet.member x xs = x <> "'"
  | otherwise = x
  where
    xs =
      fromList
        [ "Service",
          "Status",
          "ccall",
          "as",
          "case",
          "class",
          "data",
          "default",
          "delete",
          "deriving",
          "do",
          "else",
          "export",
          "filter",
          "forall",
          "foreign",
          "get",
          "group",
          "head",
          "hiding",
          "if",
          "import",
          "in",
          "infix",
          "infixl",
          "infixr",
          "instance",
          "let",
          "lex",
          "map",
          "module",
          "newtype",
          "object",
          "of",
          "pattern",
          "primitive",
          "primitive",
          "pure",
          "qualified",
          "return",
          "role",
          "tail",
          "then",
          "type",
          "where"
        ]

toPascalCase :: Text -> Text
toPascalCase = upperHead . toCamelCase

toCamelCase :: Text -> Text
toCamelCase = toCamelCase' . Text.split (not . Char.isAlphaNum) . Text.dropWhile (not . Char.isAlphaNum)
  where
    toCamelCase' :: [Text] -> Text
    toCamelCase' xs =
      case filter (not . Text.null) xs of
        [] -> ""
        y : ys -> mconcat (lowerHead y : map upperHead ys)

lowerHead :: Text -> Text
lowerHead text
  | Text.all (\c -> Char.isUpper c || Char.isDigit c) text =
      Text.toLower text
  | otherwise =
      fromMaybe (mapHead Char.toLower text)
        . msum
        $ map
          (\acronym -> mappend (Text.toLower acronym) <$> Text.stripPrefix acronym text)
          [ "KMS",
            "DB",
            "MFA"
          ]

upperHead :: Text -> Text
upperHead = mapHead Char.toUpper

mapHead :: (Char -> Char) -> Text -> Text
mapHead f text =
  fromMaybe text $ do
    (c, cs) <- Text.uncons text
    pure (Text.cons (f c) cs)

replaceHead :: (Char -> Maybe Text) -> Text -> Text
replaceHead f text =
  fromMaybe text $ do
    (c, cs) <- Text.uncons text
    replace <- f c
    pure (replace <> cs)

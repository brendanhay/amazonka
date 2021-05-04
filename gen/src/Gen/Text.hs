-- Module      : Gen.Text
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Text
  ( stripLens,
    stripTilUpper,
    stripPrefix,
    stripSuffix,
    renameOperation,
    renameServiceFunction,
    renameService,
    renameBranch,
    renameReserved,
    lowerHead,
    upperHead,
    toPascalCase,
    toCamelCase,
    replaceHead,
  )
where

import Control.Error
import Control.Monad
import Data.Bifunctor
import qualified Data.Char as Char
import qualified Data.HashSet as Set
import Data.Text (Text)
import qualified Data.Text as Text

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
renameOperation = Text.dropWhileEnd (not . Char.isAlpha)

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

renameBranch :: Text -> (Text, Text)
renameBranch = first go . join (,)
  where
    go = Text.map (\c -> if Char.isAlphaNum c then c else '_') . upperHead

renameReserved :: Text -> Text
renameReserved x
  | Text.isPrefixOf "new" x = x <> "'"
  | Set.member x xs = x <> "'"
  | otherwise = x
  where
    xs =
      Set.fromList $
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

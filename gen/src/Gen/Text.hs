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
import Data.Char qualified as Char
import Data.Foldable qualified as Fold
import Data.HashSet qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

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

-- Since 'takeLowerText' is used for FromText parser instances,
-- the branch value is lowercased here.
--
-- Tangentially the 'takeLowerText' function exists to avoid the
-- horrendous inlining that use Data.CaseInsensitive provokes and
-- the subsequent compilation time explosion on a project of this size.
renameBranch :: Text -> (Text, Text)
renameBranch = first (renameReserved . go) . join (,)
  where
    go x
      | decimal x = Text.cons 'D' . cat $ split x
      | Text.all Char.isDigit x = Text.cons 'N' x
      | Text.length x <= 2 = Text.toUpper x
      | otherwise = upperHead . cat $ split x

    cat = Fold.foldMap (Text.intercalate "_" . map component . Text.split dot)
    split = Text.split seperator

    dot x = x == '.'

    seperator x =
      x == '\\'
        || x == '/'
        || x == '+'
        || x == ' '
        || x == '('
        || x == ')'
        || x == ':'
        || x == '-'
        || x == '_'
        || x == '*'

    component x
      | Text.length x <= 1 = x
      | Char.isDigit (Text.last x) = Text.toUpper x
      | otherwise = toPascalCase x

    decimal = Text.all (\c -> Char.isDigit c || c == '.')

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
lowerHead text =
  fromMaybe (mapHead Char.toLower text)
    . msum
    $ map
      (\acronym -> mappend (Text.toLower acronym) <$> Text.stripPrefix acronym text)
      [ "KMS",
        "DB"
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

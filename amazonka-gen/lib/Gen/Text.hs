-- |
-- Module      : Gen.Text
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Text where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.Manipulate as Manipulate
import Gen.Prelude
import qualified Text.Parsec.Language as Parsec.Language
import qualified Text.Parsec.Token as Parsec.Token

asText :: (Text -> Text) -> String -> String
asText f = Text.unpack . f . Text.pack

dropLower :: Text -> Text
dropLower = Text.dropWhile (not . Char.isUpper)

safeHead :: Text -> Maybe Text
safeHead = fmap (Text.singleton . fst) . Text.uncons

stripLens :: Text -> Text
stripLens t
  | "_" `Text.isPrefixOf` t = Manipulate.lowerHead (dropLower t)
  | otherwise = t

stripPrefix :: Text -> Text -> Text
stripPrefix p t = Text.strip . fromMaybe t $ p `Text.stripPrefix` t

stripSuffix :: Text -> Text -> Text
stripSuffix s t = Text.strip . fromMaybe t $ s `Text.stripSuffix` t

renameOperation :: Text -> Text
renameOperation t
  | "S3" `Text.isSuffixOf` t = t
  | "V2" `Text.isSuffixOf` t = t -- Allow "ListObjectsV2"
  | otherwise = Text.dropWhileEnd f (Text.strip t)
  where
    f x = x == '_' || Char.isDigit x

serviceFunction :: Text -> Text
serviceFunction = const "mkServiceConfig"

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
      | otherwise = cat $ split x

    cat = Foldable.foldMap (Text.intercalate "_" . map component . Text.split dot)
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
      | Text.all Char.isUpper x = Manipulate.toPascal (Text.toLower x)
      | otherwise = Manipulate.toPascal x

    decimal = Text.all (\c -> Char.isDigit c || c == '.')

renameReserved :: Text -> Text
renameReserved x
  | HashSet.member x xs = x <> "'"
  | otherwise = x
  where
    xs =
      HashSet.fromList $
        map Text.pack $
          Parsec.Token.reservedNames Parsec.Language.haskellDef
            ++ [ "role",
                 "pattern"
               ]

camelAcronym :: Text -> Text
camelAcronym = id

lowerFirstAcronym :: Text -> Text
lowerFirstAcronym x
  | Text.all Char.isUpper x = Text.toLower x
  | otherwise = Manipulate.lowerHead x

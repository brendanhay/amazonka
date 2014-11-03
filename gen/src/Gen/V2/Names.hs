{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.V2.Names
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Names where

import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate

enumName :: Text -> Text -> Text
enumName k v1 = Text.toUpper k <> toPascal v3
  where
    v3 | Text.all f v2 = Text.toLower v2
       | otherwise     = v2

    v2 = Text.replace ":" "-" v1

    f c = isUpper c || c `elem` "-_/"

lensName :: Text -> Text
lensName = stripText "_"

fieldName :: Text -> Text
fieldName = mappend "_" . lensName

keyPython :: Text -> Text
keyPython = toSnake . keyName . Text.replace "." "_"

keyName :: Text -> Text
keyName t
    | "_" `Text.isPrefixOf` t = lowerHead (dropLower t)
    | otherwise               = t

ctorName :: Text -> Text
ctorName t = toSpinal (fromMaybe t ("'" `Text.stripSuffix` t))

dropLower :: Text -> Text
dropLower = Text.dropWhile (not . isUpper)

stripAWS :: Text -> Text
stripAWS = Text.strip
    . stripText "Amazon"
    . Text.strip
    . stripText "AWS"
    . Text.strip

stripText :: Text -> Text -> Text
stripText p t = fromMaybe t (p `Text.stripPrefix` t)

numericSuffix :: Text -> Text
numericSuffix t
    | Text.null t                 = Text.singleton '1'
    | x <- Text.last t, isDigit x = Text.init t `Text.snoc` succ x
    | otherwise                   = t `Text.snoc` '1'

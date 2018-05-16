{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Types.Notation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Notation where

import Control.Applicative

import Data.Aeson
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text          (Text)

import Gen.Types.Id

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as Text

data Key a
    = Key  { fromKey :: a }
    | Each { fromKey :: a }
    | Last { fromKey :: a }
      deriving (Eq, Show, Functor, Foldable)

data Notation a
    = Access       (NonEmpty (Key a))
    | NonEmptyList (Key a)
    | NonEmptyText (Key a)
    | Choice       (Notation a) (Notation a)
      deriving (Eq, Show, Functor, Foldable)

instance FromJSON (Notation Id) where
    parseJSON = withText "notation" (either fail pure . parseNotation)

parseNotation :: Text -> Either String (Notation Id)
parseNotation t = mappend msg `first` A.parseOnly expr1 t
  where
    msg = "Failed parsing index notation: "
        ++ Text.unpack t
        ++ ", with: "

    expr0 = nonEmptyList <|> nonEmptyText <|> access
    expr1 = choice <|> expr0

    choice = Choice
        <$> expr0
         <* A.string "||"
        <*> expr1

    nonEmptyList = NonEmptyList
        <$> (A.string "length(" *> key1 <* A.char ')')
        <*  strip (A.char '>')
        <*  strip (A.string "`0`")

    nonEmptyText = NonEmptyText
        <$> (A.string "length(" *> key0 <* A.char ')')
        <*  strip (A.char '>')
        <*  strip (A.string "`0`")

    access = do
        x:xs <- A.sepBy1 key (A.char '.')
        pure $! Access (x :| xs)

    key   = key1 <|> key0
    key1  = (Each <$> label <* A.string "[]")
        <|> (Last <$> label <* A.string "[-1]")
    key0  = (Key  <$> label)

    label = mkId <$> strip (A.takeWhile1 delim)

    delim = A.notInClass "0-9[].`()|><= "

    strip p = A.skipSpace *> p <* A.skipSpace

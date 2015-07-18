{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Types.Notation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Types.Notation where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Gen.Types.Id
import           Gen.Types.Orphans    ()

data Key a
    = Key  { fromKey :: a }
    | Each { fromKey :: a }
    | Last { fromKey :: a }
      deriving (Eq, Show, Functor, Foldable)

data Notation a
    = Access   (NonEmpty (Key a))
    | NonEmpty (Key a)
    | Choice   (Notation a) (Notation a)
      deriving (Eq, Show, Functor, Foldable)

instance FromJSON (Notation Id) where
    parseJSON = withText "notation" (either fail pure . parseNotation)

parseNotation :: Text -> Either String (Notation Id)
parseNotation t = mappend msg `first` A.parseOnly expr1 t
  where
    msg = "Failed parsing index notation: "
        ++ Text.unpack t
        ++ ", with: "

    expr0 = nonEmpty <|> access
    expr1 = choice   <|> expr0

    choice = Choice
        <$> expr0
         <* A.string "||"
        <*> expr1

    nonEmpty = NonEmpty
        <$> (A.string "length(" *> key <* A.char ')')
        <*  strip (A.char '>')
        <*  strip (A.string "`0`")

    access = do
        x:xs <- A.sepBy1 key (A.char '.')
        return $! Access (x :| xs)

    key   = (Each <$> label <* A.string "[]")
        <|> (Last <$> label <* A.string "[-1]")
        <|> (Key  <$> label)

    label = mkId <$> strip (A.takeWhile1 delim)

    delim = A.notInClass "0-9[].`()|><= "

    strip p = A.skipSpace *> p <* A.skipSpace

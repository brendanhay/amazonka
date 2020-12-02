{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Types.Notation
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import qualified Data.Attoparsec.Text as A
import qualified Control.Monad as Monad
import Data.Bifunctor
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Gen.Types.Id

data Key a
  = Key {fromKey :: a}
  | Each {fromKey :: a}
  | Last {fromKey :: a}
  deriving (Eq, Show, Functor, Foldable)

data Op
  = Equal
  | Greater
  deriving (Eq, Show)

data Notation a
  = Deref (NonEmpty (Key a))
  | Length Text (Notation a) Op Integer
  | Choice (Notation a) (Notation a)
  deriving (Eq, Show, Functor, Foldable)

instance FromJSON (Notation Id) where
  parseJSON = withText "notation" (either fail pure . parseNotation)

parseNotation :: Text -> Either String (Notation Id)
parseNotation t = mappend msg `first` A.parseOnly (expr1 ) t
  where
    msg =
      "Failed parsing index notation: "
        ++ Text.unpack t
        ++ ", with "

    expr0 = nonEmptyList <|> nonEmptyText <|> dereference
    expr1 = choice <|> expr0

    choice =
      Choice
        <$> expr0
          <* A.string "||"
        <*> expr1
        A.<?> "expr1 || expr2"

    nonEmptyList =
      apply "length" expr1

    nonEmptyText =
      apply "textLength" expr0

    apply function p = do
      expr <- A.string "length(" *> p <* A.char ')'
      
      relation <-
       strip $
        (A.string "==" *> pure Equal)
          <|> (A.char '>' *> pure Greater)
      
      decimal <- strip (A.char '`' *> A.decimal <* A.char '`')

      pure (Length function expr relation decimal)

    dereference = do
      x : xs <- A.sepBy1 key (A.char '.')
      pure $! Deref (x :| xs)

    key = key1 <|> key0
    key1 = (Each <$> label <* A.string "[]") <|> (Last <$> label <* A.string "[-1]")
    key0 = (Key <$> label)

    label =
      strip $ do
        text <- A.takeWhile1 (A.notInClass "[].()|><= ")
        next <- A.peekChar

        Monad.when (next == Just '(') $
          fail $
            "encountered start of unexpected function application "
              ++ show text
              ++ "(...)"

        pure (mkId text)

    strip p =
      A.skipSpace *> p <* A.skipSpace

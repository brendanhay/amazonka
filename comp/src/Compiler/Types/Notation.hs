{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.Types.Notation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Notation where

import           Compiler.Types.Id
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as A
import           Data.Foldable        (foldl')
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text

-- NextMarker || Contents[-1].Key
-- Jobs[-1].JobId
-- StreamDescription.Shards[-1].ShardId
-- StreamNames[-1]
-- "Reservations[].Instances[].State.Name"
-- length(PasswordData) > `0`
-- services | [@[?length(deployments)!=`1`], @[?desiredCount!=runningCount]][] | length(@) == `0`
-- InstanceStatuses[].SystemStatus.Status

data Notation
    = Key    Id                  -- StreamDescription
    | Each                       -- []
    | Index  !Int                -- [n]
    | Or     !Notation !Notation -- \||
    | Apply  !Notation !Notation
    | Length Id !Ordering !Int
      deriving (Eq, Show)

instance FromJSON Notation where
    parseJSON = withText "notation" (either fail pure . parseOnly p)
      where
        p = notationParser <* A.endOfInput

notationParser :: Parser Notation
notationParser = foldl' Apply <$> expr1 <*> many expr1
  where
    expr1 = cond  <|> expr0
    expr0 = terms <|> term

    cond = Or
        <$> (expr0 <* or)
        <*> expr0

    terms = foldl' Apply
        <$> (term <* A.char '.')
        <*> A.sepBy1 term (A.char '.')

    term = do
        k <- Key <$> key
        m <- optional (each <|> index)
        return $! maybe k (Apply k) m

    length = Length
        <$> (A.string "length(" *> key <* A.char ')')
        <*> relation
        <*> int

    key = mkId <$> A.takeWhile1 (A.notInClass "0-9[].`()|><= ")

    each  = A.char '[' *> return Each <* A.char ']'
    index = A.char '[' *> (Index <$> A.signed A.decimal) <* A.char ']'

    int = A.char '`' *> A.signed A.decimal <* A.char '`'

    or = A.skipSpace *> A.string "||" <* A.skipSpace

    relation = A.skipSpace *> (eq <|> gt <|> lt) <* A.skipSpace
      where
        eq = A.string "==" >> return EQ
        gt = A.char   '>'  >> return GT
        lt = A.char   '<'  >> return LT

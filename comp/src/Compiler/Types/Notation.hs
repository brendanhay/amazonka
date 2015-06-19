{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Foldable        (foldl', foldr')
import           Data.Maybe
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

data Notation a
    = Key    a                         -- StreamDescription
    | Index  a !Int                    -- [n]
    | Each   a                         -- []
    | Or     (Notation a) (Notation a) -- \||
    | Apply  (Notation a) (Notation a)
    | Length a !Ordering !Int
      deriving (Eq, Show, Functor, Foldable, Traversable)

instance FromJSON (Notation Id) where
    parseJSON = withText "notation" (either fail pure . parseOnly notationParser)

-- lastKey :: Notation a -> Maybe a
-- lastKey = listToMaybe . reverse . foldMap (:[])

    -- Key   n      -> n
    -- Index n _    -> n
    -- Each  n _    -> n
    -- Or    _ y    -> lastKey y
    -- Apply _ y    -> lastKey y
    -- Length n _ _ -> n

notationParser :: Parser (Notation Id)
notationParser = (expr1 <* A.endOfInput) <|> (Key . mkId <$> A.takeText)
  where
    expr1 = cond   <|> expr0
    expr0 = length <|> apply <|> label

    cond = Or <$> expr0 <* strip (A.string "||") <*> expr1

    apply = foldl' Apply
        <$> (label <* A.char '.')
        <*> A.sepBy1 label (A.char '.')

    label = strip (index <|> each <|> key)

    key   = Key   <$> ident
    index = Index <$> ident <*> (A.char '[' *> num <* A.char ']')
    each  = Each  <$> ident <*  A.string "[]"

    ident = mkId <$> A.takeWhile1 (A.notInClass "0-9[].`()|><= ")

    length = Length
        <$> (A.string "length(" *> ident <* A.char ')')
        <*> strip (eq <|> gt <|> lt)
        <*> (A.char '`' *> num <* A.char '`')

    eq = A.string "==" >> return EQ
    gt = A.char   '>'  >> return GT
    lt = A.char   '<'  >> return LT

    num = A.signed A.decimal

    strip p = A.skipSpace *> p <* A.skipSpace

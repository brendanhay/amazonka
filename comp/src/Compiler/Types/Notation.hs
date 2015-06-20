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
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor
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
    = Blank
    | Key      a
    | NonEmpty a
    | Each     a
    | Last     a
    | Apply    (Notation a) (Notation a)
    | Choice   (Notation a) (Notation a)
      deriving (Eq, Show, Functor, Foldable, Traversable)

-- keys :: Traversal' (Notation a) a
-- keys f = traverse f

instance FromJSON (Notation Id) where
    parseJSON = withText "notation" (either fail pure . parseNotation)

parseNotation :: Text -> Either String (Notation Id)
parseNotation t = mappend msg `first` A.parseOnly expr1 t
      where
        msg = "Failed parsing index notation: "
            ++ Text.unpack t
            ++ ", with: "

        expr0 = length' <|> each' <|> last' <|> keyed
        expr1 = choice  <|> apply <|> expr0

        each' = Each <$> label <* A.string "[]"
        last' = Last <$> label <* A.string "[-1]"

        length' = NonEmpty
            <$> (A.string "length(" *> label <* A.char ')')
            <*  strip (A.char '>')
            <*  strip (A.string "`0`")

        apply = Apply
            <$> expr0
            <* A.char '.'
            <*> expr1

        choice = Choice
            <$> expr0
             <* A.string "||"
            <*> expr1

        keyed = Key <$> label

        label = mkId <$> strip (A.takeWhile1 delim)

        delim = A.notInClass "0-9[].`()|><= "

        strip p = A.skipSpace *> p <* A.skipSpace

-- instance A.ToJSON Key where
--     toJSON = A.toJSON . go
--       where
--         go = \case
--             Blank                -> "(to id)"
--             Key    n             -> n
--             Index  n k           -> "index "   <> n <> " " <> go k
--             Apply  n (Index a b) -> "index ("  <> go (Apply n (Key a)) <> ") " <> go b
--             Apply  n k           -> n <> " . " <> go k
--             Choice n k           -> "choice (" <> go n <> ") (" <> go k <> ")"

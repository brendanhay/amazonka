{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

-- Module      : Compiler.Types.Ann
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Ann where

import           Compiler.TH
import           Compiler.Types.Id
import           Compiler.Types.Timestamp
import           Control.Lens
import           Data.Aeson               (ToJSON (..))
import           Data.Hashable
import qualified Data.HashSet             as Set
import           Data.Jason               hiding (ToJSON (..))
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics

type Set = Set.HashSet

-- FIXME: consider replacing raw tuples with something fancier for
-- deconstructing and avoiding rebalancing of tuples or type operators.
type Related  = (Id, Relation)
type Prefixed = (Id, Relation, Maybe Text)
type Solved   = (Id, Relation, Maybe Text, TType, [Derive], [Instance])

data Direction
   = Input
   | Output
     deriving (Eq, Show)

data Relation
    = Uni (Set Id) Direction
    | Bi  (Set Id)
      deriving (Eq, Show)

instance Monoid Relation where
    mempty      = Bi mempty
    mappend a b =
        case (a, b) of
            (Bi  x,   Bi  y)   -> Bi  (x <> y)
            (Bi  x,   Uni y _) -> Bi  (x <> y)
            (Uni x _, Bi  y)   -> Bi  (x <> y)
            (Uni x i, Uni y o)
                | i == o       -> Uni (x <> y) i
                | otherwise    -> Bi  (x <> y)

relation :: Id -> Direction -> Relation
relation n = Uni (Set.singleton n)

calls :: Traversal' Relation Id
calls = lens f (flip g) . each
  where
    f = Set.toList . \case
        Uni x _ -> x
        Bi  x   -> x

    g (Set.fromList -> x) = \case
        Uni _ d -> Uni x d
        Bi  _   -> Bi  x

shared :: Relation -> Bool
shared = (> 1) . lengthOf calls

data Lit
    = Int
    | Long
    | Double
    | Text
    | Blob
    | Time (Maybe Timestamp)
    | Bool
      deriving (Show)

data TType
    = TType      Text
    | TLit       Lit
    | TNatural
    | TStream
    | TMaybe     TType
    | TSensitive TType
    | TList      TType
    | TList1     TType
    | TMap       TType TType
      deriving (Show)

data Derive
    = DEq
    | DOrd
    | DRead
    | DShow
    | DEnum
    | DNum
    | DIntegral
    | DReal
    | DRealFrac
    | DRealFloat
    | DMonoid
    | DSemigroup
    | DIsString
      deriving (Eq, Ord, Show, Generic)

instance Hashable Derive

instance FromJSON Derive where
    parseJSON = gParseJSON' (spinal & ctor %~ (. Text.drop 1))

data Instance
    = FromJSON  -- headers, status, json object
    | ToJSON
    | FromXML   -- headers, status, xml cursor
    | ToXML     -- xml
    | ToQuery   -- query params
    | FromBody  -- headers, status, streaming response body
    | ToPath    -- uri and query components
    | ToBody    -- streaming request body
    | ToHeaders -- headers
      deriving (Eq, Ord, Show, Generic)

instance Hashable Instance

instToText :: Instance -> Text
instToText = Text.pack . show

instance ToJSON Instance where
    toJSON = toJSON . instToText

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
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
import           Control.Comonad
import           Control.Comonad.Cofree
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
    mappend l r =
        case (l, r) of
            (Bi  x,   Bi  y)   -> Bi  (x <> y)
            (Bi  x,   Uni y _) -> Bi  (x <> y)
            (Uni x _, Bi  y)   -> Bi  (x <> y)
            (Uni x a, Uni y b)
                | a == b       -> Uni (x <> y) b
                | otherwise    -> Bi  (x <> y)

mkRelation :: Id -> Direction -> Relation
mkRelation n = Uni (Set.singleton n)

parents :: Traversal' Relation Id
parents = lens f (flip g) . each
  where
    f = Set.toList . \case
        Uni  x _ -> x
        Bi   x   -> x

    g (Set.fromList -> x) = \case
        Uni _ d -> Uni x d
        Bi  _   -> Bi  x

shared :: Relation -> Bool
shared = (> 1) . lengthOf parents

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

data Related = Related
    { _annOp       :: !Bool
    , _annId       :: Id
    , _annRelation :: Relation
    } deriving (Show)

makeClassy ''Related

instance (Functor f, HasRelated a) => HasRelated (Cofree f a) where
    related = lens extract (flip (:<) . unwrap) . related

instance HasId Related where
    identifier = _annId

data Prefixed = Prefixed
    { _annRelated :: Related
    , _annPrefix  :: Maybe Text
    } deriving (Show)

makeClassy ''Prefixed

instance (Functor f, HasPrefixed a) => HasPrefixed (Cofree f a) where
    prefixed = lens extract (flip (:<) . unwrap) . prefixed

instance HasRelated Prefixed where
    related = annRelated

instance HasId Prefixed where
    identifier = view annId

data Solved = Solved
    { _annPrefixed  :: Prefixed
    , _annType      :: TType
    , _annDerive    :: [Derive]
    , _annInstances :: [Instance]
    } deriving (Show)

makeClassy ''Solved

instance (Functor f, HasSolved a) => HasSolved (Cofree f a) where
    solved = lens extract (flip (:<) . unwrap) . solved

instance HasRelated Solved where
    related = prefixed . related

instance HasPrefixed Solved where
    prefixed = annPrefixed

instance HasId Solved where
    identifier = view annId

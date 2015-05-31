{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

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
import           Compiler.Types.URI
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Data.Hashable
import qualified Data.HashSet           as Set
import           Data.Jason             hiding (ToJSON (..))
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GHC.Generics           (Generic)

type Set = Set.HashSet

data Direction
   = Output
   | Input
     deriving (Eq, Show)

data Mode
    = Bi
    | Uni Direction
      deriving (Eq, Show)

instance Monoid Mode where
    mempty                  = Bi
    mappend (Uni i) (Uni o)
        | i == o            = Uni o
    mappend _       _       = Bi

data Relation = Relation
    { _relParents :: Set Id
    , _relMode    :: !Mode
    } deriving (Eq, Show)

makeClassy ''Relation

instance Monoid Relation where
    mempty      = Relation mempty mempty
    mappend a b = Relation
        (_relParents a <> _relParents b)
        (_relMode    a <> _relMode    b)

instance (Functor f, HasRelation a) => HasRelation (Cofree f a) where
    relation = lens extract (flip (:<) . unwrap) . relation

mkRelation :: Set Id -> Direction -> Relation
mkRelation xs = Relation xs . Uni

isShared :: HasRelation a => a -> Bool
isShared = (> 1) . Set.size . view relParents

data Lit
    = Int
    | Long
    | Double
    | Text
    | Blob
    | Time
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
    | DGeneric
      deriving (Eq, Ord, Show, Generic)

instance Hashable Derive

instance FromJSON Derive where
    parseJSON = gParseJSON' (spinal & ctor %~ (. Text.drop 1))

data Related
    = RShape Id Relation
    | RReq   Id Relation (HTTP Identity)
    | RRes   Id Relation
      deriving (Show)

makePrisms ''Related

relPairs :: Related -> (Id, Relation)
relPairs = \case
    RShape n r   -> (n, r)
    RReq   n r _ -> (n, r)
    RRes   n r   -> (n, r)

class HasRelated a where
    related     :: Lens' a Related
    annId       :: Lens' a Id
    annRelation :: Lens' a Relation

    annId = related . lens (fst . relPairs) (flip f)
      where
        f n = \case
            RShape _ r   -> RShape n r
            RReq   _ r h -> RReq   n r h
            RRes   _ r   -> RRes   n r

    annRelation = related . lens (snd . relPairs) (flip f)
      where
        f r = \case
            RShape n _   -> RShape n r
            RReq   n _ h -> RReq   n r h
            RRes   n _   -> RRes   n r

instance HasRelated Related where
    related = id

instance (Functor f, HasRelated a) => HasRelated (Cofree f a) where
    related = lens extract (flip (:<) . unwrap) . related

instance HasId Related where
    identifier = view annId

instance HasRelation Related where
    relation = annRelation

mkRelShape :: HasRelation a => Id -> a -> Related
mkRelShape n = RShape n . view relation

mkRelOp :: HasRelation a => Direction -> Id -> HTTP Identity -> a -> Related
mkRelOp Input  n h r = RReq n (r ^. relation) h
mkRelOp Output n _ r = RRes n (r ^. relation)

-- isReq :: HasRelated a => a -> Bool
-- isReq = not . isn't _RReq . view related

-- isRes :: HasRelated a => a -> Bool
-- isRes = not . isn't _RRes . view related

data Prefixed = Prefixed
    { _annRelated :: Related
    , _annPrefix  :: Maybe Text
    } deriving (Show)

makeClassy ''Prefixed

instance (Functor f, HasPrefixed a) => HasPrefixed (Cofree f a) where
    prefixed = lens extract (flip (:<) . unwrap) . prefixed

instance HasRelation Prefixed where
    relation = related . relation

instance HasRelated Prefixed where
    related = annRelated

instance HasId Prefixed where
    identifier = view annId

data Solved = Solved
    { _annPrefixed :: Prefixed
    , _annType     :: TType
    , _annDerive   :: [Derive]
    } deriving (Show)

makeClassy ''Solved

instance (Functor f, HasSolved a) => HasSolved (Cofree f a) where
    solved = lens extract (flip (:<) . unwrap) . solved

instance HasRelation Solved where
    relation = prefixed . relation

instance HasRelated Solved where
    related = prefixed . related

instance HasPrefixed Solved where
    prefixed = annPrefixed

instance HasId Solved where
    identifier = view annId

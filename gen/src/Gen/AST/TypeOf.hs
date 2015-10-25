{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Gen.AST.TypeOf
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.TypeOf
    ( TypeOf (..)
    , derivingOf
    , pointerTo
    , isEQ
    , typeDefault
    ) where

import           Control.Comonad.Cofree
import           Control.Lens           hiding (enum, mapping, (??))
import           Data.Foldable          (foldr')
import           Data.List              (intersect)
import           Data.Monoid
import           Gen.Types

class TypeOf a where
    typeOf :: a -> TType

instance TypeOf TType where
    typeOf = id

instance TypeOf Solved where
    typeOf = _annType

instance TypeOf Replace where
    typeOf Replace{..} =
        TType (typeId _replaceName) (uniq (_replaceDeriving <> base))

instance HasId a => TypeOf (Shape a) where
    typeOf (x :< s) = sensitive s (shape s)
      where
        n = identifier x

        shape = \case
            Ptr _ t              -> t
            Struct st            -> TType  (typeId n) (struct st)
            Enum   {}            -> TType  (typeId n) (enum <> base)
            List (ListF i e)
                | nonEmpty i     -> TList1 (typeOf e)
                | otherwise      -> TList  (typeOf e)
            Map (MapF _ k v)     -> TMap   (typeOf k) (typeOf v)
            Lit i l              -> lit i l

        lit i = \case
            Int                  -> natural i (TLit Int)
            Long                 -> natural i (TLit Long)
            Blob | isStreaming i -> TStream
            l                    -> TLit l

        struct st
            | isStreaming st = stream
            | otherwise      = uniq $
                foldr' (intersect . derivingOf) base (st ^.. references)

instance HasId a => TypeOf (RefF (Shape a)) where
    typeOf r
        | isStreaming r = TStream
        | otherwise     = typeOf (r ^. refAnn)

isEQ :: TypeOf a => a -> Bool
isEQ = elem DEq . derivingOf

-- FIXME: this whole concept of pointers and limiting the recursion stack
-- when calculating types is broken - there are plenty of more robust/sane
-- ways to acheive this, revisit.
pointerTo :: Id -> ShapeF a -> TType
pointerTo n = \case
    List (ListF i e)
        | nonEmpty i     -> TList1 (t (_refShape e))
        | otherwise      -> TList  (t (_refShape e))
    Map (MapF _ k v)     -> TMap   (t (_refShape k)) (t (_refShape v))
    _                    -> t n
  where
    t x = TType (typeId x) base

derivingOf :: TypeOf a => a -> [Derive]
derivingOf = uniq . typ . typeOf
  where
    typ = \case
        TType      _ ds -> ds
        TLit       l    -> lit l
        TNatural        -> base <> num
        TStream         -> stream
        TMaybe     t    -> typ t
        TSensitive t    -> DShow : typ t
        TList      e    -> monoid <> intersect base (typ e)
        TList1     e    -> DSemigroup : intersect base (typ e)
        TMap       k v  -> monoid <> intersect (typ k) (typ v)

    lit = \case
        Int    -> base <> num
        Long   -> base <> num
        Double -> base <> frac
        Text   -> base <> string
        Blob   -> base
        Time   -> DOrd : base
        Bool   -> enum <> base
        Json   -> [DEq, DShow, DRead, DData, DTypeable, DGeneric]

stream, string, num, frac, monoid, enum, base :: [Derive]
stream = [DShow, DGeneric]
string = [DOrd, DIsString]
num    = DNum : DIntegral : DReal : enum
frac   = [DOrd, DRealFrac, DRealFloat]
monoid = [DMonoid, DSemigroup]
enum   = [DOrd, DEnum]
base   = [DEq, DRead, DShow, DData, DTypeable, DGeneric]

typeDefault :: TType -> Bool
typeDefault = \case
    TSensitive t       -> typeDefault t
    TMaybe     {}      -> True
    TList      {}      -> True
    TMap       {}      -> True
    _                  -> False

natural :: HasInfo a => a -> TType -> TType
natural x
    | Just i <- x ^. infoMin
    , i >= 0    = const TNatural
    | otherwise = id

sensitive :: HasInfo a => a -> TType -> TType
sensitive x
    | x ^. infoSensitive = TSensitive
    | otherwise          = id

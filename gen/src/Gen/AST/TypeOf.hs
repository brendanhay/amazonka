{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Gen.AST.TypeOf
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.AST.TypeOf
    ( TypeOf (..)
    , derivingOf
    , isEQ
    , typeDefault
    ) where

import           Control.Comonad.Cofree
import           Control.Lens           hiding (enum, mapping, (??))
import           Data.Foldable          (foldr')
import qualified Data.HashSet           as Set
import           Data.List              (intersect)
import           Data.Monoid
import           Gen.Types

class TypeOf a where
    typeOf :: a -> TType

instance TypeOf TType where
    typeOf = id

instance TypeOf Solved where
    typeOf = view annType

instance HasId a => TypeOf (Shape a) where
    typeOf (x :< s) = sensitive s (shape s)
      where
        n = identifier x

        shape = \case
            Ptr    _ ds          -> TType  (n ^. typeId) (ptr ds)
            Struct st            -> TType  (n ^. typeId) (struct st)
            Enum   {}            -> TType  (n ^. typeId) enum
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

        ptr = uniq . mappend [DEq, DShow] . Set.toList

        struct st
            | isStreaming st = [DShow]
            | otherwise      = uniq $
                foldr' (intersect . derivingOf) base (st ^.. references)

instance HasId a => TypeOf (RefF (Shape a)) where
    typeOf r
        | isStreaming r = TStream
        | otherwise     = typeOf (r ^. refAnn)


isEQ :: TypeOf a => a -> Bool
isEQ = elem DEq . derivingOf

derivingOf :: TypeOf a => a -> [Derive]
derivingOf = uniq . typ . typeOf
  where
    typ = \case
        TType      _ ds -> ds
        TLit       l    -> lit l
        TNatural        -> enum
        TStream         -> [DShow]
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
        Time   -> enum
        Bool   -> enum

string, num, frac, monoid, enum, base :: [Derive]
string = [DOrd, DIsString]
num    = [DOrd, DEnum, DNum, DIntegral, DReal]
frac   = [DOrd, DRealFrac, DRealFloat]
monoid = [DMonoid, DSemigroup]
enum   = [DOrd, DEnum, DGeneric] <> base
base   = [DEq, DRead, DShow]

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

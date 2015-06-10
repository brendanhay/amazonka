{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST.Solve
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Solve
    ( solve
    ) where

import           Compiler.AST.Cofree
import           Compiler.AST.TypeOf
import           Compiler.Import
import           Compiler.Types
import           Control.Arrow          ((&&&))
import           Control.Comonad.Cofree
import           Control.Lens           hiding (enum, mapping, (??))
import           Control.Monad.State
import           Data.Foldable          (foldr')
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.List              (intersect, nub, sort)
import           Data.Monoid            hiding (Product, Sum)
import           Debug.Trace

-- Can just assume for the purposes of determing instances
-- that if a Shape's relation contains no parents then it's
-- an operation?

-- FIXME: Necessary to update the Relation?
solve :: (Traversable t)
      => Config
      -> t (Shape Prefixed)
      -> t (Shape Solved)
solve cfg = (`evalState` env) . traverse (annotate con id (pure . ann))
 where
    env :: Map Id (TType, [Derive])
    env = replaced def cfg
      where
        def x =
           let t  = x ^. replaceName . typeId . to TType
               ds = x ^. replaceDeriving . to Set.toList
            in (t, ds)

    ann :: Shape Prefixed -> (TType, [Derive])
    ann x = (typeOf x, derive x)

    con :: Prefixed -> (TType, [Derive]) -> Solved
    con p (t, ds) = Solved p t ds

-- FIXME: Filter constraints based on info like min/max of lists etc.
derive :: HasId a => Shape a -> [Derive]
derive (a :< s) = uniq (shape s)
  where
    shape :: HasId a => ShapeF (Shape a) -> [Derive]
    shape = \case
        Ptr  _ ds -> base <> Set.toList ds
        Struct st -> refs st
        List   l  -> monoid <> refs l -- Think about how these should
        Map    m  -> monoid <> refs m -- be intersected with a parent's.
        Enum   {} -> base <> enum
        Lit   i l
            | streaming i -> [DShow]
            | otherwise   -> lit l

    lit :: Lit -> [Derive]
    lit = \case
        Int    -> base <> num
        Long   -> base <> num
        Double -> base <> frac
        Text   -> base <> string
        Blob   -> base
        Time   -> base <> enum
        Bool   -> base <> enum

    refs :: (Foldable f, HasId a) => f (Shape a) -> [Derive]
    refs = foldr' (intersect . derive) base

    string = [DOrd, DIsString]
    num    = [DOrd, DEnum, DNum, DIntegral, DReal]
    frac   = [DOrd, DRealFrac, DRealFloat]
    monoid = [DMonoid, DSemigroup]
    enum   = [DOrd, DEnum, DGeneric]
    base   = [DEq, DRead, DShow]

replaced :: (Replace -> a) -> Config -> Map Id a
replaced f =
      Map.fromList
    . map (_replaceName &&& f)
    . Map.elems
    . vMapMaybe _replacedBy
    . _typeOverrides

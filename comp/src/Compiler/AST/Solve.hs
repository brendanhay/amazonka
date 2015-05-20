{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
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
import           Compiler.Protocol
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

-- FIXME: Necessary to update the Relation?
solve :: (Traversable t)
      => Config
      -> Protocol
      -> t (Shape Prefixed)
      -> t (Shape Solved)
solve cfg proto = (`evalState` env) . traverse go
 where
    go  = fmap (fmap assoc) . annotate id (pure . ann)
    env = replaced def cfg
      where
        def x =
           ( x ^. replaceName . typeId . to TType
           , x ^. replaceDeriving . to Set.toList
           , instances proto mempty
           )

    assoc :: (Prefixed, (TType, [Derive], [Instance]))
          -> Solved
    assoc ((n, r, p), (t, ds, is)) = (n, r, p, t, ds, is)

    ann :: Shape Prefixed -> (TType, [Derive], [Instance])
    ann x@((_, r, _) :< _) = (typeOf x, derive x, instances proto r)

-- FIXME: Filter constraints based on info like min/max of lists etc.
derive :: Shape a -> [Derive]
derive (_ :< s) = nub . sort $ shape s
  where
    shape :: ShapeF (Shape a) -> [Derive]
    shape = \case
        Ptr  _ ds -> base <> Set.toList ds
        Struct st -> foldr' (intersect . derive) base st
        List   {} -> base <> monoid
        Map    {} -> base <> monoid
        Enum   {} -> base <> enum
        Lit  _  l -> lit l

    lit :: Lit -> [Derive]
    lit = \case
        Int    -> base <> num
        Long   -> base <> num
        Double -> base <> frac
        Text   -> base <> string
        Blob   -> [DShow]
        Time _ -> base <> enum
        Bool   -> base <> enum

    string = [DIsString]
    num    = [DEnum, DNum, DIntegral, DReal]
    frac   = [DRealFrac, DRealFloat]
    monoid = [DMonoid, DSemigroup]
    enum   = [DEnum]
    base   = [DEq, DOrd, DRead, DShow]

replaced :: (Replace -> a) -> Config -> Map Id a
replaced f =
      Map.fromList
    . map (_replaceName &&& f)
    . Map.elems
    . vMapMaybe _replacedBy
    . _typeOverrides

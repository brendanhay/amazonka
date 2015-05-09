{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Compiler.Rewrite.Solve
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Solve where
    -- ( annotateTypes
    -- ) where

import           Compiler.Protocol
import           Compiler.Rewrite.Syntax
import           Compiler.Rewrite.TypeOf
import           Compiler.Types
import           Control.Arrow                ((&&&))
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens                 hiding (enum, mapping, (??))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Foldable                (foldl', foldr')
import           Data.Hashable
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.List                    (intersect, nub, sort)
import           Data.Monoid                  hiding (Product, Sum)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           Data.Text.Manipulate
import           HIndent
import           Language.Haskell.Exts.Build  (app, infixApp, paren)
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

type Solve = State (Map Id (TType))

solve :: Config -> [Shape Id] -> [Shape (Id :*: TType)]
solve cfg = (`evalState` initial) . traverse go
 where
    initial :: Map Id TType
    initial = replaced (TType . view (replaceName . typeId)) cfg

    go :: Shape Id -> Solve (Shape (Id :*: TType))
    go = sequence . annotate (memo (pure . typeOf))

typeOf :: Shape Id -> TType
typeOf (n :< s) = sensitive s $
    case s of
        Struct {}    -> TType (n ^. typeId)
        Enum   {}    -> TType (n ^. typeId)
        List   i e
            | nonEmpty i -> TList1 t
            | otherwise  -> TList  t
          where
            t = typeOf (e ^. refAnn)
        Map    _ k v -> TMap (typeOf (k ^. refAnn)) (typeOf (v ^. refAnn))
        Lit    i l   ->
            case l of
                Int  -> natural i (TLit l)
                Long -> natural i (TLit l)
                _    -> TLit l

memo :: (Shape Id -> Solve TType) -> Shape Id -> Solve TType
memo f x@(n :< _) = gets (Map.lookup n) >>= maybe act return
  where
    act = do
        r <- f x
        modify (Map.insert n r)
        return r

-- FIXME: Filter constraints based on info like min/max of lists etc.
derive :: Shape Id -> [Derive]
derive (_ :< s) = nub . sort $ shape s
  where
    shape :: ShapeF (Shape Id) -> [Derive]
    shape = \case
        Struct _ ms -> foldr' (intersect . derive) base ms
        List   {}   -> base <> monoid
        Map    {}   -> base <> monoid
        Enum   {}   -> base <> enum
        Lit    _ l  -> lit l

    lit :: Lit -> [Derive]
    lit = \case
        Int    -> base <> num
        Long   -> base <> num
        Double -> base <> frac
        Text   -> base <> string
        Blob   -> [DShow]
        Time   -> base <> enum
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

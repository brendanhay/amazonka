{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Compiler.AST.TypeOf
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.TypeOf
    ( shapeType
    , memberType
    , requiredType
    ) where

import           Compiler.AST.Cofree
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

shapeType :: HasId a => Shape a -> TType
shapeType (x :< s) =
    let n = identifier x
     in sensitive s $
        case s of
            Struct {}    -> TType (n ^. typeId)
            Enum   {}    -> TType (n ^. typeId)
            List   i e
                | nonEmpty i -> TList1 t
                | otherwise  -> TList  t
              where
                t = shapeType (e ^. refAnn)
            Map    _ k v ->
                TMap (shapeType (k ^. refAnn)) (shapeType (v ^. refAnn))
            Lit    i l   ->
                case l of
                    Int  -> natural i (TLit l)
                    Long -> natural i (TLit l)
                    _    -> TLit l

memberType :: Id -> Set Id -> TType -> TType
memberType n req = canDefault (Set.member n req)
  where
    canDefault :: Bool -> TType -> TType
    canDefault True  t   = t -- This field is required.
    canDefault False t
          -- This field is not required, but the TType can't be defaulted sensibly.
        | requiredType t = TMaybe t
          -- This field is not required, and can be defaulted using mempty/Nothing.
        | otherwise      = t

requiredType :: TType -> Bool
requiredType = \case
    TSensitive t  -> requiredType t
    TMaybe     {} -> False
    TList      {} -> False
    TList1     {} -> False
    TMap       {} -> False
    _             -> True

natural :: HasInfo a => a -> TType -> TType
natural x
    | Just i <- x ^. infoMin
    , i >= 0    = const TNatural
    | otherwise = id

sensitive :: HasInfo a => a -> TType -> TType
sensitive x
    | x ^. infoSensitive = TSensitive
    | otherwise          = id

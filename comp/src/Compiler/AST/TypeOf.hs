{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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
    ( TypeOf (..)
    , typeRequired
    ) where

import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Lens           hiding (enum, mapping, (??))

class TypeOf a where
    typeOf :: a -> TType

instance TypeOf TType where
    typeOf = id

instance TypeOf (a ::: Solved) where
    typeOf (_ ::: t ::: _ ::: _) = t

instance HasId a => TypeOf (Shape a) where
    typeOf (x :< s) =
        let n = identifier x
         in sensitive s $
            case s of
                Ptr    {}        -> TType  (n ^. typeId)
                Struct {}        -> TType  (n ^. typeId)
                Enum   {}        -> TType  (n ^. typeId)
                List (ListF i e)
                    | nonEmpty i -> TList1 (typeOf e)
                    | otherwise  -> TList  (typeOf e)
                Map (MapF _ k v) -> TMap   (typeOf k) (typeOf v)
                Lit i l          ->
                    case l of
                        Int      -> natural i (TLit l)
                        Long     -> natural i (TLit l)
                        _        -> TLit l

instance TypeOf a => TypeOf (RefF a) where
    typeOf = typeOf . view refAnn

typeRequired :: TType -> Bool
typeRequired = \case
    TSensitive t  -> typeRequired t
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

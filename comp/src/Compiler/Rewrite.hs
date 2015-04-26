{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Rewrite
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite where

import           Compiler.AST
import           Compiler.Rewrite.Default
import           Compiler.Rewrite.Override
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as Map
import           Data.Monoid
import           Data.Text                 (Text)

createPackage :: Monad m
              => SemVer
              -> API Maybe
              -> EitherT LazyText m Package
createPackage ver api = do
    let x = defaults api & shapes %~ overrides (api ^. typeOverrides)
    return $! Package x ver [] []

-- AST.hs
-- Constraint.hs
-- TypeOf.hs

-- Rewrite.hs
-- Rewrite/Default.hs
-- Rewrite/Override.hs
-- Rewrite/Prune.hs
-- Rewrite/Share.hs
-- Rewrite/Elaborate.hs

-- Rewrite.hs: rewrite process:
-- 1. set defaults (Default)
-- 2. apply overrides (Override)
--  - Fields:
--    * mark required fields
--    * mark optional fields
--    * rename fields
--    * retype fields
--    * prefix enums
--  - Types:
--    * rename types
--    * replace types
-- 4. determine (+ prune) unused/unreferenced shapes (Prune)
-- 5. determine sharing (Share)
-- 6. create/insert request + response shapes (Elaborate)
-- 7. generate unique prefixes (Prefix)
-- 8. generate shape->constraint index (Index)
-- 9. generate shape->type index (Index)

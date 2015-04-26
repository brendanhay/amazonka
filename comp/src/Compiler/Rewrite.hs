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
import           Compiler.Rewrite.Share
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as Map
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText

createPackage :: Monad m
              => SemVer
              -> API Maybe
              -> EitherT LazyText m Package
createPackage ver api = do
    let x = defaults api & shapes %~ overrides (api ^. typeOverrides)
--    left $ LText.pack (show (sharing ))
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
-- 3. determine (+ prune) unused/unreferenced shapes (Prune)
-- 4. determine sharing (Share)
-- 5. create/insert request + response shapes (Elaborate)
-- 6. generate unique prefixes (Prefix)
-- 7. generate shape->constraint index (Index)
-- 8. generate shape->type index (Index)

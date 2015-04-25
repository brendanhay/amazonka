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
import           Compiler.Defaults
import           Compiler.Types
import           Control.Error
import           Control.Error
import           Control.Lens
import           Data.Functor.Identity
import qualified Data.HashMap.Strict   as Map
import           Data.Monoid
import           Data.Text             (Text)

createPackage :: Monad m
              => SemVer
              -> API Maybe
              -> EitherT LazyText m Package
createPackage ver api = do
    return $! Package (setDefaults api) ver [] []

-- rewrite process:
-- 1. set defaults
-- 2. apply overrides
--  - Fields:
--    * mark required fields
--    * mark optional fields
--    * rename fields
--    * retype fields
--    * prefix enums
--    * append values to enums
--  - Types:
--    * rename types
--    * replace types
-- 3. generate ref->shape index
-- 4. determine (+ prune) unused/unreferenced shapes
-- 5. determine sharing
-- 6. create/insert request + response shapes
-- 7. generate unique prefixes
-- 8. generate shape->constraint index
-- 9. generate shape->type index

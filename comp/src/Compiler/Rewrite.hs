{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import           Compiler.Rewrite.Subst
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as Map
import qualified Data.HashSet              as Set
import           Data.List                 (sort)
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText

createLibrary :: Monad m
              => Versions
              -> Config
              -> Service Maybe Ref Shape
              -> EitherT LazyText m Library
createLibrary v x y = do
    (c, s) <- rewrite x y

    let ns     = NS ["Network", "AWS", s ^. serviceAbbrev]
        other  = c ^. operationImports ++ c ^. typeImports
        expose = ns
               : ns <> "Types"
               : ns <> "Waiters"
               : map (mappend ns . textToNS)
                     (s ^.. operations . ifolded . asIndex)

    return $! Library v c s ns (sort expose) (sort other)
  where
    rewrite c' s' = do
        let (c, s) = substitute c' s'
        (c,) <$> defaulted (override c s)

-- TODO:
-- constraints and prefixing

-- Rewrite.hs: rewrite process:
-- 1. set defaults (Default)
-- 2. apply overrides (Override)
--  - Fields:
--    *  mark required fields
--    * mark fields optional
--    * rename fields
--    * retype fields
--    * prefix enums
--  - Types:
--    * rename types
--    * replace types
-- 3. determine sharing (Share)
-- 4. create/insert request + response shapes ()
-- 5. generate unique prefixes (Prefix)
-- 6. generate shape->constraint index (Index)
-- 7. generate shape->type index (Index)

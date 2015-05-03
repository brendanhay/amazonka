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
import           Compiler.Rewrite.Acronym
import           Compiler.Rewrite.Default
import           Compiler.Rewrite.Override
import           Compiler.Rewrite.Prefix
import           Compiler.Rewrite.Subst
import           Compiler.Rewrite.TypeOf
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

-- FIXME:
-- Constraint solving
-- Add a rename step which renames the acronyms in enums/structs
-- to the correct casing.

-- substitute
-- recase
-- override
-- default
-- prefix
-- type

createLibrary :: Monad m
              => Versions
              -> Config
              -> Service Maybe Ref Shape
              -> EitherT LazyText m Library
createLibrary v x y = do
    let (x1, y1) = substitute x y
        x2       = recase x1 (y1 ^. shapes)
        y2       = override x2 y1

    y3 <- defaulted y2
    ps <- prefixes (y3 ^. shapes)
    y4 <- typed ps y3

    let ns     = NS ["Network", "AWS", y4 ^. serviceAbbrev]
        other  = x2 ^. operationImports ++ x2 ^. typeImports
        expose = ns
               : ns <> "Types"
               : ns <> "Waiters"
               : map (mappend ns . textToNS)
                     (y4 ^.. operations . ifolded . asIndex)

    return $! Library v x2 y4 ns
        (sort expose)
        (sort other)

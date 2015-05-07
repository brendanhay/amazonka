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

import           Compiler.Rewrite.Ann
import           Compiler.Rewrite.Default
import           Compiler.Rewrite.Override
import           Compiler.Rewrite.Subst
import           Compiler.Types
import           Control.Lens
import           Data.List                 (sort)
import           Data.Monoid

-- Order:
-- substitute
-- recase
-- override
-- default
-- prefix
-- type

rewrite :: Versions
        -> Config
        -> Service Maybe Ref Shape
        -> Either Error Library
rewrite v c s' = do
    s <- setDefaults (substitute (override c s'))
        >>= annotateTypes c

    let ns     = NS ["Network", "AWS", s ^. serviceAbbrev]
        other  = c ^. operationImports ++ c ^. typeImports
        expose = ns
               : ns <> "Types"
               : ns <> "Waiters"
               : map (mappend ns . textToNS)
                     (s ^.. operations . ifolded . asIndex . ctorId)

    return $! Library v c s ns (sort expose) (sort other)

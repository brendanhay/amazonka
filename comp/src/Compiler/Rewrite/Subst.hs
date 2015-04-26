{-# LANGUAGE RecordWildCards #-}

-- Module      : Compiler.Rewrite.Subst
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Subst where

import           Compiler.AST
import           Compiler.Types
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.Text           (Text)

-- FIXME:
-- input/output ref only has:
--   shape
--   documentation
--   pxmlNamespace
--   locationName

type Shapes = State (Map Text (Shape Identity))

subst :: Set Text
          -> Map Text (Operation Identity Ref)
          -> Map Text (Shape Identity)
          -> Map Text (Operation Identity Shape)
subst shared os ss = evalState (Map.traverseWithKey go os) ss
  where
    -- 1. extract the shape used as the input (or output)
    -- 2. if the shape is shared, do nothing
    -- 3. if the shape is not-shared, delete it from the shapes map
    -- 4. modify the copied shape according to the ref
    -- 5. discard the ref, and set the operation's input (or output)
    --    to the new shape.

    go :: Text -> Operation Identity Ref -> Shapes (Operation Identity Shape)
    go k o = do
        rq <- elaborate (op ^. input)
        rs <- elaborate (op ^. output)
        return $! o
            { _input  = rq
            , _output = rs
            }

    elaborate :: Ref Identity -> Shapes (Shape Identity)
    elaborate = undefined

    share = (`Set.member` shared)

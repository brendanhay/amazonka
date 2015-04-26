-- Module      : Compiler.Rewrite.Elaborate
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Elaborate where

import           Compiler.AST
import           Compiler.Types
import           Control.Lens
import           Data.Text      (Text)

elaborate :: Set Text
          -> Map Text (Operation Identity Ref)
          -> Map Text (Shape Identity)
          -> Map Text (Operation Identity Shape)
elaborate shared os ss = undefined

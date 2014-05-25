-- Module      : Network.AWS.Signing
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing
    (
    -- * Types
      Signed           (..)
    , SigningAlgorithm (..)

    -- * Algorithms
    -- , V2
    -- , V3
    , V4
    ) where

import Network.AWS.Signing.Types
import Network.AWS.Signing.V4    (V4)
import Network.AWS.Types

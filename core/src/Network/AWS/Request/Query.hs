-- Module      : Network.AWS.Request.Query
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.Query
    ( post
    ) where

import Control.Lens              hiding (Action)
import Data.Default
import Network.AWS.Data
import Network.AWS.Types
import Network.HTTP.Types.Method

post :: ToQuery a => Action -> a -> Request a
post a x = def & rqMethod .~ POST & rqQuery <>~ toQuery x & rqQuery <>~ toQuery a
{-# INLINE post #-}

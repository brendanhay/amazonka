-- Module      : Network.AWS.Request.JSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.JSON
    ( get
    , delete
    , post
    , put
    ) where

import Control.Lens
import Data.Aeson
import Data.Default
import Network.AWS.Data
import Network.AWS.Internal.Request
import Network.AWS.Types
import Network.HTTP.Types.Method

post :: ToJSON a => a -> Request a
post x = put x & rqMethod .~ POST

put :: ToJSON a => a -> Request a
put x = def & rqMethod .~ PUT & rqBody .~ toBody (toJSON x)

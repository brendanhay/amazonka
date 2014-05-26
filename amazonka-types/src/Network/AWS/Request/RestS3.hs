-- Module      : Network.AWS.Request.RestS3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.RestS3
    ( get
    , Network.AWS.Request.Lens.head
    , delete
    , post
    ) where

import Network.AWS.Data
import Network.AWS.Request.Lens
import Network.AWS.Types
import Network.HTTP.Types.Method

post :: (ToPath a, ToQuery a, ToHeaders a, ToBody b)
     => a
     -> b
     -> Context (Sg (Sv a))
post x y = get x & meth .~ POST & bdy .~ y

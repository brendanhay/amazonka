-- Module      : Network.AWS.Request.RestXML
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.RestXML
    ( get
    , delete
    , post
    , put
    ) where

import Control.Lens
import Network.AWS.Data
import Network.AWS.Request.Internal
import Network.AWS.Types
import Network.HTTP.Types.Method

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get = get'
{-# INLINE get #-}

-- head :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
-- head = head'
-- {-# INLINE head #-}

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete = delete'
{-# INLINE delete #-}

post :: (ToPath a, ToQuery a, ToHeaders a, ToXML a) => a -> Request a
post x = put x & rqMethod .~ POST
{-# INLINE post #-}

put :: (ToPath a, ToQuery a, ToHeaders a, ToXML a) => a -> Request a
put x = get x & rqMethod .~ PUT & rqBody .~ toBody (encodeXML x)
{-# INLINE put #-}

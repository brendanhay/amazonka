-- Module      : Network.AWS.Request.RestXML
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.RestXML
    ( get
    , head
    , delete
    , post
    , put
    ) where

import Control.Lens
import Network.AWS.Data
import Network.AWS.Request.Internal
import Network.AWS.Types
import Network.HTTP.Types.Method
import Prelude                      hiding (head)

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get = defaultRequest
{-# INLINE get #-}

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete x = get x & rqMethod .~ DELETE
{-# INLINE delete #-}

head :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
head x = get x & rqMethod .~ HEAD
{-# INLINE head #-}

post :: (ToPath a, ToQuery a, ToHeaders a, ToXMLRoot a) => a -> Request a
post x = put x & rqMethod .~ POST
{-# INLINE post #-}

put :: (ToPath a, ToQuery a, ToHeaders a, ToXMLRoot a) => a -> Request a
put x = get x & rqMethod .~ PUT & rqBody .~ toBody (encodeXML x)
{-# INLINE put #-}

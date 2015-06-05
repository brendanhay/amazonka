-- Module      : Network.AWS.Protocol.XML
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Protocol.XML where
    -- ( get
    -- , head
    -- , delete
    -- , post
    -- , put
    -- ) where

import           Control.Lens
import           Network.AWS.Request
import           Network.AWS.Types
import           Network.HTTP.Types.Method

-- get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
-- get = defaultRequest

-- delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
-- delete x = get x & rqMethod .~ DELETE

-- head :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
-- head x = get x & rqMethod .~ HEAD

-- postXML :: (ToPath a, ToQuery a, ToHeaders a, ToElement a) => a -> Request a
-- postXML x = putXML x & rqMethod .~ POST

putXML :: (ToPath a, ToQuery a, ToHeaders a, ToElement a) => a -> Request a
putXML x = defaultRequest x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (encodeXML x)

putBody :: (ToPath a, ToQuery a, ToHeaders a, ToBody a) => a -> Request a
putBody m x = defaultRequest x
    & rqMethod .~ m
    & rqBody   .~ toBody x
    & contentSHA256

-- Module      : Network.AWS.Request.RestJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.RestJSON
    ( post
    ) where

import Control.Lens
import Data.Aeson
import Network.AWS.Data
import Network.AWS.Request.Internal
import Network.AWS.Types

-- get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
-- get x = def
--     & rqPath    .~ Text.encodeUtf8 (toPath x)
--     & rqQuery   .~ toQuery x
--     & rqHeaders .~ toHeaders x
-- {-# INLINE get #-}

-- head :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
-- head x = get x & rqMethod .~ HEAD
-- {-# INLINE head #-}

-- delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
-- delete x = get x & rqMethod .~ DELETE
-- {-# INLINE delete #-}

-- post :: ToJSON a => a -> Request a
-- post x = put x & rqMethod .~ POST
-- {-# INLINE post #-}

post :: (ToPath a, ToQuery a, ToHeaders a, ToJSON a) => a -> Request a
post x = get' x & rqBody .~ toBody (toJSON x)
{-# INLINE post #-}

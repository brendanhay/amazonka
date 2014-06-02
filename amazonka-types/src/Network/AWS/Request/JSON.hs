{-# LANGUAGE ViewPatterns #-}

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
   ( post
   ) where

import Crypto.Hash.SHA256
import Data.Aeson
import Network.AWS.Data
import Network.AWS.Request.Lens
import Network.AWS.Types
import Network.HTTP.Client       (RequestBody(..))
import Network.HTTP.Types.Method

post :: ToJSON a => Action -> a -> Request a
post a (encode . toJSON -> x) = blank
    & rqMethod  .~ POST
    & rqQuery  <>~ toQuery a
    & rqBody    .~ RequestBodyLBS x
    & rqPayload .~ hashlazy x

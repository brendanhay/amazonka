-- Module      : Network.AWS.Data.Body
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Body where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client

class ToBody a where
    toBody :: a -> RequestBody

instance ToBody ByteString where
    toBody = RequestBodyLBS

instance ToBody Value where
    toBody = RequestBodyLBS . encode

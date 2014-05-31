-- Module      : Network.AWS.Data.Crypto
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Crypto
    (
    -- * HMAC
      hmacSHA1
    , hmacSHA256

    -- * SHA256
    , sha256
    ) where

import qualified Crypto.Hash.SHA1            as SHA1
import qualified Crypto.Hash.SHA256          as SHA256
import qualified Crypto.MAC.HMAC             as HMAC
import           Data.ByteString             (ByteString)
import           Network.AWS.Data.ByteString

hmacSHA1 :: ByteString -> ByteString -> ByteString
hmacSHA1 = HMAC.hmac SHA1.hash 64

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 = HMAC.hmac SHA256.hash 64

sha256 :: ByteString -> ByteString
sha256 = toBS . SHA256.hash

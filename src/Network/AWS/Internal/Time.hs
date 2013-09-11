-- Module      : Network.AWS.Internal.Time
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Time where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Time             (UTCTime)
import qualified Data.Time             as Time
import           System.Locale

rfc822Time, iso8601Time, awsTime, basicTime :: UTCTime -> ByteString
rfc822Time  = formatTime "%a, %d %b %Y %H:%M:%S GMT"
iso8601Time = formatTime (iso8601DateFormat $ Just "%XZ")
awsTime     = formatTime "%Y%m%dT%H%M%SZ"
basicTime   = formatTime "%Y%m%d"

formatTime :: String -> UTCTime -> ByteString
formatTime fmt = BS.pack . Time.formatTime defaultTimeLocale fmt

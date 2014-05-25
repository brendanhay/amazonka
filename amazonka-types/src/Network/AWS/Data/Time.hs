-- Module      : Network.AWS.Data.Time
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Time where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Time
import           Network.AWS.Data.ByteString
import           System.Locale

newtype AWSTime     = AWSTime     UTCTime
newtype RFC822Time  = RFC822Time  UTCTime
newtype ISO8601Time = ISO8601Time UTCTime
newtype BasicTime   = BasicTime   UTCTime

instance ToByteString AWSTime where
    toByteString (AWSTime t) = format "%Y%m%dT%H%M%SZ" t

instance ToByteString RFC822Time where
    toByteString (RFC822Time t) = format "%a, %d %b %Y %H:%M:%S GMT" t

instance ToByteString ISO8601Time where
    toByteString (ISO8601Time t) = format (iso8601DateFormat $ Just "%XZ") t

instance ToByteString BasicTime where
    toByteString (BasicTime t) = format "%Y%m%d" t

format :: String -> UTCTime -> ByteString
format fmt = BS.pack . formatTime defaultTimeLocale fmt

-- Module      : Data.Time.Formatted
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Time.Formatted where

import Data.ByteString.To
import Data.Time
import System.Locale

newtype AWSTime a = AWSTime UTCTime

instance ToByteString AWSTime where
    toByteString (AWSTime t) = format "%Y%m%dT%H%M%SZ" t

newtype RFC822Time a = RFC822Time UTCTime
    toByteString (AWSTime t) = format "%a, %d %b %Y %H:%M:%S GMT" t

newtype ISO8601Time a = ISO8601Time UTCTime
    toByteString (AWSTime t) = format (iso8601DateFormat $ Just "%XZ") t

newtype BasicTime a = BasicTime UTCTime
    toByteString (BasicTime t) = format "%Y%m%d" t

format :: String -> UTCTime -> ByteString
format = BS.pack . formatTime defaultTimeLocale

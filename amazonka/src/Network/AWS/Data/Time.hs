{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Module      : Network.AWS.Data.Time
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Time
    ( Format      (..)
    , Time        (..)
    , RFC822
    , ISO8601

    , AWSTime     (..)
    , RFC822Time  (..)
    , ISO8601Time (..)
    , BasicTime   (..)
    ) where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Time
import           Network.AWS.Data.ByteString
import           System.Locale

data Format = RFC822Format | ISO8601Format
    deriving (Eq, Show)

data Time :: Format -> * where
    Time :: UTCTime -> Time a

deriving instance Show (Time a)
deriving instance Eq   (Time a)

type RFC822  = Time RFC822Format
type ISO8601 = Time ISO8601Format

instance ToByteString RFC822 where
    toBS (Time t) = toBS (RFC822Time defaultTimeLocale t)

instance ToByteString ISO8601 where
    toBS (Time t) = toBS (ISO8601Time defaultTimeLocale t)

data AWSTime     = AWSTime     TimeLocale UTCTime
data RFC822Time  = RFC822Time  TimeLocale UTCTime
data ISO8601Time = ISO8601Time TimeLocale UTCTime
data BasicTime   = BasicTime   TimeLocale UTCTime

instance ToByteString AWSTime where
    toBS (AWSTime l t) = format l "%Y%m%dT%H%M%SZ" t

instance ToByteString RFC822Time where
    toBS (RFC822Time l t) = format l "%a, %d %b %Y %H:%M:%S GMT" t

instance ToByteString ISO8601Time where
    toBS (ISO8601Time l t) = format l (iso8601DateFormat $ Just "%XZ") t

instance ToByteString BasicTime where
    toBS (BasicTime l t) = format l "%Y%m%d" t

format :: TimeLocale -> String -> UTCTime -> ByteString
format l f = BS.pack . formatTime l f

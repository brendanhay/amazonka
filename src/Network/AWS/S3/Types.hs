{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Text            (Text)
import Data.Time
import Network.AWS.Internal

s3Service :: Service
s3Service = Service "s3" s3Version SigningVersion2 $ const "s3.amazonaws.com"

-- | Currently supported version (2006-03-01) of the S3 service.
s3Version :: ServiceVersion
s3Version = "2006-03-01"

-- | XML namespace to annotate S3 elements with.
s3NS :: ByteString
s3NS = "http://s3.amazonaws.com/doc/" <> toBS s3Version <> "/"

-- | Helper to define S3 namespaced XML elements.
s3Elem :: ByteString -> NName ByteString
s3Elem = mkNName s3NS

data S3ErrorResponse = S3ErrorResponse { sssError :: !Text }
    deriving (Eq, Show, Generic)

instance ToError S3ErrorResponse where
    toError = Error . show

instance IsXML S3ErrorResponse

--
-- Service
--

data Owner = Owner
    { oDisplayName :: !Text
      -- ^ Bucket owner's display name.
    , oID          :: !Text
      -- ^ Bucket owner's user ID.
    } deriving (Eq, Show, Generic)

instance IsXML Owner where
    xmlPickler = withNS s3NS

data Bucket = Bucket
    { bName         :: !Text
      -- ^ Bucket's name.
    , bCreationDate :: !UTCTime
      -- ^ Date the bucket was created.
    } deriving (Eq, Show, Generic)

instance IsXML Bucket where
    xmlPickler = withNS s3NS

--
-- Buckets
--

--
-- Objects
--

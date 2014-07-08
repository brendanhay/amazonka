{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListObjectVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns metadata about all of the versions of objects in a bucket.
module Network.AWS.S3.V2006_03_01.ListObjectVersions where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable ListObjectVersions request.
listObjectVersions :: BucketName -- ^ 'lovrBucket'
                   -> ListObjectVersions
listObjectVersions p1 = ListObjectVersions
    { lovrBucket = p1
    , lovrDelimiter = Nothing
    , lovrEncodingType = Nothing
    , lovrKeyMarker = Nothing
    , lovrMaxKeys = Nothing
    , lovrPrefix = Nothing
    , lovrVersionIdMarker = Nothing
    }

data ListObjectVersions = ListObjectVersions
    { lovrBucket :: BucketName
    , lovrDelimiter :: Maybe Text
      -- ^ A delimiter is a character you use to group keys.
    , lovrEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and
      -- specifies the encoding method to use. An object key may contain
      -- any Unicode character; however, XML 1.0 parser cannot parse some
      -- characters, such as characters with an ASCII value from 0 to 10.
      -- For characters that are not supported in XML 1.0, you can add
      -- this parameter to request that Amazon S3 encode the keys in the
      -- response.
    , lovrKeyMarker :: Maybe Text
      -- ^ Specifies the key to start with when listing objects in a bucket.
    , lovrMaxKeys :: Maybe Integer
      -- ^ Sets the maximum number of keys returned in the response. The
      -- response might contain fewer keys but will never contain more.
    , lovrPrefix :: Maybe Text
      -- ^ Limits the response to keys that begin with the specified prefix.
    , lovrVersionIdMarker :: Maybe Text
      -- ^ Specifies the object version you want to start listing from.
    } deriving (Eq, Show, Generic)

instance ToPath ListObjectVersions where
    toPath ListObjectVersions{..} = mconcat
        [ "/"
        , toBS lovrBucket
        ]

instance ToQuery ListObjectVersions

instance ToHeaders ListObjectVersions

instance ToBody ListObjectVersions

instance AWSRequest ListObjectVersions where
    type Sv ListObjectVersions = S3

    request  = get
    response = response' $

instance AWSPager ListObjectVersions where
    next rq rs
        | not (lovoIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { lovrKeyMarker = lovoNextKeyMarker rs
            , lovrVersionIdMarker = lovoNextVersionIdMarker rs
            }

data instance Rs ListObjectVersions = ListObjectVersionsResponse
    { lovoIsTruncated :: Bool
      -- ^ A flag that indicates whether or not Amazon S3 returned all of
      -- the results that satisfied the search criteria. If your results
      -- were truncated, you can make a follow-up paginated request using
      -- the NextKeyMarker and NextVersionIdMarker response parameters as
      -- a starting place in another request to return the rest of the
      -- results.
    , lovoName :: Maybe BucketName
    , lovoCommonPrefixes :: [CommonPrefix]
    , lovoDeleteMarkers :: [DeleteMarkerEntry]
    , lovoEncodingType :: Maybe EncodingType
      -- ^ Encoding type used by Amazon S3 to encode object keys in the
      -- response.
    , lovoKeyMarker :: Maybe Text
      -- ^ Marks the last Key returned in a truncated response.
    , lovoMaxKeys :: Maybe Integer
    , lovoNextKeyMarker :: Maybe Text
      -- ^ Use this value for the key marker request parameter in a
      -- subsequent request.
    , lovoNextVersionIdMarker :: Maybe Text
      -- ^ Use this value for the next version id marker parameter in a
      -- subsequent request.
    , lovoVersions :: [ObjectVersion]
    , lovoPrefix :: Maybe Text
    , lovoVersionIdMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

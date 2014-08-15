{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type GetBucketObjectVersions = ListObjectVersions

-- | Minimum specification for a 'ListObjectVersions' request.
listObjectVersions :: BucketName -- ^ '_lovrBucket'
                   -> ListObjectVersions
listObjectVersions p1 = ListObjectVersions
    { _lovrBucket = p1
    , _lovrDelimiter = Nothing
    , _lovrEncodingType = Nothing
    , _lovrKeyMarker = Nothing
    , _lovrMaxKeys = Nothing
    , _lovrPrefix = Nothing
    , _lovrVersionIdMarker = Nothing
    }

data ListObjectVersions = ListObjectVersions
    { _lovrBucket :: BucketName
    , _lovrDelimiter :: Maybe Char
      -- ^ A delimiter is a character you use to group keys.
    , _lovrEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and
      -- specifies the encoding method to use. An object key may contain
      -- any Unicode character; however, XML 1.0 parser cannot parse some
      -- characters, such as characters with an ASCII value from 0 to 10.
      -- For characters that are not supported in XML 1.0, you can add
      -- this parameter to request that Amazon S3 encode the keys in the
      -- response.
    , _lovrKeyMarker :: Maybe Text
      -- ^ Specifies the key to start with when listing objects in a bucket.
    , _lovrMaxKeys :: Maybe Integer
      -- ^ Sets the maximum number of keys returned in the response. The
      -- response might contain fewer keys but will never contain more.
    , _lovrPrefix :: Maybe Text
      -- ^ Limits the response to keys that begin with the specified prefix.
    , _lovrVersionIdMarker :: Maybe Text
      -- ^ Specifies the object version you want to start listing from.
    } deriving (Show, Generic)

makeLenses ''ListObjectVersions

instance ToPath ListObjectVersions where
    toPath ListObjectVersions{..} = mconcat
        [ "/"
        , toBS _lovrBucket
        ]

instance ToQuery ListObjectVersions where
    toQuery ListObjectVersions{..} = mconcat
        [ "encoding-type" =? _lovrEncodingType
        , "key-marker" =? _lovrKeyMarker
        , "max-keys" =? _lovrMaxKeys
        , "prefix" =? _lovrPrefix
        , "version-id-marker" =? _lovrVersionIdMarker
        , "versions&delimiter" =? _lovrDelimiter
        ]

instance ToHeaders ListObjectVersions

instance ToBody ListObjectVersions

data ListObjectVersionsResponse = ListObjectVersionsResponse
    { _lovoIsTruncated :: Bool
      -- ^ A flag that indicates whether or not Amazon S3 returned all of
      -- the results that satisfied the search criteria. If your results
      -- were truncated, you can make a follow-up paginated request using
      -- the NextKeyMarker and NextVersionIdMarker response parameters as
      -- a starting place in another request to return the rest of the
      -- results.
    , _lovoName :: Maybe BucketName
    , _lovoCommonPrefixes :: [CommonPrefix]
    , _lovoDeleteMarkers :: [DeleteMarkerEntry]
    , _lovoEncodingType :: Maybe EncodingType
      -- ^ Encoding type used by Amazon S3 to encode object keys in the
      -- response.
    , _lovoKeyMarker :: Maybe Text
      -- ^ Marks the last Key returned in a truncated response.
    , _lovoMaxKeys :: Maybe Integer
    , _lovoNextKeyMarker :: Maybe Text
      -- ^ Use this value for the key marker request parameter in a
      -- subsequent request.
    , _lovoNextVersionIdMarker :: Maybe Text
      -- ^ Use this value for the next version id marker parameter in a
      -- subsequent request.
    , _lovoVersions :: [ObjectVersion]
    , _lovoPrefix :: Maybe Text
    , _lovoVersionIdMarker :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''ListObjectVersionsResponse

instance FromXML ListObjectVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListObjectVersions where
    type Sv ListObjectVersions = S3
    type Rs ListObjectVersions = ListObjectVersionsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListObjectVersions where
    next rq rs
        | not (_lovoIsTruncated rs) = Nothing
        | and [isNothing p1, isNothing p2] = Nothing
        | otherwise = Just $ rq
            { _lovrKeyMarker = p1
            , _lovrVersionIdMarker = p2
            }
      where
        p1 = _lovoNextKeyMarker rs
        p2 = _lovoNextVersionIdMarker rs

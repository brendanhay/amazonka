{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.ListObjectVersions
    (
    -- * Request
      ListObjectVersions
    -- ** Request alias
    , GetBucketObjectVersions
    -- ** Request constructor
    , mkListObjectVersions
    -- ** Request lenses
    , lovBucket
    , lovDelimiter
    , lovEncodingType
    , lovKeyMarker
    , lovMaxKeys
    , lovPrefix
    , lovVersionIdMarker

    -- * Response
    , ListObjectVersionsResponse
    -- ** Response lenses
    , lovrsIsTruncated
    , lovrsKeyMarker
    , lovrsVersionIdMarker
    , lovrsNextKeyMarker
    , lovrsNextVersionIdMarker
    , lovrsVersions
    , lovrsDeleteMarkers
    , lovrsName
    , lovrsPrefix
    , lovrsMaxKeys
    , lovrsCommonPrefixes
    , lovrsEncodingType
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type GetBucketObjectVersions = ListObjectVersions

data ListObjectVersions = ListObjectVersions
    { _lovBucket :: BucketName
    , _lovDelimiter :: Maybe Char
    , _lovEncodingType :: Maybe EncodingType
    , _lovKeyMarker :: Maybe Text
    , _lovMaxKeys :: Maybe Integer
    , _lovPrefix :: Maybe Text
    , _lovVersionIdMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListObjectVersions' request.
mkListObjectVersions :: BucketName -- ^ 'lovBucket'
                     -> ListObjectVersions
mkListObjectVersions p1 = ListObjectVersions
    { _lovBucket = p1
    , _lovDelimiter = Nothing
    , _lovEncodingType = Nothing
    , _lovKeyMarker = Nothing
    , _lovMaxKeys = Nothing
    , _lovPrefix = Nothing
    , _lovVersionIdMarker = Nothing
    }

lovBucket :: Lens' ListObjectVersions BucketName
lovBucket = lens _lovBucket (\s a -> s { _lovBucket = a })

-- | A delimiter is a character you use to group keys.
lovDelimiter :: Lens' ListObjectVersions (Maybe Char)
lovDelimiter = lens _lovDelimiter (\s a -> s { _lovDelimiter = a })

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
lovEncodingType :: Lens' ListObjectVersions (Maybe EncodingType)
lovEncodingType = lens _lovEncodingType (\s a -> s { _lovEncodingType = a })

-- | Specifies the key to start with when listing objects in a bucket.
lovKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lovKeyMarker = lens _lovKeyMarker (\s a -> s { _lovKeyMarker = a })

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lovMaxKeys :: Lens' ListObjectVersions (Maybe Integer)
lovMaxKeys = lens _lovMaxKeys (\s a -> s { _lovMaxKeys = a })

-- | Limits the response to keys that begin with the specified prefix.
lovPrefix :: Lens' ListObjectVersions (Maybe Text)
lovPrefix = lens _lovPrefix (\s a -> s { _lovPrefix = a })

-- | Specifies the object version you want to start listing from.
lovVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lovVersionIdMarker =
    lens _lovVersionIdMarker (\s a -> s { _lovVersionIdMarker = a })

instance ToPath ListObjectVersions where
    toPath ListObjectVersions{..} = mconcat
        [ "/"
        , toBS _lovBucket
        ]

instance ToQuery ListObjectVersions where
    toQuery ListObjectVersions{..} = mconcat
        [ "encoding-type" =? _lovEncodingType
        , "key-marker" =? _lovKeyMarker
        , "max-keys" =? _lovMaxKeys
        , "prefix" =? _lovPrefix
        , "version-id-marker" =? _lovVersionIdMarker
        , "versions&delimiter" =? _lovDelimiter
        ]

instance ToHeaders ListObjectVersions

instance ToBody ListObjectVersions

data ListObjectVersionsResponse = ListObjectVersionsResponse
    { _lovrsIsTruncated :: Bool
    , _lovrsKeyMarker :: Maybe Text
    , _lovrsVersionIdMarker :: Maybe Text
    , _lovrsNextKeyMarker :: Maybe Text
    , _lovrsNextVersionIdMarker :: Maybe Text
    , _lovrsVersions :: [ObjectVersion]
    , _lovrsDeleteMarkers :: [DeleteMarkerEntry]
    , _lovrsName :: Maybe BucketName
    , _lovrsPrefix :: Maybe Text
    , _lovrsMaxKeys :: Maybe Integer
    , _lovrsCommonPrefixes :: [CommonPrefix]
    , _lovrsEncodingType :: Maybe EncodingType
    } deriving (Show, Generic)

-- | A flag that indicates whether or not Amazon S3 returned all of the results
-- that satisfied the search criteria. If your results were truncated, you can
-- make a follow-up paginated request using the NextKeyMarker and
-- NextVersionIdMarker response parameters as a starting place in another
-- request to return the rest of the results.
lovrsIsTruncated :: Lens' ListObjectVersionsResponse Bool
lovrsIsTruncated =
    lens _lovrsIsTruncated (\s a -> s { _lovrsIsTruncated = a })

-- | Marks the last Key returned in a truncated response.
lovrsKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsKeyMarker = lens _lovrsKeyMarker (\s a -> s { _lovrsKeyMarker = a })

lovrsVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsVersionIdMarker =
    lens _lovrsVersionIdMarker (\s a -> s { _lovrsVersionIdMarker = a })

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovrsNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsNextKeyMarker =
    lens _lovrsNextKeyMarker (\s a -> s { _lovrsNextKeyMarker = a })

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovrsNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsNextVersionIdMarker =
    lens _lovrsNextVersionIdMarker
         (\s a -> s { _lovrsNextVersionIdMarker = a })

lovrsVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lovrsVersions = lens _lovrsVersions (\s a -> s { _lovrsVersions = a })

lovrsDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lovrsDeleteMarkers =
    lens _lovrsDeleteMarkers (\s a -> s { _lovrsDeleteMarkers = a })

lovrsName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lovrsName = lens _lovrsName (\s a -> s { _lovrsName = a })

lovrsPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsPrefix = lens _lovrsPrefix (\s a -> s { _lovrsPrefix = a })

lovrsMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Integer)
lovrsMaxKeys = lens _lovrsMaxKeys (\s a -> s { _lovrsMaxKeys = a })

lovrsCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lovrsCommonPrefixes =
    lens _lovrsCommonPrefixes (\s a -> s { _lovrsCommonPrefixes = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovrsEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lovrsEncodingType =
    lens _lovrsEncodingType (\s a -> s { _lovrsEncodingType = a })

instance FromXML ListObjectVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListObjectVersions where
    type Sv ListObjectVersions = S3
    type Rs ListObjectVersions = ListObjectVersionsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListObjectVersions where
    next rq rs
        | not (rs ^. lovrsIsTruncated) = Nothing
        | isNothing p1 && isNothing p2 = Nothing
        | otherwise = Just $ rq
            & lovKeyMarker .~ p1
            & lovVersionIdMarker .~ p2
      where
        p1 = rs ^. lovrsNextKeyMarker
        p2 = rs ^. lovrsNextVersionIdMarker

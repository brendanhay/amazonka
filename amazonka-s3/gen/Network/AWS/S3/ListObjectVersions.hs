{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns metadata about all of the versions of objects in a bucket.
module Network.AWS.S3.ListObjectVersions
    (
    -- * Request
      ListObjectVersions
    -- ** Request alias
    , GetBucketObjectVersions
    -- ** Request constructor
    , listObjectVersions
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
    -- ** Response constructor
    , listObjectVersionsResponse
    -- ** Response lenses
    , lovrIsTruncated
    , lovrKeyMarker
    , lovrVersionIdMarker
    , lovrNextKeyMarker
    , lovrNextVersionIdMarker
    , lovrVersions
    , lovrDeleteMarkers
    , lovrName
    , lovrPrefix
    , lovrMaxKeys
    , lovrCommonPrefixes
    , lovrEncodingType
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListObjectVersions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Delimiter ::@ @Maybe Char@
--
-- * @EncodingType ::@ @Maybe EncodingType@
--
-- * @KeyMarker ::@ @Maybe Text@
--
-- * @MaxKeys ::@ @Maybe Integer@
--
-- * @Prefix ::@ @Maybe Text@
--
-- * @VersionIdMarker ::@ @Maybe Text@
--
listObjectVersions :: BucketName -- ^ 'lovBucket'
                   -> ListObjectVersions
listObjectVersions p1 = ListObjectVersions
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

instance ToPath ListObjectVersions

instance ToQuery ListObjectVersions

instance ToHeaders ListObjectVersions

instance ToBody ListObjectVersions

data ListObjectVersionsResponse = ListObjectVersionsResponse
    { _lovrIsTruncated :: !Bool
    , _lovrKeyMarker :: Maybe Text
    , _lovrVersionIdMarker :: Maybe Text
    , _lovrNextKeyMarker :: Maybe Text
    , _lovrNextVersionIdMarker :: Maybe Text
    , _lovrVersions :: [ObjectVersion]
    , _lovrDeleteMarkers :: [DeleteMarkerEntry]
    , _lovrName :: Maybe BucketName
    , _lovrPrefix :: Maybe Text
    , _lovrMaxKeys :: Maybe Integer
    , _lovrCommonPrefixes :: [CommonPrefix]
    , _lovrEncodingType :: Maybe EncodingType
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListObjectVersionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IsTruncated ::@ @Bool@
--
-- * @KeyMarker ::@ @Maybe Text@
--
-- * @VersionIdMarker ::@ @Maybe Text@
--
-- * @NextKeyMarker ::@ @Maybe Text@
--
-- * @NextVersionIdMarker ::@ @Maybe Text@
--
-- * @Versions ::@ @[ObjectVersion]@
--
-- * @DeleteMarkers ::@ @[DeleteMarkerEntry]@
--
-- * @Name ::@ @Maybe BucketName@
--
-- * @Prefix ::@ @Maybe Text@
--
-- * @MaxKeys ::@ @Maybe Integer@
--
-- * @CommonPrefixes ::@ @[CommonPrefix]@
--
-- * @EncodingType ::@ @Maybe EncodingType@
--
listObjectVersionsResponse :: Bool -- ^ 'lovrIsTruncated'
                           -> ListObjectVersionsResponse
listObjectVersionsResponse p1 = ListObjectVersionsResponse
    { _lovrIsTruncated = p1
    , _lovrKeyMarker = Nothing
    , _lovrVersionIdMarker = Nothing
    , _lovrNextKeyMarker = Nothing
    , _lovrNextVersionIdMarker = Nothing
    , _lovrVersions = mempty
    , _lovrDeleteMarkers = mempty
    , _lovrName = Nothing
    , _lovrPrefix = Nothing
    , _lovrMaxKeys = Nothing
    , _lovrCommonPrefixes = mempty
    , _lovrEncodingType = Nothing
    }

-- | A flag that indicates whether or not Amazon S3 returned all of the results
-- that satisfied the search criteria. If your results were truncated, you can
-- make a follow-up paginated request using the NextKeyMarker and
-- NextVersionIdMarker response parameters as a starting place in another
-- request to return the rest of the results.
lovrIsTruncated :: Lens' ListObjectVersionsResponse Bool
lovrIsTruncated = lens _lovrIsTruncated (\s a -> s { _lovrIsTruncated = a })

-- | Marks the last Key returned in a truncated response.
lovrKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrKeyMarker = lens _lovrKeyMarker (\s a -> s { _lovrKeyMarker = a })

lovrVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrVersionIdMarker =
    lens _lovrVersionIdMarker (\s a -> s { _lovrVersionIdMarker = a })

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovrNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrNextKeyMarker =
    lens _lovrNextKeyMarker (\s a -> s { _lovrNextKeyMarker = a })

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovrNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrNextVersionIdMarker =
    lens _lovrNextVersionIdMarker
         (\s a -> s { _lovrNextVersionIdMarker = a })

lovrVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lovrVersions = lens _lovrVersions (\s a -> s { _lovrVersions = a })

lovrDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lovrDeleteMarkers =
    lens _lovrDeleteMarkers (\s a -> s { _lovrDeleteMarkers = a })

lovrName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lovrName = lens _lovrName (\s a -> s { _lovrName = a })

lovrPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrPrefix = lens _lovrPrefix (\s a -> s { _lovrPrefix = a })

lovrMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Integer)
lovrMaxKeys = lens _lovrMaxKeys (\s a -> s { _lovrMaxKeys = a })

lovrCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lovrCommonPrefixes =
    lens _lovrCommonPrefixes (\s a -> s { _lovrCommonPrefixes = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovrEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lovrEncodingType =
    lens _lovrEncodingType (\s a -> s { _lovrEncodingType = a })

instance FromXML ListObjectVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListObjectVersions where
    type Sv ListObjectVersions = S3
    type Rs ListObjectVersions = ListObjectVersionsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListObjectVersions where
    next rq rs
        | not (rs ^. lovrIsTruncated) = Nothing
        | isNothing p1 && isNothing p2 = Nothing
        | otherwise = Just $ rq
            & lovKeyMarker .~ p1
            & lovVersionIdMarker .~ p2
      where
        p1 = rs ^. lovrNextKeyMarker
        p2 = rs ^. lovrNextVersionIdMarker

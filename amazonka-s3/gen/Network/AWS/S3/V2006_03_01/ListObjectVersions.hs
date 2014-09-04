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
    , mkListObjectVersionsRequest
    -- ** Request lenses
    , lovrBucket
    , lovrDelimiter
    , lovrEncodingType
    , lovrKeyMarker
    , lovrMaxKeys
    , lovrPrefix
    , lovrVersionIdMarker

    -- * Response
    , ListObjectVersionsResponse
    -- ** Response lenses
    , lovoIsTruncated
    , lovoKeyMarker
    , lovoVersionIdMarker
    , lovoNextKeyMarker
    , lovoNextVersionIdMarker
    , lovoVersions
    , lovoDeleteMarkers
    , lovoName
    , lovoPrefix
    , lovoMaxKeys
    , lovoCommonPrefixes
    , lovoEncodingType
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type GetBucketObjectVersions = ListObjectVersions

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListObjectVersions' request.
mkListObjectVersionsRequest :: BucketName -- ^ 'lovrBucket'
                            -> ListObjectVersions
mkListObjectVersionsRequest p1 = ListObjectVersions
    { _lovrBucket = p1
    , _lovrDelimiter = Nothing
    , _lovrEncodingType = Nothing
    , _lovrKeyMarker = Nothing
    , _lovrMaxKeys = Nothing
    , _lovrPrefix = Nothing
    , _lovrVersionIdMarker = Nothing
    }
{-# INLINE mkListObjectVersionsRequest #-}

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

lovrBucket :: Lens' ListObjectVersions (BucketName)
lovrBucket = lens _lovrBucket (\s a -> s { _lovrBucket = a })
{-# INLINE lovrBucket #-}

-- | A delimiter is a character you use to group keys.
lovrDelimiter :: Lens' ListObjectVersions (Maybe Char)
lovrDelimiter = lens _lovrDelimiter (\s a -> s { _lovrDelimiter = a })
{-# INLINE lovrDelimiter #-}

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
lovrEncodingType :: Lens' ListObjectVersions (Maybe EncodingType)
lovrEncodingType = lens _lovrEncodingType (\s a -> s { _lovrEncodingType = a })
{-# INLINE lovrEncodingType #-}

-- | Specifies the key to start with when listing objects in a bucket.
lovrKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lovrKeyMarker = lens _lovrKeyMarker (\s a -> s { _lovrKeyMarker = a })
{-# INLINE lovrKeyMarker #-}

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lovrMaxKeys :: Lens' ListObjectVersions (Maybe Integer)
lovrMaxKeys = lens _lovrMaxKeys (\s a -> s { _lovrMaxKeys = a })
{-# INLINE lovrMaxKeys #-}

-- | Limits the response to keys that begin with the specified prefix.
lovrPrefix :: Lens' ListObjectVersions (Maybe Text)
lovrPrefix = lens _lovrPrefix (\s a -> s { _lovrPrefix = a })
{-# INLINE lovrPrefix #-}

-- | Specifies the object version you want to start listing from.
lovrVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lovrVersionIdMarker = lens _lovrVersionIdMarker (\s a -> s { _lovrVersionIdMarker = a })
{-# INLINE lovrVersionIdMarker #-}

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
    , _lovoKeyMarker :: Maybe Text
      -- ^ Marks the last Key returned in a truncated response.
    , _lovoVersionIdMarker :: Maybe Text
    , _lovoNextKeyMarker :: Maybe Text
      -- ^ Use this value for the key marker request parameter in a
      -- subsequent request.
    , _lovoNextVersionIdMarker :: Maybe Text
      -- ^ Use this value for the next version id marker parameter in a
      -- subsequent request.
    , _lovoVersions :: [ObjectVersion]
    , _lovoDeleteMarkers :: [DeleteMarkerEntry]
    , _lovoName :: Maybe BucketName
    , _lovoPrefix :: Maybe Text
    , _lovoMaxKeys :: Maybe Integer
    , _lovoCommonPrefixes :: [CommonPrefix]
    , _lovoEncodingType :: Maybe EncodingType
      -- ^ Encoding type used by Amazon S3 to encode object keys in the
      -- response.
    } deriving (Show, Generic)

-- | A flag that indicates whether or not Amazon S3 returned all of the results
-- that satisfied the search criteria. If your results were truncated, you can
-- make a follow-up paginated request using the NextKeyMarker and
-- NextVersionIdMarker response parameters as a starting place in another
-- request to return the rest of the results.
lovoIsTruncated :: Lens' ListObjectVersionsResponse (Bool)
lovoIsTruncated = lens _lovoIsTruncated (\s a -> s { _lovoIsTruncated = a })
{-# INLINE lovoIsTruncated #-}

-- | Marks the last Key returned in a truncated response.
lovoKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovoKeyMarker = lens _lovoKeyMarker (\s a -> s { _lovoKeyMarker = a })
{-# INLINE lovoKeyMarker #-}

lovoVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovoVersionIdMarker = lens _lovoVersionIdMarker (\s a -> s { _lovoVersionIdMarker = a })
{-# INLINE lovoVersionIdMarker #-}

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovoNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovoNextKeyMarker = lens _lovoNextKeyMarker (\s a -> s { _lovoNextKeyMarker = a })
{-# INLINE lovoNextKeyMarker #-}

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovoNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovoNextVersionIdMarker = lens _lovoNextVersionIdMarker (\s a -> s { _lovoNextVersionIdMarker = a })
{-# INLINE lovoNextVersionIdMarker #-}

lovoVersions :: Lens' ListObjectVersionsResponse ([ObjectVersion])
lovoVersions = lens _lovoVersions (\s a -> s { _lovoVersions = a })
{-# INLINE lovoVersions #-}

lovoDeleteMarkers :: Lens' ListObjectVersionsResponse ([DeleteMarkerEntry])
lovoDeleteMarkers = lens _lovoDeleteMarkers (\s a -> s { _lovoDeleteMarkers = a })
{-# INLINE lovoDeleteMarkers #-}

lovoName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lovoName = lens _lovoName (\s a -> s { _lovoName = a })
{-# INLINE lovoName #-}

lovoPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lovoPrefix = lens _lovoPrefix (\s a -> s { _lovoPrefix = a })
{-# INLINE lovoPrefix #-}

lovoMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Integer)
lovoMaxKeys = lens _lovoMaxKeys (\s a -> s { _lovoMaxKeys = a })
{-# INLINE lovoMaxKeys #-}

lovoCommonPrefixes :: Lens' ListObjectVersionsResponse ([CommonPrefix])
lovoCommonPrefixes = lens _lovoCommonPrefixes (\s a -> s { _lovoCommonPrefixes = a })
{-# INLINE lovoCommonPrefixes #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovoEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lovoEncodingType = lens _lovoEncodingType (\s a -> s { _lovoEncodingType = a })
{-# INLINE lovoEncodingType #-}

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

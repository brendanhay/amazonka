{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns some or all (up to 1000) of the objects in a bucket. You can use
-- the request parameters as selection criteria to return a subset of the
-- objects in a bucket.
module Network.AWS.S3.ListObjects
    (
    -- * Request
      ListObjects
    -- ** Request alias
    , GetBucket
    -- ** Request constructor
    , listObjects
    -- ** Request lenses
    , loBucket
    , loDelimiter
    , loEncodingType
    , loMarker
    , loMaxKeys
    , loPrefix

    -- * Response
    , ListObjectsResponse
    -- ** Response constructor
    , listObjectsResponse
    -- ** Response lenses
    , lorIsTruncated
    , lorMarker
    , lorNextMarker
    , lorContents
    , lorName
    , lorPrefix
    , lorMaxKeys
    , lorCommonPrefixes
    , lorEncodingType
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type GetBucket = ListObjects

data ListObjects = ListObjects
    { _loBucket :: BucketName
    , _loDelimiter :: Maybe Char
    , _loEncodingType :: Maybe EncodingType
    , _loMarker :: Maybe Text
    , _loMaxKeys :: Maybe Integer
    , _loPrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListObjects' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Delimiter ::@ @Maybe Char@
--
-- * @EncodingType ::@ @Maybe EncodingType@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxKeys ::@ @Maybe Integer@
--
-- * @Prefix ::@ @Maybe Text@
--
listObjects :: BucketName -- ^ 'loBucket'
            -> ListObjects
listObjects p1 = ListObjects
    { _loBucket = p1
    , _loDelimiter = Nothing
    , _loEncodingType = Nothing
    , _loMarker = Nothing
    , _loMaxKeys = Nothing
    , _loPrefix = Nothing
    }

loBucket :: Lens' ListObjects BucketName
loBucket = lens _loBucket (\s a -> s { _loBucket = a })

-- | A delimiter is a character you use to group keys.
loDelimiter :: Lens' ListObjects (Maybe Char)
loDelimiter = lens _loDelimiter (\s a -> s { _loDelimiter = a })

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
loEncodingType :: Lens' ListObjects (Maybe EncodingType)
loEncodingType = lens _loEncodingType (\s a -> s { _loEncodingType = a })

-- | Specifies the key to start with when listing objects in a bucket.
loMarker :: Lens' ListObjects (Maybe Text)
loMarker = lens _loMarker (\s a -> s { _loMarker = a })

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
loMaxKeys :: Lens' ListObjects (Maybe Integer)
loMaxKeys = lens _loMaxKeys (\s a -> s { _loMaxKeys = a })

-- | Limits the response to keys that begin with the specified prefix.
loPrefix :: Lens' ListObjects (Maybe Text)
loPrefix = lens _loPrefix (\s a -> s { _loPrefix = a })

instance ToPath ListObjects

instance ToQuery ListObjects

instance ToHeaders ListObjects

instance ToBody ListObjects

data ListObjectsResponse = ListObjectsResponse
    { _lorIsTruncated :: !Bool
    , _lorMarker :: Maybe Text
    , _lorNextMarker :: Maybe Text
    , _lorContents :: [Object]
    , _lorName :: BucketName
    , _lorPrefix :: Maybe Text
    , _lorMaxKeys :: Maybe Integer
    , _lorCommonPrefixes :: [CommonPrefix]
    , _lorEncodingType :: Maybe EncodingType
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListObjectsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @NextMarker ::@ @Maybe Text@
--
-- * @Contents ::@ @[Object]@
--
-- * @Name ::@ @BucketName@
--
-- * @Prefix ::@ @Maybe Text@
--
-- * @MaxKeys ::@ @Maybe Integer@
--
-- * @CommonPrefixes ::@ @[CommonPrefix]@
--
-- * @EncodingType ::@ @Maybe EncodingType@
--
listObjectsResponse :: Bool -- ^ 'lorIsTruncated'
                    -> BucketName -- ^ 'lorName'
                    -> ListObjectsResponse
listObjectsResponse p1 p5 = ListObjectsResponse
    { _lorIsTruncated = p1
    , _lorMarker = Nothing
    , _lorNextMarker = Nothing
    , _lorContents = mempty
    , _lorName = p5
    , _lorPrefix = Nothing
    , _lorMaxKeys = Nothing
    , _lorCommonPrefixes = mempty
    , _lorEncodingType = Nothing
    }

-- | A flag that indicates whether or not Amazon S3 returned all of the results
-- that satisfied the search criteria.
lorIsTruncated :: Lens' ListObjectsResponse Bool
lorIsTruncated = lens _lorIsTruncated (\s a -> s { _lorIsTruncated = a })

lorMarker :: Lens' ListObjectsResponse (Maybe Text)
lorMarker = lens _lorMarker (\s a -> s { _lorMarker = a })

-- | When response is truncated (the IsTruncated element value in the response
-- is true), you can use the key name in this field as marker in the
-- subsequent request to get next set of objects. Amazon S3 lists objects in
-- alphabetical order Note: This element is returned only if you have
-- delimiter request parameter specified. If response does not include the
-- NextMaker and it is truncated, you can use the value of the last Key in the
-- response as the marker in the subsequent request to get the next set of
-- object keys.
lorNextMarker :: Lens' ListObjectsResponse (Maybe Text)
lorNextMarker = lens _lorNextMarker (\s a -> s { _lorNextMarker = a })

lorContents :: Lens' ListObjectsResponse [Object]
lorContents = lens _lorContents (\s a -> s { _lorContents = a })

lorName :: Lens' ListObjectsResponse BucketName
lorName = lens _lorName (\s a -> s { _lorName = a })

lorPrefix :: Lens' ListObjectsResponse (Maybe Text)
lorPrefix = lens _lorPrefix (\s a -> s { _lorPrefix = a })

lorMaxKeys :: Lens' ListObjectsResponse (Maybe Integer)
lorMaxKeys = lens _lorMaxKeys (\s a -> s { _lorMaxKeys = a })

lorCommonPrefixes :: Lens' ListObjectsResponse [CommonPrefix]
lorCommonPrefixes =
    lens _lorCommonPrefixes (\s a -> s { _lorCommonPrefixes = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lorEncodingType :: Lens' ListObjectsResponse (Maybe EncodingType)
lorEncodingType = lens _lorEncodingType (\s a -> s { _lorEncodingType = a })

instance FromXML ListObjectsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListObjects where
    next rq rs
        | not (rs ^. lorIsTruncated) = Nothing
        | otherwise = Just $
            rq & loMarker .~ rs ^. lorNextMarker

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListObjects
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
module Network.AWS.S3.V2006_03_01.ListObjects
    (
    -- * Request
      ListObjects
    -- ** Request alias
    , GetBucket
    -- ** Request constructor
    , mkListObjects
    -- ** Request lenses
    , loBucket
    , loDelimiter
    , loEncodingType
    , loMarker
    , loMaxKeys
    , loPrefix

    -- * Response
    , ListObjectsResponse
    -- ** Response lenses
    , lorsIsTruncated
    , lorsMarker
    , lorsNextMarker
    , lorsContents
    , lorsName
    , lorsPrefix
    , lorsMaxKeys
    , lorsCommonPrefixes
    , lorsEncodingType
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListObjects' request.
mkListObjects :: BucketName -- ^ 'loBucket'
              -> ListObjects
mkListObjects p1 = ListObjects
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

instance ToPath ListObjects where
    toPath ListObjects{..} = mconcat
        [ "/"
        , toBS _loBucket
        ]

instance ToQuery ListObjects where
    toQuery ListObjects{..} = mconcat
        [ "delimiter" =? _loDelimiter
        , "encoding-type" =? _loEncodingType
        , "marker" =? _loMarker
        , "max-keys" =? _loMaxKeys
        , "prefix" =? _loPrefix
        ]

instance ToHeaders ListObjects

instance ToBody ListObjects

data ListObjectsResponse = ListObjectsResponse
    { _lorsIsTruncated :: Bool
    , _lorsMarker :: Maybe Text
    , _lorsNextMarker :: Maybe Text
    , _lorsContents :: [Object]
    , _lorsName :: BucketName
    , _lorsPrefix :: Maybe Text
    , _lorsMaxKeys :: Maybe Integer
    , _lorsCommonPrefixes :: [CommonPrefix]
    , _lorsEncodingType :: Maybe EncodingType
    } deriving (Show, Generic)

-- | A flag that indicates whether or not Amazon S3 returned all of the results
-- that satisfied the search criteria.
lorsIsTruncated :: Lens' ListObjectsResponse Bool
lorsIsTruncated = lens _lorsIsTruncated (\s a -> s { _lorsIsTruncated = a })

lorsMarker :: Lens' ListObjectsResponse (Maybe Text)
lorsMarker = lens _lorsMarker (\s a -> s { _lorsMarker = a })

-- | When response is truncated (the IsTruncated element value in the response
-- is true), you can use the key name in this field as marker in the
-- subsequent request to get next set of objects. Amazon S3 lists objects in
-- alphabetical order Note: This element is returned only if you have
-- delimiter request parameter specified. If response does not include the
-- NextMaker and it is truncated, you can use the value of the last Key in the
-- response as the marker in the subsequent request to get the next set of
-- object keys.
lorsNextMarker :: Lens' ListObjectsResponse (Maybe Text)
lorsNextMarker = lens _lorsNextMarker (\s a -> s { _lorsNextMarker = a })

lorsContents :: Lens' ListObjectsResponse [Object]
lorsContents = lens _lorsContents (\s a -> s { _lorsContents = a })

lorsName :: Lens' ListObjectsResponse BucketName
lorsName = lens _lorsName (\s a -> s { _lorsName = a })

lorsPrefix :: Lens' ListObjectsResponse (Maybe Text)
lorsPrefix = lens _lorsPrefix (\s a -> s { _lorsPrefix = a })

lorsMaxKeys :: Lens' ListObjectsResponse (Maybe Integer)
lorsMaxKeys = lens _lorsMaxKeys (\s a -> s { _lorsMaxKeys = a })

lorsCommonPrefixes :: Lens' ListObjectsResponse [CommonPrefix]
lorsCommonPrefixes =
    lens _lorsCommonPrefixes (\s a -> s { _lorsCommonPrefixes = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lorsEncodingType :: Lens' ListObjectsResponse (Maybe EncodingType)
lorsEncodingType =
    lens _lorsEncodingType (\s a -> s { _lorsEncodingType = a })

instance FromXML ListObjectsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListObjects where
    next rq rs
        | not (rs ^. lorsIsTruncated) = Nothing
        | otherwise = Just (rq & loMarker .~ rs ^. lorsNextMarker)

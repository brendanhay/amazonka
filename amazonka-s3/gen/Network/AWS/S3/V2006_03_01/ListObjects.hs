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
    -- ** Request constructor
    , listObjects
    -- ** Request lenses
    , lorBucket
    , lorDelimiter
    , lorEncodingType
    , lorMarker
    , lorMaxKeys
    , lorPrefix

    -- * Response
    , ListObjectsResponse
    -- ** Response lenses
    , looIsTruncated
    , looName
    , looCommonPrefixes
    , looEncodingType
    , looMarker
    , looMaxKeys
    , looNextMarker
    , looContents
    , looPrefix
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type GetBucket = ListObjects

-- | Minimum specification for a 'ListObjects' request.
listObjects :: BucketName -- ^ 'lorBucket'
            -> ListObjects
listObjects p1 = ListObjects
    { _lorBucket = p1
    , _lorDelimiter = Nothing
    , _lorEncodingType = Nothing
    , _lorMarker = Nothing
    , _lorMaxKeys = Nothing
    , _lorPrefix = Nothing
    }
{-# INLINE listObjects #-}

data ListObjects = ListObjects
    { _lorBucket :: BucketName
    , _lorDelimiter :: Maybe Char
      -- ^ A delimiter is a character you use to group keys.
    , _lorEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and
      -- specifies the encoding method to use. An object key may contain
      -- any Unicode character; however, XML 1.0 parser cannot parse some
      -- characters, such as characters with an ASCII value from 0 to 10.
      -- For characters that are not supported in XML 1.0, you can add
      -- this parameter to request that Amazon S3 encode the keys in the
      -- response.
    , _lorMarker :: Maybe Text
      -- ^ Specifies the key to start with when listing objects in a bucket.
    , _lorMaxKeys :: Maybe Integer
      -- ^ Sets the maximum number of keys returned in the response. The
      -- response might contain fewer keys but will never contain more.
    , _lorPrefix :: Maybe Text
      -- ^ Limits the response to keys that begin with the specified prefix.
    } deriving (Show, Generic)

lorBucket :: Lens' ListObjects (BucketName)
lorBucket f x =
    f (_lorBucket x)
        <&> \y -> x { _lorBucket = y }
{-# INLINE lorBucket #-}

-- | A delimiter is a character you use to group keys.
lorDelimiter :: Lens' ListObjects (Maybe Char)
lorDelimiter f x =
    f (_lorDelimiter x)
        <&> \y -> x { _lorDelimiter = y }
{-# INLINE lorDelimiter #-}

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
lorEncodingType :: Lens' ListObjects (Maybe EncodingType)
lorEncodingType f x =
    f (_lorEncodingType x)
        <&> \y -> x { _lorEncodingType = y }
{-# INLINE lorEncodingType #-}

-- | Specifies the key to start with when listing objects in a bucket.
lorMarker :: Lens' ListObjects (Maybe Text)
lorMarker f x =
    f (_lorMarker x)
        <&> \y -> x { _lorMarker = y }
{-# INLINE lorMarker #-}

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lorMaxKeys :: Lens' ListObjects (Maybe Integer)
lorMaxKeys f x =
    f (_lorMaxKeys x)
        <&> \y -> x { _lorMaxKeys = y }
{-# INLINE lorMaxKeys #-}

-- | Limits the response to keys that begin with the specified prefix.
lorPrefix :: Lens' ListObjects (Maybe Text)
lorPrefix f x =
    f (_lorPrefix x)
        <&> \y -> x { _lorPrefix = y }
{-# INLINE lorPrefix #-}

instance ToPath ListObjects where
    toPath ListObjects{..} = mconcat
        [ "/"
        , toBS _lorBucket
        ]

instance ToQuery ListObjects where
    toQuery ListObjects{..} = mconcat
        [ "delimiter" =? _lorDelimiter
        , "encoding-type" =? _lorEncodingType
        , "marker" =? _lorMarker
        , "max-keys" =? _lorMaxKeys
        , "prefix" =? _lorPrefix
        ]

instance ToHeaders ListObjects

instance ToBody ListObjects

data ListObjectsResponse = ListObjectsResponse
    { _looIsTruncated :: Bool
      -- ^ A flag that indicates whether or not Amazon S3 returned all of
      -- the results that satisfied the search criteria.
    , _looName :: BucketName
    , _looCommonPrefixes :: [CommonPrefix]
    , _looEncodingType :: Maybe EncodingType
      -- ^ Encoding type used by Amazon S3 to encode object keys in the
      -- response.
    , _looMarker :: Maybe Text
    , _looMaxKeys :: Maybe Integer
    , _looNextMarker :: Maybe Text
      -- ^ When response is truncated (the IsTruncated element value in the
      -- response is true), you can use the key name in this field as
      -- marker in the subsequent request to get next set of objects.
      -- Amazon S3 lists objects in alphabetical order Note: This element
      -- is returned only if you have delimiter request parameter
      -- specified. If response does not include the NextMaker and it is
      -- truncated, you can use the value of the last Key in the response
      -- as the marker in the subsequent request to get the next set of
      -- object keys.
    , _looContents :: [Object]
    , _looPrefix :: Maybe Text
    } deriving (Show, Generic)

-- | A flag that indicates whether or not Amazon S3 returned all of the results
-- that satisfied the search criteria.
looIsTruncated :: Lens' ListObjectsResponse (Bool)
looIsTruncated f x =
    f (_looIsTruncated x)
        <&> \y -> x { _looIsTruncated = y }
{-# INLINE looIsTruncated #-}

looName :: Lens' ListObjectsResponse (BucketName)
looName f x =
    f (_looName x)
        <&> \y -> x { _looName = y }
{-# INLINE looName #-}

looCommonPrefixes :: Lens' ListObjectsResponse ([CommonPrefix])
looCommonPrefixes f x =
    f (_looCommonPrefixes x)
        <&> \y -> x { _looCommonPrefixes = y }
{-# INLINE looCommonPrefixes #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
looEncodingType :: Lens' ListObjectsResponse (Maybe EncodingType)
looEncodingType f x =
    f (_looEncodingType x)
        <&> \y -> x { _looEncodingType = y }
{-# INLINE looEncodingType #-}

looMarker :: Lens' ListObjectsResponse (Maybe Text)
looMarker f x =
    f (_looMarker x)
        <&> \y -> x { _looMarker = y }
{-# INLINE looMarker #-}

looMaxKeys :: Lens' ListObjectsResponse (Maybe Integer)
looMaxKeys f x =
    f (_looMaxKeys x)
        <&> \y -> x { _looMaxKeys = y }
{-# INLINE looMaxKeys #-}

-- | When response is truncated (the IsTruncated element value in the response
-- is true), you can use the key name in this field as marker in the
-- subsequent request to get next set of objects. Amazon S3 lists objects in
-- alphabetical order Note: This element is returned only if you have
-- delimiter request parameter specified. If response does not include the
-- NextMaker and it is truncated, you can use the value of the last Key in the
-- response as the marker in the subsequent request to get the next set of
-- object keys.
looNextMarker :: Lens' ListObjectsResponse (Maybe Text)
looNextMarker f x =
    f (_looNextMarker x)
        <&> \y -> x { _looNextMarker = y }
{-# INLINE looNextMarker #-}

looContents :: Lens' ListObjectsResponse ([Object])
looContents f x =
    f (_looContents x)
        <&> \y -> x { _looContents = y }
{-# INLINE looContents #-}

looPrefix :: Lens' ListObjectsResponse (Maybe Text)
looPrefix f x =
    f (_looPrefix x)
        <&> \y -> x { _looPrefix = y }
{-# INLINE looPrefix #-}

instance FromXML ListObjectsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListObjects where
    next rq rs
        | not (_looIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lorMarker = _looNextMarker rs
            }

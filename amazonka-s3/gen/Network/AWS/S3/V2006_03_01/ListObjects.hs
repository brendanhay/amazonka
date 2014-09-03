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

lorBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> ListObjects
    -> f ListObjects
lorBucket f x =
    (\y -> x { _lorBucket = y })
       <$> f (_lorBucket x)
{-# INLINE lorBucket #-}

-- | A delimiter is a character you use to group keys.
lorDelimiter
    :: Functor f
    => (Maybe Char
    -> f (Maybe Char))
    -> ListObjects
    -> f ListObjects
lorDelimiter f x =
    (\y -> x { _lorDelimiter = y })
       <$> f (_lorDelimiter x)
{-# INLINE lorDelimiter #-}

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
lorEncodingType
    :: Functor f
    => (Maybe EncodingType
    -> f (Maybe EncodingType))
    -> ListObjects
    -> f ListObjects
lorEncodingType f x =
    (\y -> x { _lorEncodingType = y })
       <$> f (_lorEncodingType x)
{-# INLINE lorEncodingType #-}

-- | Specifies the key to start with when listing objects in a bucket.
lorMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjects
    -> f ListObjects
lorMarker f x =
    (\y -> x { _lorMarker = y })
       <$> f (_lorMarker x)
{-# INLINE lorMarker #-}

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lorMaxKeys
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListObjects
    -> f ListObjects
lorMaxKeys f x =
    (\y -> x { _lorMaxKeys = y })
       <$> f (_lorMaxKeys x)
{-# INLINE lorMaxKeys #-}

-- | Limits the response to keys that begin with the specified prefix.
lorPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjects
    -> f ListObjects
lorPrefix f x =
    (\y -> x { _lorPrefix = y })
       <$> f (_lorPrefix x)
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
looIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looIsTruncated f x =
    (\y -> x { _looIsTruncated = y })
       <$> f (_looIsTruncated x)
{-# INLINE looIsTruncated #-}

looName
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looName f x =
    (\y -> x { _looName = y })
       <$> f (_looName x)
{-# INLINE looName #-}

looCommonPrefixes
    :: Functor f
    => ([CommonPrefix]
    -> f ([CommonPrefix]))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looCommonPrefixes f x =
    (\y -> x { _looCommonPrefixes = y })
       <$> f (_looCommonPrefixes x)
{-# INLINE looCommonPrefixes #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
looEncodingType
    :: Functor f
    => (Maybe EncodingType
    -> f (Maybe EncodingType))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looEncodingType f x =
    (\y -> x { _looEncodingType = y })
       <$> f (_looEncodingType x)
{-# INLINE looEncodingType #-}

looMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looMarker f x =
    (\y -> x { _looMarker = y })
       <$> f (_looMarker x)
{-# INLINE looMarker #-}

looMaxKeys
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looMaxKeys f x =
    (\y -> x { _looMaxKeys = y })
       <$> f (_looMaxKeys x)
{-# INLINE looMaxKeys #-}

-- | When response is truncated (the IsTruncated element value in the response
-- is true), you can use the key name in this field as marker in the
-- subsequent request to get next set of objects. Amazon S3 lists objects in
-- alphabetical order Note: This element is returned only if you have
-- delimiter request parameter specified. If response does not include the
-- NextMaker and it is truncated, you can use the value of the last Key in the
-- response as the marker in the subsequent request to get the next set of
-- object keys.
looNextMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looNextMarker f x =
    (\y -> x { _looNextMarker = y })
       <$> f (_looNextMarker x)
{-# INLINE looNextMarker #-}

looContents
    :: Functor f
    => ([Object]
    -> f ([Object]))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looContents f x =
    (\y -> x { _looContents = y })
       <$> f (_looContents x)
{-# INLINE looContents #-}

looPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectsResponse
    -> f ListObjectsResponse
looPrefix f x =
    (\y -> x { _looPrefix = y })
       <$> f (_looPrefix x)
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

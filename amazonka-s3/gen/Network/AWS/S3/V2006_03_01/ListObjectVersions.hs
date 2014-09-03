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
    -- ** Request constructor
    , listObjectVersions
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
    , lovoName
    , lovoCommonPrefixes
    , lovoDeleteMarkers
    , lovoEncodingType
    , lovoKeyMarker
    , lovoMaxKeys
    , lovoNextKeyMarker
    , lovoNextVersionIdMarker
    , lovoVersions
    , lovoPrefix
    , lovoVersionIdMarker
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type GetBucketObjectVersions = ListObjectVersions

-- | Minimum specification for a 'ListObjectVersions' request.
listObjectVersions :: BucketName -- ^ 'lovrBucket'
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

lovrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> ListObjectVersions
    -> f ListObjectVersions
lovrBucket f x =
    (\y -> x { _lovrBucket = y })
       <$> f (_lovrBucket x)
{-# INLINE lovrBucket #-}

-- | A delimiter is a character you use to group keys.
lovrDelimiter
    :: Functor f
    => (Maybe Char
    -> f (Maybe Char))
    -> ListObjectVersions
    -> f ListObjectVersions
lovrDelimiter f x =
    (\y -> x { _lovrDelimiter = y })
       <$> f (_lovrDelimiter x)
{-# INLINE lovrDelimiter #-}

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
lovrEncodingType
    :: Functor f
    => (Maybe EncodingType
    -> f (Maybe EncodingType))
    -> ListObjectVersions
    -> f ListObjectVersions
lovrEncodingType f x =
    (\y -> x { _lovrEncodingType = y })
       <$> f (_lovrEncodingType x)
{-# INLINE lovrEncodingType #-}

-- | Specifies the key to start with when listing objects in a bucket.
lovrKeyMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersions
    -> f ListObjectVersions
lovrKeyMarker f x =
    (\y -> x { _lovrKeyMarker = y })
       <$> f (_lovrKeyMarker x)
{-# INLINE lovrKeyMarker #-}

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lovrMaxKeys
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListObjectVersions
    -> f ListObjectVersions
lovrMaxKeys f x =
    (\y -> x { _lovrMaxKeys = y })
       <$> f (_lovrMaxKeys x)
{-# INLINE lovrMaxKeys #-}

-- | Limits the response to keys that begin with the specified prefix.
lovrPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersions
    -> f ListObjectVersions
lovrPrefix f x =
    (\y -> x { _lovrPrefix = y })
       <$> f (_lovrPrefix x)
{-# INLINE lovrPrefix #-}

-- | Specifies the object version you want to start listing from.
lovrVersionIdMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersions
    -> f ListObjectVersions
lovrVersionIdMarker f x =
    (\y -> x { _lovrVersionIdMarker = y })
       <$> f (_lovrVersionIdMarker x)
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

-- | A flag that indicates whether or not Amazon S3 returned all of the results
-- that satisfied the search criteria. If your results were truncated, you can
-- make a follow-up paginated request using the NextKeyMarker and
-- NextVersionIdMarker response parameters as a starting place in another
-- request to return the rest of the results.
lovoIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoIsTruncated f x =
    (\y -> x { _lovoIsTruncated = y })
       <$> f (_lovoIsTruncated x)
{-# INLINE lovoIsTruncated #-}

lovoName
    :: Functor f
    => (Maybe BucketName
    -> f (Maybe BucketName))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoName f x =
    (\y -> x { _lovoName = y })
       <$> f (_lovoName x)
{-# INLINE lovoName #-}

lovoCommonPrefixes
    :: Functor f
    => ([CommonPrefix]
    -> f ([CommonPrefix]))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoCommonPrefixes f x =
    (\y -> x { _lovoCommonPrefixes = y })
       <$> f (_lovoCommonPrefixes x)
{-# INLINE lovoCommonPrefixes #-}

lovoDeleteMarkers
    :: Functor f
    => ([DeleteMarkerEntry]
    -> f ([DeleteMarkerEntry]))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoDeleteMarkers f x =
    (\y -> x { _lovoDeleteMarkers = y })
       <$> f (_lovoDeleteMarkers x)
{-# INLINE lovoDeleteMarkers #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovoEncodingType
    :: Functor f
    => (Maybe EncodingType
    -> f (Maybe EncodingType))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoEncodingType f x =
    (\y -> x { _lovoEncodingType = y })
       <$> f (_lovoEncodingType x)
{-# INLINE lovoEncodingType #-}

-- | Marks the last Key returned in a truncated response.
lovoKeyMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoKeyMarker f x =
    (\y -> x { _lovoKeyMarker = y })
       <$> f (_lovoKeyMarker x)
{-# INLINE lovoKeyMarker #-}

lovoMaxKeys
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoMaxKeys f x =
    (\y -> x { _lovoMaxKeys = y })
       <$> f (_lovoMaxKeys x)
{-# INLINE lovoMaxKeys #-}

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovoNextKeyMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoNextKeyMarker f x =
    (\y -> x { _lovoNextKeyMarker = y })
       <$> f (_lovoNextKeyMarker x)
{-# INLINE lovoNextKeyMarker #-}

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovoNextVersionIdMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoNextVersionIdMarker f x =
    (\y -> x { _lovoNextVersionIdMarker = y })
       <$> f (_lovoNextVersionIdMarker x)
{-# INLINE lovoNextVersionIdMarker #-}

lovoVersions
    :: Functor f
    => ([ObjectVersion]
    -> f ([ObjectVersion]))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoVersions f x =
    (\y -> x { _lovoVersions = y })
       <$> f (_lovoVersions x)
{-# INLINE lovoVersions #-}

lovoPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoPrefix f x =
    (\y -> x { _lovoPrefix = y })
       <$> f (_lovoPrefix x)
{-# INLINE lovoPrefix #-}

lovoVersionIdMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListObjectVersionsResponse
    -> f ListObjectVersionsResponse
lovoVersionIdMarker f x =
    (\y -> x { _lovoVersionIdMarker = y })
       <$> f (_lovoVersionIdMarker x)
{-# INLINE lovoVersionIdMarker #-}

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

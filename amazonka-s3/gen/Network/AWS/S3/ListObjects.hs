{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , ListObjectsOutput
    -- ** Response constructor
    , listObjectsOutput
    -- ** Response lenses
    , looCommonPrefixes
    , looContents
    , looEncodingType
    , looIsTruncated
    , looMarker
    , looMaxKeys
    , looName
    , looNextMarker
    , looPrefix
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data ListObjects = ListObjects
    { _lorBucket       :: Text
    , _lorDelimiter    :: Maybe Text
    , _lorEncodingType :: Maybe Text
    , _lorMarker       :: Maybe Text
    , _lorMaxKeys      :: Maybe Int
    , _lorPrefix       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lorBucket' @::@ 'Text'
--
-- * 'lorDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lorEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lorMarker' @::@ 'Maybe' 'Text'
--
-- * 'lorMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'lorPrefix' @::@ 'Maybe' 'Text'
--
listObjects :: Text -- ^ 'lorBucket'
            -> ListObjects
listObjects p1 = ListObjects
    { _lorBucket       = p1
    , _lorDelimiter    = Nothing
    , _lorEncodingType = Nothing
    , _lorMarker       = Nothing
    , _lorMaxKeys      = Nothing
    , _lorPrefix       = Nothing
    }

lorBucket :: Lens' ListObjects Text
lorBucket = lens _lorBucket (\s a -> s { _lorBucket = a })

-- | A delimiter is a character you use to group keys.
lorDelimiter :: Lens' ListObjects (Maybe Text)
lorDelimiter = lens _lorDelimiter (\s a -> s { _lorDelimiter = a })

lorEncodingType :: Lens' ListObjects (Maybe Text)
lorEncodingType = lens _lorEncodingType (\s a -> s { _lorEncodingType = a })

-- | Specifies the key to start with when listing objects in a bucket.
lorMarker :: Lens' ListObjects (Maybe Text)
lorMarker = lens _lorMarker (\s a -> s { _lorMarker = a })

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lorMaxKeys :: Lens' ListObjects (Maybe Int)
lorMaxKeys = lens _lorMaxKeys (\s a -> s { _lorMaxKeys = a })

-- | Limits the response to keys that begin with the specified prefix.
lorPrefix :: Lens' ListObjects (Maybe Text)
lorPrefix = lens _lorPrefix (\s a -> s { _lorPrefix = a })

instance ToPath ListObjects where
    toPath ListObjects{..} = mconcat
        [ "/"
        , toText _lorBucket
        ]

instance ToQuery ListObjects where
    toQuery ListObjects{..} = mconcat
        [ "delimiter"     =? _lorDelimiter
        , "encoding-type" =? _lorEncodingType
        , "marker"        =? _lorMarker
        , "max-keys"      =? _lorMaxKeys
        , "prefix"        =? _lorPrefix
        ]

instance ToHeaders ListObjects

data ListObjectsOutput = ListObjectsOutput
    { _looCommonPrefixes :: [CommonPrefix]
    , _looContents       :: [Object]
    , _looEncodingType   :: Maybe Text
    , _looIsTruncated    :: Maybe Bool
    , _looMarker         :: Maybe Text
    , _looMaxKeys        :: Maybe Int
    , _looName           :: Maybe Text
    , _looNextMarker     :: Maybe Text
    , _looPrefix         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListObjectsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'looCommonPrefixes' @::@ ['CommonPrefix']
--
-- * 'looContents' @::@ ['Object']
--
-- * 'looEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'looIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'looMarker' @::@ 'Maybe' 'Text'
--
-- * 'looMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'looName' @::@ 'Maybe' 'Text'
--
-- * 'looNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'looPrefix' @::@ 'Maybe' 'Text'
--
listObjectsOutput :: ListObjectsOutput
listObjectsOutput = ListObjectsOutput
    { _looIsTruncated    = Nothing
    , _looMarker         = Nothing
    , _looNextMarker     = Nothing
    , _looContents       = mempty
    , _looName           = Nothing
    , _looPrefix         = Nothing
    , _looMaxKeys        = Nothing
    , _looCommonPrefixes = mempty
    , _looEncodingType   = Nothing
    }

looCommonPrefixes :: Lens' ListObjectsOutput [CommonPrefix]
looCommonPrefixes =
    lens _looCommonPrefixes (\s a -> s { _looCommonPrefixes = a })

looContents :: Lens' ListObjectsOutput [Object]
looContents = lens _looContents (\s a -> s { _looContents = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
looEncodingType :: Lens' ListObjectsOutput (Maybe Text)
looEncodingType = lens _looEncodingType (\s a -> s { _looEncodingType = a })

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria.
looIsTruncated :: Lens' ListObjectsOutput (Maybe Bool)
looIsTruncated = lens _looIsTruncated (\s a -> s { _looIsTruncated = a })

looMarker :: Lens' ListObjectsOutput (Maybe Text)
looMarker = lens _looMarker (\s a -> s { _looMarker = a })

looMaxKeys :: Lens' ListObjectsOutput (Maybe Int)
looMaxKeys = lens _looMaxKeys (\s a -> s { _looMaxKeys = a })

looName :: Lens' ListObjectsOutput (Maybe Text)
looName = lens _looName (\s a -> s { _looName = a })

-- | When response is truncated (the IsTruncated element value in the response
-- is true), you can use the key name in this field as marker in the
-- subsequent request to get next set of objects. Amazon S3 lists objects in
-- alphabetical order Note: This element is returned only if you have
-- delimiter request parameter specified. If response does not include the
-- NextMaker and it is truncated, you can use the value of the last Key in
-- the response as the marker in the subsequent request to get the next set
-- of object keys.
looNextMarker :: Lens' ListObjectsOutput (Maybe Text)
looNextMarker = lens _looNextMarker (\s a -> s { _looNextMarker = a })

looPrefix :: Lens' ListObjectsOutput (Maybe Text)
looPrefix = lens _looPrefix (\s a -> s { _looPrefix = a })

instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsOutput

    request  = get'
    response = const . xmlResponse $ \h x -> ListObjectsOutput
        <$> x %| "CommonPrefixes"
        <*> x %| "Contents"
        <*> x %| "EncodingType"
        <*> x %| "IsTruncated"
        <*> x %| "Marker"
        <*> x %| "MaxKeys"
        <*> x %| "Name"
        <*> x %| "NextMarker"
        <*> x %| "Prefix"

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

data ListObjects = ListObjects
    { _lorBucket       :: BucketName
    , _lorDelimiter    :: Maybe Char
    , _lorEncodingType :: Maybe Text
    , _lorMarker       :: Maybe Text
    , _lorMaxKeys      :: Maybe Int
    , _lorPrefix       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lorBucket' @::@ 'BucketName'
--
-- * 'lorDelimiter' @::@ 'Maybe' 'Char'
--
-- * 'lorEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lorMarker' @::@ 'Maybe' 'Text'
--
-- * 'lorMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'lorPrefix' @::@ 'Maybe' 'Text'
--
listObjects :: BucketName -- ^ 'lorBucket'
            -> ListObjects
listObjects p1 = ListObjects
    { _lorBucket       = p1
    , _lorDelimiter    = Nothing
    , _lorEncodingType = Nothing
    , _lorMarker       = Nothing
    , _lorMaxKeys      = Nothing
    , _lorPrefix       = Nothing
    }

lorBucket :: Lens' ListObjects BucketName
lorBucket = lens _lorBucket (\s a -> s { _lorBucket = a })

-- | A delimiter is a character you use to group keys.
lorDelimiter :: Lens' ListObjects (Maybe Char)
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

instance ToBody ListObjects

data ListObjectsOutput = ListObjectsOutput
    { _looCommonPrefixes :: [CommonPrefix]
    , _looContents       :: [Object]
    , _looEncodingType   :: Maybe Text
    , _looIsTruncated    :: Maybe Bool
    , _looMarker         :: Maybe Text
    , _looMaxKeys        :: Maybe Int
    , _looName           :: Maybe BucketName
    , _looNextMarker     :: Maybe Text
    , _looPrefix         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "CommonPrefixes"
        <*> x %| "Contents"
        <*> x %| "EncodingType"
        <*> x %| "IsTruncated"
        <*> x %| "Marker"
        <*> x %| "MaxKeys"
        <*> x %| "Name"
        <*> x %| "NextMarker"
        <*> x %| "Prefix"

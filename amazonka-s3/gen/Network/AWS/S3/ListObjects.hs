{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListObjects.html>
module Network.AWS.S3.ListObjects
    (
    -- * Request
      ListObjects
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
    , lorCommonPrefixes
    , lorContents
    , lorDelimiter
    , lorEncodingType
    , lorIsTruncated
    , lorMarker
    , lorMaxKeys
    , lorName
    , lorNextMarker
    , lorPrefix
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data ListObjects = ListObjects
    { _loBucket       :: Text
    , _loDelimiter    :: Maybe Text
    , _loEncodingType :: Maybe Text
    , _loMarker       :: Maybe Text
    , _loMaxKeys      :: Maybe Int
    , _loPrefix       :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loBucket' @::@ 'Text'
--
-- * 'loDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'loEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'loMarker' @::@ 'Maybe' 'Text'
--
-- * 'loMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'loPrefix' @::@ 'Maybe' 'Text'
--
listObjects :: Text -- ^ 'loBucket'
            -> ListObjects
listObjects p1 = ListObjects
    { _loBucket       = p1
    , _loDelimiter    = Nothing
    , _loEncodingType = Nothing
    , _loMarker       = Nothing
    , _loMaxKeys      = Nothing
    , _loPrefix       = Nothing
    }

loBucket :: Lens' ListObjects Text
loBucket = lens _loBucket (\s a -> s { _loBucket = a })

-- | A delimiter is a character you use to group keys.
loDelimiter :: Lens' ListObjects (Maybe Text)
loDelimiter = lens _loDelimiter (\s a -> s { _loDelimiter = a })

loEncodingType :: Lens' ListObjects (Maybe Text)
loEncodingType = lens _loEncodingType (\s a -> s { _loEncodingType = a })

-- | Specifies the key to start with when listing objects in a bucket.
loMarker :: Lens' ListObjects (Maybe Text)
loMarker = lens _loMarker (\s a -> s { _loMarker = a })

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
loMaxKeys :: Lens' ListObjects (Maybe Int)
loMaxKeys = lens _loMaxKeys (\s a -> s { _loMaxKeys = a })

-- | Limits the response to keys that begin with the specified prefix.
loPrefix :: Lens' ListObjects (Maybe Text)
loPrefix = lens _loPrefix (\s a -> s { _loPrefix = a })

data ListObjectsResponse = ListObjectsResponse
    { _lorCommonPrefixes :: List "CommonPrefixes" CommonPrefix
    , _lorContents       :: List "Contents" Object
    , _lorDelimiter      :: Maybe Text
    , _lorEncodingType   :: Maybe Text
    , _lorIsTruncated    :: Maybe Bool
    , _lorMarker         :: Maybe Text
    , _lorMaxKeys        :: Maybe Int
    , _lorName           :: Maybe Text
    , _lorNextMarker     :: Maybe Text
    , _lorPrefix         :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListObjectsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lorCommonPrefixes' @::@ ['CommonPrefix']
--
-- * 'lorContents' @::@ ['Object']
--
-- * 'lorDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lorEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lorIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lorMarker' @::@ 'Maybe' 'Text'
--
-- * 'lorMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'lorName' @::@ 'Maybe' 'Text'
--
-- * 'lorNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'lorPrefix' @::@ 'Maybe' 'Text'
--
listObjectsResponse :: ListObjectsResponse
listObjectsResponse = ListObjectsResponse
    { _lorIsTruncated    = Nothing
    , _lorMarker         = Nothing
    , _lorNextMarker     = Nothing
    , _lorContents       = Nothing
    , _lorName           = Nothing
    , _lorPrefix         = Nothing
    , _lorDelimiter      = Nothing
    , _lorMaxKeys        = Nothing
    , _lorCommonPrefixes = Nothing
    , _lorEncodingType   = Nothing
    }

lorCommonPrefixes :: Lens' ListObjectsResponse [CommonPrefix]
lorCommonPrefixes =
    lens _lorCommonPrefixes (\s a -> s { _lorCommonPrefixes = a })
        . _List

lorContents :: Lens' ListObjectsResponse [Object]
lorContents = lens _lorContents (\s a -> s { _lorContents = a }) . _List

lorDelimiter :: Lens' ListObjectsResponse (Maybe Text)
lorDelimiter = lens _lorDelimiter (\s a -> s { _lorDelimiter = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lorEncodingType :: Lens' ListObjectsResponse (Maybe Text)
lorEncodingType = lens _lorEncodingType (\s a -> s { _lorEncodingType = a })

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria.
lorIsTruncated :: Lens' ListObjectsResponse (Maybe Bool)
lorIsTruncated = lens _lorIsTruncated (\s a -> s { _lorIsTruncated = a })

lorMarker :: Lens' ListObjectsResponse (Maybe Text)
lorMarker = lens _lorMarker (\s a -> s { _lorMarker = a })

lorMaxKeys :: Lens' ListObjectsResponse (Maybe Int)
lorMaxKeys = lens _lorMaxKeys (\s a -> s { _lorMaxKeys = a })

lorName :: Lens' ListObjectsResponse (Maybe Text)
lorName = lens _lorName (\s a -> s { _lorName = a })

-- | When response is truncated (the IsTruncated element value in the response
-- is true), you can use the key name in this field as marker in the
-- subsequent request to get next set of objects. Amazon S3 lists objects in
-- alphabetical order Note: This element is returned only if you have
-- delimiter request parameter specified. If response does not include the
-- NextMaker and it is truncated, you can use the value of the last Key in
-- the response as the marker in the subsequent request to get the next set
-- of object keys.
lorNextMarker :: Lens' ListObjectsResponse (Maybe Text)
lorNextMarker = lens _lorNextMarker (\s a -> s { _lorNextMarker = a })

lorPrefix :: Lens' ListObjectsResponse (Maybe Text)
lorPrefix = lens _lorPrefix (\s a -> s { _lorPrefix = a })

instance ToPath ListObjects where
    toPath ListObjects{..} = mconcat
        [ "/"
        , toText _loBucket
        ]

instance ToQuery ListObjects where
    toQuery ListObjects{..} = mconcat
        [ "delimiter"     =? _loDelimiter
        , "encoding-type" =? _loEncodingType
        , "marker"        =? _loMarker
        , "max-keys"      =? _loMaxKeys
        , "prefix"        =? _loPrefix
        ]

instance ToHeaders ListObjects

instance ToXMLRoot ListObjects where
    toXMLRoot = const (namespaced ns "ListObjects" [])

instance ToXML ListObjects

instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsResponse

    request  = get
    response = xmlResponse

instance FromXML ListObjectsResponse where
    parseXML x = ListObjectsResponse
        <$> parseXML x
        <*> parseXML x
        <*> x .@? "Delimiter"
        <*> x .@? "EncodingType"
        <*> x .@? "IsTruncated"
        <*> x .@? "Marker"
        <*> x .@? "MaxKeys"
        <*> x .@? "Name"
        <*> x .@? "NextMarker"
        <*> x .@? "Prefix"

instance AWSPager ListObjects where
    page rq rs
        | stop (rs ^. lorIsTruncated) = Nothing
        | otherwise = Just $ rq
            & loMarker .~ rs ^. lorNextMarker

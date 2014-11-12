{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , loBucket
    , loDelimiter
    , loEncodingType
    , loMarker
    , loMaxKeys
    , loPrefix

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
    { _loBucket       :: Text
    , _loDelimiter    :: Maybe Text
    , _loEncodingType :: Maybe Text
    , _loMarker       :: Maybe Text
    , _loMaxKeys      :: Maybe Int
    , _loPrefix       :: Maybe Text
    } (Eq, Ord, Show, Generic)

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
    } (Eq, Show, Generic)

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

instance FromXML ListObjectsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListObjectsOutput"
instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsOutput

    request  = get
    response = xmlResponse $ \h x -> ListObjectsOutput
        <$> x %| "CommonPrefixes"
        <*> x %| "Contents"
        <*> x %| "EncodingType"
        <*> x %| "IsTruncated"
        <*> x %| "Marker"
        <*> x %| "MaxKeys"
        <*> x %| "Name"
        <*> x %| "NextMarker"
        <*> x %| "Prefix"

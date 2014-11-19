{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns metadata about all of the versions of objects in a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListObjectVersions.html>
module Network.AWS.S3.ListObjectVersions
    (
    -- * Request
      ListObjectVersions
    -- ** Request constructor
    , listObjectVersions
    -- ** Request lenses
    , lovBucket
    , lovDelimiter
    , lovEncodingType
    , lovKeyMarker
    , lovMaxKeys
    , lovPrefix
    , lovVersionIdMarker

    -- * Response
    , ListObjectVersionsResponse
    -- ** Response constructor
    , listObjectVersionsResponse
    -- ** Response lenses
    , lovrCommonPrefixes
    , lovrDeleteMarkers
    , lovrDelimiter
    , lovrEncodingType
    , lovrIsTruncated
    , lovrKeyMarker
    , lovrMaxKeys
    , lovrName
    , lovrNextKeyMarker
    , lovrNextVersionIdMarker
    , lovrPrefix
    , lovrVersionIdMarker
    , lovrVersions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data ListObjectVersions = ListObjectVersions
    { _lovBucket          :: Text
    , _lovDelimiter       :: Maybe Text
    , _lovEncodingType    :: Maybe Text
    , _lovKeyMarker       :: Maybe Text
    , _lovMaxKeys         :: Maybe Int
    , _lovPrefix          :: Maybe Text
    , _lovVersionIdMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListObjectVersions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lovBucket' @::@ 'Text'
--
-- * 'lovDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lovEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lovKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'lovPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lovVersionIdMarker' @::@ 'Maybe' 'Text'
--
listObjectVersions :: Text -- ^ 'lovBucket'
                   -> ListObjectVersions
listObjectVersions p1 = ListObjectVersions
    { _lovBucket          = p1
    , _lovDelimiter       = Nothing
    , _lovEncodingType    = Nothing
    , _lovKeyMarker       = Nothing
    , _lovMaxKeys         = Nothing
    , _lovPrefix          = Nothing
    , _lovVersionIdMarker = Nothing
    }

lovBucket :: Lens' ListObjectVersions Text
lovBucket = lens _lovBucket (\s a -> s { _lovBucket = a })

-- | A delimiter is a character you use to group keys.
lovDelimiter :: Lens' ListObjectVersions (Maybe Text)
lovDelimiter = lens _lovDelimiter (\s a -> s { _lovDelimiter = a })

lovEncodingType :: Lens' ListObjectVersions (Maybe Text)
lovEncodingType = lens _lovEncodingType (\s a -> s { _lovEncodingType = a })

-- | Specifies the key to start with when listing objects in a bucket.
lovKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lovKeyMarker = lens _lovKeyMarker (\s a -> s { _lovKeyMarker = a })

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lovMaxKeys :: Lens' ListObjectVersions (Maybe Int)
lovMaxKeys = lens _lovMaxKeys (\s a -> s { _lovMaxKeys = a })

-- | Limits the response to keys that begin with the specified prefix.
lovPrefix :: Lens' ListObjectVersions (Maybe Text)
lovPrefix = lens _lovPrefix (\s a -> s { _lovPrefix = a })

-- | Specifies the object version you want to start listing from.
lovVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lovVersionIdMarker =
    lens _lovVersionIdMarker (\s a -> s { _lovVersionIdMarker = a })

data ListObjectVersionsResponse = ListObjectVersionsResponse
    { _lovrCommonPrefixes      :: Flatten [CommonPrefix]
    , _lovrDeleteMarkers       :: Flatten [DeleteMarkerEntry]
    , _lovrDelimiter           :: Maybe Text
    , _lovrEncodingType        :: Maybe Text
    , _lovrIsTruncated         :: Maybe Bool
    , _lovrKeyMarker           :: Maybe Text
    , _lovrMaxKeys             :: Maybe Int
    , _lovrName                :: Maybe Text
    , _lovrNextKeyMarker       :: Maybe Text
    , _lovrNextVersionIdMarker :: Maybe Text
    , _lovrPrefix              :: Maybe Text
    , _lovrVersionIdMarker     :: Maybe Text
    , _lovrVersions            :: Flatten [ObjectVersion]
    } deriving (Eq, Show, Generic)

-- | 'ListObjectVersionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lovrCommonPrefixes' @::@ ['CommonPrefix']
--
-- * 'lovrDeleteMarkers' @::@ ['DeleteMarkerEntry']
--
-- * 'lovrDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lovrEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lovrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lovrKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovrMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'lovrName' @::@ 'Maybe' 'Text'
--
-- * 'lovrNextKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovrNextVersionIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovrPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lovrVersionIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovrVersions' @::@ ['ObjectVersion']
--
listObjectVersionsResponse :: [ObjectVersion] -- ^ 'lovrVersions'
                           -> [DeleteMarkerEntry] -- ^ 'lovrDeleteMarkers'
                           -> [CommonPrefix] -- ^ 'lovrCommonPrefixes'
                           -> ListObjectVersionsResponse
listObjectVersionsResponse p1 p2 p3 = ListObjectVersionsResponse
    { _lovrVersions            = withIso _Flatten (const id) p1
    , _lovrDeleteMarkers       = withIso _Flatten (const id) p2
    , _lovrCommonPrefixes      = withIso _Flatten (const id) p3
    , _lovrIsTruncated         = Nothing
    , _lovrKeyMarker           = Nothing
    , _lovrVersionIdMarker     = Nothing
    , _lovrNextKeyMarker       = Nothing
    , _lovrNextVersionIdMarker = Nothing
    , _lovrName                = Nothing
    , _lovrPrefix              = Nothing
    , _lovrDelimiter           = Nothing
    , _lovrMaxKeys             = Nothing
    , _lovrEncodingType        = Nothing
    }

lovrCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lovrCommonPrefixes =
    lens _lovrCommonPrefixes (\s a -> s { _lovrCommonPrefixes = a })
        . _Flatten

lovrDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lovrDeleteMarkers =
    lens _lovrDeleteMarkers (\s a -> s { _lovrDeleteMarkers = a })
        . _Flatten

lovrDelimiter :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrDelimiter = lens _lovrDelimiter (\s a -> s { _lovrDelimiter = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovrEncodingType :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrEncodingType = lens _lovrEncodingType (\s a -> s { _lovrEncodingType = a })

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria. If your results were
-- truncated, you can make a follow-up paginated request using the
-- NextKeyMarker and NextVersionIdMarker response parameters as a starting
-- place in another request to return the rest of the results.
lovrIsTruncated :: Lens' ListObjectVersionsResponse (Maybe Bool)
lovrIsTruncated = lens _lovrIsTruncated (\s a -> s { _lovrIsTruncated = a })

-- | Marks the last Key returned in a truncated response.
lovrKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrKeyMarker = lens _lovrKeyMarker (\s a -> s { _lovrKeyMarker = a })

lovrMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Int)
lovrMaxKeys = lens _lovrMaxKeys (\s a -> s { _lovrMaxKeys = a })

lovrName :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrName = lens _lovrName (\s a -> s { _lovrName = a })

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovrNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrNextKeyMarker =
    lens _lovrNextKeyMarker (\s a -> s { _lovrNextKeyMarker = a })

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovrNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrNextVersionIdMarker =
    lens _lovrNextVersionIdMarker (\s a -> s { _lovrNextVersionIdMarker = a })

lovrPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrPrefix = lens _lovrPrefix (\s a -> s { _lovrPrefix = a })

lovrVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrVersionIdMarker =
    lens _lovrVersionIdMarker (\s a -> s { _lovrVersionIdMarker = a })

lovrVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lovrVersions = lens _lovrVersions (\s a -> s { _lovrVersions = a })
    . _Flatten

instance ToPath ListObjectVersions where
    toPath ListObjectVersions{..} = mconcat
        [ "/"
        , toText _lovBucket
        ]

instance ToQuery ListObjectVersions where
    toQuery ListObjectVersions{..} = mconcat
        [ "versions"
        , "delimiter"         =? _lovDelimiter
        , "encoding-type"     =? _lovEncodingType
        , "key-marker"        =? _lovKeyMarker
        , "max-keys"          =? _lovMaxKeys
        , "prefix"            =? _lovPrefix
        , "version-id-marker" =? _lovVersionIdMarker
        ]

instance ToHeaders ListObjectVersions

instance ToXMLRoot ListObjectVersions where
    toXMLRoot = const (element "ListObjectVersions" [])

instance ToXML ListObjectVersions

instance AWSRequest ListObjectVersions where
    type Sv ListObjectVersions = S3
    type Rs ListObjectVersions = ListObjectVersionsResponse

    request  = get
    response = xmlResponse

instance FromXML ListObjectVersionsResponse where
    parseXML x = ListObjectVersionsResponse
            <$> parseXML x
            <*> parseXML x
            <*> x .@? "Delimiter"
            <*> x .@? "EncodingType"
            <*> x .@? "IsTruncated"
            <*> x .@? "KeyMarker"
            <*> x .@? "MaxKeys"
            <*> x .@? "Name"
            <*> x .@? "NextKeyMarker"
            <*> x .@? "NextVersionIdMarker"
            <*> x .@? "Prefix"
            <*> x .@? "VersionIdMarker"
            <*> parseXML x

instance AWSPager ListObjectVersions where
    next rq rs
        | stop (rs ^. lovrIsTruncated) = Nothing
        | isNothing p1 && isNothing p2 = Nothing
        | otherwise = Just $ rq
            & lovKeyMarker .~ p1
            & lovVersionIdMarker .~ p2
      where
        p1 = rs ^. lovrNextKeyMarker
        p2 = rs ^. lovrNextVersionIdMarker

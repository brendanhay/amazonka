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
    , ListObjectVersionsOutput
    -- ** Response constructor
    , listObjectVersionsOutput
    -- ** Response lenses
    , lovoCommonPrefixes
    , lovoDeleteMarkers
    , lovoEncodingType
    , lovoIsTruncated
    , lovoKeyMarker
    , lovoMaxKeys
    , lovoName
    , lovoNextKeyMarker
    , lovoNextVersionIdMarker
    , lovoPrefix
    , lovoVersionIdMarker
    , lovoVersions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

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

data ListObjectVersionsOutput = ListObjectVersionsOutput
    { _lovoCommonPrefixes      :: [CommonPrefix]
    , _lovoDeleteMarkers       :: [DeleteMarkerEntry]
    , _lovoEncodingType        :: Maybe Text
    , _lovoIsTruncated         :: Maybe Bool
    , _lovoKeyMarker           :: Maybe Text
    , _lovoMaxKeys             :: Maybe Int
    , _lovoName                :: Maybe Text
    , _lovoNextKeyMarker       :: Maybe Text
    , _lovoNextVersionIdMarker :: Maybe Text
    , _lovoPrefix              :: Maybe Text
    , _lovoVersionIdMarker     :: Maybe Text
    , _lovoVersions            :: [ObjectVersion]
    } deriving (Eq, Show, Generic)

-- | 'ListObjectVersionsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lovoCommonPrefixes' @::@ ['CommonPrefix']
--
-- * 'lovoDeleteMarkers' @::@ ['DeleteMarkerEntry']
--
-- * 'lovoEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lovoIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lovoKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovoMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'lovoName' @::@ 'Maybe' 'Text'
--
-- * 'lovoNextKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovoNextVersionIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovoPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lovoVersionIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovoVersions' @::@ ['ObjectVersion']
--
listObjectVersionsOutput :: ListObjectVersionsOutput
listObjectVersionsOutput = ListObjectVersionsOutput
    { _lovoIsTruncated         = Nothing
    , _lovoKeyMarker           = Nothing
    , _lovoVersionIdMarker     = Nothing
    , _lovoNextKeyMarker       = Nothing
    , _lovoNextVersionIdMarker = Nothing
    , _lovoVersions            = mempty
    , _lovoDeleteMarkers       = mempty
    , _lovoName                = Nothing
    , _lovoPrefix              = Nothing
    , _lovoMaxKeys             = Nothing
    , _lovoCommonPrefixes      = mempty
    , _lovoEncodingType        = Nothing
    }

lovoCommonPrefixes :: Lens' ListObjectVersionsOutput [CommonPrefix]
lovoCommonPrefixes =
    lens _lovoCommonPrefixes (\s a -> s { _lovoCommonPrefixes = a })

lovoDeleteMarkers :: Lens' ListObjectVersionsOutput [DeleteMarkerEntry]
lovoDeleteMarkers =
    lens _lovoDeleteMarkers (\s a -> s { _lovoDeleteMarkers = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovoEncodingType :: Lens' ListObjectVersionsOutput (Maybe Text)
lovoEncodingType = lens _lovoEncodingType (\s a -> s { _lovoEncodingType = a })

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria. If your results were
-- truncated, you can make a follow-up paginated request using the
-- NextKeyMarker and NextVersionIdMarker response parameters as a starting
-- place in another request to return the rest of the results.
lovoIsTruncated :: Lens' ListObjectVersionsOutput (Maybe Bool)
lovoIsTruncated = lens _lovoIsTruncated (\s a -> s { _lovoIsTruncated = a })

-- | Marks the last Key returned in a truncated response.
lovoKeyMarker :: Lens' ListObjectVersionsOutput (Maybe Text)
lovoKeyMarker = lens _lovoKeyMarker (\s a -> s { _lovoKeyMarker = a })

lovoMaxKeys :: Lens' ListObjectVersionsOutput (Maybe Int)
lovoMaxKeys = lens _lovoMaxKeys (\s a -> s { _lovoMaxKeys = a })

lovoName :: Lens' ListObjectVersionsOutput (Maybe Text)
lovoName = lens _lovoName (\s a -> s { _lovoName = a })

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovoNextKeyMarker :: Lens' ListObjectVersionsOutput (Maybe Text)
lovoNextKeyMarker =
    lens _lovoNextKeyMarker (\s a -> s { _lovoNextKeyMarker = a })

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovoNextVersionIdMarker :: Lens' ListObjectVersionsOutput (Maybe Text)
lovoNextVersionIdMarker =
    lens _lovoNextVersionIdMarker (\s a -> s { _lovoNextVersionIdMarker = a })

lovoPrefix :: Lens' ListObjectVersionsOutput (Maybe Text)
lovoPrefix = lens _lovoPrefix (\s a -> s { _lovoPrefix = a })

lovoVersionIdMarker :: Lens' ListObjectVersionsOutput (Maybe Text)
lovoVersionIdMarker =
    lens _lovoVersionIdMarker (\s a -> s { _lovoVersionIdMarker = a })

lovoVersions :: Lens' ListObjectVersionsOutput [ObjectVersion]
lovoVersions = lens _lovoVersions (\s a -> s { _lovoVersions = a })

instance FromXML ListObjectVersionsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListObjectVersionsOutput"
instance AWSRequest ListObjectVersions where
    type Sv ListObjectVersions = S3
    type Rs ListObjectVersions = ListObjectVersionsOutput

    request  = get
    response = xmlResponse $ \h x -> ListObjectVersionsOutput
        <$> x %| "CommonPrefixes"
        <*> x %| "DeleteMarker"
        <*> x %| "EncodingType"
        <*> x %| "IsTruncated"
        <*> x %| "KeyMarker"
        <*> x %| "MaxKeys"
        <*> x %| "Name"
        <*> x %| "NextKeyMarker"
        <*> x %| "NextVersionIdMarker"
        <*> x %| "Prefix"
        <*> x %| "VersionIdMarker"
        <*> x %| "Version"

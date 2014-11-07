{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , lovrBucket
    , lovrDelimiter
    , lovrEncodingType
    , lovrKeyMarker
    , lovrMaxKeys
    , lovrPrefix
    , lovrVersionIdMarker

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
    { _lovrBucket          :: Text
    , _lovrDelimiter       :: Maybe Text
    , _lovrEncodingType    :: Maybe Text
    , _lovrKeyMarker       :: Maybe Text
    , _lovrMaxKeys         :: Maybe Int
    , _lovrPrefix          :: Maybe Text
    , _lovrVersionIdMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListObjectVersions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lovrBucket' @::@ 'Text'
--
-- * 'lovrDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lovrEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lovrKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lovrMaxKeys' @::@ 'Maybe' 'Int'
--
-- * 'lovrPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lovrVersionIdMarker' @::@ 'Maybe' 'Text'
--
listObjectVersions :: Text -- ^ 'lovrBucket'
                   -> ListObjectVersions
listObjectVersions p1 = ListObjectVersions
    { _lovrBucket          = p1
    , _lovrDelimiter       = Nothing
    , _lovrEncodingType    = Nothing
    , _lovrKeyMarker       = Nothing
    , _lovrMaxKeys         = Nothing
    , _lovrPrefix          = Nothing
    , _lovrVersionIdMarker = Nothing
    }

lovrBucket :: Lens' ListObjectVersions Text
lovrBucket = lens _lovrBucket (\s a -> s { _lovrBucket = a })

-- | A delimiter is a character you use to group keys.
lovrDelimiter :: Lens' ListObjectVersions (Maybe Text)
lovrDelimiter = lens _lovrDelimiter (\s a -> s { _lovrDelimiter = a })

lovrEncodingType :: Lens' ListObjectVersions (Maybe Text)
lovrEncodingType = lens _lovrEncodingType (\s a -> s { _lovrEncodingType = a })

-- | Specifies the key to start with when listing objects in a bucket.
lovrKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lovrKeyMarker = lens _lovrKeyMarker (\s a -> s { _lovrKeyMarker = a })

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lovrMaxKeys :: Lens' ListObjectVersions (Maybe Int)
lovrMaxKeys = lens _lovrMaxKeys (\s a -> s { _lovrMaxKeys = a })

-- | Limits the response to keys that begin with the specified prefix.
lovrPrefix :: Lens' ListObjectVersions (Maybe Text)
lovrPrefix = lens _lovrPrefix (\s a -> s { _lovrPrefix = a })

-- | Specifies the object version you want to start listing from.
lovrVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lovrVersionIdMarker =
    lens _lovrVersionIdMarker (\s a -> s { _lovrVersionIdMarker = a })

instance ToPath ListObjectVersions where
    toPath ListObjectVersions{..} = mconcat
        [ "/"
        , toText _lovrBucket
        ]

instance ToQuery ListObjectVersions where
    toQuery ListObjectVersions{..} = mconcat
        [ "versions"
        , "delimiter"         =? _lovrDelimiter
        , "encoding-type"     =? _lovrEncodingType
        , "key-marker"        =? _lovrKeyMarker
        , "max-keys"          =? _lovrMaxKeys
        , "prefix"            =? _lovrPrefix
        , "version-id-marker" =? _lovrVersionIdMarker
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
-- * 'lovoCommonPrefixes' @::@ '[CommonPrefix]'
--
-- * 'lovoDeleteMarkers' @::@ '[DeleteMarkerEntry]'
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
-- * 'lovoVersions' @::@ '[ObjectVersion]'
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

instance AWSRequest ListObjectVersions where
    type Sv ListObjectVersions = S3
    type Rs ListObjectVersions = ListObjectVersionsOutput

    request  = get'
    response = const . xmlResponse $ \h x -> ListObjectVersionsOutput
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

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

-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the parts that have been uploaded for a specific multipart upload.
module Network.AWS.S3.ListParts
    (
    -- * Request
      ListParts
    -- ** Request constructor
    , listParts
    -- ** Request lenses
    , lpBucket
    , lpKey
    , lpMaxParts
    , lpPartNumberMarker
    , lpUploadId

    -- * Response
    , ListPartsOutput
    -- ** Response constructor
    , listPartsResponse
    -- ** Response lenses
    , lpoBucket
    , lpoInitiator
    , lpoIsTruncated
    , lpoKey
    , lpoMaxParts
    , lpoNextPartNumberMarker
    , lpoOwner
    , lpoPartNumberMarker
    , lpoParts
    , lpoStorageClass
    , lpoUploadId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data ListParts = ListParts
    { _lpBucket           :: Text
    , _lpKey              :: Text
    , _lpMaxParts         :: Maybe Int
    , _lpPartNumberMarker :: Maybe Int
    , _lpUploadId         :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListParts' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpBucket' @::@ 'Text'
--
-- * 'lpKey' @::@ 'Text'
--
-- * 'lpMaxParts' @::@ 'Maybe' 'Int'
--
-- * 'lpPartNumberMarker' @::@ 'Maybe' 'Int'
--
-- * 'lpUploadId' @::@ 'Text'
--
listParts :: Text -- ^ 'lpBucket'
          -> Text -- ^ 'lpKey'
          -> Text -- ^ 'lpUploadId'
          -> ListParts
listParts p1 p2 p3 = ListParts
    { _lpBucket           = p1
    , _lpKey              = p2
    , _lpUploadId         = p3
    , _lpMaxParts         = Nothing
    , _lpPartNumberMarker = Nothing
    }

lpBucket :: Lens' ListParts Text
lpBucket = lens _lpBucket (\s a -> s { _lpBucket = a })

lpKey :: Lens' ListParts Text
lpKey = lens _lpKey (\s a -> s { _lpKey = a })

-- | Sets the maximum number of parts to return.
lpMaxParts :: Lens' ListParts (Maybe Int)
lpMaxParts = lens _lpMaxParts (\s a -> s { _lpMaxParts = a })

-- | Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
lpPartNumberMarker :: Lens' ListParts (Maybe Int)
lpPartNumberMarker =
    lens _lpPartNumberMarker (\s a -> s { _lpPartNumberMarker = a })

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpUploadId :: Lens' ListParts Text
lpUploadId = lens _lpUploadId (\s a -> s { _lpUploadId = a })

instance ToPath ListParts where
    toPath ListParts{..} = mconcat
        [ "/"
        , toText _lpBucket
        , "/"
        , toText _lpKey
        ]

instance ToQuery ListParts where
    toQuery ListParts{..} = mconcat
        [ "max-parts"          =? _lpMaxParts
        , "part-number-marker" =? _lpPartNumberMarker
        , "uploadId"           =? _lpUploadId
        ]

instance ToHeaders ListParts

data ListPartsOutput = ListPartsOutput
    { _lpoBucket               :: Maybe Text
    , _lpoInitiator            :: Maybe Initiator
    , _lpoIsTruncated          :: Maybe Bool
    , _lpoKey                  :: Maybe Text
    , _lpoMaxParts             :: Maybe Int
    , _lpoNextPartNumberMarker :: Maybe Int
    , _lpoOwner                :: Maybe Owner
    , _lpoPartNumberMarker     :: Maybe Int
    , _lpoParts                :: [Part]
    , _lpoStorageClass         :: Maybe Text
    , _lpoUploadId             :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListPartsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpoBucket' @::@ 'Maybe' 'Text'
--
-- * 'lpoInitiator' @::@ 'Maybe' 'Initiator'
--
-- * 'lpoIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lpoKey' @::@ 'Maybe' 'Text'
--
-- * 'lpoMaxParts' @::@ 'Maybe' 'Int'
--
-- * 'lpoNextPartNumberMarker' @::@ 'Maybe' 'Int'
--
-- * 'lpoOwner' @::@ 'Maybe' 'Owner'
--
-- * 'lpoPartNumberMarker' @::@ 'Maybe' 'Int'
--
-- * 'lpoParts' @::@ ['Part']
--
-- * 'lpoStorageClass' @::@ 'Maybe' 'Text'
--
-- * 'lpoUploadId' @::@ 'Maybe' 'Text'
--
listPartsResponse :: ListPartsOutput
listPartsResponse = ListPartsOutput
    { _lpoBucket               = Nothing
    , _lpoKey                  = Nothing
    , _lpoUploadId             = Nothing
    , _lpoPartNumberMarker     = Nothing
    , _lpoNextPartNumberMarker = Nothing
    , _lpoMaxParts             = Nothing
    , _lpoIsTruncated          = Nothing
    , _lpoParts                = mempty
    , _lpoInitiator            = Nothing
    , _lpoOwner                = Nothing
    , _lpoStorageClass         = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
lpoBucket :: Lens' ListPartsOutput (Maybe Text)
lpoBucket = lens _lpoBucket (\s a -> s { _lpoBucket = a })

-- | Identifies who initiated the multipart upload.
lpoInitiator :: Lens' ListPartsOutput (Maybe Initiator)
lpoInitiator = lens _lpoInitiator (\s a -> s { _lpoInitiator = a })

-- | Indicates whether the returned list of parts is truncated.
lpoIsTruncated :: Lens' ListPartsOutput (Maybe Bool)
lpoIsTruncated = lens _lpoIsTruncated (\s a -> s { _lpoIsTruncated = a })

-- | Object key for which the multipart upload was initiated.
lpoKey :: Lens' ListPartsOutput (Maybe Text)
lpoKey = lens _lpoKey (\s a -> s { _lpoKey = a })

-- | Maximum number of parts that were allowed in the response.
lpoMaxParts :: Lens' ListPartsOutput (Maybe Int)
lpoMaxParts = lens _lpoMaxParts (\s a -> s { _lpoMaxParts = a })

-- | When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
lpoNextPartNumberMarker :: Lens' ListPartsOutput (Maybe Int)
lpoNextPartNumberMarker =
    lens _lpoNextPartNumberMarker (\s a -> s { _lpoNextPartNumberMarker = a })

lpoOwner :: Lens' ListPartsOutput (Maybe Owner)
lpoOwner = lens _lpoOwner (\s a -> s { _lpoOwner = a })

-- | Part number after which listing begins.
lpoPartNumberMarker :: Lens' ListPartsOutput (Maybe Int)
lpoPartNumberMarker =
    lens _lpoPartNumberMarker (\s a -> s { _lpoPartNumberMarker = a })

lpoParts :: Lens' ListPartsOutput [Part]
lpoParts = lens _lpoParts (\s a -> s { _lpoParts = a })

-- | The class of storage used to store the object.
lpoStorageClass :: Lens' ListPartsOutput (Maybe Text)
lpoStorageClass = lens _lpoStorageClass (\s a -> s { _lpoStorageClass = a })

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpoUploadId :: Lens' ListPartsOutput (Maybe Text)
lpoUploadId = lens _lpoUploadId (\s a -> s { _lpoUploadId = a })

instance FromXML ListPartsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListPartsOutput"
instance AWSRequest ListParts where
    type Sv ListParts = S3
    type Rs ListParts = ListPartsOutput

    request  = get
    response = xmlResponse $ \h x -> ListPartsOutput
        <$> x %| "Bucket"
        <*> x %| "Initiator"
        <*> x %| "IsTruncated"
        <*> x %| "Key"
        <*> x %| "MaxParts"
        <*> x %| "NextPartNumberMarker"
        <*> x %| "Owner"
        <*> x %| "PartNumberMarker"
        <*> x %| "Part"
        <*> x %| "StorageClass"
        <*> x %| "UploadId"

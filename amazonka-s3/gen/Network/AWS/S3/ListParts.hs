{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , lprBucket
    , lprKey
    , lprMaxParts
    , lprPartNumberMarker
    , lprUploadId

    -- * Response
    , ListPartsOutput
    -- ** Response constructor
    , listPartsOutput
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
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data ListParts = ListParts
    { _lprBucket           :: BucketName
    , _lprKey              :: ObjectKey
    , _lprMaxParts         :: Maybe Int
    , _lprPartNumberMarker :: Maybe Int
    , _lprUploadId         :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListParts' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprBucket' @::@ 'BucketName'
--
-- * 'lprKey' @::@ 'ObjectKey'
--
-- * 'lprMaxParts' @::@ 'Maybe' 'Int'
--
-- * 'lprPartNumberMarker' @::@ 'Maybe' 'Int'
--
-- * 'lprUploadId' @::@ 'Text'
--
listParts :: BucketName -- ^ 'lprBucket'
          -> ObjectKey -- ^ 'lprKey'
          -> Text -- ^ 'lprUploadId'
          -> ListParts
listParts p1 p2 p3 = ListParts
    { _lprBucket           = p1
    , _lprKey              = p2
    , _lprUploadId         = p3
    , _lprMaxParts         = Nothing
    , _lprPartNumberMarker = Nothing
    }

lprBucket :: Lens' ListParts BucketName
lprBucket = lens _lprBucket (\s a -> s { _lprBucket = a })

lprKey :: Lens' ListParts ObjectKey
lprKey = lens _lprKey (\s a -> s { _lprKey = a })

-- | Sets the maximum number of parts to return.
lprMaxParts :: Lens' ListParts (Maybe Int)
lprMaxParts = lens _lprMaxParts (\s a -> s { _lprMaxParts = a })

-- | Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
lprPartNumberMarker :: Lens' ListParts (Maybe Int)
lprPartNumberMarker =
    lens _lprPartNumberMarker (\s a -> s { _lprPartNumberMarker = a })

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprUploadId :: Lens' ListParts Text
lprUploadId = lens _lprUploadId (\s a -> s { _lprUploadId = a })

instance ToPath ListParts where
    toPath ListParts{..} = mconcat
        [ "/"
        , toText _lprBucket
        , "/"
        , toText _lprKey
        ]

instance ToQuery ListParts where
    toQuery ListParts{..} = mconcat
        [ "max-parts"          =? _lprMaxParts
        , "part-number-marker" =? _lprPartNumberMarker
        , "uploadId"           =? _lprUploadId
        ]

instance ToHeaders ListParts

data ListPartsOutput = ListPartsOutput
    { _lpoBucket               :: Maybe BucketName
    , _lpoInitiator            :: Maybe Initiator
    , _lpoIsTruncated          :: Maybe Bool
    , _lpoKey                  :: Maybe ObjectKey
    , _lpoMaxParts             :: Maybe Int
    , _lpoNextPartNumberMarker :: Maybe Int
    , _lpoOwner                :: Maybe Owner
    , _lpoPartNumberMarker     :: Maybe Int
    , _lpoParts                :: [Part]
    , _lpoStorageClass         :: Maybe Text
    , _lpoUploadId             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest ListParts where
    type Sv ListParts = S3
    type Rs ListParts = ListPartsOutput

    request  = get
    response = const . xmlResponse $ \h x ->
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

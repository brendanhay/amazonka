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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListParts.html>
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
    , ListPartsResponse
    -- ** Response constructor
    , listPartsResponse
    -- ** Response lenses
    , lprBucket
    , lprInitiator
    , lprIsTruncated
    , lprKey
    , lprMaxParts
    , lprNextPartNumberMarker
    , lprOwner
    , lprPartNumberMarker
    , lprParts
    , lprStorageClass
    , lprUploadId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data ListParts = ListParts
    { _lpBucket           :: Text
    , _lpKey              :: Text
    , _lpMaxParts         :: Maybe Int
    , _lpPartNumberMarker :: Maybe Int
    , _lpUploadId         :: Text
    } deriving (Eq, Ord, Show)

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

data ListPartsResponse = ListPartsResponse
    { _lprBucket               :: Maybe Text
    , _lprInitiator            :: Maybe Initiator
    , _lprIsTruncated          :: Maybe Bool
    , _lprKey                  :: Maybe Text
    , _lprMaxParts             :: Maybe Int
    , _lprNextPartNumberMarker :: Maybe Int
    , _lprOwner                :: Maybe Owner
    , _lprPartNumberMarker     :: Maybe Int
    , _lprParts                :: List "Part" Part
    , _lprStorageClass         :: Maybe Text
    , _lprUploadId             :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListPartsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprBucket' @::@ 'Maybe' 'Text'
--
-- * 'lprInitiator' @::@ 'Maybe' 'Initiator'
--
-- * 'lprIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lprKey' @::@ 'Maybe' 'Text'
--
-- * 'lprMaxParts' @::@ 'Maybe' 'Int'
--
-- * 'lprNextPartNumberMarker' @::@ 'Maybe' 'Int'
--
-- * 'lprOwner' @::@ 'Maybe' 'Owner'
--
-- * 'lprPartNumberMarker' @::@ 'Maybe' 'Int'
--
-- * 'lprParts' @::@ ['Part']
--
-- * 'lprStorageClass' @::@ 'Maybe' 'Text'
--
-- * 'lprUploadId' @::@ 'Maybe' 'Text'
--
listPartsResponse :: [Part] -- ^ 'lprParts'
                  -> ListPartsResponse
listPartsResponse p1 = ListPartsResponse
    { _lprParts                = withIso _List (const id) p1
    , _lprBucket               = Nothing
    , _lprKey                  = Nothing
    , _lprUploadId             = Nothing
    , _lprPartNumberMarker     = Nothing
    , _lprNextPartNumberMarker = Nothing
    , _lprMaxParts             = Nothing
    , _lprIsTruncated          = Nothing
    , _lprInitiator            = Nothing
    , _lprOwner                = Nothing
    , _lprStorageClass         = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
lprBucket :: Lens' ListPartsResponse (Maybe Text)
lprBucket = lens _lprBucket (\s a -> s { _lprBucket = a })

-- | Identifies who initiated the multipart upload.
lprInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lprInitiator = lens _lprInitiator (\s a -> s { _lprInitiator = a })

-- | Indicates whether the returned list of parts is truncated.
lprIsTruncated :: Lens' ListPartsResponse (Maybe Bool)
lprIsTruncated = lens _lprIsTruncated (\s a -> s { _lprIsTruncated = a })

-- | Object key for which the multipart upload was initiated.
lprKey :: Lens' ListPartsResponse (Maybe Text)
lprKey = lens _lprKey (\s a -> s { _lprKey = a })

-- | Maximum number of parts that were allowed in the response.
lprMaxParts :: Lens' ListPartsResponse (Maybe Int)
lprMaxParts = lens _lprMaxParts (\s a -> s { _lprMaxParts = a })

-- | When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
lprNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprNextPartNumberMarker =
    lens _lprNextPartNumberMarker (\s a -> s { _lprNextPartNumberMarker = a })

lprOwner :: Lens' ListPartsResponse (Maybe Owner)
lprOwner = lens _lprOwner (\s a -> s { _lprOwner = a })

-- | Part number after which listing begins.
lprPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprPartNumberMarker =
    lens _lprPartNumberMarker (\s a -> s { _lprPartNumberMarker = a })

lprParts :: Lens' ListPartsResponse [Part]
lprParts = lens _lprParts (\s a -> s { _lprParts = a }) . _List

-- | The class of storage used to store the object.
lprStorageClass :: Lens' ListPartsResponse (Maybe Text)
lprStorageClass = lens _lprStorageClass (\s a -> s { _lprStorageClass = a })

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprUploadId :: Lens' ListPartsResponse (Maybe Text)
lprUploadId = lens _lprUploadId (\s a -> s { _lprUploadId = a })

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

instance ToXMLRoot ListParts where
    toXMLRoot = const (element "ListParts" [])

instance ToXML ListParts

xml

instance AWSRequest ListParts where
    type Sv ListParts = S3
    type Rs ListParts = ListPartsResponse

    request  = get
    response = xmlResponse

instance FromXML ListPartsResponse where
    parseXML x = ListPartsResponse
        <$> x .@? "Bucket"
        <*> x .@? "Initiator"
        <*> x .@? "IsTruncated"
        <*> x .@? "Key"
        <*> x .@? "MaxParts"
        <*> x .@? "NextPartNumberMarker"
        <*> x .@? "Owner"
        <*> x .@? "PartNumberMarker"
        <*> parseXML x
        <*> x .@? "StorageClass"
        <*> x .@? "UploadId"

instance AWSPager ListParts where
    page rq rs
        | stop (rs ^. lprIsTruncated) = Nothing
        | otherwise = Just $ rq
            & lpPartNumberMarker .~ rs ^. lprNextPartNumberMarker

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , lpMaxParts
    , lpRequestPayer
    , lpPartNumberMarker
    , lpBucket
    , lpKey
    , lpUploadId

    -- * Response
    , ListPartsResponse
    -- ** Response constructor
    , listPartsResponse
    -- ** Response lenses
    , lprParts
    , lprRequestCharged
    , lprMaxParts
    , lprInitiator
    , lprBucket
    , lprNextPartNumberMarker
    , lprOwner
    , lprKey
    , lprStorageClass
    , lprIsTruncated
    , lprPartNumberMarker
    , lprUploadId
    , lprStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'listParts' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpMaxParts'
--
-- * 'lpRequestPayer'
--
-- * 'lpPartNumberMarker'
--
-- * 'lpBucket'
--
-- * 'lpKey'
--
-- * 'lpUploadId'
data ListParts = ListParts'
    { _lpMaxParts         :: !(Maybe Int)
    , _lpRequestPayer     :: !(Maybe RequestPayer)
    , _lpPartNumberMarker :: !(Maybe Int)
    , _lpBucket           :: !BucketName
    , _lpKey              :: !ObjectKey
    , _lpUploadId         :: !Text
    } deriving (Eq,Show)

-- | 'ListParts' smart constructor.
listParts :: BucketName -> ObjectKey -> Text -> ListParts
listParts pBucket pKey pUploadId =
    ListParts'
    { _lpMaxParts = Nothing
    , _lpRequestPayer = Nothing
    , _lpPartNumberMarker = Nothing
    , _lpBucket = pBucket
    , _lpKey = pKey
    , _lpUploadId = pUploadId
    }

-- | Sets the maximum number of parts to return.
lpMaxParts :: Lens' ListParts (Maybe Int)
lpMaxParts = lens _lpMaxParts (\ s a -> s{_lpMaxParts = a});

-- | FIXME: Undocumented member.
lpRequestPayer :: Lens' ListParts (Maybe RequestPayer)
lpRequestPayer = lens _lpRequestPayer (\ s a -> s{_lpRequestPayer = a});

-- | Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
lpPartNumberMarker :: Lens' ListParts (Maybe Int)
lpPartNumberMarker = lens _lpPartNumberMarker (\ s a -> s{_lpPartNumberMarker = a});

-- | FIXME: Undocumented member.
lpBucket :: Lens' ListParts BucketName
lpBucket = lens _lpBucket (\ s a -> s{_lpBucket = a});

-- | FIXME: Undocumented member.
lpKey :: Lens' ListParts ObjectKey
lpKey = lens _lpKey (\ s a -> s{_lpKey = a});

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpUploadId :: Lens' ListParts Text
lpUploadId = lens _lpUploadId (\ s a -> s{_lpUploadId = a});

instance AWSPager ListParts where
        page rq rs
          | stop (rs ^. lprIsTruncated) = Nothing
          | isNothing (rs ^. lprNextPartNumberMarker) = Nothing
          | otherwise =
            Just $ rq &
              lpPartNumberMarker .~ rs ^. lprNextPartNumberMarker

instance AWSRequest ListParts where
        type Sv ListParts = S3
        type Rs ListParts = ListPartsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListPartsResponse' <$>
                   (may (parseXMLList "Part") x) <*>
                     (h .#? "x-amz-request-charged")
                     <*> (x .@? "MaxParts")
                     <*> (x .@? "Initiator")
                     <*> (x .@? "Bucket")
                     <*> (x .@? "NextPartNumberMarker")
                     <*> (x .@? "Owner")
                     <*> (x .@? "Key")
                     <*> (x .@? "StorageClass")
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "PartNumberMarker")
                     <*> (x .@? "UploadId")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListParts where
        toHeaders ListParts'{..}
          = mconcat ["x-amz-request-payer" =# _lpRequestPayer]

instance ToPath ListParts where
        toPath ListParts'{..}
          = mconcat ["/", toText _lpBucket, "/", toText _lpKey]

instance ToQuery ListParts where
        toQuery ListParts'{..}
          = mconcat
              ["max-parts" =: _lpMaxParts,
               "part-number-marker" =: _lpPartNumberMarker,
               "uploadId" =: _lpUploadId]

-- | /See:/ 'listPartsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprParts'
--
-- * 'lprRequestCharged'
--
-- * 'lprMaxParts'
--
-- * 'lprInitiator'
--
-- * 'lprBucket'
--
-- * 'lprNextPartNumberMarker'
--
-- * 'lprOwner'
--
-- * 'lprKey'
--
-- * 'lprStorageClass'
--
-- * 'lprIsTruncated'
--
-- * 'lprPartNumberMarker'
--
-- * 'lprUploadId'
--
-- * 'lprStatus'
data ListPartsResponse = ListPartsResponse'
    { _lprParts                :: !(Maybe [Part])
    , _lprRequestCharged       :: !(Maybe RequestCharged)
    , _lprMaxParts             :: !(Maybe Int)
    , _lprInitiator            :: !(Maybe Initiator)
    , _lprBucket               :: !(Maybe BucketName)
    , _lprNextPartNumberMarker :: !(Maybe Int)
    , _lprOwner                :: !(Maybe Owner)
    , _lprKey                  :: !(Maybe ObjectKey)
    , _lprStorageClass         :: !(Maybe StorageClass)
    , _lprIsTruncated          :: !(Maybe Bool)
    , _lprPartNumberMarker     :: !(Maybe Int)
    , _lprUploadId             :: !(Maybe Text)
    , _lprStatus               :: !Int
    } deriving (Eq,Show)

-- | 'ListPartsResponse' smart constructor.
listPartsResponse :: Int -> ListPartsResponse
listPartsResponse pStatus =
    ListPartsResponse'
    { _lprParts = Nothing
    , _lprRequestCharged = Nothing
    , _lprMaxParts = Nothing
    , _lprInitiator = Nothing
    , _lprBucket = Nothing
    , _lprNextPartNumberMarker = Nothing
    , _lprOwner = Nothing
    , _lprKey = Nothing
    , _lprStorageClass = Nothing
    , _lprIsTruncated = Nothing
    , _lprPartNumberMarker = Nothing
    , _lprUploadId = Nothing
    , _lprStatus = pStatus
    }

-- | FIXME: Undocumented member.
lprParts :: Lens' ListPartsResponse [Part]
lprParts = lens _lprParts (\ s a -> s{_lprParts = a}) . _Default;

-- | FIXME: Undocumented member.
lprRequestCharged :: Lens' ListPartsResponse (Maybe RequestCharged)
lprRequestCharged = lens _lprRequestCharged (\ s a -> s{_lprRequestCharged = a});

-- | Maximum number of parts that were allowed in the response.
lprMaxParts :: Lens' ListPartsResponse (Maybe Int)
lprMaxParts = lens _lprMaxParts (\ s a -> s{_lprMaxParts = a});

-- | Identifies who initiated the multipart upload.
lprInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lprInitiator = lens _lprInitiator (\ s a -> s{_lprInitiator = a});

-- | Name of the bucket to which the multipart upload was initiated.
lprBucket :: Lens' ListPartsResponse (Maybe BucketName)
lprBucket = lens _lprBucket (\ s a -> s{_lprBucket = a});

-- | When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
lprNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprNextPartNumberMarker = lens _lprNextPartNumberMarker (\ s a -> s{_lprNextPartNumberMarker = a});

-- | FIXME: Undocumented member.
lprOwner :: Lens' ListPartsResponse (Maybe Owner)
lprOwner = lens _lprOwner (\ s a -> s{_lprOwner = a});

-- | Object key for which the multipart upload was initiated.
lprKey :: Lens' ListPartsResponse (Maybe ObjectKey)
lprKey = lens _lprKey (\ s a -> s{_lprKey = a});

-- | The class of storage used to store the object.
lprStorageClass :: Lens' ListPartsResponse (Maybe StorageClass)
lprStorageClass = lens _lprStorageClass (\ s a -> s{_lprStorageClass = a});

-- | Indicates whether the returned list of parts is truncated.
lprIsTruncated :: Lens' ListPartsResponse (Maybe Bool)
lprIsTruncated = lens _lprIsTruncated (\ s a -> s{_lprIsTruncated = a});

-- | Part number after which listing begins.
lprPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprPartNumberMarker = lens _lprPartNumberMarker (\ s a -> s{_lprPartNumberMarker = a});

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprUploadId :: Lens' ListPartsResponse (Maybe Text)
lprUploadId = lens _lprUploadId (\ s a -> s{_lprUploadId = a});

-- | FIXME: Undocumented member.
lprStatus :: Lens' ListPartsResponse Int
lprStatus = lens _lprStatus (\ s a -> s{_lprStatus = a});

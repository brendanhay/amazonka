{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the parts that have been uploaded for a specific multipart upload.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListParts.html>
module Network.AWS.S3.ListParts
    (
    -- * Request
      ListParts
    -- ** Request constructor
    , listParts
    -- ** Request lenses
    , lprqMaxParts
    , lprqRequestPayer
    , lprqPartNumberMarker
    , lprqBucket
    , lprqKey
    , lprqUploadId

    -- * Response
    , ListPartsResponse
    -- ** Response constructor
    , listPartsResponse
    -- ** Response lenses
    , lprsParts
    , lprsRequestCharged
    , lprsMaxParts
    , lprsInitiator
    , lprsBucket
    , lprsNextPartNumberMarker
    , lprsOwner
    , lprsKey
    , lprsStorageClass
    , lprsIsTruncated
    , lprsPartNumberMarker
    , lprsUploadId
    , lprsStatus
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
-- * 'lprqMaxParts'
--
-- * 'lprqRequestPayer'
--
-- * 'lprqPartNumberMarker'
--
-- * 'lprqBucket'
--
-- * 'lprqKey'
--
-- * 'lprqUploadId'
data ListParts = ListParts'
    { _lprqMaxParts         :: !(Maybe Int)
    , _lprqRequestPayer     :: !(Maybe RequestPayer)
    , _lprqPartNumberMarker :: !(Maybe Int)
    , _lprqBucket           :: !BucketName
    , _lprqKey              :: !ObjectKey
    , _lprqUploadId         :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListParts' smart constructor.
listParts :: BucketName -> ObjectKey -> Text -> ListParts
listParts pBucket_ pKey_ pUploadId_ =
    ListParts'
    { _lprqMaxParts = Nothing
    , _lprqRequestPayer = Nothing
    , _lprqPartNumberMarker = Nothing
    , _lprqBucket = pBucket_
    , _lprqKey = pKey_
    , _lprqUploadId = pUploadId_
    }

-- | Sets the maximum number of parts to return.
lprqMaxParts :: Lens' ListParts (Maybe Int)
lprqMaxParts = lens _lprqMaxParts (\ s a -> s{_lprqMaxParts = a});

-- | FIXME: Undocumented member.
lprqRequestPayer :: Lens' ListParts (Maybe RequestPayer)
lprqRequestPayer = lens _lprqRequestPayer (\ s a -> s{_lprqRequestPayer = a});

-- | Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
lprqPartNumberMarker :: Lens' ListParts (Maybe Int)
lprqPartNumberMarker = lens _lprqPartNumberMarker (\ s a -> s{_lprqPartNumberMarker = a});

-- | FIXME: Undocumented member.
lprqBucket :: Lens' ListParts BucketName
lprqBucket = lens _lprqBucket (\ s a -> s{_lprqBucket = a});

-- | FIXME: Undocumented member.
lprqKey :: Lens' ListParts ObjectKey
lprqKey = lens _lprqKey (\ s a -> s{_lprqKey = a});

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprqUploadId :: Lens' ListParts Text
lprqUploadId = lens _lprqUploadId (\ s a -> s{_lprqUploadId = a});

instance AWSPager ListParts where
        page rq rs
          | stop (rs ^. lprsIsTruncated) = Nothing
          | isNothing (rs ^. lprsNextPartNumberMarker) =
            Nothing
          | otherwise =
            Just $ rq &
              lprqPartNumberMarker .~
                rs ^. lprsNextPartNumberMarker

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
          = mconcat
              ["x-amz-request-payer" =# _lprqRequestPayer]

instance ToPath ListParts where
        toPath ListParts'{..}
          = mconcat
              ["/", toText _lprqBucket, "/", toText _lprqKey]

instance ToQuery ListParts where
        toQuery ListParts'{..}
          = mconcat
              ["max-parts" =: _lprqMaxParts,
               "part-number-marker" =: _lprqPartNumberMarker,
               "uploadId" =: _lprqUploadId]

-- | /See:/ 'listPartsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprsParts'
--
-- * 'lprsRequestCharged'
--
-- * 'lprsMaxParts'
--
-- * 'lprsInitiator'
--
-- * 'lprsBucket'
--
-- * 'lprsNextPartNumberMarker'
--
-- * 'lprsOwner'
--
-- * 'lprsKey'
--
-- * 'lprsStorageClass'
--
-- * 'lprsIsTruncated'
--
-- * 'lprsPartNumberMarker'
--
-- * 'lprsUploadId'
--
-- * 'lprsStatus'
data ListPartsResponse = ListPartsResponse'
    { _lprsParts                :: !(Maybe [Part])
    , _lprsRequestCharged       :: !(Maybe RequestCharged)
    , _lprsMaxParts             :: !(Maybe Int)
    , _lprsInitiator            :: !(Maybe Initiator)
    , _lprsBucket               :: !(Maybe BucketName)
    , _lprsNextPartNumberMarker :: !(Maybe Int)
    , _lprsOwner                :: !(Maybe Owner)
    , _lprsKey                  :: !(Maybe ObjectKey)
    , _lprsStorageClass         :: !(Maybe StorageClass)
    , _lprsIsTruncated          :: !(Maybe Bool)
    , _lprsPartNumberMarker     :: !(Maybe Int)
    , _lprsUploadId             :: !(Maybe Text)
    , _lprsStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListPartsResponse' smart constructor.
listPartsResponse :: Int -> ListPartsResponse
listPartsResponse pStatus_ =
    ListPartsResponse'
    { _lprsParts = Nothing
    , _lprsRequestCharged = Nothing
    , _lprsMaxParts = Nothing
    , _lprsInitiator = Nothing
    , _lprsBucket = Nothing
    , _lprsNextPartNumberMarker = Nothing
    , _lprsOwner = Nothing
    , _lprsKey = Nothing
    , _lprsStorageClass = Nothing
    , _lprsIsTruncated = Nothing
    , _lprsPartNumberMarker = Nothing
    , _lprsUploadId = Nothing
    , _lprsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
lprsParts :: Lens' ListPartsResponse [Part]
lprsParts = lens _lprsParts (\ s a -> s{_lprsParts = a}) . _Default;

-- | FIXME: Undocumented member.
lprsRequestCharged :: Lens' ListPartsResponse (Maybe RequestCharged)
lprsRequestCharged = lens _lprsRequestCharged (\ s a -> s{_lprsRequestCharged = a});

-- | Maximum number of parts that were allowed in the response.
lprsMaxParts :: Lens' ListPartsResponse (Maybe Int)
lprsMaxParts = lens _lprsMaxParts (\ s a -> s{_lprsMaxParts = a});

-- | Identifies who initiated the multipart upload.
lprsInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lprsInitiator = lens _lprsInitiator (\ s a -> s{_lprsInitiator = a});

-- | Name of the bucket to which the multipart upload was initiated.
lprsBucket :: Lens' ListPartsResponse (Maybe BucketName)
lprsBucket = lens _lprsBucket (\ s a -> s{_lprsBucket = a});

-- | When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
lprsNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprsNextPartNumberMarker = lens _lprsNextPartNumberMarker (\ s a -> s{_lprsNextPartNumberMarker = a});

-- | FIXME: Undocumented member.
lprsOwner :: Lens' ListPartsResponse (Maybe Owner)
lprsOwner = lens _lprsOwner (\ s a -> s{_lprsOwner = a});

-- | Object key for which the multipart upload was initiated.
lprsKey :: Lens' ListPartsResponse (Maybe ObjectKey)
lprsKey = lens _lprsKey (\ s a -> s{_lprsKey = a});

-- | The class of storage used to store the object.
lprsStorageClass :: Lens' ListPartsResponse (Maybe StorageClass)
lprsStorageClass = lens _lprsStorageClass (\ s a -> s{_lprsStorageClass = a});

-- | Indicates whether the returned list of parts is truncated.
lprsIsTruncated :: Lens' ListPartsResponse (Maybe Bool)
lprsIsTruncated = lens _lprsIsTruncated (\ s a -> s{_lprsIsTruncated = a});

-- | Part number after which listing begins.
lprsPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprsPartNumberMarker = lens _lprsPartNumberMarker (\ s a -> s{_lprsPartNumberMarker = a});

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprsUploadId :: Lens' ListPartsResponse (Maybe Text)
lprsUploadId = lens _lprsUploadId (\ s a -> s{_lprsUploadId = a});

-- | FIXME: Undocumented member.
lprsStatus :: Lens' ListPartsResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});

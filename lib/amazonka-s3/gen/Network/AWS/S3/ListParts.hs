{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the parts that have been uploaded for a specific multipart upload.
--
-- This operation returns paginated results.
module Network.AWS.S3.ListParts
    (
    -- * Creating a Request
      listParts
    , ListParts
    -- * Request Lenses
    , lpMaxParts
    , lpRequestPayer
    , lpPartNumberMarker
    , lpBucket
    , lpKey
    , lpUploadId

    -- * Destructuring the Response
    , listPartsResponse
    , ListPartsResponse
    -- * Response Lenses
    , lprsParts
    , lprsRequestCharged
    , lprsMaxParts
    , lprsInitiator
    , lprsBucket
    , lprsAbortDate
    , lprsNextPartNumberMarker
    , lprsAbortRuleId
    , lprsOwner
    , lprsKey
    , lprsStorageClass
    , lprsIsTruncated
    , lprsPartNumberMarker
    , lprsUploadId
    , lprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'listParts' smart constructor.
data ListParts = ListParts'
  { _lpMaxParts         :: !(Maybe Int)
  , _lpRequestPayer     :: !(Maybe RequestPayer)
  , _lpPartNumberMarker :: !(Maybe Int)
  , _lpBucket           :: !BucketName
  , _lpKey              :: !ObjectKey
  , _lpUploadId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListParts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpMaxParts' - Sets the maximum number of parts to return.
--
-- * 'lpRequestPayer' - Undocumented member.
--
-- * 'lpPartNumberMarker' - Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
--
-- * 'lpBucket' - Undocumented member.
--
-- * 'lpKey' - Undocumented member.
--
-- * 'lpUploadId' - Upload ID identifying the multipart upload whose parts are being listed.
listParts
    :: BucketName -- ^ 'lpBucket'
    -> ObjectKey -- ^ 'lpKey'
    -> Text -- ^ 'lpUploadId'
    -> ListParts
listParts pBucket_ pKey_ pUploadId_ =
  ListParts'
    { _lpMaxParts = Nothing
    , _lpRequestPayer = Nothing
    , _lpPartNumberMarker = Nothing
    , _lpBucket = pBucket_
    , _lpKey = pKey_
    , _lpUploadId = pUploadId_
    }


-- | Sets the maximum number of parts to return.
lpMaxParts :: Lens' ListParts (Maybe Int)
lpMaxParts = lens _lpMaxParts (\ s a -> s{_lpMaxParts = a})

-- | Undocumented member.
lpRequestPayer :: Lens' ListParts (Maybe RequestPayer)
lpRequestPayer = lens _lpRequestPayer (\ s a -> s{_lpRequestPayer = a})

-- | Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
lpPartNumberMarker :: Lens' ListParts (Maybe Int)
lpPartNumberMarker = lens _lpPartNumberMarker (\ s a -> s{_lpPartNumberMarker = a})

-- | Undocumented member.
lpBucket :: Lens' ListParts BucketName
lpBucket = lens _lpBucket (\ s a -> s{_lpBucket = a})

-- | Undocumented member.
lpKey :: Lens' ListParts ObjectKey
lpKey = lens _lpKey (\ s a -> s{_lpKey = a})

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpUploadId :: Lens' ListParts Text
lpUploadId = lens _lpUploadId (\ s a -> s{_lpUploadId = a})

instance AWSPager ListParts where
        page rq rs
          | stop (rs ^. lprsIsTruncated) = Nothing
          | isNothing (rs ^. lprsNextPartNumberMarker) =
            Nothing
          | otherwise =
            Just $ rq &
              lpPartNumberMarker .~ rs ^. lprsNextPartNumberMarker

instance AWSRequest ListParts where
        type Rs ListParts = ListPartsResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListPartsResponse' <$>
                   (may (parseXMLList "Part") x) <*>
                     (h .#? "x-amz-request-charged")
                     <*> (x .@? "MaxParts")
                     <*> (x .@? "Initiator")
                     <*> (x .@? "Bucket")
                     <*> (h .#? "x-amz-abort-date")
                     <*> (x .@? "NextPartNumberMarker")
                     <*> (h .#? "x-amz-abort-rule-id")
                     <*> (x .@? "Owner")
                     <*> (x .@? "Key")
                     <*> (x .@? "StorageClass")
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "PartNumberMarker")
                     <*> (x .@? "UploadId")
                     <*> (pure (fromEnum s)))

instance Hashable ListParts where

instance NFData ListParts where

instance ToHeaders ListParts where
        toHeaders ListParts'{..}
          = mconcat ["x-amz-request-payer" =# _lpRequestPayer]

instance ToPath ListParts where
        toPath ListParts'{..}
          = mconcat ["/", toBS _lpBucket, "/", toBS _lpKey]

instance ToQuery ListParts where
        toQuery ListParts'{..}
          = mconcat
              ["max-parts" =: _lpMaxParts,
               "part-number-marker" =: _lpPartNumberMarker,
               "uploadId" =: _lpUploadId]

-- | /See:/ 'listPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { _lprsParts                :: !(Maybe [Part])
  , _lprsRequestCharged       :: !(Maybe RequestCharged)
  , _lprsMaxParts             :: !(Maybe Int)
  , _lprsInitiator            :: !(Maybe Initiator)
  , _lprsBucket               :: !(Maybe BucketName)
  , _lprsAbortDate            :: !(Maybe RFC822)
  , _lprsNextPartNumberMarker :: !(Maybe Int)
  , _lprsAbortRuleId          :: !(Maybe Text)
  , _lprsOwner                :: !(Maybe Owner)
  , _lprsKey                  :: !(Maybe ObjectKey)
  , _lprsStorageClass         :: !(Maybe StorageClass)
  , _lprsIsTruncated          :: !(Maybe Bool)
  , _lprsPartNumberMarker     :: !(Maybe Int)
  , _lprsUploadId             :: !(Maybe Text)
  , _lprsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPartsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsParts' - Undocumented member.
--
-- * 'lprsRequestCharged' - Undocumented member.
--
-- * 'lprsMaxParts' - Maximum number of parts that were allowed in the response.
--
-- * 'lprsInitiator' - Identifies who initiated the multipart upload.
--
-- * 'lprsBucket' - Name of the bucket to which the multipart upload was initiated.
--
-- * 'lprsAbortDate' - Date when multipart upload will become eligible for abort operation by lifecycle.
--
-- * 'lprsNextPartNumberMarker' - When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
--
-- * 'lprsAbortRuleId' - Id of the lifecycle rule that makes a multipart upload eligible for abort operation.
--
-- * 'lprsOwner' - Undocumented member.
--
-- * 'lprsKey' - Object key for which the multipart upload was initiated.
--
-- * 'lprsStorageClass' - The class of storage used to store the object.
--
-- * 'lprsIsTruncated' - Indicates whether the returned list of parts is truncated.
--
-- * 'lprsPartNumberMarker' - Part number after which listing begins.
--
-- * 'lprsUploadId' - Upload ID identifying the multipart upload whose parts are being listed.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPartsResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPartsResponse
listPartsResponse pResponseStatus_ =
  ListPartsResponse'
    { _lprsParts = Nothing
    , _lprsRequestCharged = Nothing
    , _lprsMaxParts = Nothing
    , _lprsInitiator = Nothing
    , _lprsBucket = Nothing
    , _lprsAbortDate = Nothing
    , _lprsNextPartNumberMarker = Nothing
    , _lprsAbortRuleId = Nothing
    , _lprsOwner = Nothing
    , _lprsKey = Nothing
    , _lprsStorageClass = Nothing
    , _lprsIsTruncated = Nothing
    , _lprsPartNumberMarker = Nothing
    , _lprsUploadId = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lprsParts :: Lens' ListPartsResponse [Part]
lprsParts = lens _lprsParts (\ s a -> s{_lprsParts = a}) . _Default . _Coerce

-- | Undocumented member.
lprsRequestCharged :: Lens' ListPartsResponse (Maybe RequestCharged)
lprsRequestCharged = lens _lprsRequestCharged (\ s a -> s{_lprsRequestCharged = a})

-- | Maximum number of parts that were allowed in the response.
lprsMaxParts :: Lens' ListPartsResponse (Maybe Int)
lprsMaxParts = lens _lprsMaxParts (\ s a -> s{_lprsMaxParts = a})

-- | Identifies who initiated the multipart upload.
lprsInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lprsInitiator = lens _lprsInitiator (\ s a -> s{_lprsInitiator = a})

-- | Name of the bucket to which the multipart upload was initiated.
lprsBucket :: Lens' ListPartsResponse (Maybe BucketName)
lprsBucket = lens _lprsBucket (\ s a -> s{_lprsBucket = a})

-- | Date when multipart upload will become eligible for abort operation by lifecycle.
lprsAbortDate :: Lens' ListPartsResponse (Maybe UTCTime)
lprsAbortDate = lens _lprsAbortDate (\ s a -> s{_lprsAbortDate = a}) . mapping _Time

-- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
lprsNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprsNextPartNumberMarker = lens _lprsNextPartNumberMarker (\ s a -> s{_lprsNextPartNumberMarker = a})

-- | Id of the lifecycle rule that makes a multipart upload eligible for abort operation.
lprsAbortRuleId :: Lens' ListPartsResponse (Maybe Text)
lprsAbortRuleId = lens _lprsAbortRuleId (\ s a -> s{_lprsAbortRuleId = a})

-- | Undocumented member.
lprsOwner :: Lens' ListPartsResponse (Maybe Owner)
lprsOwner = lens _lprsOwner (\ s a -> s{_lprsOwner = a})

-- | Object key for which the multipart upload was initiated.
lprsKey :: Lens' ListPartsResponse (Maybe ObjectKey)
lprsKey = lens _lprsKey (\ s a -> s{_lprsKey = a})

-- | The class of storage used to store the object.
lprsStorageClass :: Lens' ListPartsResponse (Maybe StorageClass)
lprsStorageClass = lens _lprsStorageClass (\ s a -> s{_lprsStorageClass = a})

-- | Indicates whether the returned list of parts is truncated.
lprsIsTruncated :: Lens' ListPartsResponse (Maybe Bool)
lprsIsTruncated = lens _lprsIsTruncated (\ s a -> s{_lprsIsTruncated = a})

-- | Part number after which listing begins.
lprsPartNumberMarker :: Lens' ListPartsResponse (Maybe Int)
lprsPartNumberMarker = lens _lprsPartNumberMarker (\ s a -> s{_lprsPartNumberMarker = a})

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprsUploadId :: Lens' ListPartsResponse (Maybe Text)
lprsUploadId = lens _lprsUploadId (\ s a -> s{_lprsUploadId = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPartsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPartsResponse where

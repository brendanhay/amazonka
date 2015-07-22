{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.AbortMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Aborts a multipart upload.
--
-- To verify that all parts have been removed, so you don\'t get charged
-- for the part storage, you should call the List Parts operation and
-- ensure the parts list is empty.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/AbortMultipartUpload.html>
module Network.AWS.S3.AbortMultipartUpload
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , abortMultipartUpload
    -- ** Request lenses
    , amurqRequestPayer
    , amurqBucket
    , amurqKey
    , amurqUploadId

    -- * Response
    , AbortMultipartUploadResponse
    -- ** Response constructor
    , abortMultipartUploadResponse
    -- ** Response lenses
    , amursRequestCharged
    , amursStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'abortMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amurqRequestPayer'
--
-- * 'amurqBucket'
--
-- * 'amurqKey'
--
-- * 'amurqUploadId'
data AbortMultipartUpload = AbortMultipartUpload'
    { _amurqRequestPayer :: !(Maybe RequestPayer)
    , _amurqBucket       :: !BucketName
    , _amurqKey          :: !ObjectKey
    , _amurqUploadId     :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUpload' smart constructor.
abortMultipartUpload :: BucketName -> ObjectKey -> Text -> AbortMultipartUpload
abortMultipartUpload pBucket_ pKey_ pUploadId_ =
    AbortMultipartUpload'
    { _amurqRequestPayer = Nothing
    , _amurqBucket = pBucket_
    , _amurqKey = pKey_
    , _amurqUploadId = pUploadId_
    }

-- | FIXME: Undocumented member.
amurqRequestPayer :: Lens' AbortMultipartUpload (Maybe RequestPayer)
amurqRequestPayer = lens _amurqRequestPayer (\ s a -> s{_amurqRequestPayer = a});

-- | FIXME: Undocumented member.
amurqBucket :: Lens' AbortMultipartUpload BucketName
amurqBucket = lens _amurqBucket (\ s a -> s{_amurqBucket = a});

-- | FIXME: Undocumented member.
amurqKey :: Lens' AbortMultipartUpload ObjectKey
amurqKey = lens _amurqKey (\ s a -> s{_amurqKey = a});

-- | FIXME: Undocumented member.
amurqUploadId :: Lens' AbortMultipartUpload Text
amurqUploadId = lens _amurqUploadId (\ s a -> s{_amurqUploadId = a});

instance AWSRequest AbortMultipartUpload where
        type Sv AbortMultipartUpload = S3
        type Rs AbortMultipartUpload =
             AbortMultipartUploadResponse
        request = delete
        response
          = receiveXML
              (\ s h x ->
                 AbortMultipartUploadResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s)))

instance ToHeaders AbortMultipartUpload where
        toHeaders AbortMultipartUpload'{..}
          = mconcat
              ["x-amz-request-payer" =# _amurqRequestPayer]

instance ToPath AbortMultipartUpload where
        toPath AbortMultipartUpload'{..}
          = mconcat
              ["/", toText _amurqBucket, "/", toText _amurqKey]

instance ToQuery AbortMultipartUpload where
        toQuery AbortMultipartUpload'{..}
          = mconcat ["uploadId" =: _amurqUploadId]

-- | /See:/ 'abortMultipartUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amursRequestCharged'
--
-- * 'amursStatus'
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
    { _amursRequestCharged :: !(Maybe RequestCharged)
    , _amursStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUploadResponse' smart constructor.
abortMultipartUploadResponse :: Int -> AbortMultipartUploadResponse
abortMultipartUploadResponse pStatus_ =
    AbortMultipartUploadResponse'
    { _amursRequestCharged = Nothing
    , _amursStatus = pStatus_
    }

-- | FIXME: Undocumented member.
amursRequestCharged :: Lens' AbortMultipartUploadResponse (Maybe RequestCharged)
amursRequestCharged = lens _amursRequestCharged (\ s a -> s{_amursRequestCharged = a});

-- | FIXME: Undocumented member.
amursStatus :: Lens' AbortMultipartUploadResponse Int
amursStatus = lens _amursStatus (\ s a -> s{_amursStatus = a});

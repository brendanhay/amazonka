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
    , amuRequestPayer
    , amuBucket
    , amuKey
    , amuUploadId

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
-- * 'amuRequestPayer'
--
-- * 'amuBucket'
--
-- * 'amuKey'
--
-- * 'amuUploadId'
data AbortMultipartUpload = AbortMultipartUpload'
    { _amuRequestPayer :: !(Maybe RequestPayer)
    , _amuBucket       :: !BucketName
    , _amuKey          :: !ObjectKey
    , _amuUploadId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUpload' smart constructor.
abortMultipartUpload :: BucketName -> ObjectKey -> Text -> AbortMultipartUpload
abortMultipartUpload pBucket_ pKey_ pUploadId_ =
    AbortMultipartUpload'
    { _amuRequestPayer = Nothing
    , _amuBucket = pBucket_
    , _amuKey = pKey_
    , _amuUploadId = pUploadId_
    }

-- | FIXME: Undocumented member.
amuRequestPayer :: Lens' AbortMultipartUpload (Maybe RequestPayer)
amuRequestPayer = lens _amuRequestPayer (\ s a -> s{_amuRequestPayer = a});

-- | FIXME: Undocumented member.
amuBucket :: Lens' AbortMultipartUpload BucketName
amuBucket = lens _amuBucket (\ s a -> s{_amuBucket = a});

-- | FIXME: Undocumented member.
amuKey :: Lens' AbortMultipartUpload ObjectKey
amuKey = lens _amuKey (\ s a -> s{_amuKey = a});

-- | FIXME: Undocumented member.
amuUploadId :: Lens' AbortMultipartUpload Text
amuUploadId = lens _amuUploadId (\ s a -> s{_amuUploadId = a});

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
          = mconcat ["x-amz-request-payer" =# _amuRequestPayer]

instance ToPath AbortMultipartUpload where
        toPath AbortMultipartUpload'{..}
          = [toBS _amuBucket, toBS _amuKey]

instance ToQuery AbortMultipartUpload where
        toQuery AbortMultipartUpload'{..}
          = mconcat ["uploadId" =: _amuUploadId]

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

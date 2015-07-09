{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , amurRequestCharged
    , amurStatus
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
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUpload' smart constructor.
abortMultipartUpload :: BucketName -> ObjectKey -> Text -> AbortMultipartUpload
abortMultipartUpload pBucket pKey pUploadId =
    AbortMultipartUpload'
    { _amuRequestPayer = Nothing
    , _amuBucket = pBucket
    , _amuKey = pKey
    , _amuUploadId = pUploadId
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
          = mconcat
              ["/", toText _amuBucket, "/", toText _amuKey]

instance ToQuery AbortMultipartUpload where
        toQuery AbortMultipartUpload'{..}
          = mconcat ["uploadId" =: _amuUploadId]

-- | /See:/ 'abortMultipartUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amurRequestCharged'
--
-- * 'amurStatus'
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
    { _amurRequestCharged :: !(Maybe RequestCharged)
    , _amurStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortMultipartUploadResponse' smart constructor.
abortMultipartUploadResponse :: Int -> AbortMultipartUploadResponse
abortMultipartUploadResponse pStatus =
    AbortMultipartUploadResponse'
    { _amurRequestCharged = Nothing
    , _amurStatus = pStatus
    }

-- | FIXME: Undocumented member.
amurRequestCharged :: Lens' AbortMultipartUploadResponse (Maybe RequestCharged)
amurRequestCharged = lens _amurRequestCharged (\ s a -> s{_amurRequestCharged = a});

-- | FIXME: Undocumented member.
amurStatus :: Lens' AbortMultipartUploadResponse Int
amurStatus = lens _amurStatus (\ s a -> s{_amurStatus = a});

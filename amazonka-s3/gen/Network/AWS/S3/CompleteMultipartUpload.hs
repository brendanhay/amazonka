{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Completes a multipart upload by assembling previously uploaded parts.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CompleteMultipartUpload.html>
module Network.AWS.S3.CompleteMultipartUpload
    (
    -- * Request
      CompleteMultipartUpload
    -- ** Request constructor
    , completeMultipartUpload
    -- ** Request lenses
    , comRequestPayer
    , comMultipartUpload
    , comBucket
    , comKey
    , comUploadId

    -- * Response
    , CompleteMultipartUploadResponse
    -- ** Response constructor
    , completeMultipartUploadResponse
    -- ** Response lenses
    , cVersionId
    , cETag
    , cRequestCharged
    , cLocation
    , cExpiration
    , cBucket
    , cKey
    , cSSEKMSKeyId
    , cServerSideEncryption
    , cStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'completeMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'comRequestPayer'
--
-- * 'comMultipartUpload'
--
-- * 'comBucket'
--
-- * 'comKey'
--
-- * 'comUploadId'
data CompleteMultipartUpload = CompleteMultipartUpload'
    { _comRequestPayer    :: !(Maybe RequestPayer)
    , _comMultipartUpload :: !(Maybe CompletedMultipartUpload)
    , _comBucket          :: !BucketName
    , _comKey             :: !ObjectKey
    , _comUploadId        :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CompleteMultipartUpload' smart constructor.
completeMultipartUpload :: BucketName -> ObjectKey -> Text -> CompleteMultipartUpload
completeMultipartUpload pBucket pKey pUploadId =
    CompleteMultipartUpload'
    { _comRequestPayer = Nothing
    , _comMultipartUpload = Nothing
    , _comBucket = pBucket
    , _comKey = pKey
    , _comUploadId = pUploadId
    }

-- | FIXME: Undocumented member.
comRequestPayer :: Lens' CompleteMultipartUpload (Maybe RequestPayer)
comRequestPayer = lens _comRequestPayer (\ s a -> s{_comRequestPayer = a});

-- | FIXME: Undocumented member.
comMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
comMultipartUpload = lens _comMultipartUpload (\ s a -> s{_comMultipartUpload = a});

-- | FIXME: Undocumented member.
comBucket :: Lens' CompleteMultipartUpload BucketName
comBucket = lens _comBucket (\ s a -> s{_comBucket = a});

-- | FIXME: Undocumented member.
comKey :: Lens' CompleteMultipartUpload ObjectKey
comKey = lens _comKey (\ s a -> s{_comKey = a});

-- | FIXME: Undocumented member.
comUploadId :: Lens' CompleteMultipartUpload Text
comUploadId = lens _comUploadId (\ s a -> s{_comUploadId = a});

instance AWSRequest CompleteMultipartUpload where
        type Sv CompleteMultipartUpload = S3
        type Rs CompleteMultipartUpload =
             CompleteMultipartUploadResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CompleteMultipartUploadResponse' <$>
                   (h .#? "x-amz-version-id") <*> (x .@? "ETag") <*>
                     (h .#? "x-amz-request-charged")
                     <*> (x .@? "Location")
                     <*> (h .#? "x-amz-expiration")
                     <*> (x .@? "Bucket")
                     <*> (x .@? "Key")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance ToElement CompleteMultipartUpload where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}CompleteMultipartUpload"
              .
              _comMultipartUpload

instance ToHeaders CompleteMultipartUpload where
        toHeaders CompleteMultipartUpload'{..}
          = mconcat ["x-amz-request-payer" =# _comRequestPayer]

instance ToPath CompleteMultipartUpload where
        toPath CompleteMultipartUpload'{..}
          = mconcat
              ["/", toText _comBucket, "/", toText _comKey]

instance ToQuery CompleteMultipartUpload where
        toQuery CompleteMultipartUpload'{..}
          = mconcat ["uploadId" =: _comUploadId]

-- | /See:/ 'completeMultipartUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cVersionId'
--
-- * 'cETag'
--
-- * 'cRequestCharged'
--
-- * 'cLocation'
--
-- * 'cExpiration'
--
-- * 'cBucket'
--
-- * 'cKey'
--
-- * 'cSSEKMSKeyId'
--
-- * 'cServerSideEncryption'
--
-- * 'cStatus'
data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse'
    { _cVersionId            :: !(Maybe ObjectVersionId)
    , _cETag                 :: !(Maybe ETag)
    , _cRequestCharged       :: !(Maybe RequestCharged)
    , _cLocation             :: !(Maybe Text)
    , _cExpiration           :: !(Maybe Text)
    , _cBucket               :: !(Maybe BucketName)
    , _cKey                  :: !(Maybe ObjectKey)
    , _cSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _cServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _cStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CompleteMultipartUploadResponse' smart constructor.
completeMultipartUploadResponse :: Int -> CompleteMultipartUploadResponse
completeMultipartUploadResponse pStatus =
    CompleteMultipartUploadResponse'
    { _cVersionId = Nothing
    , _cETag = Nothing
    , _cRequestCharged = Nothing
    , _cLocation = Nothing
    , _cExpiration = Nothing
    , _cBucket = Nothing
    , _cKey = Nothing
    , _cSSEKMSKeyId = Nothing
    , _cServerSideEncryption = Nothing
    , _cStatus = pStatus
    }

-- | Version of the object.
cVersionId :: Lens' CompleteMultipartUploadResponse (Maybe ObjectVersionId)
cVersionId = lens _cVersionId (\ s a -> s{_cVersionId = a});

-- | Entity tag of the object.
cETag :: Lens' CompleteMultipartUploadResponse (Maybe ETag)
cETag = lens _cETag (\ s a -> s{_cETag = a});

-- | FIXME: Undocumented member.
cRequestCharged :: Lens' CompleteMultipartUploadResponse (Maybe RequestCharged)
cRequestCharged = lens _cRequestCharged (\ s a -> s{_cRequestCharged = a});

-- | FIXME: Undocumented member.
cLocation :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cLocation = lens _cLocation (\ s a -> s{_cLocation = a});

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
cExpiration :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cExpiration = lens _cExpiration (\ s a -> s{_cExpiration = a});

-- | FIXME: Undocumented member.
cBucket :: Lens' CompleteMultipartUploadResponse (Maybe BucketName)
cBucket = lens _cBucket (\ s a -> s{_cBucket = a});

-- | FIXME: Undocumented member.
cKey :: Lens' CompleteMultipartUploadResponse (Maybe ObjectKey)
cKey = lens _cKey (\ s a -> s{_cKey = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
cSSEKMSKeyId :: Lens' CompleteMultipartUploadResponse (Maybe Text)
cSSEKMSKeyId = lens _cSSEKMSKeyId (\ s a -> s{_cSSEKMSKeyId = a}) . mapping _Sensitive;

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
cServerSideEncryption = lens _cServerSideEncryption (\ s a -> s{_cServerSideEncryption = a});

-- | FIXME: Undocumented member.
cStatus :: Lens' CompleteMultipartUploadResponse Int
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Completes a multipart upload by assembling previously uploaded parts.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CompleteMultipartUpload.html>
module Network.AWS.S3.CompleteMultipartUpload
    (
    -- * Request
      CompleteMultipartUpload
    -- ** Request constructor
    , completeMultipartUpload
    -- ** Request lenses
    , crqRequestPayer
    , crqMultipartUpload
    , crqBucket
    , crqKey
    , crqUploadId

    -- * Response
    , CompleteMultipartUploadResponse
    -- ** Response constructor
    , completeMultipartUploadResponse
    -- ** Response lenses
    , crsVersionId
    , crsETag
    , crsRequestCharged
    , crsLocation
    , crsExpiration
    , crsBucket
    , crsKey
    , crsSSEKMSKeyId
    , crsServerSideEncryption
    , crsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'completeMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crqRequestPayer'
--
-- * 'crqMultipartUpload'
--
-- * 'crqBucket'
--
-- * 'crqKey'
--
-- * 'crqUploadId'
data CompleteMultipartUpload = CompleteMultipartUpload'
    { _crqRequestPayer    :: !(Maybe RequestPayer)
    , _crqMultipartUpload :: !(Maybe CompletedMultipartUpload)
    , _crqBucket          :: !BucketName
    , _crqKey             :: !ObjectKey
    , _crqUploadId        :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CompleteMultipartUpload' smart constructor.
completeMultipartUpload :: BucketName -> ObjectKey -> Text -> CompleteMultipartUpload
completeMultipartUpload pBucket pKey pUploadId =
    CompleteMultipartUpload'
    { _crqRequestPayer = Nothing
    , _crqMultipartUpload = Nothing
    , _crqBucket = pBucket
    , _crqKey = pKey
    , _crqUploadId = pUploadId
    }

-- | FIXME: Undocumented member.
crqRequestPayer :: Lens' CompleteMultipartUpload (Maybe RequestPayer)
crqRequestPayer = lens _crqRequestPayer (\ s a -> s{_crqRequestPayer = a});

-- | FIXME: Undocumented member.
crqMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
crqMultipartUpload = lens _crqMultipartUpload (\ s a -> s{_crqMultipartUpload = a});

-- | FIXME: Undocumented member.
crqBucket :: Lens' CompleteMultipartUpload BucketName
crqBucket = lens _crqBucket (\ s a -> s{_crqBucket = a});

-- | FIXME: Undocumented member.
crqKey :: Lens' CompleteMultipartUpload ObjectKey
crqKey = lens _crqKey (\ s a -> s{_crqKey = a});

-- | FIXME: Undocumented member.
crqUploadId :: Lens' CompleteMultipartUpload Text
crqUploadId = lens _crqUploadId (\ s a -> s{_crqUploadId = a});

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
              _crqMultipartUpload

instance ToHeaders CompleteMultipartUpload where
        toHeaders CompleteMultipartUpload'{..}
          = mconcat ["x-amz-request-payer" =# _crqRequestPayer]

instance ToPath CompleteMultipartUpload where
        toPath CompleteMultipartUpload'{..}
          = mconcat
              ["/", toText _crqBucket, "/", toText _crqKey]

instance ToQuery CompleteMultipartUpload where
        toQuery CompleteMultipartUpload'{..}
          = mconcat ["uploadId" =: _crqUploadId]

-- | /See:/ 'completeMultipartUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crsVersionId'
--
-- * 'crsETag'
--
-- * 'crsRequestCharged'
--
-- * 'crsLocation'
--
-- * 'crsExpiration'
--
-- * 'crsBucket'
--
-- * 'crsKey'
--
-- * 'crsSSEKMSKeyId'
--
-- * 'crsServerSideEncryption'
--
-- * 'crsStatus'
data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse'
    { _crsVersionId            :: !(Maybe ObjectVersionId)
    , _crsETag                 :: !(Maybe ETag)
    , _crsRequestCharged       :: !(Maybe RequestCharged)
    , _crsLocation             :: !(Maybe Text)
    , _crsExpiration           :: !(Maybe Text)
    , _crsBucket               :: !(Maybe BucketName)
    , _crsKey                  :: !(Maybe ObjectKey)
    , _crsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _crsServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _crsStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CompleteMultipartUploadResponse' smart constructor.
completeMultipartUploadResponse :: Int -> CompleteMultipartUploadResponse
completeMultipartUploadResponse pStatus =
    CompleteMultipartUploadResponse'
    { _crsVersionId = Nothing
    , _crsETag = Nothing
    , _crsRequestCharged = Nothing
    , _crsLocation = Nothing
    , _crsExpiration = Nothing
    , _crsBucket = Nothing
    , _crsKey = Nothing
    , _crsSSEKMSKeyId = Nothing
    , _crsServerSideEncryption = Nothing
    , _crsStatus = pStatus
    }

-- | Version of the object.
crsVersionId :: Lens' CompleteMultipartUploadResponse (Maybe ObjectVersionId)
crsVersionId = lens _crsVersionId (\ s a -> s{_crsVersionId = a});

-- | Entity tag of the object.
crsETag :: Lens' CompleteMultipartUploadResponse (Maybe ETag)
crsETag = lens _crsETag (\ s a -> s{_crsETag = a});

-- | FIXME: Undocumented member.
crsRequestCharged :: Lens' CompleteMultipartUploadResponse (Maybe RequestCharged)
crsRequestCharged = lens _crsRequestCharged (\ s a -> s{_crsRequestCharged = a});

-- | FIXME: Undocumented member.
crsLocation :: Lens' CompleteMultipartUploadResponse (Maybe Text)
crsLocation = lens _crsLocation (\ s a -> s{_crsLocation = a});

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
crsExpiration :: Lens' CompleteMultipartUploadResponse (Maybe Text)
crsExpiration = lens _crsExpiration (\ s a -> s{_crsExpiration = a});

-- | FIXME: Undocumented member.
crsBucket :: Lens' CompleteMultipartUploadResponse (Maybe BucketName)
crsBucket = lens _crsBucket (\ s a -> s{_crsBucket = a});

-- | FIXME: Undocumented member.
crsKey :: Lens' CompleteMultipartUploadResponse (Maybe ObjectKey)
crsKey = lens _crsKey (\ s a -> s{_crsKey = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
crsSSEKMSKeyId :: Lens' CompleteMultipartUploadResponse (Maybe Text)
crsSSEKMSKeyId = lens _crsSSEKMSKeyId (\ s a -> s{_crsSSEKMSKeyId = a}) . mapping _Sensitive;

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
crsServerSideEncryption :: Lens' CompleteMultipartUploadResponse (Maybe ServerSideEncryption)
crsServerSideEncryption = lens _crsServerSideEncryption (\ s a -> s{_crsServerSideEncryption = a});

-- | FIXME: Undocumented member.
crsStatus :: Lens' CompleteMultipartUploadResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});

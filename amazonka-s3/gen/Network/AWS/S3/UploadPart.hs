{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPart
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Uploads a part in a multipart upload.
--
-- __Note:__ After you initiate multipart upload and upload one or more
-- parts, you must either complete or abort multipart upload in order to
-- stop getting charged for storage of the uploaded parts. Only after you
-- either complete or abort multipart upload, Amazon S3 frees up the parts
-- storage and stops charging you for the parts storage.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/UploadPart.html>
module Network.AWS.S3.UploadPart
    (
    -- * Request
      UploadPart
    -- ** Request constructor
    , uploadPart
    -- ** Request lenses
    , upContentLength
    , upSSECustomerAlgorithm
    , upSSECustomerKey
    , upRequestPayer
    , upSSECustomerKeyMD5
    , upContentMD5
    , upBucket
    , upKey
    , upPartNumber
    , upUploadId
    , upBody

    -- * Response
    , UploadPartResponse
    -- ** Response constructor
    , uploadPartResponse
    -- ** Response lenses
    , uprETag
    , uprRequestCharged
    , uprSSECustomerAlgorithm
    , uprSSEKMSKeyId
    , uprSSECustomerKeyMD5
    , uprServerSideEncryption
    , uprStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'uploadPart' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upContentLength'
--
-- * 'upSSECustomerAlgorithm'
--
-- * 'upSSECustomerKey'
--
-- * 'upRequestPayer'
--
-- * 'upSSECustomerKeyMD5'
--
-- * 'upContentMD5'
--
-- * 'upBucket'
--
-- * 'upKey'
--
-- * 'upPartNumber'
--
-- * 'upUploadId'
--
-- * 'upBody'
data UploadPart = UploadPart'
    { _upContentLength        :: !(Maybe Int)
    , _upSSECustomerAlgorithm :: !(Maybe Text)
    , _upSSECustomerKey       :: !(Maybe (Sensitive Text))
    , _upRequestPayer         :: !(Maybe RequestPayer)
    , _upSSECustomerKeyMD5    :: !(Maybe Text)
    , _upContentMD5           :: !(Maybe Text)
    , _upBucket               :: !BucketName
    , _upKey                  :: !ObjectKey
    , _upPartNumber           :: !Int
    , _upUploadId             :: !Text
    , _upBody                 :: !RqBody
    } deriving (Show,Generic)

-- | 'UploadPart' smart constructor.
uploadPart :: BucketName -> ObjectKey -> Int -> Text -> RqBody -> UploadPart
uploadPart pBucket pKey pPartNumber pUploadId pBody =
    UploadPart'
    { _upContentLength = Nothing
    , _upSSECustomerAlgorithm = Nothing
    , _upSSECustomerKey = Nothing
    , _upRequestPayer = Nothing
    , _upSSECustomerKeyMD5 = Nothing
    , _upContentMD5 = Nothing
    , _upBucket = pBucket
    , _upKey = pKey
    , _upPartNumber = pPartNumber
    , _upUploadId = pUploadId
    , _upBody = pBody
    }

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
upContentLength :: Lens' UploadPart (Maybe Int)
upContentLength = lens _upContentLength (\ s a -> s{_upContentLength = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
upSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
upSSECustomerAlgorithm = lens _upSSECustomerAlgorithm (\ s a -> s{_upSSECustomerAlgorithm = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
upSSECustomerKey :: Lens' UploadPart (Maybe Text)
upSSECustomerKey = lens _upSSECustomerKey (\ s a -> s{_upSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
upRequestPayer :: Lens' UploadPart (Maybe RequestPayer)
upRequestPayer = lens _upRequestPayer (\ s a -> s{_upRequestPayer = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
upSSECustomerKeyMD5 = lens _upSSECustomerKeyMD5 (\ s a -> s{_upSSECustomerKeyMD5 = a});

-- | FIXME: Undocumented member.
upContentMD5 :: Lens' UploadPart (Maybe Text)
upContentMD5 = lens _upContentMD5 (\ s a -> s{_upContentMD5 = a});

-- | FIXME: Undocumented member.
upBucket :: Lens' UploadPart BucketName
upBucket = lens _upBucket (\ s a -> s{_upBucket = a});

-- | FIXME: Undocumented member.
upKey :: Lens' UploadPart ObjectKey
upKey = lens _upKey (\ s a -> s{_upKey = a});

-- | Part number of part being uploaded.
upPartNumber :: Lens' UploadPart Int
upPartNumber = lens _upPartNumber (\ s a -> s{_upPartNumber = a});

-- | Upload ID identifying the multipart upload whose part is being uploaded.
upUploadId :: Lens' UploadPart Text
upUploadId = lens _upUploadId (\ s a -> s{_upUploadId = a});

-- | FIXME: Undocumented member.
upBody :: Lens' UploadPart RqBody
upBody = lens _upBody (\ s a -> s{_upBody = a});

instance AWSRequest UploadPart where
        type Sv UploadPart = S3
        type Rs UploadPart = UploadPartResponse
        request = putBody
        response
          = receiveXML
              (\ s h x ->
                 UploadPartResponse' <$>
                   (h .#? "ETag") <*> (h .#? "x-amz-request-charged")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance ToBody UploadPart where
        toBody = _upBody

instance ToHeaders UploadPart where
        toHeaders UploadPart'{..}
          = mconcat
              ["Content-Length" =# _upContentLength,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _upSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _upSSECustomerKey,
               "x-amz-request-payer" =# _upRequestPayer,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _upSSECustomerKeyMD5,
               "Content-MD5" =# _upContentMD5]

instance ToPath UploadPart where
        toPath UploadPart'{..}
          = mconcat ["/", toText _upBucket, "/", toText _upKey]

instance ToQuery UploadPart where
        toQuery UploadPart'{..}
          = mconcat
              ["partNumber" =: _upPartNumber,
               "uploadId" =: _upUploadId]

-- | /See:/ 'uploadPartResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uprETag'
--
-- * 'uprRequestCharged'
--
-- * 'uprSSECustomerAlgorithm'
--
-- * 'uprSSEKMSKeyId'
--
-- * 'uprSSECustomerKeyMD5'
--
-- * 'uprServerSideEncryption'
--
-- * 'uprStatus'
data UploadPartResponse = UploadPartResponse'
    { _uprETag                 :: !(Maybe ETag)
    , _uprRequestCharged       :: !(Maybe RequestCharged)
    , _uprSSECustomerAlgorithm :: !(Maybe Text)
    , _uprSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _uprSSECustomerKeyMD5    :: !(Maybe Text)
    , _uprServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _uprStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UploadPartResponse' smart constructor.
uploadPartResponse :: Int -> UploadPartResponse
uploadPartResponse pStatus =
    UploadPartResponse'
    { _uprETag = Nothing
    , _uprRequestCharged = Nothing
    , _uprSSECustomerAlgorithm = Nothing
    , _uprSSEKMSKeyId = Nothing
    , _uprSSECustomerKeyMD5 = Nothing
    , _uprServerSideEncryption = Nothing
    , _uprStatus = pStatus
    }

-- | Entity tag for the uploaded object.
uprETag :: Lens' UploadPartResponse (Maybe ETag)
uprETag = lens _uprETag (\ s a -> s{_uprETag = a});

-- | FIXME: Undocumented member.
uprRequestCharged :: Lens' UploadPartResponse (Maybe RequestCharged)
uprRequestCharged = lens _uprRequestCharged (\ s a -> s{_uprRequestCharged = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
uprSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
uprSSECustomerAlgorithm = lens _uprSSECustomerAlgorithm (\ s a -> s{_uprSSECustomerAlgorithm = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
uprSSEKMSKeyId :: Lens' UploadPartResponse (Maybe Text)
uprSSEKMSKeyId = lens _uprSSEKMSKeyId (\ s a -> s{_uprSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
uprSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
uprSSECustomerKeyMD5 = lens _uprSSECustomerKeyMD5 (\ s a -> s{_uprSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
uprServerSideEncryption :: Lens' UploadPartResponse (Maybe ServerSideEncryption)
uprServerSideEncryption = lens _uprServerSideEncryption (\ s a -> s{_uprServerSideEncryption = a});

-- | FIXME: Undocumented member.
uprStatus :: Lens' UploadPartResponse Int
uprStatus = lens _uprStatus (\ s a -> s{_uprStatus = a});

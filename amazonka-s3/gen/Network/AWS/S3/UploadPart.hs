{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPart
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part in a multipart upload.
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
    , uprqContentLength
    , uprqSSECustomerAlgorithm
    , uprqSSECustomerKey
    , uprqRequestPayer
    , uprqSSECustomerKeyMD5
    , uprqContentMD5
    , uprqBucket
    , uprqKey
    , uprqPartNumber
    , uprqUploadId
    , uprqBody

    -- * Response
    , UploadPartResponse
    -- ** Response constructor
    , uploadPartResponse
    -- ** Response lenses
    , uprsETag
    , uprsRequestCharged
    , uprsSSECustomerAlgorithm
    , uprsSSEKMSKeyId
    , uprsSSECustomerKeyMD5
    , uprsServerSideEncryption
    , uprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'uploadPart' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uprqContentLength'
--
-- * 'uprqSSECustomerAlgorithm'
--
-- * 'uprqSSECustomerKey'
--
-- * 'uprqRequestPayer'
--
-- * 'uprqSSECustomerKeyMD5'
--
-- * 'uprqContentMD5'
--
-- * 'uprqBucket'
--
-- * 'uprqKey'
--
-- * 'uprqPartNumber'
--
-- * 'uprqUploadId'
--
-- * 'uprqBody'
data UploadPart = UploadPart'
    { _uprqContentLength        :: !(Maybe Int)
    , _uprqSSECustomerAlgorithm :: !(Maybe Text)
    , _uprqSSECustomerKey       :: !(Maybe (Sensitive Text))
    , _uprqRequestPayer         :: !(Maybe RequestPayer)
    , _uprqSSECustomerKeyMD5    :: !(Maybe Text)
    , _uprqContentMD5           :: !(Maybe Text)
    , _uprqBucket               :: !BucketName
    , _uprqKey                  :: !ObjectKey
    , _uprqPartNumber           :: !Int
    , _uprqUploadId             :: !Text
    , _uprqBody                 :: !RqBody
    } deriving (Show,Generic)

-- | 'UploadPart' smart constructor.
uploadPart :: BucketName -> ObjectKey -> Int -> Text -> RqBody -> UploadPart
uploadPart pBucket pKey pPartNumber pUploadId pBody =
    UploadPart'
    { _uprqContentLength = Nothing
    , _uprqSSECustomerAlgorithm = Nothing
    , _uprqSSECustomerKey = Nothing
    , _uprqRequestPayer = Nothing
    , _uprqSSECustomerKeyMD5 = Nothing
    , _uprqContentMD5 = Nothing
    , _uprqBucket = pBucket
    , _uprqKey = pKey
    , _uprqPartNumber = pPartNumber
    , _uprqUploadId = pUploadId
    , _uprqBody = pBody
    }

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
uprqContentLength :: Lens' UploadPart (Maybe Int)
uprqContentLength = lens _uprqContentLength (\ s a -> s{_uprqContentLength = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
uprqSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
uprqSSECustomerAlgorithm = lens _uprqSSECustomerAlgorithm (\ s a -> s{_uprqSSECustomerAlgorithm = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
uprqSSECustomerKey :: Lens' UploadPart (Maybe Text)
uprqSSECustomerKey = lens _uprqSSECustomerKey (\ s a -> s{_uprqSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
uprqRequestPayer :: Lens' UploadPart (Maybe RequestPayer)
uprqRequestPayer = lens _uprqRequestPayer (\ s a -> s{_uprqRequestPayer = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
uprqSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
uprqSSECustomerKeyMD5 = lens _uprqSSECustomerKeyMD5 (\ s a -> s{_uprqSSECustomerKeyMD5 = a});

-- | FIXME: Undocumented member.
uprqContentMD5 :: Lens' UploadPart (Maybe Text)
uprqContentMD5 = lens _uprqContentMD5 (\ s a -> s{_uprqContentMD5 = a});

-- | FIXME: Undocumented member.
uprqBucket :: Lens' UploadPart BucketName
uprqBucket = lens _uprqBucket (\ s a -> s{_uprqBucket = a});

-- | FIXME: Undocumented member.
uprqKey :: Lens' UploadPart ObjectKey
uprqKey = lens _uprqKey (\ s a -> s{_uprqKey = a});

-- | Part number of part being uploaded.
uprqPartNumber :: Lens' UploadPart Int
uprqPartNumber = lens _uprqPartNumber (\ s a -> s{_uprqPartNumber = a});

-- | Upload ID identifying the multipart upload whose part is being uploaded.
uprqUploadId :: Lens' UploadPart Text
uprqUploadId = lens _uprqUploadId (\ s a -> s{_uprqUploadId = a});

-- | FIXME: Undocumented member.
uprqBody :: Lens' UploadPart RqBody
uprqBody = lens _uprqBody (\ s a -> s{_uprqBody = a});

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
        toBody = _uprqBody

instance ToHeaders UploadPart where
        toHeaders UploadPart'{..}
          = mconcat
              ["Content-Length" =# _uprqContentLength,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _uprqSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _uprqSSECustomerKey,
               "x-amz-request-payer" =# _uprqRequestPayer,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _uprqSSECustomerKeyMD5,
               "Content-MD5" =# _uprqContentMD5]

instance ToPath UploadPart where
        toPath UploadPart'{..}
          = mconcat
              ["/", toText _uprqBucket, "/", toText _uprqKey]

instance ToQuery UploadPart where
        toQuery UploadPart'{..}
          = mconcat
              ["partNumber" =: _uprqPartNumber,
               "uploadId" =: _uprqUploadId]

-- | /See:/ 'uploadPartResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uprsETag'
--
-- * 'uprsRequestCharged'
--
-- * 'uprsSSECustomerAlgorithm'
--
-- * 'uprsSSEKMSKeyId'
--
-- * 'uprsSSECustomerKeyMD5'
--
-- * 'uprsServerSideEncryption'
--
-- * 'uprsStatus'
data UploadPartResponse = UploadPartResponse'
    { _uprsETag                 :: !(Maybe ETag)
    , _uprsRequestCharged       :: !(Maybe RequestCharged)
    , _uprsSSECustomerAlgorithm :: !(Maybe Text)
    , _uprsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _uprsSSECustomerKeyMD5    :: !(Maybe Text)
    , _uprsServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _uprsStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UploadPartResponse' smart constructor.
uploadPartResponse :: Int -> UploadPartResponse
uploadPartResponse pStatus =
    UploadPartResponse'
    { _uprsETag = Nothing
    , _uprsRequestCharged = Nothing
    , _uprsSSECustomerAlgorithm = Nothing
    , _uprsSSEKMSKeyId = Nothing
    , _uprsSSECustomerKeyMD5 = Nothing
    , _uprsServerSideEncryption = Nothing
    , _uprsStatus = pStatus
    }

-- | Entity tag for the uploaded object.
uprsETag :: Lens' UploadPartResponse (Maybe ETag)
uprsETag = lens _uprsETag (\ s a -> s{_uprsETag = a});

-- | FIXME: Undocumented member.
uprsRequestCharged :: Lens' UploadPartResponse (Maybe RequestCharged)
uprsRequestCharged = lens _uprsRequestCharged (\ s a -> s{_uprsRequestCharged = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
uprsSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
uprsSSECustomerAlgorithm = lens _uprsSSECustomerAlgorithm (\ s a -> s{_uprsSSECustomerAlgorithm = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
uprsSSEKMSKeyId :: Lens' UploadPartResponse (Maybe Text)
uprsSSEKMSKeyId = lens _uprsSSEKMSKeyId (\ s a -> s{_uprsSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
uprsSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
uprsSSECustomerKeyMD5 = lens _uprsSSECustomerKeyMD5 (\ s a -> s{_uprsSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
uprsServerSideEncryption :: Lens' UploadPartResponse (Maybe ServerSideEncryption)
uprsServerSideEncryption = lens _uprsServerSideEncryption (\ s a -> s{_uprsServerSideEncryption = a});

-- | FIXME: Undocumented member.
uprsStatus :: Lens' UploadPartResponse Int
uprsStatus = lens _uprsStatus (\ s a -> s{_uprsStatus = a});

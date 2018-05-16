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
-- Module      : Network.AWS.S3.UploadPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part in a multipart upload.
--
--
-- __Note:__ After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.
--
module Network.AWS.S3.UploadPart
    (
    -- * Creating a Request
      uploadPart
    , UploadPart
    -- * Request Lenses
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

    -- * Destructuring the Response
    , uploadPartResponse
    , UploadPartResponse
    -- * Response Lenses
    , uprsRequestCharged
    , uprsETag
    , uprsSSECustomerAlgorithm
    , uprsSSECustomerKeyMD5
    , uprsSSEKMSKeyId
    , uprsServerSideEncryption
    , uprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'uploadPart' smart constructor.
data UploadPart = UploadPart'
  { _upContentLength        :: !(Maybe Integer)
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
  } deriving (Show, Generic)


-- | Creates a value of 'UploadPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upContentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
--
-- * 'upSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'upSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header. This must be the same encryption key specified in the initiate multipart upload request.
--
-- * 'upRequestPayer' - Undocumented member.
--
-- * 'upSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'upContentMD5' - The base64-encoded 128-bit MD5 digest of the part data.
--
-- * 'upBucket' - Name of the bucket to which the multipart upload was initiated.
--
-- * 'upKey' - Object key for which the multipart upload was initiated.
--
-- * 'upPartNumber' - Part number of part being uploaded. This is a positive integer between 1 and 10,000.
--
-- * 'upUploadId' - Upload ID identifying the multipart upload whose part is being uploaded.
--
-- * 'upBody' - Object data.
uploadPart
    :: BucketName -- ^ 'upBucket'
    -> ObjectKey -- ^ 'upKey'
    -> Int -- ^ 'upPartNumber'
    -> Text -- ^ 'upUploadId'
    -> RqBody -- ^ 'upBody'
    -> UploadPart
uploadPart pBucket_ pKey_ pPartNumber_ pUploadId_ pBody_ =
  UploadPart'
    { _upContentLength = Nothing
    , _upSSECustomerAlgorithm = Nothing
    , _upSSECustomerKey = Nothing
    , _upRequestPayer = Nothing
    , _upSSECustomerKeyMD5 = Nothing
    , _upContentMD5 = Nothing
    , _upBucket = pBucket_
    , _upKey = pKey_
    , _upPartNumber = pPartNumber_
    , _upUploadId = pUploadId_
    , _upBody = pBody_
    }


-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
upContentLength :: Lens' UploadPart (Maybe Integer)
upContentLength = lens _upContentLength (\ s a -> s{_upContentLength = a})

-- | Specifies the algorithm to use to when encrypting the object (e.g., AES256).
upSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
upSSECustomerAlgorithm = lens _upSSECustomerAlgorithm (\ s a -> s{_upSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header. This must be the same encryption key specified in the initiate multipart upload request.
upSSECustomerKey :: Lens' UploadPart (Maybe Text)
upSSECustomerKey = lens _upSSECustomerKey (\ s a -> s{_upSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
upRequestPayer :: Lens' UploadPart (Maybe RequestPayer)
upRequestPayer = lens _upRequestPayer (\ s a -> s{_upRequestPayer = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
upSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
upSSECustomerKeyMD5 = lens _upSSECustomerKeyMD5 (\ s a -> s{_upSSECustomerKeyMD5 = a})

-- | The base64-encoded 128-bit MD5 digest of the part data.
upContentMD5 :: Lens' UploadPart (Maybe Text)
upContentMD5 = lens _upContentMD5 (\ s a -> s{_upContentMD5 = a})

-- | Name of the bucket to which the multipart upload was initiated.
upBucket :: Lens' UploadPart BucketName
upBucket = lens _upBucket (\ s a -> s{_upBucket = a})

-- | Object key for which the multipart upload was initiated.
upKey :: Lens' UploadPart ObjectKey
upKey = lens _upKey (\ s a -> s{_upKey = a})

-- | Part number of part being uploaded. This is a positive integer between 1 and 10,000.
upPartNumber :: Lens' UploadPart Int
upPartNumber = lens _upPartNumber (\ s a -> s{_upPartNumber = a})

-- | Upload ID identifying the multipart upload whose part is being uploaded.
upUploadId :: Lens' UploadPart Text
upUploadId = lens _upUploadId (\ s a -> s{_upUploadId = a})

-- | Object data.
upBody :: Lens' UploadPart RqBody
upBody = lens _upBody (\ s a -> s{_upBody = a})

instance AWSRequest UploadPart where
        type Rs UploadPart = UploadPartResponse
        request = putBody s3
        response
          = receiveEmpty
              (\ s h x ->
                 UploadPartResponse' <$>
                   (h .#? "x-amz-request-charged") <*> (h .#? "ETag")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance ToBody UploadPart where
        toBody = toBody . _upBody

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
          = mconcat ["/", toBS _upBucket, "/", toBS _upKey]

instance ToQuery UploadPart where
        toQuery UploadPart'{..}
          = mconcat
              ["partNumber" =: _upPartNumber,
               "uploadId" =: _upUploadId]

-- | /See:/ 'uploadPartResponse' smart constructor.
data UploadPartResponse = UploadPartResponse'
  { _uprsRequestCharged       :: !(Maybe RequestCharged)
  , _uprsETag                 :: !(Maybe ETag)
  , _uprsSSECustomerAlgorithm :: !(Maybe Text)
  , _uprsSSECustomerKeyMD5    :: !(Maybe Text)
  , _uprsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
  , _uprsServerSideEncryption :: !(Maybe ServerSideEncryption)
  , _uprsResponseStatus       :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadPartResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsRequestCharged' - Undocumented member.
--
-- * 'uprsETag' - Entity tag for the uploaded object.
--
-- * 'uprsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'uprsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
--
-- * 'uprsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
--
-- * 'uprsServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'uprsResponseStatus' - -- | The response status code.
uploadPartResponse
    :: Int -- ^ 'uprsResponseStatus'
    -> UploadPartResponse
uploadPartResponse pResponseStatus_ =
  UploadPartResponse'
    { _uprsRequestCharged = Nothing
    , _uprsETag = Nothing
    , _uprsSSECustomerAlgorithm = Nothing
    , _uprsSSECustomerKeyMD5 = Nothing
    , _uprsSSEKMSKeyId = Nothing
    , _uprsServerSideEncryption = Nothing
    , _uprsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
uprsRequestCharged :: Lens' UploadPartResponse (Maybe RequestCharged)
uprsRequestCharged = lens _uprsRequestCharged (\ s a -> s{_uprsRequestCharged = a})

-- | Entity tag for the uploaded object.
uprsETag :: Lens' UploadPartResponse (Maybe ETag)
uprsETag = lens _uprsETag (\ s a -> s{_uprsETag = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
uprsSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
uprsSSECustomerAlgorithm = lens _uprsSSECustomerAlgorithm (\ s a -> s{_uprsSSECustomerAlgorithm = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
uprsSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
uprsSSECustomerKeyMD5 = lens _uprsSSECustomerKeyMD5 (\ s a -> s{_uprsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
uprsSSEKMSKeyId :: Lens' UploadPartResponse (Maybe Text)
uprsSSEKMSKeyId = lens _uprsSSEKMSKeyId (\ s a -> s{_uprsSSEKMSKeyId = a}) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
uprsServerSideEncryption :: Lens' UploadPartResponse (Maybe ServerSideEncryption)
uprsServerSideEncryption = lens _uprsServerSideEncryption (\ s a -> s{_uprsServerSideEncryption = a})

-- | -- | The response status code.
uprsResponseStatus :: Lens' UploadPartResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\ s a -> s{_uprsResponseStatus = a})

instance NFData UploadPartResponse where

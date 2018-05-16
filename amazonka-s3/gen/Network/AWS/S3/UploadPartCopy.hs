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
-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part by copying data from an existing object as data source.
module Network.AWS.S3.UploadPartCopy
    (
    -- * Creating a Request
      uploadPartCopy
    , UploadPartCopy
    -- * Request Lenses
    , upcCopySourceIfModifiedSince
    , upcCopySourceIfUnmodifiedSince
    , upcCopySourceRange
    , upcCopySourceSSECustomerKeyMD5
    , upcCopySourceIfNoneMatch
    , upcSSECustomerAlgorithm
    , upcSSECustomerKey
    , upcRequestPayer
    , upcCopySourceIfMatch
    , upcSSECustomerKeyMD5
    , upcCopySourceSSECustomerKey
    , upcCopySourceSSECustomerAlgorithm
    , upcBucket
    , upcCopySource
    , upcKey
    , upcPartNumber
    , upcUploadId

    -- * Destructuring the Response
    , uploadPartCopyResponse
    , UploadPartCopyResponse
    -- * Response Lenses
    , upcrsRequestCharged
    , upcrsCopyPartResult
    , upcrsSSECustomerAlgorithm
    , upcrsCopySourceVersionId
    , upcrsSSECustomerKeyMD5
    , upcrsSSEKMSKeyId
    , upcrsServerSideEncryption
    , upcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'uploadPartCopy' smart constructor.
data UploadPartCopy = UploadPartCopy'
  { _upcCopySourceIfModifiedSince      :: !(Maybe RFC822)
  , _upcCopySourceIfUnmodifiedSince    :: !(Maybe RFC822)
  , _upcCopySourceRange                :: !(Maybe Text)
  , _upcCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
  , _upcCopySourceIfNoneMatch          :: !(Maybe Text)
  , _upcSSECustomerAlgorithm           :: !(Maybe Text)
  , _upcSSECustomerKey                 :: !(Maybe (Sensitive Text))
  , _upcRequestPayer                   :: !(Maybe RequestPayer)
  , _upcCopySourceIfMatch              :: !(Maybe Text)
  , _upcSSECustomerKeyMD5              :: !(Maybe Text)
  , _upcCopySourceSSECustomerKey       :: !(Maybe (Sensitive Text))
  , _upcCopySourceSSECustomerAlgorithm :: !(Maybe Text)
  , _upcBucket                         :: !BucketName
  , _upcCopySource                     :: !Text
  , _upcKey                            :: !ObjectKey
  , _upcPartNumber                     :: !Int
  , _upcUploadId                       :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadPartCopy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcCopySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- * 'upcCopySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
--
-- * 'upcCopySourceRange' - The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first ten bytes of the source. You can copy a range only if the source object is greater than 5 GB.
--
-- * 'upcCopySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'upcCopySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- * 'upcSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'upcSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header. This must be the same encryption key specified in the initiate multipart upload request.
--
-- * 'upcRequestPayer' - Undocumented member.
--
-- * 'upcCopySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
--
-- * 'upcSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'upcCopySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- * 'upcCopySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (e.g., AES256).
--
-- * 'upcBucket' - Undocumented member.
--
-- * 'upcCopySource' - The name of the source bucket and key name of the source object, separated by a slash (/). Must be URL-encoded.
--
-- * 'upcKey' - Undocumented member.
--
-- * 'upcPartNumber' - Part number of part being copied. This is a positive integer between 1 and 10,000.
--
-- * 'upcUploadId' - Upload ID identifying the multipart upload whose part is being copied.
uploadPartCopy
    :: BucketName -- ^ 'upcBucket'
    -> Text -- ^ 'upcCopySource'
    -> ObjectKey -- ^ 'upcKey'
    -> Int -- ^ 'upcPartNumber'
    -> Text -- ^ 'upcUploadId'
    -> UploadPartCopy
uploadPartCopy pBucket_ pCopySource_ pKey_ pPartNumber_ pUploadId_ =
  UploadPartCopy'
    { _upcCopySourceIfModifiedSince = Nothing
    , _upcCopySourceIfUnmodifiedSince = Nothing
    , _upcCopySourceRange = Nothing
    , _upcCopySourceSSECustomerKeyMD5 = Nothing
    , _upcCopySourceIfNoneMatch = Nothing
    , _upcSSECustomerAlgorithm = Nothing
    , _upcSSECustomerKey = Nothing
    , _upcRequestPayer = Nothing
    , _upcCopySourceIfMatch = Nothing
    , _upcSSECustomerKeyMD5 = Nothing
    , _upcCopySourceSSECustomerKey = Nothing
    , _upcCopySourceSSECustomerAlgorithm = Nothing
    , _upcBucket = pBucket_
    , _upcCopySource = pCopySource_
    , _upcKey = pKey_
    , _upcPartNumber = pPartNumber_
    , _upcUploadId = pUploadId_
    }


-- | Copies the object if it has been modified since the specified time.
upcCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfModifiedSince = lens _upcCopySourceIfModifiedSince (\ s a -> s{_upcCopySourceIfModifiedSince = a}) . mapping _Time

-- | Copies the object if it hasn't been modified since the specified time.
upcCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfUnmodifiedSince = lens _upcCopySourceIfUnmodifiedSince (\ s a -> s{_upcCopySourceIfUnmodifiedSince = a}) . mapping _Time

-- | The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first ten bytes of the source. You can copy a range only if the source object is greater than 5 GB.
upcCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceRange = lens _upcCopySourceRange (\ s a -> s{_upcCopySourceRange = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
upcCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKeyMD5 = lens _upcCopySourceSSECustomerKeyMD5 (\ s a -> s{_upcCopySourceSSECustomerKeyMD5 = a})

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
upcCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfNoneMatch = lens _upcCopySourceIfNoneMatch (\ s a -> s{_upcCopySourceIfNoneMatch = a})

-- | Specifies the algorithm to use to when encrypting the object (e.g., AES256).
upcSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerAlgorithm = lens _upcSSECustomerAlgorithm (\ s a -> s{_upcSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header. This must be the same encryption key specified in the initiate multipart upload request.
upcSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKey = lens _upcSSECustomerKey (\ s a -> s{_upcSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
upcRequestPayer :: Lens' UploadPartCopy (Maybe RequestPayer)
upcRequestPayer = lens _upcRequestPayer (\ s a -> s{_upcRequestPayer = a})

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfMatch = lens _upcCopySourceIfMatch (\ s a -> s{_upcCopySourceIfMatch = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
upcSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKeyMD5 = lens _upcSSECustomerKeyMD5 (\ s a -> s{_upcSSECustomerKeyMD5 = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
upcCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKey = lens _upcCopySourceSSECustomerKey (\ s a -> s{_upcCopySourceSSECustomerKey = a}) . mapping _Sensitive

-- | Specifies the algorithm to use when decrypting the source object (e.g., AES256).
upcCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerAlgorithm = lens _upcCopySourceSSECustomerAlgorithm (\ s a -> s{_upcCopySourceSSECustomerAlgorithm = a})

-- | Undocumented member.
upcBucket :: Lens' UploadPartCopy BucketName
upcBucket = lens _upcBucket (\ s a -> s{_upcBucket = a})

-- | The name of the source bucket and key name of the source object, separated by a slash (/). Must be URL-encoded.
upcCopySource :: Lens' UploadPartCopy Text
upcCopySource = lens _upcCopySource (\ s a -> s{_upcCopySource = a})

-- | Undocumented member.
upcKey :: Lens' UploadPartCopy ObjectKey
upcKey = lens _upcKey (\ s a -> s{_upcKey = a})

-- | Part number of part being copied. This is a positive integer between 1 and 10,000.
upcPartNumber :: Lens' UploadPartCopy Int
upcPartNumber = lens _upcPartNumber (\ s a -> s{_upcPartNumber = a})

-- | Upload ID identifying the multipart upload whose part is being copied.
upcUploadId :: Lens' UploadPartCopy Text
upcUploadId = lens _upcUploadId (\ s a -> s{_upcUploadId = a})

instance AWSRequest UploadPartCopy where
        type Rs UploadPartCopy = UploadPartCopyResponse
        request = put s3
        response
          = receiveXML
              (\ s h x ->
                 UploadPartCopyResponse' <$>
                   (h .#? "x-amz-request-charged") <*> (parseXML x) <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (h .#? "x-amz-copy-source-version-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance Hashable UploadPartCopy where

instance NFData UploadPartCopy where

instance ToHeaders UploadPartCopy where
        toHeaders UploadPartCopy'{..}
          = mconcat
              ["x-amz-copy-source-if-modified-since" =#
                 _upcCopySourceIfModifiedSince,
               "x-amz-copy-source-if-unmodified-since" =#
                 _upcCopySourceIfUnmodifiedSince,
               "x-amz-copy-source-range" =# _upcCopySourceRange,
               "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                 =# _upcCopySourceSSECustomerKeyMD5,
               "x-amz-copy-source-if-none-match" =#
                 _upcCopySourceIfNoneMatch,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _upcSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _upcSSECustomerKey,
               "x-amz-request-payer" =# _upcRequestPayer,
               "x-amz-copy-source-if-match" =#
                 _upcCopySourceIfMatch,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _upcSSECustomerKeyMD5,
               "x-amz-copy-source-server-side-encryption-customer-key"
                 =# _upcCopySourceSSECustomerKey,
               "x-amz-copy-source-server-side-encryption-customer-algorithm"
                 =# _upcCopySourceSSECustomerAlgorithm,
               "x-amz-copy-source" =# _upcCopySource]

instance ToPath UploadPartCopy where
        toPath UploadPartCopy'{..}
          = mconcat ["/", toBS _upcBucket, "/", toBS _upcKey]

instance ToQuery UploadPartCopy where
        toQuery UploadPartCopy'{..}
          = mconcat
              ["partNumber" =: _upcPartNumber,
               "uploadId" =: _upcUploadId]

-- | /See:/ 'uploadPartCopyResponse' smart constructor.
data UploadPartCopyResponse = UploadPartCopyResponse'
  { _upcrsRequestCharged       :: !(Maybe RequestCharged)
  , _upcrsCopyPartResult       :: !(Maybe CopyPartResult)
  , _upcrsSSECustomerAlgorithm :: !(Maybe Text)
  , _upcrsCopySourceVersionId  :: !(Maybe Text)
  , _upcrsSSECustomerKeyMD5    :: !(Maybe Text)
  , _upcrsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
  , _upcrsServerSideEncryption :: !(Maybe ServerSideEncryption)
  , _upcrsResponseStatus       :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadPartCopyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcrsRequestCharged' - Undocumented member.
--
-- * 'upcrsCopyPartResult' - Undocumented member.
--
-- * 'upcrsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'upcrsCopySourceVersionId' - The version of the source object that was copied, if you have enabled versioning on the source bucket.
--
-- * 'upcrsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
--
-- * 'upcrsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
--
-- * 'upcrsServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'upcrsResponseStatus' - -- | The response status code.
uploadPartCopyResponse
    :: Int -- ^ 'upcrsResponseStatus'
    -> UploadPartCopyResponse
uploadPartCopyResponse pResponseStatus_ =
  UploadPartCopyResponse'
    { _upcrsRequestCharged = Nothing
    , _upcrsCopyPartResult = Nothing
    , _upcrsSSECustomerAlgorithm = Nothing
    , _upcrsCopySourceVersionId = Nothing
    , _upcrsSSECustomerKeyMD5 = Nothing
    , _upcrsSSEKMSKeyId = Nothing
    , _upcrsServerSideEncryption = Nothing
    , _upcrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
upcrsRequestCharged :: Lens' UploadPartCopyResponse (Maybe RequestCharged)
upcrsRequestCharged = lens _upcrsRequestCharged (\ s a -> s{_upcrsRequestCharged = a})

-- | Undocumented member.
upcrsCopyPartResult :: Lens' UploadPartCopyResponse (Maybe CopyPartResult)
upcrsCopyPartResult = lens _upcrsCopyPartResult (\ s a -> s{_upcrsCopyPartResult = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
upcrsSSECustomerAlgorithm :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSECustomerAlgorithm = lens _upcrsSSECustomerAlgorithm (\ s a -> s{_upcrsSSECustomerAlgorithm = a})

-- | The version of the source object that was copied, if you have enabled versioning on the source bucket.
upcrsCopySourceVersionId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsCopySourceVersionId = lens _upcrsCopySourceVersionId (\ s a -> s{_upcrsCopySourceVersionId = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
upcrsSSECustomerKeyMD5 :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSECustomerKeyMD5 = lens _upcrsSSECustomerKeyMD5 (\ s a -> s{_upcrsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
upcrsSSEKMSKeyId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSEKMSKeyId = lens _upcrsSSEKMSKeyId (\ s a -> s{_upcrsSSEKMSKeyId = a}) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
upcrsServerSideEncryption :: Lens' UploadPartCopyResponse (Maybe ServerSideEncryption)
upcrsServerSideEncryption = lens _upcrsServerSideEncryption (\ s a -> s{_upcrsServerSideEncryption = a})

-- | -- | The response status code.
upcrsResponseStatus :: Lens' UploadPartCopyResponse Int
upcrsResponseStatus = lens _upcrsResponseStatus (\ s a -> s{_upcrsResponseStatus = a})

instance NFData UploadPartCopyResponse where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Uploads a part by copying data from an existing object as data source.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/UploadPartCopy.html>
module Network.AWS.S3.UploadPartCopy
    (
    -- * Request
      UploadPartCopy
    -- ** Request constructor
    , uploadPartCopy
    -- ** Request lenses
    , upcCopySourceIfModifiedSince
    , upcCopySourceIfUnmodifiedSince
    , upcCopySourceRange
    , upcCopySourceSSECustomerKeyMD5
    , upcSSECustomerAlgorithm
    , upcCopySourceIfNoneMatch
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

    -- * Response
    , UploadPartCopyResponse
    -- ** Response constructor
    , uploadPartCopyResponse
    -- ** Response lenses
    , upcrRequestCharged
    , upcrCopyPartResult
    , upcrSSECustomerAlgorithm
    , upcrCopySourceVersionId
    , upcrSSEKMSKeyId
    , upcrSSECustomerKeyMD5
    , upcrServerSideEncryption
    , upcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'uploadPartCopy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcCopySourceIfModifiedSince'
--
-- * 'upcCopySourceIfUnmodifiedSince'
--
-- * 'upcCopySourceRange'
--
-- * 'upcCopySourceSSECustomerKeyMD5'
--
-- * 'upcSSECustomerAlgorithm'
--
-- * 'upcCopySourceIfNoneMatch'
--
-- * 'upcSSECustomerKey'
--
-- * 'upcRequestPayer'
--
-- * 'upcCopySourceIfMatch'
--
-- * 'upcSSECustomerKeyMD5'
--
-- * 'upcCopySourceSSECustomerKey'
--
-- * 'upcCopySourceSSECustomerAlgorithm'
--
-- * 'upcBucket'
--
-- * 'upcCopySource'
--
-- * 'upcKey'
--
-- * 'upcPartNumber'
--
-- * 'upcUploadId'
data UploadPartCopy = UploadPartCopy'
    { _upcCopySourceIfModifiedSince      :: !(Maybe RFC822)
    , _upcCopySourceIfUnmodifiedSince    :: !(Maybe RFC822)
    , _upcCopySourceRange                :: !(Maybe Text)
    , _upcCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
    , _upcSSECustomerAlgorithm           :: !(Maybe Text)
    , _upcCopySourceIfNoneMatch          :: !(Maybe Text)
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
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UploadPartCopy' smart constructor.
uploadPartCopy :: BucketName -> Text -> ObjectKey -> Int -> Text -> UploadPartCopy
uploadPartCopy pBucket pCopySource pKey pPartNumber pUploadId =
    UploadPartCopy'
    { _upcCopySourceIfModifiedSince = Nothing
    , _upcCopySourceIfUnmodifiedSince = Nothing
    , _upcCopySourceRange = Nothing
    , _upcCopySourceSSECustomerKeyMD5 = Nothing
    , _upcSSECustomerAlgorithm = Nothing
    , _upcCopySourceIfNoneMatch = Nothing
    , _upcSSECustomerKey = Nothing
    , _upcRequestPayer = Nothing
    , _upcCopySourceIfMatch = Nothing
    , _upcSSECustomerKeyMD5 = Nothing
    , _upcCopySourceSSECustomerKey = Nothing
    , _upcCopySourceSSECustomerAlgorithm = Nothing
    , _upcBucket = pBucket
    , _upcCopySource = pCopySource
    , _upcKey = pKey
    , _upcPartNumber = pPartNumber
    , _upcUploadId = pUploadId
    }

-- | Copies the object if it has been modified since the specified time.
upcCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfModifiedSince = lens _upcCopySourceIfModifiedSince (\ s a -> s{_upcCopySourceIfModifiedSince = a}) . mapping _Time;

-- | Copies the object if it hasn\'t been modified since the specified time.
upcCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfUnmodifiedSince = lens _upcCopySourceIfUnmodifiedSince (\ s a -> s{_upcCopySourceIfUnmodifiedSince = a}) . mapping _Time;

-- | The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first ten bytes of the source. You can copy a range
-- only if the source object is greater than 5 GB.
upcCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceRange = lens _upcCopySourceRange (\ s a -> s{_upcCopySourceRange = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKeyMD5 = lens _upcCopySourceSSECustomerKeyMD5 (\ s a -> s{_upcCopySourceSSECustomerKeyMD5 = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
upcSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerAlgorithm = lens _upcSSECustomerAlgorithm (\ s a -> s{_upcSSECustomerAlgorithm = a});

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
upcCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfNoneMatch = lens _upcCopySourceIfNoneMatch (\ s a -> s{_upcCopySourceIfNoneMatch = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
upcSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKey = lens _upcSSECustomerKey (\ s a -> s{_upcSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
upcRequestPayer :: Lens' UploadPartCopy (Maybe RequestPayer)
upcRequestPayer = lens _upcRequestPayer (\ s a -> s{_upcRequestPayer = a});

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfMatch = lens _upcCopySourceIfMatch (\ s a -> s{_upcCopySourceIfMatch = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKeyMD5 = lens _upcSSECustomerKeyMD5 (\ s a -> s{_upcSSECustomerKeyMD5 = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
upcCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKey = lens _upcCopySourceSSECustomerKey (\ s a -> s{_upcCopySourceSSECustomerKey = a}) . mapping _Sensitive;

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
upcCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerAlgorithm = lens _upcCopySourceSSECustomerAlgorithm (\ s a -> s{_upcCopySourceSSECustomerAlgorithm = a});

-- | FIXME: Undocumented member.
upcBucket :: Lens' UploadPartCopy BucketName
upcBucket = lens _upcBucket (\ s a -> s{_upcBucket = a});

-- | The name of the source bucket and key name of the source object,
-- separated by a slash (\/). Must be URL-encoded.
upcCopySource :: Lens' UploadPartCopy Text
upcCopySource = lens _upcCopySource (\ s a -> s{_upcCopySource = a});

-- | FIXME: Undocumented member.
upcKey :: Lens' UploadPartCopy ObjectKey
upcKey = lens _upcKey (\ s a -> s{_upcKey = a});

-- | Part number of part being copied.
upcPartNumber :: Lens' UploadPartCopy Int
upcPartNumber = lens _upcPartNumber (\ s a -> s{_upcPartNumber = a});

-- | Upload ID identifying the multipart upload whose part is being copied.
upcUploadId :: Lens' UploadPartCopy Text
upcUploadId = lens _upcUploadId (\ s a -> s{_upcUploadId = a});

instance AWSRequest UploadPartCopy where
        type Sv UploadPartCopy = S3
        type Rs UploadPartCopy = UploadPartCopyResponse
        request = put
        response
          = receiveXML
              (\ s h x ->
                 UploadPartCopyResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (x .@? "CopyPartResult")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (h .#? "x-amz-copy-source-version-id")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

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
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _upcSSECustomerAlgorithm,
               "x-amz-copy-source-if-none-match" =#
                 _upcCopySourceIfNoneMatch,
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
          = mconcat
              ["/", toText _upcBucket, "/", toText _upcKey]

instance ToQuery UploadPartCopy where
        toQuery UploadPartCopy'{..}
          = mconcat
              ["partNumber" =: _upcPartNumber,
               "uploadId" =: _upcUploadId]

-- | /See:/ 'uploadPartCopyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcrRequestCharged'
--
-- * 'upcrCopyPartResult'
--
-- * 'upcrSSECustomerAlgorithm'
--
-- * 'upcrCopySourceVersionId'
--
-- * 'upcrSSEKMSKeyId'
--
-- * 'upcrSSECustomerKeyMD5'
--
-- * 'upcrServerSideEncryption'
--
-- * 'upcrStatus'
data UploadPartCopyResponse = UploadPartCopyResponse'
    { _upcrRequestCharged       :: !(Maybe RequestCharged)
    , _upcrCopyPartResult       :: !(Maybe CopyPartResult)
    , _upcrSSECustomerAlgorithm :: !(Maybe Text)
    , _upcrCopySourceVersionId  :: !(Maybe Text)
    , _upcrSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _upcrSSECustomerKeyMD5    :: !(Maybe Text)
    , _upcrServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _upcrStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UploadPartCopyResponse' smart constructor.
uploadPartCopyResponse :: Int -> UploadPartCopyResponse
uploadPartCopyResponse pStatus =
    UploadPartCopyResponse'
    { _upcrRequestCharged = Nothing
    , _upcrCopyPartResult = Nothing
    , _upcrSSECustomerAlgorithm = Nothing
    , _upcrCopySourceVersionId = Nothing
    , _upcrSSEKMSKeyId = Nothing
    , _upcrSSECustomerKeyMD5 = Nothing
    , _upcrServerSideEncryption = Nothing
    , _upcrStatus = pStatus
    }

-- | FIXME: Undocumented member.
upcrRequestCharged :: Lens' UploadPartCopyResponse (Maybe RequestCharged)
upcrRequestCharged = lens _upcrRequestCharged (\ s a -> s{_upcrRequestCharged = a});

-- | FIXME: Undocumented member.
upcrCopyPartResult :: Lens' UploadPartCopyResponse (Maybe CopyPartResult)
upcrCopyPartResult = lens _upcrCopyPartResult (\ s a -> s{_upcrCopyPartResult = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
upcrSSECustomerAlgorithm :: Lens' UploadPartCopyResponse (Maybe Text)
upcrSSECustomerAlgorithm = lens _upcrSSECustomerAlgorithm (\ s a -> s{_upcrSSECustomerAlgorithm = a});

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
upcrCopySourceVersionId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrCopySourceVersionId = lens _upcrCopySourceVersionId (\ s a -> s{_upcrCopySourceVersionId = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
upcrSSEKMSKeyId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrSSEKMSKeyId = lens _upcrSSEKMSKeyId (\ s a -> s{_upcrSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upcrSSECustomerKeyMD5 :: Lens' UploadPartCopyResponse (Maybe Text)
upcrSSECustomerKeyMD5 = lens _upcrSSECustomerKeyMD5 (\ s a -> s{_upcrSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
upcrServerSideEncryption :: Lens' UploadPartCopyResponse (Maybe ServerSideEncryption)
upcrServerSideEncryption = lens _upcrServerSideEncryption (\ s a -> s{_upcrServerSideEncryption = a});

-- | FIXME: Undocumented member.
upcrStatus :: Lens' UploadPartCopyResponse Int
upcrStatus = lens _upcrStatus (\ s a -> s{_upcrStatus = a});

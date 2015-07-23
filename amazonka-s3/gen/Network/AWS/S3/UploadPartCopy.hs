{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part by copying data from an existing object as data source.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/UploadPartCopy.html>
module Network.AWS.S3.UploadPartCopy
    (
    -- * Request
      UploadPartCopy
    -- ** Request constructor
    , uploadPartCopy
    -- ** Request lenses
    , upcrqCopySourceIfModifiedSince
    , upcrqCopySourceIfUnmodifiedSince
    , upcrqCopySourceRange
    , upcrqCopySourceSSECustomerKeyMD5
    , upcrqSSECustomerAlgorithm
    , upcrqCopySourceIfNoneMatch
    , upcrqSSECustomerKey
    , upcrqRequestPayer
    , upcrqCopySourceIfMatch
    , upcrqSSECustomerKeyMD5
    , upcrqCopySourceSSECustomerKey
    , upcrqCopySourceSSECustomerAlgorithm
    , upcrqBucket
    , upcrqCopySource
    , upcrqKey
    , upcrqPartNumber
    , upcrqUploadId

    -- * Response
    , UploadPartCopyResponse
    -- ** Response constructor
    , uploadPartCopyResponse
    -- ** Response lenses
    , upcrsRequestCharged
    , upcrsCopyPartResult
    , upcrsSSECustomerAlgorithm
    , upcrsCopySourceVersionId
    , upcrsSSEKMSKeyId
    , upcrsSSECustomerKeyMD5
    , upcrsServerSideEncryption
    , upcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'uploadPartCopy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcrqCopySourceIfModifiedSince'
--
-- * 'upcrqCopySourceIfUnmodifiedSince'
--
-- * 'upcrqCopySourceRange'
--
-- * 'upcrqCopySourceSSECustomerKeyMD5'
--
-- * 'upcrqSSECustomerAlgorithm'
--
-- * 'upcrqCopySourceIfNoneMatch'
--
-- * 'upcrqSSECustomerKey'
--
-- * 'upcrqRequestPayer'
--
-- * 'upcrqCopySourceIfMatch'
--
-- * 'upcrqSSECustomerKeyMD5'
--
-- * 'upcrqCopySourceSSECustomerKey'
--
-- * 'upcrqCopySourceSSECustomerAlgorithm'
--
-- * 'upcrqBucket'
--
-- * 'upcrqCopySource'
--
-- * 'upcrqKey'
--
-- * 'upcrqPartNumber'
--
-- * 'upcrqUploadId'
data UploadPartCopy = UploadPartCopy'
    { _upcrqCopySourceIfModifiedSince      :: !(Maybe RFC822)
    , _upcrqCopySourceIfUnmodifiedSince    :: !(Maybe RFC822)
    , _upcrqCopySourceRange                :: !(Maybe Text)
    , _upcrqCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
    , _upcrqSSECustomerAlgorithm           :: !(Maybe Text)
    , _upcrqCopySourceIfNoneMatch          :: !(Maybe Text)
    , _upcrqSSECustomerKey                 :: !(Maybe (Sensitive Text))
    , _upcrqRequestPayer                   :: !(Maybe RequestPayer)
    , _upcrqCopySourceIfMatch              :: !(Maybe Text)
    , _upcrqSSECustomerKeyMD5              :: !(Maybe Text)
    , _upcrqCopySourceSSECustomerKey       :: !(Maybe (Sensitive Text))
    , _upcrqCopySourceSSECustomerAlgorithm :: !(Maybe Text)
    , _upcrqBucket                         :: !BucketName
    , _upcrqCopySource                     :: !Text
    , _upcrqKey                            :: !ObjectKey
    , _upcrqPartNumber                     :: !Int
    , _upcrqUploadId                       :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UploadPartCopy' smart constructor.
uploadPartCopy :: BucketName -> Text -> ObjectKey -> Int -> Text -> UploadPartCopy
uploadPartCopy pBucket_ pCopySource_ pKey_ pPartNumber_ pUploadId_ =
    UploadPartCopy'
    { _upcrqCopySourceIfModifiedSince = Nothing
    , _upcrqCopySourceIfUnmodifiedSince = Nothing
    , _upcrqCopySourceRange = Nothing
    , _upcrqCopySourceSSECustomerKeyMD5 = Nothing
    , _upcrqSSECustomerAlgorithm = Nothing
    , _upcrqCopySourceIfNoneMatch = Nothing
    , _upcrqSSECustomerKey = Nothing
    , _upcrqRequestPayer = Nothing
    , _upcrqCopySourceIfMatch = Nothing
    , _upcrqSSECustomerKeyMD5 = Nothing
    , _upcrqCopySourceSSECustomerKey = Nothing
    , _upcrqCopySourceSSECustomerAlgorithm = Nothing
    , _upcrqBucket = pBucket_
    , _upcrqCopySource = pCopySource_
    , _upcrqKey = pKey_
    , _upcrqPartNumber = pPartNumber_
    , _upcrqUploadId = pUploadId_
    }

-- | Copies the object if it has been modified since the specified time.
upcrqCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcrqCopySourceIfModifiedSince = lens _upcrqCopySourceIfModifiedSince (\ s a -> s{_upcrqCopySourceIfModifiedSince = a}) . mapping _Time;

-- | Copies the object if it hasn\'t been modified since the specified time.
upcrqCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcrqCopySourceIfUnmodifiedSince = lens _upcrqCopySourceIfUnmodifiedSince (\ s a -> s{_upcrqCopySourceIfUnmodifiedSince = a}) . mapping _Time;

-- | The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first ten bytes of the source. You can copy a range
-- only if the source object is greater than 5 GB.
upcrqCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcrqCopySourceRange = lens _upcrqCopySourceRange (\ s a -> s{_upcrqCopySourceRange = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrqCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcrqCopySourceSSECustomerKeyMD5 = lens _upcrqCopySourceSSECustomerKeyMD5 (\ s a -> s{_upcrqCopySourceSSECustomerKeyMD5 = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
upcrqSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcrqSSECustomerAlgorithm = lens _upcrqSSECustomerAlgorithm (\ s a -> s{_upcrqSSECustomerAlgorithm = a});

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
upcrqCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcrqCopySourceIfNoneMatch = lens _upcrqCopySourceIfNoneMatch (\ s a -> s{_upcrqCopySourceIfNoneMatch = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
upcrqSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcrqSSECustomerKey = lens _upcrqSSECustomerKey (\ s a -> s{_upcrqSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
upcrqRequestPayer :: Lens' UploadPartCopy (Maybe RequestPayer)
upcrqRequestPayer = lens _upcrqRequestPayer (\ s a -> s{_upcrqRequestPayer = a});

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcrqCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcrqCopySourceIfMatch = lens _upcrqCopySourceIfMatch (\ s a -> s{_upcrqCopySourceIfMatch = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrqSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcrqSSECustomerKeyMD5 = lens _upcrqSSECustomerKeyMD5 (\ s a -> s{_upcrqSSECustomerKeyMD5 = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
upcrqCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcrqCopySourceSSECustomerKey = lens _upcrqCopySourceSSECustomerKey (\ s a -> s{_upcrqCopySourceSSECustomerKey = a}) . mapping _Sensitive;

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
upcrqCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcrqCopySourceSSECustomerAlgorithm = lens _upcrqCopySourceSSECustomerAlgorithm (\ s a -> s{_upcrqCopySourceSSECustomerAlgorithm = a});

-- | FIXME: Undocumented member.
upcrqBucket :: Lens' UploadPartCopy BucketName
upcrqBucket = lens _upcrqBucket (\ s a -> s{_upcrqBucket = a});

-- | The name of the source bucket and key name of the source object,
-- separated by a slash (\/). Must be URL-encoded.
upcrqCopySource :: Lens' UploadPartCopy Text
upcrqCopySource = lens _upcrqCopySource (\ s a -> s{_upcrqCopySource = a});

-- | FIXME: Undocumented member.
upcrqKey :: Lens' UploadPartCopy ObjectKey
upcrqKey = lens _upcrqKey (\ s a -> s{_upcrqKey = a});

-- | Part number of part being copied.
upcrqPartNumber :: Lens' UploadPartCopy Int
upcrqPartNumber = lens _upcrqPartNumber (\ s a -> s{_upcrqPartNumber = a});

-- | Upload ID identifying the multipart upload whose part is being copied.
upcrqUploadId :: Lens' UploadPartCopy Text
upcrqUploadId = lens _upcrqUploadId (\ s a -> s{_upcrqUploadId = a});

instance AWSRequest UploadPartCopy where
        type Sv UploadPartCopy = S3
        type Rs UploadPartCopy = UploadPartCopyResponse
        request = put
        response
          = receiveXML
              (\ s h x ->
                 UploadPartCopyResponse' <$>
                   (h .#? "x-amz-request-charged") <*> (parseXML x) <*>
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
                 _upcrqCopySourceIfModifiedSince,
               "x-amz-copy-source-if-unmodified-since" =#
                 _upcrqCopySourceIfUnmodifiedSince,
               "x-amz-copy-source-range" =# _upcrqCopySourceRange,
               "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                 =# _upcrqCopySourceSSECustomerKeyMD5,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _upcrqSSECustomerAlgorithm,
               "x-amz-copy-source-if-none-match" =#
                 _upcrqCopySourceIfNoneMatch,
               "x-amz-server-side-encryption-customer-key" =#
                 _upcrqSSECustomerKey,
               "x-amz-request-payer" =# _upcrqRequestPayer,
               "x-amz-copy-source-if-match" =#
                 _upcrqCopySourceIfMatch,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _upcrqSSECustomerKeyMD5,
               "x-amz-copy-source-server-side-encryption-customer-key"
                 =# _upcrqCopySourceSSECustomerKey,
               "x-amz-copy-source-server-side-encryption-customer-algorithm"
                 =# _upcrqCopySourceSSECustomerAlgorithm,
               "x-amz-copy-source" =# _upcrqCopySource]

instance ToPath UploadPartCopy where
        toPath UploadPartCopy'{..}
          = mconcat
              ["/", toText _upcrqBucket, "/", toText _upcrqKey]

instance ToQuery UploadPartCopy where
        toQuery UploadPartCopy'{..}
          = mconcat
              ["partNumber" =: _upcrqPartNumber,
               "uploadId" =: _upcrqUploadId]

-- | /See:/ 'uploadPartCopyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcrsRequestCharged'
--
-- * 'upcrsCopyPartResult'
--
-- * 'upcrsSSECustomerAlgorithm'
--
-- * 'upcrsCopySourceVersionId'
--
-- * 'upcrsSSEKMSKeyId'
--
-- * 'upcrsSSECustomerKeyMD5'
--
-- * 'upcrsServerSideEncryption'
--
-- * 'upcrsStatus'
data UploadPartCopyResponse = UploadPartCopyResponse'
    { _upcrsRequestCharged       :: !(Maybe RequestCharged)
    , _upcrsCopyPartResult       :: !(Maybe CopyPartResult)
    , _upcrsSSECustomerAlgorithm :: !(Maybe Text)
    , _upcrsCopySourceVersionId  :: !(Maybe Text)
    , _upcrsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _upcrsSSECustomerKeyMD5    :: !(Maybe Text)
    , _upcrsServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _upcrsStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'UploadPartCopyResponse' smart constructor.
uploadPartCopyResponse :: Int -> UploadPartCopyResponse
uploadPartCopyResponse pStatus_ =
    UploadPartCopyResponse'
    { _upcrsRequestCharged = Nothing
    , _upcrsCopyPartResult = Nothing
    , _upcrsSSECustomerAlgorithm = Nothing
    , _upcrsCopySourceVersionId = Nothing
    , _upcrsSSEKMSKeyId = Nothing
    , _upcrsSSECustomerKeyMD5 = Nothing
    , _upcrsServerSideEncryption = Nothing
    , _upcrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
upcrsRequestCharged :: Lens' UploadPartCopyResponse (Maybe RequestCharged)
upcrsRequestCharged = lens _upcrsRequestCharged (\ s a -> s{_upcrsRequestCharged = a});

-- | FIXME: Undocumented member.
upcrsCopyPartResult :: Lens' UploadPartCopyResponse (Maybe CopyPartResult)
upcrsCopyPartResult = lens _upcrsCopyPartResult (\ s a -> s{_upcrsCopyPartResult = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
upcrsSSECustomerAlgorithm :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSECustomerAlgorithm = lens _upcrsSSECustomerAlgorithm (\ s a -> s{_upcrsSSECustomerAlgorithm = a});

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
upcrsCopySourceVersionId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsCopySourceVersionId = lens _upcrsCopySourceVersionId (\ s a -> s{_upcrsCopySourceVersionId = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
upcrsSSEKMSKeyId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSEKMSKeyId = lens _upcrsSSEKMSKeyId (\ s a -> s{_upcrsSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upcrsSSECustomerKeyMD5 :: Lens' UploadPartCopyResponse (Maybe Text)
upcrsSSECustomerKeyMD5 = lens _upcrsSSECustomerKeyMD5 (\ s a -> s{_upcrsSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
upcrsServerSideEncryption :: Lens' UploadPartCopyResponse (Maybe ServerSideEncryption)
upcrsServerSideEncryption = lens _upcrsServerSideEncryption (\ s a -> s{_upcrsServerSideEncryption = a});

-- | FIXME: Undocumented member.
upcrsStatus :: Lens' UploadPartCopyResponse Int
upcrsStatus = lens _upcrsStatus (\ s a -> s{_upcrsStatus = a});

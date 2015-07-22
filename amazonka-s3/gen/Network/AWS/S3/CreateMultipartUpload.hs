{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Initiates a multipart upload and returns an upload ID.
--
-- __Note:__ After you initiate multipart upload and upload one or more
-- parts, you must either complete or abort multipart upload in order to
-- stop getting charged for storage of the uploaded parts. Only after you
-- either complete or abort multipart upload, Amazon S3 frees up the parts
-- storage and stops charging you for the parts storage.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CreateMultipartUpload.html>
module Network.AWS.S3.CreateMultipartUpload
    (
    -- * Request
      CreateMultipartUpload
    -- ** Request constructor
    , createMultipartUpload
    -- ** Request lenses
    , cmurqExpires
    , cmurqSSECustomerAlgorithm
    , cmurqGrantReadACP
    , cmurqSSECustomerKey
    , cmurqRequestPayer
    , cmurqGrantWriteACP
    , cmurqWebsiteRedirectLocation
    , cmurqGrantRead
    , cmurqStorageClass
    , cmurqContentEncoding
    , cmurqSSEKMSKeyId
    , cmurqGrantFullControl
    , cmurqSSECustomerKeyMD5
    , cmurqMetadata
    , cmurqCacheControl
    , cmurqContentLanguage
    , cmurqACL
    , cmurqContentDisposition
    , cmurqServerSideEncryption
    , cmurqContentType
    , cmurqBucket
    , cmurqKey

    -- * Response
    , CreateMultipartUploadResponse
    -- ** Response constructor
    , createMultipartUploadResponse
    -- ** Response lenses
    , cmursRequestCharged
    , cmursSSECustomerAlgorithm
    , cmursBucket
    , cmursKey
    , cmursSSEKMSKeyId
    , cmursSSECustomerKeyMD5
    , cmursUploadId
    , cmursServerSideEncryption
    , cmursStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'createMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmurqExpires'
--
-- * 'cmurqSSECustomerAlgorithm'
--
-- * 'cmurqGrantReadACP'
--
-- * 'cmurqSSECustomerKey'
--
-- * 'cmurqRequestPayer'
--
-- * 'cmurqGrantWriteACP'
--
-- * 'cmurqWebsiteRedirectLocation'
--
-- * 'cmurqGrantRead'
--
-- * 'cmurqStorageClass'
--
-- * 'cmurqContentEncoding'
--
-- * 'cmurqSSEKMSKeyId'
--
-- * 'cmurqGrantFullControl'
--
-- * 'cmurqSSECustomerKeyMD5'
--
-- * 'cmurqMetadata'
--
-- * 'cmurqCacheControl'
--
-- * 'cmurqContentLanguage'
--
-- * 'cmurqACL'
--
-- * 'cmurqContentDisposition'
--
-- * 'cmurqServerSideEncryption'
--
-- * 'cmurqContentType'
--
-- * 'cmurqBucket'
--
-- * 'cmurqKey'
data CreateMultipartUpload = CreateMultipartUpload'
    { _cmurqExpires                 :: !(Maybe RFC822)
    , _cmurqSSECustomerAlgorithm    :: !(Maybe Text)
    , _cmurqGrantReadACP            :: !(Maybe Text)
    , _cmurqSSECustomerKey          :: !(Maybe (Sensitive Text))
    , _cmurqRequestPayer            :: !(Maybe RequestPayer)
    , _cmurqGrantWriteACP           :: !(Maybe Text)
    , _cmurqWebsiteRedirectLocation :: !(Maybe Text)
    , _cmurqGrantRead               :: !(Maybe Text)
    , _cmurqStorageClass            :: !(Maybe StorageClass)
    , _cmurqContentEncoding         :: !(Maybe Text)
    , _cmurqSSEKMSKeyId             :: !(Maybe (Sensitive Text))
    , _cmurqGrantFullControl        :: !(Maybe Text)
    , _cmurqSSECustomerKeyMD5       :: !(Maybe Text)
    , _cmurqMetadata                :: !(Map Text Text)
    , _cmurqCacheControl            :: !(Maybe Text)
    , _cmurqContentLanguage         :: !(Maybe Text)
    , _cmurqACL                     :: !(Maybe ObjectCannedACL)
    , _cmurqContentDisposition      :: !(Maybe Text)
    , _cmurqServerSideEncryption    :: !(Maybe ServerSideEncryption)
    , _cmurqContentType             :: !(Maybe Text)
    , _cmurqBucket                  :: !BucketName
    , _cmurqKey                     :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CreateMultipartUpload' smart constructor.
createMultipartUpload :: BucketName -> ObjectKey -> CreateMultipartUpload
createMultipartUpload pBucket_ pKey_ =
    CreateMultipartUpload'
    { _cmurqExpires = Nothing
    , _cmurqSSECustomerAlgorithm = Nothing
    , _cmurqGrantReadACP = Nothing
    , _cmurqSSECustomerKey = Nothing
    , _cmurqRequestPayer = Nothing
    , _cmurqGrantWriteACP = Nothing
    , _cmurqWebsiteRedirectLocation = Nothing
    , _cmurqGrantRead = Nothing
    , _cmurqStorageClass = Nothing
    , _cmurqContentEncoding = Nothing
    , _cmurqSSEKMSKeyId = Nothing
    , _cmurqGrantFullControl = Nothing
    , _cmurqSSECustomerKeyMD5 = Nothing
    , _cmurqMetadata = mempty
    , _cmurqCacheControl = Nothing
    , _cmurqContentLanguage = Nothing
    , _cmurqACL = Nothing
    , _cmurqContentDisposition = Nothing
    , _cmurqServerSideEncryption = Nothing
    , _cmurqContentType = Nothing
    , _cmurqBucket = pBucket_
    , _cmurqKey = pKey_
    }

-- | The date and time at which the object is no longer cacheable.
cmurqExpires :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmurqExpires = lens _cmurqExpires (\ s a -> s{_cmurqExpires = a}) . mapping _Time;

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
cmurqSSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmurqSSECustomerAlgorithm = lens _cmurqSSECustomerAlgorithm (\ s a -> s{_cmurqSSECustomerAlgorithm = a});

-- | Allows grantee to read the object ACL.
cmurqGrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmurqGrantReadACP = lens _cmurqGrantReadACP (\ s a -> s{_cmurqGrantReadACP = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
cmurqSSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmurqSSECustomerKey = lens _cmurqSSECustomerKey (\ s a -> s{_cmurqSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
cmurqRequestPayer :: Lens' CreateMultipartUpload (Maybe RequestPayer)
cmurqRequestPayer = lens _cmurqRequestPayer (\ s a -> s{_cmurqRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable object.
cmurqGrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmurqGrantWriteACP = lens _cmurqGrantWriteACP (\ s a -> s{_cmurqGrantWriteACP = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
cmurqWebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmurqWebsiteRedirectLocation = lens _cmurqWebsiteRedirectLocation (\ s a -> s{_cmurqWebsiteRedirectLocation = a});

-- | Allows grantee to read the object data and its metadata.
cmurqGrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmurqGrantRead = lens _cmurqGrantRead (\ s a -> s{_cmurqGrantRead = a});

-- | The type of storage to use for the object. Defaults to \'STANDARD\'.
cmurqStorageClass :: Lens' CreateMultipartUpload (Maybe StorageClass)
cmurqStorageClass = lens _cmurqStorageClass (\ s a -> s{_cmurqStorageClass = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmurqContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmurqContentEncoding = lens _cmurqContentEncoding (\ s a -> s{_cmurqContentEncoding = a});

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. Documentation on configuring any of the
-- officially supported AWS SDKs and CLI can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/UsingAWSSDK.html#specify-signature-version
cmurqSSEKMSKeyId :: Lens' CreateMultipartUpload (Maybe Text)
cmurqSSEKMSKeyId = lens _cmurqSSEKMSKeyId (\ s a -> s{_cmurqSSEKMSKeyId = a}) . mapping _Sensitive;

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
cmurqGrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmurqGrantFullControl = lens _cmurqGrantFullControl (\ s a -> s{_cmurqGrantFullControl = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmurqSSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmurqSSECustomerKeyMD5 = lens _cmurqSSECustomerKeyMD5 (\ s a -> s{_cmurqSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
cmurqMetadata :: Lens' CreateMultipartUpload (HashMap Text Text)
cmurqMetadata = lens _cmurqMetadata (\ s a -> s{_cmurqMetadata = a}) . _Map;

-- | Specifies caching behavior along the request\/reply chain.
cmurqCacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmurqCacheControl = lens _cmurqCacheControl (\ s a -> s{_cmurqCacheControl = a});

-- | The language the content is in.
cmurqContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmurqContentLanguage = lens _cmurqContentLanguage (\ s a -> s{_cmurqContentLanguage = a});

-- | The canned ACL to apply to the object.
cmurqACL :: Lens' CreateMultipartUpload (Maybe ObjectCannedACL)
cmurqACL = lens _cmurqACL (\ s a -> s{_cmurqACL = a});

-- | Specifies presentational information for the object.
cmurqContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmurqContentDisposition = lens _cmurqContentDisposition (\ s a -> s{_cmurqContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cmurqServerSideEncryption :: Lens' CreateMultipartUpload (Maybe ServerSideEncryption)
cmurqServerSideEncryption = lens _cmurqServerSideEncryption (\ s a -> s{_cmurqServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
cmurqContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmurqContentType = lens _cmurqContentType (\ s a -> s{_cmurqContentType = a});

-- | FIXME: Undocumented member.
cmurqBucket :: Lens' CreateMultipartUpload BucketName
cmurqBucket = lens _cmurqBucket (\ s a -> s{_cmurqBucket = a});

-- | FIXME: Undocumented member.
cmurqKey :: Lens' CreateMultipartUpload ObjectKey
cmurqKey = lens _cmurqKey (\ s a -> s{_cmurqKey = a});

instance AWSRequest CreateMultipartUpload where
        type Sv CreateMultipartUpload = S3
        type Rs CreateMultipartUpload =
             CreateMultipartUploadResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateMultipartUploadResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (x .@? "Bucket")
                     <*> (x .@? "Key")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*> (x .@? "UploadId")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance ToHeaders CreateMultipartUpload where
        toHeaders CreateMultipartUpload'{..}
          = mconcat
              ["Expires" =# _cmurqExpires,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _cmurqSSECustomerAlgorithm,
               "x-amz-grant-read-acp" =# _cmurqGrantReadACP,
               "x-amz-server-side-encryption-customer-key" =#
                 _cmurqSSECustomerKey,
               "x-amz-request-payer" =# _cmurqRequestPayer,
               "x-amz-grant-write-acp" =# _cmurqGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _cmurqWebsiteRedirectLocation,
               "x-amz-grant-read" =# _cmurqGrantRead,
               "x-amz-storage-class" =# _cmurqStorageClass,
               "Content-Encoding" =# _cmurqContentEncoding,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _cmurqSSEKMSKeyId,
               "x-amz-grant-full-control" =# _cmurqGrantFullControl,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _cmurqSSECustomerKeyMD5,
               "x-amz-meta-" =# _cmurqMetadata,
               "Cache-Control" =# _cmurqCacheControl,
               "Content-Language" =# _cmurqContentLanguage,
               "x-amz-acl" =# _cmurqACL,
               "Content-Disposition" =# _cmurqContentDisposition,
               "x-amz-server-side-encryption" =#
                 _cmurqServerSideEncryption,
               "Content-Type" =# _cmurqContentType]

instance ToPath CreateMultipartUpload where
        toPath CreateMultipartUpload'{..}
          = mconcat
              ["/", toText _cmurqBucket, "/", toText _cmurqKey]

instance ToQuery CreateMultipartUpload where
        toQuery = const (mconcat ["uploads"])

-- | /See:/ 'createMultipartUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmursRequestCharged'
--
-- * 'cmursSSECustomerAlgorithm'
--
-- * 'cmursBucket'
--
-- * 'cmursKey'
--
-- * 'cmursSSEKMSKeyId'
--
-- * 'cmursSSECustomerKeyMD5'
--
-- * 'cmursUploadId'
--
-- * 'cmursServerSideEncryption'
--
-- * 'cmursStatus'
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
    { _cmursRequestCharged       :: !(Maybe RequestCharged)
    , _cmursSSECustomerAlgorithm :: !(Maybe Text)
    , _cmursBucket               :: !(Maybe BucketName)
    , _cmursKey                  :: !(Maybe ObjectKey)
    , _cmursSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _cmursSSECustomerKeyMD5    :: !(Maybe Text)
    , _cmursUploadId             :: !(Maybe Text)
    , _cmursServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _cmursStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CreateMultipartUploadResponse' smart constructor.
createMultipartUploadResponse :: Int -> CreateMultipartUploadResponse
createMultipartUploadResponse pStatus_ =
    CreateMultipartUploadResponse'
    { _cmursRequestCharged = Nothing
    , _cmursSSECustomerAlgorithm = Nothing
    , _cmursBucket = Nothing
    , _cmursKey = Nothing
    , _cmursSSEKMSKeyId = Nothing
    , _cmursSSECustomerKeyMD5 = Nothing
    , _cmursUploadId = Nothing
    , _cmursServerSideEncryption = Nothing
    , _cmursStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cmursRequestCharged :: Lens' CreateMultipartUploadResponse (Maybe RequestCharged)
cmursRequestCharged = lens _cmursRequestCharged (\ s a -> s{_cmursRequestCharged = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
cmursSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSECustomerAlgorithm = lens _cmursSSECustomerAlgorithm (\ s a -> s{_cmursSSECustomerAlgorithm = a});

-- | Name of the bucket to which the multipart upload was initiated.
cmursBucket :: Lens' CreateMultipartUploadResponse (Maybe BucketName)
cmursBucket = lens _cmursBucket (\ s a -> s{_cmursBucket = a});

-- | Object key for which the multipart upload was initiated.
cmursKey :: Lens' CreateMultipartUploadResponse (Maybe ObjectKey)
cmursKey = lens _cmursKey (\ s a -> s{_cmursKey = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
cmursSSEKMSKeyId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSEKMSKeyId = lens _cmursSSEKMSKeyId (\ s a -> s{_cmursSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmursSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSECustomerKeyMD5 = lens _cmursSSECustomerKeyMD5 (\ s a -> s{_cmursSSECustomerKeyMD5 = a});

-- | ID for the initiated multipart upload.
cmursUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursUploadId = lens _cmursUploadId (\ s a -> s{_cmursUploadId = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cmursServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe ServerSideEncryption)
cmursServerSideEncryption = lens _cmursServerSideEncryption (\ s a -> s{_cmursServerSideEncryption = a});

-- | FIXME: Undocumented member.
cmursStatus :: Lens' CreateMultipartUploadResponse Int
cmursStatus = lens _cmursStatus (\ s a -> s{_cmursStatus = a});

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Initiates a multipart upload and returns an upload ID.
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
    , cmuExpires
    , cmuSSECustomerAlgorithm
    , cmuGrantReadACP
    , cmuSSECustomerKey
    , cmuRequestPayer
    , cmuGrantWriteACP
    , cmuWebsiteRedirectLocation
    , cmuGrantRead
    , cmuStorageClass
    , cmuContentEncoding
    , cmuSSEKMSKeyId
    , cmuGrantFullControl
    , cmuSSECustomerKeyMD5
    , cmuMetadata
    , cmuCacheControl
    , cmuContentLanguage
    , cmuACL
    , cmuContentDisposition
    , cmuServerSideEncryption
    , cmuContentType
    , cmuBucket
    , cmuKey

    -- * Response
    , CreateMultipartUploadResponse
    -- ** Response constructor
    , createMultipartUploadResponse
    -- ** Response lenses
    , cmurRequestCharged
    , cmurSSECustomerAlgorithm
    , cmurBucket
    , cmurKey
    , cmurSSEKMSKeyId
    , cmurSSECustomerKeyMD5
    , cmurUploadId
    , cmurServerSideEncryption
    , cmurStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'createMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuExpires'
--
-- * 'cmuSSECustomerAlgorithm'
--
-- * 'cmuGrantReadACP'
--
-- * 'cmuSSECustomerKey'
--
-- * 'cmuRequestPayer'
--
-- * 'cmuGrantWriteACP'
--
-- * 'cmuWebsiteRedirectLocation'
--
-- * 'cmuGrantRead'
--
-- * 'cmuStorageClass'
--
-- * 'cmuContentEncoding'
--
-- * 'cmuSSEKMSKeyId'
--
-- * 'cmuGrantFullControl'
--
-- * 'cmuSSECustomerKeyMD5'
--
-- * 'cmuMetadata'
--
-- * 'cmuCacheControl'
--
-- * 'cmuContentLanguage'
--
-- * 'cmuACL'
--
-- * 'cmuContentDisposition'
--
-- * 'cmuServerSideEncryption'
--
-- * 'cmuContentType'
--
-- * 'cmuBucket'
--
-- * 'cmuKey'
data CreateMultipartUpload = CreateMultipartUpload'
    { _cmuExpires                 :: !(Maybe RFC822)
    , _cmuSSECustomerAlgorithm    :: !(Maybe Text)
    , _cmuGrantReadACP            :: !(Maybe Text)
    , _cmuSSECustomerKey          :: !(Maybe (Sensitive Text))
    , _cmuRequestPayer            :: !(Maybe RequestPayer)
    , _cmuGrantWriteACP           :: !(Maybe Text)
    , _cmuWebsiteRedirectLocation :: !(Maybe Text)
    , _cmuGrantRead               :: !(Maybe Text)
    , _cmuStorageClass            :: !(Maybe StorageClass)
    , _cmuContentEncoding         :: !(Maybe Text)
    , _cmuSSEKMSKeyId             :: !(Maybe (Sensitive Text))
    , _cmuGrantFullControl        :: !(Maybe Text)
    , _cmuSSECustomerKeyMD5       :: !(Maybe Text)
    , _cmuMetadata                :: !(Map Text Text)
    , _cmuCacheControl            :: !(Maybe Text)
    , _cmuContentLanguage         :: !(Maybe Text)
    , _cmuACL                     :: !(Maybe ObjectCannedACL)
    , _cmuContentDisposition      :: !(Maybe Text)
    , _cmuServerSideEncryption    :: !(Maybe ServerSideEncryption)
    , _cmuContentType             :: !(Maybe Text)
    , _cmuBucket                  :: !BucketName
    , _cmuKey                     :: !ObjectKey
    } deriving (Eq,Read,Show)

-- | 'CreateMultipartUpload' smart constructor.
createMultipartUpload :: BucketName -> ObjectKey -> CreateMultipartUpload
createMultipartUpload pBucket pKey =
    CreateMultipartUpload'
    { _cmuExpires = Nothing
    , _cmuSSECustomerAlgorithm = Nothing
    , _cmuGrantReadACP = Nothing
    , _cmuSSECustomerKey = Nothing
    , _cmuRequestPayer = Nothing
    , _cmuGrantWriteACP = Nothing
    , _cmuWebsiteRedirectLocation = Nothing
    , _cmuGrantRead = Nothing
    , _cmuStorageClass = Nothing
    , _cmuContentEncoding = Nothing
    , _cmuSSEKMSKeyId = Nothing
    , _cmuGrantFullControl = Nothing
    , _cmuSSECustomerKeyMD5 = Nothing
    , _cmuMetadata = mempty
    , _cmuCacheControl = Nothing
    , _cmuContentLanguage = Nothing
    , _cmuACL = Nothing
    , _cmuContentDisposition = Nothing
    , _cmuServerSideEncryption = Nothing
    , _cmuContentType = Nothing
    , _cmuBucket = pBucket
    , _cmuKey = pKey
    }

-- | The date and time at which the object is no longer cacheable.
cmuExpires :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmuExpires = lens _cmuExpires (\ s a -> s{_cmuExpires = a}) . mapping _Time;

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
cmuSSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerAlgorithm = lens _cmuSSECustomerAlgorithm (\ s a -> s{_cmuSSECustomerAlgorithm = a});

-- | Allows grantee to read the object ACL.
cmuGrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantReadACP = lens _cmuGrantReadACP (\ s a -> s{_cmuGrantReadACP = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
cmuSSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKey = lens _cmuSSECustomerKey (\ s a -> s{_cmuSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
cmuRequestPayer :: Lens' CreateMultipartUpload (Maybe RequestPayer)
cmuRequestPayer = lens _cmuRequestPayer (\ s a -> s{_cmuRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable object.
cmuGrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantWriteACP = lens _cmuGrantWriteACP (\ s a -> s{_cmuGrantWriteACP = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
cmuWebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmuWebsiteRedirectLocation = lens _cmuWebsiteRedirectLocation (\ s a -> s{_cmuWebsiteRedirectLocation = a});

-- | Allows grantee to read the object data and its metadata.
cmuGrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantRead = lens _cmuGrantRead (\ s a -> s{_cmuGrantRead = a});

-- | The type of storage to use for the object. Defaults to \'STANDARD\'.
cmuStorageClass :: Lens' CreateMultipartUpload (Maybe StorageClass)
cmuStorageClass = lens _cmuStorageClass (\ s a -> s{_cmuStorageClass = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
cmuContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentEncoding = lens _cmuContentEncoding (\ s a -> s{_cmuContentEncoding = a});

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. Documentation on configuring any of the
-- officially supported AWS SDKs and CLI can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/UsingAWSSDK.html#specify-signature-version
cmuSSEKMSKeyId :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSEKMSKeyId = lens _cmuSSEKMSKeyId (\ s a -> s{_cmuSSEKMSKeyId = a}) . mapping _Sensitive;

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
cmuGrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantFullControl = lens _cmuGrantFullControl (\ s a -> s{_cmuGrantFullControl = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
cmuSSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKeyMD5 = lens _cmuSSECustomerKeyMD5 (\ s a -> s{_cmuSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
cmuMetadata :: Lens' CreateMultipartUpload (HashMap Text Text)
cmuMetadata = lens _cmuMetadata (\ s a -> s{_cmuMetadata = a}) . _Map;

-- | Specifies caching behavior along the request\/reply chain.
cmuCacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuCacheControl = lens _cmuCacheControl (\ s a -> s{_cmuCacheControl = a});

-- | The language the content is in.
cmuContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentLanguage = lens _cmuContentLanguage (\ s a -> s{_cmuContentLanguage = a});

-- | The canned ACL to apply to the object.
cmuACL :: Lens' CreateMultipartUpload (Maybe ObjectCannedACL)
cmuACL = lens _cmuACL (\ s a -> s{_cmuACL = a});

-- | Specifies presentational information for the object.
cmuContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentDisposition = lens _cmuContentDisposition (\ s a -> s{_cmuContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cmuServerSideEncryption :: Lens' CreateMultipartUpload (Maybe ServerSideEncryption)
cmuServerSideEncryption = lens _cmuServerSideEncryption (\ s a -> s{_cmuServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
cmuContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentType = lens _cmuContentType (\ s a -> s{_cmuContentType = a});

-- | FIXME: Undocumented member.
cmuBucket :: Lens' CreateMultipartUpload BucketName
cmuBucket = lens _cmuBucket (\ s a -> s{_cmuBucket = a});

-- | FIXME: Undocumented member.
cmuKey :: Lens' CreateMultipartUpload ObjectKey
cmuKey = lens _cmuKey (\ s a -> s{_cmuKey = a});

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
                     <*> (pure s))

instance ToHeaders CreateMultipartUpload where
        toHeaders CreateMultipartUpload'{..}
          = mconcat
              ["Expires" =# _cmuExpires,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _cmuSSECustomerAlgorithm,
               "x-amz-grant-read-acp" =# _cmuGrantReadACP,
               "x-amz-server-side-encryption-customer-key" =#
                 _cmuSSECustomerKey,
               "x-amz-request-payer" =# _cmuRequestPayer,
               "x-amz-grant-write-acp" =# _cmuGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _cmuWebsiteRedirectLocation,
               "x-amz-grant-read" =# _cmuGrantRead,
               "x-amz-storage-class" =# _cmuStorageClass,
               "Content-Encoding" =# _cmuContentEncoding,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _cmuSSEKMSKeyId,
               "x-amz-grant-full-control" =# _cmuGrantFullControl,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _cmuSSECustomerKeyMD5,
               "x-amz-meta-" =# _cmuMetadata,
               "Cache-Control" =# _cmuCacheControl,
               "Content-Language" =# _cmuContentLanguage,
               "x-amz-acl" =# _cmuACL,
               "Content-Disposition" =# _cmuContentDisposition,
               "x-amz-server-side-encryption" =#
                 _cmuServerSideEncryption,
               "Content-Type" =# _cmuContentType]

instance ToPath CreateMultipartUpload where
        toPath CreateMultipartUpload'{..}
          = mconcat
              ["/", toText _cmuBucket, "/", toText _cmuKey]

instance ToQuery CreateMultipartUpload where
        toQuery = const (mconcat ["uploads"])

-- | /See:/ 'createMultipartUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmurRequestCharged'
--
-- * 'cmurSSECustomerAlgorithm'
--
-- * 'cmurBucket'
--
-- * 'cmurKey'
--
-- * 'cmurSSEKMSKeyId'
--
-- * 'cmurSSECustomerKeyMD5'
--
-- * 'cmurUploadId'
--
-- * 'cmurServerSideEncryption'
--
-- * 'cmurStatus'
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
    { _cmurRequestCharged       :: !(Maybe RequestCharged)
    , _cmurSSECustomerAlgorithm :: !(Maybe Text)
    , _cmurBucket               :: !(Maybe BucketName)
    , _cmurKey                  :: !(Maybe ObjectKey)
    , _cmurSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _cmurSSECustomerKeyMD5    :: !(Maybe Text)
    , _cmurUploadId             :: !(Maybe Text)
    , _cmurServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _cmurStatus               :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateMultipartUploadResponse' smart constructor.
createMultipartUploadResponse :: Status -> CreateMultipartUploadResponse
createMultipartUploadResponse pStatus =
    CreateMultipartUploadResponse'
    { _cmurRequestCharged = Nothing
    , _cmurSSECustomerAlgorithm = Nothing
    , _cmurBucket = Nothing
    , _cmurKey = Nothing
    , _cmurSSEKMSKeyId = Nothing
    , _cmurSSECustomerKeyMD5 = Nothing
    , _cmurUploadId = Nothing
    , _cmurServerSideEncryption = Nothing
    , _cmurStatus = pStatus
    }

-- | FIXME: Undocumented member.
cmurRequestCharged :: Lens' CreateMultipartUploadResponse (Maybe RequestCharged)
cmurRequestCharged = lens _cmurRequestCharged (\ s a -> s{_cmurRequestCharged = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
cmurSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurSSECustomerAlgorithm = lens _cmurSSECustomerAlgorithm (\ s a -> s{_cmurSSECustomerAlgorithm = a});

-- | Name of the bucket to which the multipart upload was initiated.
cmurBucket :: Lens' CreateMultipartUploadResponse (Maybe BucketName)
cmurBucket = lens _cmurBucket (\ s a -> s{_cmurBucket = a});

-- | Object key for which the multipart upload was initiated.
cmurKey :: Lens' CreateMultipartUploadResponse (Maybe ObjectKey)
cmurKey = lens _cmurKey (\ s a -> s{_cmurKey = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
cmurSSEKMSKeyId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurSSEKMSKeyId = lens _cmurSSEKMSKeyId (\ s a -> s{_cmurSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
cmurSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurSSECustomerKeyMD5 = lens _cmurSSECustomerKeyMD5 (\ s a -> s{_cmurSSECustomerKeyMD5 = a});

-- | ID for the initiated multipart upload.
cmurUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmurUploadId = lens _cmurUploadId (\ s a -> s{_cmurUploadId = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
cmurServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe ServerSideEncryption)
cmurServerSideEncryption = lens _cmurServerSideEncryption (\ s a -> s{_cmurServerSideEncryption = a});

-- | FIXME: Undocumented member.
cmurStatus :: Lens' CreateMultipartUploadResponse Status
cmurStatus = lens _cmurStatus (\ s a -> s{_cmurStatus = a});

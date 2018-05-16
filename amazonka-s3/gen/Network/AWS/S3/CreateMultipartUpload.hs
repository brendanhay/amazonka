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
-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a multipart upload and returns an upload ID.
--
--
-- __Note:__ After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.
--
module Network.AWS.S3.CreateMultipartUpload
    (
    -- * Creating a Request
      createMultipartUpload
    , CreateMultipartUpload
    -- * Request Lenses
    , cmuExpires
    , cmuGrantReadACP
    , cmuSSECustomerAlgorithm
    , cmuSSECustomerKey
    , cmuRequestPayer
    , cmuGrantWriteACP
    , cmuWebsiteRedirectLocation
    , cmuGrantRead
    , cmuStorageClass
    , cmuSSECustomerKeyMD5
    , cmuSSEKMSKeyId
    , cmuGrantFullControl
    , cmuContentEncoding
    , cmuTagging
    , cmuMetadata
    , cmuCacheControl
    , cmuContentLanguage
    , cmuACL
    , cmuContentDisposition
    , cmuServerSideEncryption
    , cmuContentType
    , cmuBucket
    , cmuKey

    -- * Destructuring the Response
    , createMultipartUploadResponse
    , CreateMultipartUploadResponse
    -- * Response Lenses
    , cmursRequestCharged
    , cmursBucket
    , cmursSSECustomerAlgorithm
    , cmursAbortDate
    , cmursAbortRuleId
    , cmursKey
    , cmursSSECustomerKeyMD5
    , cmursSSEKMSKeyId
    , cmursUploadId
    , cmursServerSideEncryption
    , cmursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'createMultipartUpload' smart constructor.
data CreateMultipartUpload = CreateMultipartUpload'
  { _cmuExpires                 :: !(Maybe RFC822)
  , _cmuGrantReadACP            :: !(Maybe Text)
  , _cmuSSECustomerAlgorithm    :: !(Maybe Text)
  , _cmuSSECustomerKey          :: !(Maybe (Sensitive Text))
  , _cmuRequestPayer            :: !(Maybe RequestPayer)
  , _cmuGrantWriteACP           :: !(Maybe Text)
  , _cmuWebsiteRedirectLocation :: !(Maybe Text)
  , _cmuGrantRead               :: !(Maybe Text)
  , _cmuStorageClass            :: !(Maybe StorageClass)
  , _cmuSSECustomerKeyMD5       :: !(Maybe Text)
  , _cmuSSEKMSKeyId             :: !(Maybe (Sensitive Text))
  , _cmuGrantFullControl        :: !(Maybe Text)
  , _cmuContentEncoding         :: !(Maybe Text)
  , _cmuTagging                 :: !(Maybe Text)
  , _cmuMetadata                :: !(Map Text Text)
  , _cmuCacheControl            :: !(Maybe Text)
  , _cmuContentLanguage         :: !(Maybe Text)
  , _cmuACL                     :: !(Maybe ObjectCannedACL)
  , _cmuContentDisposition      :: !(Maybe Text)
  , _cmuServerSideEncryption    :: !(Maybe ServerSideEncryption)
  , _cmuContentType             :: !(Maybe Text)
  , _cmuBucket                  :: !BucketName
  , _cmuKey                     :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmuExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'cmuGrantReadACP' - Allows grantee to read the object ACL.
--
-- * 'cmuSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'cmuSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
--
-- * 'cmuRequestPayer' - Undocumented member.
--
-- * 'cmuGrantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- * 'cmuWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'cmuGrantRead' - Allows grantee to read the object data and its metadata.
--
-- * 'cmuStorageClass' - The type of storage to use for the object. Defaults to 'STANDARD'.
--
-- * 'cmuSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'cmuSSEKMSKeyId' - Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. Documentation on configuring any of the officially supported AWS SDKs and CLI can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version
--
-- * 'cmuGrantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- * 'cmuContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'cmuTagging' - The tag-set for the object. The tag-set must be encoded as URL Query parameters
--
-- * 'cmuMetadata' - A map of metadata to store with the object in S3.
--
-- * 'cmuCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'cmuContentLanguage' - The language the content is in.
--
-- * 'cmuACL' - The canned ACL to apply to the object.
--
-- * 'cmuContentDisposition' - Specifies presentational information for the object.
--
-- * 'cmuServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'cmuContentType' - A standard MIME type describing the format of the object data.
--
-- * 'cmuBucket' - Undocumented member.
--
-- * 'cmuKey' - Undocumented member.
createMultipartUpload
    :: BucketName -- ^ 'cmuBucket'
    -> ObjectKey -- ^ 'cmuKey'
    -> CreateMultipartUpload
createMultipartUpload pBucket_ pKey_ =
  CreateMultipartUpload'
    { _cmuExpires = Nothing
    , _cmuGrantReadACP = Nothing
    , _cmuSSECustomerAlgorithm = Nothing
    , _cmuSSECustomerKey = Nothing
    , _cmuRequestPayer = Nothing
    , _cmuGrantWriteACP = Nothing
    , _cmuWebsiteRedirectLocation = Nothing
    , _cmuGrantRead = Nothing
    , _cmuStorageClass = Nothing
    , _cmuSSECustomerKeyMD5 = Nothing
    , _cmuSSEKMSKeyId = Nothing
    , _cmuGrantFullControl = Nothing
    , _cmuContentEncoding = Nothing
    , _cmuTagging = Nothing
    , _cmuMetadata = mempty
    , _cmuCacheControl = Nothing
    , _cmuContentLanguage = Nothing
    , _cmuACL = Nothing
    , _cmuContentDisposition = Nothing
    , _cmuServerSideEncryption = Nothing
    , _cmuContentType = Nothing
    , _cmuBucket = pBucket_
    , _cmuKey = pKey_
    }


-- | The date and time at which the object is no longer cacheable.
cmuExpires :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmuExpires = lens _cmuExpires (\ s a -> s{_cmuExpires = a}) . mapping _Time

-- | Allows grantee to read the object ACL.
cmuGrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantReadACP = lens _cmuGrantReadACP (\ s a -> s{_cmuGrantReadACP = a})

-- | Specifies the algorithm to use to when encrypting the object (e.g., AES256).
cmuSSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerAlgorithm = lens _cmuSSECustomerAlgorithm (\ s a -> s{_cmuSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
cmuSSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKey = lens _cmuSSECustomerKey (\ s a -> s{_cmuSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
cmuRequestPayer :: Lens' CreateMultipartUpload (Maybe RequestPayer)
cmuRequestPayer = lens _cmuRequestPayer (\ s a -> s{_cmuRequestPayer = a})

-- | Allows grantee to write the ACL for the applicable object.
cmuGrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantWriteACP = lens _cmuGrantWriteACP (\ s a -> s{_cmuGrantWriteACP = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
cmuWebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmuWebsiteRedirectLocation = lens _cmuWebsiteRedirectLocation (\ s a -> s{_cmuWebsiteRedirectLocation = a})

-- | Allows grantee to read the object data and its metadata.
cmuGrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantRead = lens _cmuGrantRead (\ s a -> s{_cmuGrantRead = a})

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
cmuStorageClass :: Lens' CreateMultipartUpload (Maybe StorageClass)
cmuStorageClass = lens _cmuStorageClass (\ s a -> s{_cmuStorageClass = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
cmuSSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKeyMD5 = lens _cmuSSECustomerKeyMD5 (\ s a -> s{_cmuSSECustomerKeyMD5 = a})

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. Documentation on configuring any of the officially supported AWS SDKs and CLI can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version
cmuSSEKMSKeyId :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSEKMSKeyId = lens _cmuSSEKMSKeyId (\ s a -> s{_cmuSSEKMSKeyId = a}) . mapping _Sensitive

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
cmuGrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantFullControl = lens _cmuGrantFullControl (\ s a -> s{_cmuGrantFullControl = a})

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
cmuContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentEncoding = lens _cmuContentEncoding (\ s a -> s{_cmuContentEncoding = a})

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters
cmuTagging :: Lens' CreateMultipartUpload (Maybe Text)
cmuTagging = lens _cmuTagging (\ s a -> s{_cmuTagging = a})

-- | A map of metadata to store with the object in S3.
cmuMetadata :: Lens' CreateMultipartUpload (HashMap Text Text)
cmuMetadata = lens _cmuMetadata (\ s a -> s{_cmuMetadata = a}) . _Map

-- | Specifies caching behavior along the request/reply chain.
cmuCacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuCacheControl = lens _cmuCacheControl (\ s a -> s{_cmuCacheControl = a})

-- | The language the content is in.
cmuContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentLanguage = lens _cmuContentLanguage (\ s a -> s{_cmuContentLanguage = a})

-- | The canned ACL to apply to the object.
cmuACL :: Lens' CreateMultipartUpload (Maybe ObjectCannedACL)
cmuACL = lens _cmuACL (\ s a -> s{_cmuACL = a})

-- | Specifies presentational information for the object.
cmuContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentDisposition = lens _cmuContentDisposition (\ s a -> s{_cmuContentDisposition = a})

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
cmuServerSideEncryption :: Lens' CreateMultipartUpload (Maybe ServerSideEncryption)
cmuServerSideEncryption = lens _cmuServerSideEncryption (\ s a -> s{_cmuServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
cmuContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentType = lens _cmuContentType (\ s a -> s{_cmuContentType = a})

-- | Undocumented member.
cmuBucket :: Lens' CreateMultipartUpload BucketName
cmuBucket = lens _cmuBucket (\ s a -> s{_cmuBucket = a})

-- | Undocumented member.
cmuKey :: Lens' CreateMultipartUpload ObjectKey
cmuKey = lens _cmuKey (\ s a -> s{_cmuKey = a})

instance AWSRequest CreateMultipartUpload where
        type Rs CreateMultipartUpload =
             CreateMultipartUploadResponse
        request = post s3
        response
          = receiveXML
              (\ s h x ->
                 CreateMultipartUploadResponse' <$>
                   (h .#? "x-amz-request-charged") <*> (x .@? "Bucket")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (h .#? "x-amz-abort-date")
                     <*> (h .#? "x-amz-abort-rule-id")
                     <*> (x .@? "Key")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*> (x .@? "UploadId")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance Hashable CreateMultipartUpload where

instance NFData CreateMultipartUpload where

instance ToHeaders CreateMultipartUpload where
        toHeaders CreateMultipartUpload'{..}
          = mconcat
              ["Expires" =# _cmuExpires,
               "x-amz-grant-read-acp" =# _cmuGrantReadACP,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _cmuSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _cmuSSECustomerKey,
               "x-amz-request-payer" =# _cmuRequestPayer,
               "x-amz-grant-write-acp" =# _cmuGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _cmuWebsiteRedirectLocation,
               "x-amz-grant-read" =# _cmuGrantRead,
               "x-amz-storage-class" =# _cmuStorageClass,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _cmuSSECustomerKeyMD5,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _cmuSSEKMSKeyId,
               "x-amz-grant-full-control" =# _cmuGrantFullControl,
               "Content-Encoding" =# _cmuContentEncoding,
               "x-amz-tagging" =# _cmuTagging,
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
          = mconcat ["/", toBS _cmuBucket, "/", toBS _cmuKey]

instance ToQuery CreateMultipartUpload where
        toQuery = const (mconcat ["uploads"])

-- | /See:/ 'createMultipartUploadResponse' smart constructor.
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
  { _cmursRequestCharged       :: !(Maybe RequestCharged)
  , _cmursBucket               :: !(Maybe BucketName)
  , _cmursSSECustomerAlgorithm :: !(Maybe Text)
  , _cmursAbortDate            :: !(Maybe RFC822)
  , _cmursAbortRuleId          :: !(Maybe Text)
  , _cmursKey                  :: !(Maybe ObjectKey)
  , _cmursSSECustomerKeyMD5    :: !(Maybe Text)
  , _cmursSSEKMSKeyId          :: !(Maybe (Sensitive Text))
  , _cmursUploadId             :: !(Maybe Text)
  , _cmursServerSideEncryption :: !(Maybe ServerSideEncryption)
  , _cmursResponseStatus       :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMultipartUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmursRequestCharged' - Undocumented member.
--
-- * 'cmursBucket' - Name of the bucket to which the multipart upload was initiated.
--
-- * 'cmursSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'cmursAbortDate' - Date when multipart upload will become eligible for abort operation by lifecycle.
--
-- * 'cmursAbortRuleId' - Id of the lifecycle rule that makes a multipart upload eligible for abort operation.
--
-- * 'cmursKey' - Object key for which the multipart upload was initiated.
--
-- * 'cmursSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
--
-- * 'cmursSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
--
-- * 'cmursUploadId' - ID for the initiated multipart upload.
--
-- * 'cmursServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'cmursResponseStatus' - -- | The response status code.
createMultipartUploadResponse
    :: Int -- ^ 'cmursResponseStatus'
    -> CreateMultipartUploadResponse
createMultipartUploadResponse pResponseStatus_ =
  CreateMultipartUploadResponse'
    { _cmursRequestCharged = Nothing
    , _cmursBucket = Nothing
    , _cmursSSECustomerAlgorithm = Nothing
    , _cmursAbortDate = Nothing
    , _cmursAbortRuleId = Nothing
    , _cmursKey = Nothing
    , _cmursSSECustomerKeyMD5 = Nothing
    , _cmursSSEKMSKeyId = Nothing
    , _cmursUploadId = Nothing
    , _cmursServerSideEncryption = Nothing
    , _cmursResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cmursRequestCharged :: Lens' CreateMultipartUploadResponse (Maybe RequestCharged)
cmursRequestCharged = lens _cmursRequestCharged (\ s a -> s{_cmursRequestCharged = a})

-- | Name of the bucket to which the multipart upload was initiated.
cmursBucket :: Lens' CreateMultipartUploadResponse (Maybe BucketName)
cmursBucket = lens _cmursBucket (\ s a -> s{_cmursBucket = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
cmursSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSECustomerAlgorithm = lens _cmursSSECustomerAlgorithm (\ s a -> s{_cmursSSECustomerAlgorithm = a})

-- | Date when multipart upload will become eligible for abort operation by lifecycle.
cmursAbortDate :: Lens' CreateMultipartUploadResponse (Maybe UTCTime)
cmursAbortDate = lens _cmursAbortDate (\ s a -> s{_cmursAbortDate = a}) . mapping _Time

-- | Id of the lifecycle rule that makes a multipart upload eligible for abort operation.
cmursAbortRuleId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursAbortRuleId = lens _cmursAbortRuleId (\ s a -> s{_cmursAbortRuleId = a})

-- | Object key for which the multipart upload was initiated.
cmursKey :: Lens' CreateMultipartUploadResponse (Maybe ObjectKey)
cmursKey = lens _cmursKey (\ s a -> s{_cmursKey = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
cmursSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSECustomerKeyMD5 = lens _cmursSSECustomerKeyMD5 (\ s a -> s{_cmursSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
cmursSSEKMSKeyId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSEKMSKeyId = lens _cmursSSEKMSKeyId (\ s a -> s{_cmursSSEKMSKeyId = a}) . mapping _Sensitive

-- | ID for the initiated multipart upload.
cmursUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursUploadId = lens _cmursUploadId (\ s a -> s{_cmursUploadId = a})

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
cmursServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe ServerSideEncryption)
cmursServerSideEncryption = lens _cmursServerSideEncryption (\ s a -> s{_cmursServerSideEncryption = a})

-- | -- | The response status code.
cmursResponseStatus :: Lens' CreateMultipartUploadResponse Int
cmursResponseStatus = lens _cmursResponseStatus (\ s a -> s{_cmursResponseStatus = a})

instance NFData CreateMultipartUploadResponse where

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
-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an object that is already stored in Amazon S3.
module Network.AWS.S3.CopyObject
    (
    -- * Creating a Request
      copyObject
    , CopyObject
    -- * Request Lenses
    , coCopySourceIfModifiedSince
    , coCopySourceIfUnmodifiedSince
    , coCopySourceSSECustomerKeyMD5
    , coTaggingDirective
    , coMetadataDirective
    , coExpires
    , coGrantReadACP
    , coCopySourceIfNoneMatch
    , coSSECustomerAlgorithm
    , coSSECustomerKey
    , coRequestPayer
    , coGrantWriteACP
    , coCopySourceIfMatch
    , coWebsiteRedirectLocation
    , coGrantRead
    , coStorageClass
    , coSSECustomerKeyMD5
    , coSSEKMSKeyId
    , coGrantFullControl
    , coContentEncoding
    , coTagging
    , coMetadata
    , coCacheControl
    , coContentLanguage
    , coCopySourceSSECustomerKey
    , coCopySourceSSECustomerAlgorithm
    , coACL
    , coContentDisposition
    , coServerSideEncryption
    , coContentType
    , coBucket
    , coCopySource
    , coKey

    -- * Destructuring the Response
    , copyObjectResponse
    , CopyObjectResponse
    -- * Response Lenses
    , corsRequestCharged
    , corsVersionId
    , corsExpiration
    , corsSSECustomerAlgorithm
    , corsCopySourceVersionId
    , corsSSECustomerKeyMD5
    , corsSSEKMSKeyId
    , corsServerSideEncryption
    , corsCopyObjectResult
    , corsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'copyObject' smart constructor.
data CopyObject = CopyObject'
  { _coCopySourceIfModifiedSince      :: !(Maybe RFC822)
  , _coCopySourceIfUnmodifiedSince    :: !(Maybe RFC822)
  , _coCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
  , _coTaggingDirective               :: !(Maybe TaggingDirective)
  , _coMetadataDirective              :: !(Maybe MetadataDirective)
  , _coExpires                        :: !(Maybe RFC822)
  , _coGrantReadACP                   :: !(Maybe Text)
  , _coCopySourceIfNoneMatch          :: !(Maybe Text)
  , _coSSECustomerAlgorithm           :: !(Maybe Text)
  , _coSSECustomerKey                 :: !(Maybe (Sensitive Text))
  , _coRequestPayer                   :: !(Maybe RequestPayer)
  , _coGrantWriteACP                  :: !(Maybe Text)
  , _coCopySourceIfMatch              :: !(Maybe Text)
  , _coWebsiteRedirectLocation        :: !(Maybe Text)
  , _coGrantRead                      :: !(Maybe Text)
  , _coStorageClass                   :: !(Maybe StorageClass)
  , _coSSECustomerKeyMD5              :: !(Maybe Text)
  , _coSSEKMSKeyId                    :: !(Maybe (Sensitive Text))
  , _coGrantFullControl               :: !(Maybe Text)
  , _coContentEncoding                :: !(Maybe Text)
  , _coTagging                        :: !(Maybe Text)
  , _coMetadata                       :: !(Map Text Text)
  , _coCacheControl                   :: !(Maybe Text)
  , _coContentLanguage                :: !(Maybe Text)
  , _coCopySourceSSECustomerKey       :: !(Maybe (Sensitive Text))
  , _coCopySourceSSECustomerAlgorithm :: !(Maybe Text)
  , _coACL                            :: !(Maybe ObjectCannedACL)
  , _coContentDisposition             :: !(Maybe Text)
  , _coServerSideEncryption           :: !(Maybe ServerSideEncryption)
  , _coContentType                    :: !(Maybe Text)
  , _coBucket                         :: !BucketName
  , _coCopySource                     :: !Text
  , _coKey                            :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCopySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- * 'coCopySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
--
-- * 'coCopySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'coTaggingDirective' - Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
--
-- * 'coMetadataDirective' - Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
--
-- * 'coExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'coGrantReadACP' - Allows grantee to read the object ACL.
--
-- * 'coCopySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- * 'coSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'coSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
--
-- * 'coRequestPayer' - Undocumented member.
--
-- * 'coGrantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- * 'coCopySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
--
-- * 'coWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'coGrantRead' - Allows grantee to read the object data and its metadata.
--
-- * 'coStorageClass' - The type of storage to use for the object. Defaults to 'STANDARD'.
--
-- * 'coSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'coSSEKMSKeyId' - Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. Documentation on configuring any of the officially supported AWS SDKs and CLI can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version
--
-- * 'coGrantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- * 'coContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'coTagging' - The tag-set for the object destination object this value must be used in conjunction with the TaggingDirective. The tag-set must be encoded as URL Query parameters
--
-- * 'coMetadata' - A map of metadata to store with the object in S3.
--
-- * 'coCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'coContentLanguage' - The language the content is in.
--
-- * 'coCopySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- * 'coCopySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (e.g., AES256).
--
-- * 'coACL' - The canned ACL to apply to the object.
--
-- * 'coContentDisposition' - Specifies presentational information for the object.
--
-- * 'coServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'coContentType' - A standard MIME type describing the format of the object data.
--
-- * 'coBucket' - Undocumented member.
--
-- * 'coCopySource' - The name of the source bucket and key name of the source object, separated by a slash (/). Must be URL-encoded.
--
-- * 'coKey' - Undocumented member.
copyObject
    :: BucketName -- ^ 'coBucket'
    -> Text -- ^ 'coCopySource'
    -> ObjectKey -- ^ 'coKey'
    -> CopyObject
copyObject pBucket_ pCopySource_ pKey_ =
  CopyObject'
    { _coCopySourceIfModifiedSince = Nothing
    , _coCopySourceIfUnmodifiedSince = Nothing
    , _coCopySourceSSECustomerKeyMD5 = Nothing
    , _coTaggingDirective = Nothing
    , _coMetadataDirective = Nothing
    , _coExpires = Nothing
    , _coGrantReadACP = Nothing
    , _coCopySourceIfNoneMatch = Nothing
    , _coSSECustomerAlgorithm = Nothing
    , _coSSECustomerKey = Nothing
    , _coRequestPayer = Nothing
    , _coGrantWriteACP = Nothing
    , _coCopySourceIfMatch = Nothing
    , _coWebsiteRedirectLocation = Nothing
    , _coGrantRead = Nothing
    , _coStorageClass = Nothing
    , _coSSECustomerKeyMD5 = Nothing
    , _coSSEKMSKeyId = Nothing
    , _coGrantFullControl = Nothing
    , _coContentEncoding = Nothing
    , _coTagging = Nothing
    , _coMetadata = mempty
    , _coCacheControl = Nothing
    , _coContentLanguage = Nothing
    , _coCopySourceSSECustomerKey = Nothing
    , _coCopySourceSSECustomerAlgorithm = Nothing
    , _coACL = Nothing
    , _coContentDisposition = Nothing
    , _coServerSideEncryption = Nothing
    , _coContentType = Nothing
    , _coBucket = pBucket_
    , _coCopySource = pCopySource_
    , _coKey = pKey_
    }


-- | Copies the object if it has been modified since the specified time.
coCopySourceIfModifiedSince :: Lens' CopyObject (Maybe UTCTime)
coCopySourceIfModifiedSince = lens _coCopySourceIfModifiedSince (\ s a -> s{_coCopySourceIfModifiedSince = a}) . mapping _Time

-- | Copies the object if it hasn't been modified since the specified time.
coCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe UTCTime)
coCopySourceIfUnmodifiedSince = lens _coCopySourceIfUnmodifiedSince (\ s a -> s{_coCopySourceIfUnmodifiedSince = a}) . mapping _Time

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
coCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKeyMD5 = lens _coCopySourceSSECustomerKeyMD5 (\ s a -> s{_coCopySourceSSECustomerKeyMD5 = a})

-- | Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
coTaggingDirective :: Lens' CopyObject (Maybe TaggingDirective)
coTaggingDirective = lens _coTaggingDirective (\ s a -> s{_coTaggingDirective = a})

-- | Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
coMetadataDirective :: Lens' CopyObject (Maybe MetadataDirective)
coMetadataDirective = lens _coMetadataDirective (\ s a -> s{_coMetadataDirective = a})

-- | The date and time at which the object is no longer cacheable.
coExpires :: Lens' CopyObject (Maybe UTCTime)
coExpires = lens _coExpires (\ s a -> s{_coExpires = a}) . mapping _Time

-- | Allows grantee to read the object ACL.
coGrantReadACP :: Lens' CopyObject (Maybe Text)
coGrantReadACP = lens _coGrantReadACP (\ s a -> s{_coGrantReadACP = a})

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
coCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfNoneMatch = lens _coCopySourceIfNoneMatch (\ s a -> s{_coCopySourceIfNoneMatch = a})

-- | Specifies the algorithm to use to when encrypting the object (e.g., AES256).
coSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coSSECustomerAlgorithm = lens _coSSECustomerAlgorithm (\ s a -> s{_coSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
coSSECustomerKey :: Lens' CopyObject (Maybe Text)
coSSECustomerKey = lens _coSSECustomerKey (\ s a -> s{_coSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
coRequestPayer :: Lens' CopyObject (Maybe RequestPayer)
coRequestPayer = lens _coRequestPayer (\ s a -> s{_coRequestPayer = a})

-- | Allows grantee to write the ACL for the applicable object.
coGrantWriteACP :: Lens' CopyObject (Maybe Text)
coGrantWriteACP = lens _coGrantWriteACP (\ s a -> s{_coGrantWriteACP = a})

-- | Copies the object if its entity tag (ETag) matches the specified tag.
coCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfMatch = lens _coCopySourceIfMatch (\ s a -> s{_coCopySourceIfMatch = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
coWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
coWebsiteRedirectLocation = lens _coWebsiteRedirectLocation (\ s a -> s{_coWebsiteRedirectLocation = a})

-- | Allows grantee to read the object data and its metadata.
coGrantRead :: Lens' CopyObject (Maybe Text)
coGrantRead = lens _coGrantRead (\ s a -> s{_coGrantRead = a})

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
coStorageClass :: Lens' CopyObject (Maybe StorageClass)
coStorageClass = lens _coStorageClass (\ s a -> s{_coStorageClass = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
coSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coSSECustomerKeyMD5 = lens _coSSECustomerKeyMD5 (\ s a -> s{_coSSECustomerKeyMD5 = a})

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. Documentation on configuring any of the officially supported AWS SDKs and CLI can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version
coSSEKMSKeyId :: Lens' CopyObject (Maybe Text)
coSSEKMSKeyId = lens _coSSEKMSKeyId (\ s a -> s{_coSSEKMSKeyId = a}) . mapping _Sensitive

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
coGrantFullControl :: Lens' CopyObject (Maybe Text)
coGrantFullControl = lens _coGrantFullControl (\ s a -> s{_coGrantFullControl = a})

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
coContentEncoding :: Lens' CopyObject (Maybe Text)
coContentEncoding = lens _coContentEncoding (\ s a -> s{_coContentEncoding = a})

-- | The tag-set for the object destination object this value must be used in conjunction with the TaggingDirective. The tag-set must be encoded as URL Query parameters
coTagging :: Lens' CopyObject (Maybe Text)
coTagging = lens _coTagging (\ s a -> s{_coTagging = a})

-- | A map of metadata to store with the object in S3.
coMetadata :: Lens' CopyObject (HashMap Text Text)
coMetadata = lens _coMetadata (\ s a -> s{_coMetadata = a}) . _Map

-- | Specifies caching behavior along the request/reply chain.
coCacheControl :: Lens' CopyObject (Maybe Text)
coCacheControl = lens _coCacheControl (\ s a -> s{_coCacheControl = a})

-- | The language the content is in.
coContentLanguage :: Lens' CopyObject (Maybe Text)
coContentLanguage = lens _coContentLanguage (\ s a -> s{_coContentLanguage = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
coCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKey = lens _coCopySourceSSECustomerKey (\ s a -> s{_coCopySourceSSECustomerKey = a}) . mapping _Sensitive

-- | Specifies the algorithm to use when decrypting the source object (e.g., AES256).
coCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerAlgorithm = lens _coCopySourceSSECustomerAlgorithm (\ s a -> s{_coCopySourceSSECustomerAlgorithm = a})

-- | The canned ACL to apply to the object.
coACL :: Lens' CopyObject (Maybe ObjectCannedACL)
coACL = lens _coACL (\ s a -> s{_coACL = a})

-- | Specifies presentational information for the object.
coContentDisposition :: Lens' CopyObject (Maybe Text)
coContentDisposition = lens _coContentDisposition (\ s a -> s{_coContentDisposition = a})

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
coServerSideEncryption :: Lens' CopyObject (Maybe ServerSideEncryption)
coServerSideEncryption = lens _coServerSideEncryption (\ s a -> s{_coServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
coContentType :: Lens' CopyObject (Maybe Text)
coContentType = lens _coContentType (\ s a -> s{_coContentType = a})

-- | Undocumented member.
coBucket :: Lens' CopyObject BucketName
coBucket = lens _coBucket (\ s a -> s{_coBucket = a})

-- | The name of the source bucket and key name of the source object, separated by a slash (/). Must be URL-encoded.
coCopySource :: Lens' CopyObject Text
coCopySource = lens _coCopySource (\ s a -> s{_coCopySource = a})

-- | Undocumented member.
coKey :: Lens' CopyObject ObjectKey
coKey = lens _coKey (\ s a -> s{_coKey = a})

instance AWSRequest CopyObject where
        type Rs CopyObject = CopyObjectResponse
        request = put s3
        response
          = receiveXML
              (\ s h x ->
                 CopyObjectResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (h .#? "x-amz-version-id")
                     <*> (h .#? "x-amz-expiration")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (h .#? "x-amz-copy-source-version-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (parseXML x)
                     <*> (pure (fromEnum s)))

instance Hashable CopyObject where

instance NFData CopyObject where

instance ToHeaders CopyObject where
        toHeaders CopyObject'{..}
          = mconcat
              ["x-amz-copy-source-if-modified-since" =#
                 _coCopySourceIfModifiedSince,
               "x-amz-copy-source-if-unmodified-since" =#
                 _coCopySourceIfUnmodifiedSince,
               "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                 =# _coCopySourceSSECustomerKeyMD5,
               "x-amz-tagging-directive" =# _coTaggingDirective,
               "x-amz-metadata-directive" =# _coMetadataDirective,
               "Expires" =# _coExpires,
               "x-amz-grant-read-acp" =# _coGrantReadACP,
               "x-amz-copy-source-if-none-match" =#
                 _coCopySourceIfNoneMatch,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _coSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _coSSECustomerKey,
               "x-amz-request-payer" =# _coRequestPayer,
               "x-amz-grant-write-acp" =# _coGrantWriteACP,
               "x-amz-copy-source-if-match" =# _coCopySourceIfMatch,
               "x-amz-website-redirect-location" =#
                 _coWebsiteRedirectLocation,
               "x-amz-grant-read" =# _coGrantRead,
               "x-amz-storage-class" =# _coStorageClass,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _coSSECustomerKeyMD5,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _coSSEKMSKeyId,
               "x-amz-grant-full-control" =# _coGrantFullControl,
               "Content-Encoding" =# _coContentEncoding,
               "x-amz-tagging" =# _coTagging,
               "x-amz-meta-" =# _coMetadata,
               "Cache-Control" =# _coCacheControl,
               "Content-Language" =# _coContentLanguage,
               "x-amz-copy-source-server-side-encryption-customer-key"
                 =# _coCopySourceSSECustomerKey,
               "x-amz-copy-source-server-side-encryption-customer-algorithm"
                 =# _coCopySourceSSECustomerAlgorithm,
               "x-amz-acl" =# _coACL,
               "Content-Disposition" =# _coContentDisposition,
               "x-amz-server-side-encryption" =#
                 _coServerSideEncryption,
               "Content-Type" =# _coContentType,
               "x-amz-copy-source" =# _coCopySource]

instance ToPath CopyObject where
        toPath CopyObject'{..}
          = mconcat ["/", toBS _coBucket, "/", toBS _coKey]

instance ToQuery CopyObject where
        toQuery = const mempty

-- | /See:/ 'copyObjectResponse' smart constructor.
data CopyObjectResponse = CopyObjectResponse'
  { _corsRequestCharged       :: !(Maybe RequestCharged)
  , _corsVersionId            :: !(Maybe ObjectVersionId)
  , _corsExpiration           :: !(Maybe Text)
  , _corsSSECustomerAlgorithm :: !(Maybe Text)
  , _corsCopySourceVersionId  :: !(Maybe Text)
  , _corsSSECustomerKeyMD5    :: !(Maybe Text)
  , _corsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
  , _corsServerSideEncryption :: !(Maybe ServerSideEncryption)
  , _corsCopyObjectResult     :: !(Maybe CopyObjectResult)
  , _corsResponseStatus       :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corsRequestCharged' - Undocumented member.
--
-- * 'corsVersionId' - Version ID of the newly created copy.
--
-- * 'corsExpiration' - If the object expiration is configured, the response includes this header.
--
-- * 'corsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'corsCopySourceVersionId' - Undocumented member.
--
-- * 'corsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
--
-- * 'corsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
--
-- * 'corsServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'corsCopyObjectResult' - Undocumented member.
--
-- * 'corsResponseStatus' - -- | The response status code.
copyObjectResponse
    :: Int -- ^ 'corsResponseStatus'
    -> CopyObjectResponse
copyObjectResponse pResponseStatus_ =
  CopyObjectResponse'
    { _corsRequestCharged = Nothing
    , _corsVersionId = Nothing
    , _corsExpiration = Nothing
    , _corsSSECustomerAlgorithm = Nothing
    , _corsCopySourceVersionId = Nothing
    , _corsSSECustomerKeyMD5 = Nothing
    , _corsSSEKMSKeyId = Nothing
    , _corsServerSideEncryption = Nothing
    , _corsCopyObjectResult = Nothing
    , _corsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
corsRequestCharged :: Lens' CopyObjectResponse (Maybe RequestCharged)
corsRequestCharged = lens _corsRequestCharged (\ s a -> s{_corsRequestCharged = a})

-- | Version ID of the newly created copy.
corsVersionId :: Lens' CopyObjectResponse (Maybe ObjectVersionId)
corsVersionId = lens _corsVersionId (\ s a -> s{_corsVersionId = a})

-- | If the object expiration is configured, the response includes this header.
corsExpiration :: Lens' CopyObjectResponse (Maybe Text)
corsExpiration = lens _corsExpiration (\ s a -> s{_corsExpiration = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
corsSSECustomerAlgorithm :: Lens' CopyObjectResponse (Maybe Text)
corsSSECustomerAlgorithm = lens _corsSSECustomerAlgorithm (\ s a -> s{_corsSSECustomerAlgorithm = a})

-- | Undocumented member.
corsCopySourceVersionId :: Lens' CopyObjectResponse (Maybe Text)
corsCopySourceVersionId = lens _corsCopySourceVersionId (\ s a -> s{_corsCopySourceVersionId = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
corsSSECustomerKeyMD5 :: Lens' CopyObjectResponse (Maybe Text)
corsSSECustomerKeyMD5 = lens _corsSSECustomerKeyMD5 (\ s a -> s{_corsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
corsSSEKMSKeyId :: Lens' CopyObjectResponse (Maybe Text)
corsSSEKMSKeyId = lens _corsSSEKMSKeyId (\ s a -> s{_corsSSEKMSKeyId = a}) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
corsServerSideEncryption :: Lens' CopyObjectResponse (Maybe ServerSideEncryption)
corsServerSideEncryption = lens _corsServerSideEncryption (\ s a -> s{_corsServerSideEncryption = a})

-- | Undocumented member.
corsCopyObjectResult :: Lens' CopyObjectResponse (Maybe CopyObjectResult)
corsCopyObjectResult = lens _corsCopyObjectResult (\ s a -> s{_corsCopyObjectResult = a})

-- | -- | The response status code.
corsResponseStatus :: Lens' CopyObjectResponse Int
corsResponseStatus = lens _corsResponseStatus (\ s a -> s{_corsResponseStatus = a})

instance NFData CopyObjectResponse where

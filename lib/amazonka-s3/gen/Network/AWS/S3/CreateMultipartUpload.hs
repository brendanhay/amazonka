{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a multipart upload and returns an upload ID. This upload ID is used to associate all of the parts in the specific multipart upload. You specify this upload ID in each of your subsequent upload part requests (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> ). You also include this upload ID in the final request to either complete or abort the multipart upload request.
--
--
-- For more information about multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart Upload Overview> .
--
-- If you have configured a lifecycle rule to abort incomplete multipart uploads, the upload must complete within the number of days specified in the bucket lifecycle configuration. Otherwise, the incomplete multipart upload becomes eligible for an abort operation and Amazon S3 aborts the multipart upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
--
-- For information about the permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
--
-- For request signing, multipart upload is just a series of regular requests. You initiate a multipart upload, send one or more requests to upload parts, and then complete the multipart upload process. You sign each request individually. There is nothing special about signing multipart upload requests. For more information about signing, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> .
--
-- You can optionally request server-side encryption. For server-side encryption, Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts it when you access it. You can provide your own encryption key, or use AWS Key Management Service (AWS KMS) customer master keys (CMKs) or Amazon S3-managed encryption keys. If you choose to provide your own encryption key, the request headers you provide in <AmazonS3/latest/API/API_UploadPart.html UploadPart> and <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPartCopy.html UploadPartCopy> requests must match the headers you used in the request to initiate the upload by using @CreateMultipartUpload@ .
--
-- To perform a multipart upload with encryption using an AWS KMS CMK, the requester must have permission to the @kms:Encrypt@ , @kms:Decrypt@ , @kms:ReEncrypt*@ , @kms:GenerateDataKey*@ , and @kms:DescribeKey@ actions on the key. These permissions are required because Amazon S3 must decrypt and read data from the encrypted file parts before it completes the multipart upload.
--
-- If your AWS Identity and Access Management (IAM) user or role is in the same AWS account as the AWS KMS CMK, then you must have these permissions on the key policy. If your IAM user or role belongs to a different account than the key, then you must have the permissions on both the key policy and your IAM user or role.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption> .
--
--     * Access Permissions    * When copying an object, you can optionally specify the accounts or groups that should be granted specific permissions on the new object. There are two ways to grant the permissions using the request headers:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
--
--
--
-- You can use either a canned ACL or specify access permissions explicitly. You cannot do both.
--
--     * Server-Side- Encryption-Specific Request Headers    * You can optionally tell Amazon S3 to encrypt data at rest using server-side encryption. Server-side encryption is for data encryption at rest. Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts it when you access it. The option you use depends on whether you want to use AWS managed encryption keys or provide your own encryption key.
--
--     * Use encryption keys managed by Amazon S3 or customer master keys (CMKs) stored in AWS Key Management Service (AWS KMS) – If you want AWS to manage the keys used to encrypt data, specify the following headers in the request.
--
--     * x-amz-server-side-encryption
--
--     * x-amz-server-side-encryption-aws-kms-key-id
--
--     * x-amz-server-side-encryption-context
--
--
--
-- /Important:/ All GET and PUT requests for an object protected by AWS KMS fail if you don't make them with SSL or by using SigV4.
--
-- For more information about server-side encryption with CMKs stored in AWS KMS (SSE-KMS), see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs stored in AWS KMS> .
--
--     * Use customer-provided encryption keys – If you want to manage your own encryption keys, provide all the following headers in the request.
--
--     * x-amz-server-side-encryption-customer-algorithm
--
--     * x-amz-server-side-encryption-customer-key
--
--     * x-amz-server-side-encryption-customer-key-MD5
--
--
--
-- For more information about server-side encryption with CMKs stored in AWS KMS (SSE-KMS), see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs stored in AWS KMS> .
--
--
--
--     * Access-Control-List (ACL)-Specific Request Headers    * You also can use the following access control–related headers with this operation. By default, all objects are private. Only the owner has full access control. When adding a new object, you can grant permissions to individual AWS accounts or to predefined groups defined by Amazon S3. These permissions are then added to the access control list (ACL) on the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs> . With this operation, you can grant access permissions using one of the following two methods:
--
--     * Specify a canned ACL (@x-amz-acl@ ) — Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--     * Specify access permissions explicitly — To explicitly grant access permissions to specific AWS accounts or groups, use the following headers. Each header maps to specific permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> . In the header, you specify a list of grantees who get the specific permission. To grant permissions explicitly, use:
--
--     * x-amz-grant-read
--
--     * x-amz-grant-write
--
--     * x-amz-grant-read-acp
--
--     * x-amz-grant-write-acp
--
--     * x-amz-grant-full-control
--
--
--
-- You specify each grantee as a type=value pair, where the type is one of the following:
--
--     * @id@ – if the value specified is the canonical user ID of an AWS account
--
--     * @uri@ – if you are granting permissions to a predefined group
--
--     * @emailAddress@ – if the value specified is the email address of an AWS account
--
--
--
-- For example, the following @x-amz-grant-read@ header grants the AWS accounts identified by account IDs permissions to read object data and its metadata:
--
-- @x-amz-grant-read: id="11112222333", id="444455556666" @
--
--
--
--
--
-- The following operations are related to @CreateMultipartUpload@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.CreateMultipartUpload
  ( -- * Creating a Request
    createMultipartUpload,
    CreateMultipartUpload,

    -- * Request Lenses
    cmuObjectLockMode,
    cmuExpires,
    cmuGrantReadACP,
    cmuSSECustomerAlgorithm,
    cmuSSECustomerKey,
    cmuRequestPayer,
    cmuGrantWriteACP,
    cmuWebsiteRedirectLocation,
    cmuGrantRead,
    cmuStorageClass,
    cmuSSECustomerKeyMD5,
    cmuSSEKMSKeyId,
    cmuGrantFullControl,
    cmuContentEncoding,
    cmuTagging,
    cmuObjectLockRetainUntilDate,
    cmuMetadata,
    cmuSSEKMSEncryptionContext,
    cmuCacheControl,
    cmuContentLanguage,
    cmuObjectLockLegalHoldStatus,
    cmuACL,
    cmuContentDisposition,
    cmuExpectedBucketOwner,
    cmuServerSideEncryption,
    cmuContentType,
    cmuBucket,
    cmuKey,

    -- * Destructuring the Response
    createMultipartUploadResponse,
    CreateMultipartUploadResponse,

    -- * Response Lenses
    cmursRequestCharged,
    cmursBucket,
    cmursSSECustomerAlgorithm,
    cmursAbortDate,
    cmursAbortRuleId,
    cmursKey,
    cmursSSECustomerKeyMD5,
    cmursSSEKMSKeyId,
    cmursSSEKMSEncryptionContext,
    cmursUploadId,
    cmursServerSideEncryption,
    cmursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'createMultipartUpload' smart constructor.
data CreateMultipartUpload = CreateMultipartUpload'
  { _cmuObjectLockMode ::
      !(Maybe ObjectLockMode),
    _cmuExpires :: !(Maybe ISO8601),
    _cmuGrantReadACP :: !(Maybe Text),
    _cmuSSECustomerAlgorithm :: !(Maybe Text),
    _cmuSSECustomerKey :: !(Maybe (Sensitive Text)),
    _cmuRequestPayer :: !(Maybe RequestPayer),
    _cmuGrantWriteACP :: !(Maybe Text),
    _cmuWebsiteRedirectLocation :: !(Maybe Text),
    _cmuGrantRead :: !(Maybe Text),
    _cmuStorageClass :: !(Maybe StorageClass),
    _cmuSSECustomerKeyMD5 :: !(Maybe Text),
    _cmuSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _cmuGrantFullControl :: !(Maybe Text),
    _cmuContentEncoding :: !(Maybe Text),
    _cmuTagging :: !(Maybe Text),
    _cmuObjectLockRetainUntilDate ::
      !(Maybe ISO8601),
    _cmuMetadata :: !(Map Text (Text)),
    _cmuSSEKMSEncryptionContext ::
      !(Maybe (Sensitive Text)),
    _cmuCacheControl :: !(Maybe Text),
    _cmuContentLanguage :: !(Maybe Text),
    _cmuObjectLockLegalHoldStatus ::
      !(Maybe ObjectLockLegalHoldStatus),
    _cmuACL :: !(Maybe ObjectCannedACL),
    _cmuContentDisposition :: !(Maybe Text),
    _cmuExpectedBucketOwner :: !(Maybe Text),
    _cmuServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _cmuContentType :: !(Maybe Text),
    _cmuBucket :: !BucketName,
    _cmuKey :: !ObjectKey
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmuObjectLockMode' - Specifies the Object Lock mode that you want to apply to the uploaded object.
--
-- * 'cmuExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'cmuGrantReadACP' - Allows grantee to read the object ACL. This action is not supported by Amazon S3 on Outposts.
--
-- * 'cmuSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- * 'cmuSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- * 'cmuRequestPayer' - Undocumented member.
--
-- * 'cmuGrantWriteACP' - Allows grantee to write the ACL for the applicable object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'cmuWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'cmuGrantRead' - Allows grantee to read the object data and its metadata. This action is not supported by Amazon S3 on Outposts.
--
-- * 'cmuStorageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- * 'cmuSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'cmuSSEKMSKeyId' - Specifies the ID of the symmetric customer managed AWS KMS CMK to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
--
-- * 'cmuGrantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'cmuContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'cmuTagging' - The tag-set for the object. The tag-set must be encoded as URL Query parameters.
--
-- * 'cmuObjectLockRetainUntilDate' - Specifies the date and time when you want the Object Lock to expire.
--
-- * 'cmuMetadata' - A map of metadata to store with the object in S3.
--
-- * 'cmuSSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- * 'cmuCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'cmuContentLanguage' - The language the content is in.
--
-- * 'cmuObjectLockLegalHoldStatus' - Specifies whether you want to apply a Legal Hold to the uploaded object.
--
-- * 'cmuACL' - The canned ACL to apply to the object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'cmuContentDisposition' - Specifies presentational information for the object.
--
-- * 'cmuExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'cmuServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'cmuContentType' - A standard MIME type describing the format of the object data.
--
-- * 'cmuBucket' - The name of the bucket to which to initiate the upload When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'cmuKey' - Object key for which the multipart upload is to be initiated.
createMultipartUpload ::
  -- | 'cmuBucket'
  BucketName ->
  -- | 'cmuKey'
  ObjectKey ->
  CreateMultipartUpload
createMultipartUpload pBucket_ pKey_ =
  CreateMultipartUpload'
    { _cmuObjectLockMode = Nothing,
      _cmuExpires = Nothing,
      _cmuGrantReadACP = Nothing,
      _cmuSSECustomerAlgorithm = Nothing,
      _cmuSSECustomerKey = Nothing,
      _cmuRequestPayer = Nothing,
      _cmuGrantWriteACP = Nothing,
      _cmuWebsiteRedirectLocation = Nothing,
      _cmuGrantRead = Nothing,
      _cmuStorageClass = Nothing,
      _cmuSSECustomerKeyMD5 = Nothing,
      _cmuSSEKMSKeyId = Nothing,
      _cmuGrantFullControl = Nothing,
      _cmuContentEncoding = Nothing,
      _cmuTagging = Nothing,
      _cmuObjectLockRetainUntilDate = Nothing,
      _cmuMetadata = mempty,
      _cmuSSEKMSEncryptionContext = Nothing,
      _cmuCacheControl = Nothing,
      _cmuContentLanguage = Nothing,
      _cmuObjectLockLegalHoldStatus = Nothing,
      _cmuACL = Nothing,
      _cmuContentDisposition = Nothing,
      _cmuExpectedBucketOwner = Nothing,
      _cmuServerSideEncryption = Nothing,
      _cmuContentType = Nothing,
      _cmuBucket = pBucket_,
      _cmuKey = pKey_
    }

-- | Specifies the Object Lock mode that you want to apply to the uploaded object.
cmuObjectLockMode :: Lens' CreateMultipartUpload (Maybe ObjectLockMode)
cmuObjectLockMode = lens _cmuObjectLockMode (\s a -> s {_cmuObjectLockMode = a})

-- | The date and time at which the object is no longer cacheable.
cmuExpires :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmuExpires = lens _cmuExpires (\s a -> s {_cmuExpires = a}) . mapping _Time

-- | Allows grantee to read the object ACL. This action is not supported by Amazon S3 on Outposts.
cmuGrantReadACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantReadACP = lens _cmuGrantReadACP (\s a -> s {_cmuGrantReadACP = a})

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
cmuSSECustomerAlgorithm :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerAlgorithm = lens _cmuSSECustomerAlgorithm (\s a -> s {_cmuSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
cmuSSECustomerKey :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKey = lens _cmuSSECustomerKey (\s a -> s {_cmuSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
cmuRequestPayer :: Lens' CreateMultipartUpload (Maybe RequestPayer)
cmuRequestPayer = lens _cmuRequestPayer (\s a -> s {_cmuRequestPayer = a})

-- | Allows grantee to write the ACL for the applicable object. This action is not supported by Amazon S3 on Outposts.
cmuGrantWriteACP :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantWriteACP = lens _cmuGrantWriteACP (\s a -> s {_cmuGrantWriteACP = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
cmuWebsiteRedirectLocation :: Lens' CreateMultipartUpload (Maybe Text)
cmuWebsiteRedirectLocation = lens _cmuWebsiteRedirectLocation (\s a -> s {_cmuWebsiteRedirectLocation = a})

-- | Allows grantee to read the object data and its metadata. This action is not supported by Amazon S3 on Outposts.
cmuGrantRead :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantRead = lens _cmuGrantRead (\s a -> s {_cmuGrantRead = a})

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
cmuStorageClass :: Lens' CreateMultipartUpload (Maybe StorageClass)
cmuStorageClass = lens _cmuStorageClass (\s a -> s {_cmuStorageClass = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
cmuSSECustomerKeyMD5 :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSECustomerKeyMD5 = lens _cmuSSECustomerKeyMD5 (\s a -> s {_cmuSSECustomerKeyMD5 = a})

-- | Specifies the ID of the symmetric customer managed AWS KMS CMK to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
cmuSSEKMSKeyId :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSEKMSKeyId = lens _cmuSSEKMSKeyId (\s a -> s {_cmuSSEKMSKeyId = a}) . mapping _Sensitive

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object. This action is not supported by Amazon S3 on Outposts.
cmuGrantFullControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuGrantFullControl = lens _cmuGrantFullControl (\s a -> s {_cmuGrantFullControl = a})

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
cmuContentEncoding :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentEncoding = lens _cmuContentEncoding (\s a -> s {_cmuContentEncoding = a})

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters.
cmuTagging :: Lens' CreateMultipartUpload (Maybe Text)
cmuTagging = lens _cmuTagging (\s a -> s {_cmuTagging = a})

-- | Specifies the date and time when you want the Object Lock to expire.
cmuObjectLockRetainUntilDate :: Lens' CreateMultipartUpload (Maybe UTCTime)
cmuObjectLockRetainUntilDate = lens _cmuObjectLockRetainUntilDate (\s a -> s {_cmuObjectLockRetainUntilDate = a}) . mapping _Time

-- | A map of metadata to store with the object in S3.
cmuMetadata :: Lens' CreateMultipartUpload (HashMap Text (Text))
cmuMetadata = lens _cmuMetadata (\s a -> s {_cmuMetadata = a}) . _Map

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
cmuSSEKMSEncryptionContext :: Lens' CreateMultipartUpload (Maybe Text)
cmuSSEKMSEncryptionContext = lens _cmuSSEKMSEncryptionContext (\s a -> s {_cmuSSEKMSEncryptionContext = a}) . mapping _Sensitive

-- | Specifies caching behavior along the request/reply chain.
cmuCacheControl :: Lens' CreateMultipartUpload (Maybe Text)
cmuCacheControl = lens _cmuCacheControl (\s a -> s {_cmuCacheControl = a})

-- | The language the content is in.
cmuContentLanguage :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentLanguage = lens _cmuContentLanguage (\s a -> s {_cmuContentLanguage = a})

-- | Specifies whether you want to apply a Legal Hold to the uploaded object.
cmuObjectLockLegalHoldStatus :: Lens' CreateMultipartUpload (Maybe ObjectLockLegalHoldStatus)
cmuObjectLockLegalHoldStatus = lens _cmuObjectLockLegalHoldStatus (\s a -> s {_cmuObjectLockLegalHoldStatus = a})

-- | The canned ACL to apply to the object. This action is not supported by Amazon S3 on Outposts.
cmuACL :: Lens' CreateMultipartUpload (Maybe ObjectCannedACL)
cmuACL = lens _cmuACL (\s a -> s {_cmuACL = a})

-- | Specifies presentational information for the object.
cmuContentDisposition :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentDisposition = lens _cmuContentDisposition (\s a -> s {_cmuContentDisposition = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
cmuExpectedBucketOwner :: Lens' CreateMultipartUpload (Maybe Text)
cmuExpectedBucketOwner = lens _cmuExpectedBucketOwner (\s a -> s {_cmuExpectedBucketOwner = a})

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
cmuServerSideEncryption :: Lens' CreateMultipartUpload (Maybe ServerSideEncryption)
cmuServerSideEncryption = lens _cmuServerSideEncryption (\s a -> s {_cmuServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
cmuContentType :: Lens' CreateMultipartUpload (Maybe Text)
cmuContentType = lens _cmuContentType (\s a -> s {_cmuContentType = a})

-- | The name of the bucket to which to initiate the upload When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
cmuBucket :: Lens' CreateMultipartUpload BucketName
cmuBucket = lens _cmuBucket (\s a -> s {_cmuBucket = a})

-- | Object key for which the multipart upload is to be initiated.
cmuKey :: Lens' CreateMultipartUpload ObjectKey
cmuKey = lens _cmuKey (\s a -> s {_cmuKey = a})

instance AWSRequest CreateMultipartUpload where
  type Rs CreateMultipartUpload = CreateMultipartUploadResponse
  request = post s3
  response =
    receiveXML
      ( \s h x ->
          CreateMultipartUploadResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (x .@? "Bucket")
            <*> (h .#? "x-amz-server-side-encryption-customer-algorithm")
            <*> (h .#? "x-amz-abort-date")
            <*> (h .#? "x-amz-abort-rule-id")
            <*> (x .@? "Key")
            <*> (h .#? "x-amz-server-side-encryption-customer-key-MD5")
            <*> (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
            <*> (h .#? "x-amz-server-side-encryption-context")
            <*> (x .@? "UploadId")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateMultipartUpload

instance NFData CreateMultipartUpload

instance ToHeaders CreateMultipartUpload where
  toHeaders CreateMultipartUpload' {..} =
    mconcat
      [ "x-amz-object-lock-mode" =# _cmuObjectLockMode,
        "Expires" =# _cmuExpires,
        "x-amz-grant-read-acp" =# _cmuGrantReadACP,
        "x-amz-server-side-encryption-customer-algorithm"
          =# _cmuSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _cmuSSECustomerKey,
        "x-amz-request-payer" =# _cmuRequestPayer,
        "x-amz-grant-write-acp" =# _cmuGrantWriteACP,
        "x-amz-website-redirect-location" =# _cmuWebsiteRedirectLocation,
        "x-amz-grant-read" =# _cmuGrantRead,
        "x-amz-storage-class" =# _cmuStorageClass,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _cmuSSECustomerKeyMD5,
        "x-amz-server-side-encryption-aws-kms-key-id" =# _cmuSSEKMSKeyId,
        "x-amz-grant-full-control" =# _cmuGrantFullControl,
        "Content-Encoding" =# _cmuContentEncoding,
        "x-amz-tagging" =# _cmuTagging,
        "x-amz-object-lock-retain-until-date"
          =# _cmuObjectLockRetainUntilDate,
        "x-amz-meta-" =# _cmuMetadata,
        "x-amz-server-side-encryption-context"
          =# _cmuSSEKMSEncryptionContext,
        "Cache-Control" =# _cmuCacheControl,
        "Content-Language" =# _cmuContentLanguage,
        "x-amz-object-lock-legal-hold" =# _cmuObjectLockLegalHoldStatus,
        "x-amz-acl" =# _cmuACL,
        "Content-Disposition" =# _cmuContentDisposition,
        "x-amz-expected-bucket-owner" =# _cmuExpectedBucketOwner,
        "x-amz-server-side-encryption" =# _cmuServerSideEncryption,
        "Content-Type" =# _cmuContentType
      ]

instance ToPath CreateMultipartUpload where
  toPath CreateMultipartUpload' {..} =
    mconcat ["/", toBS _cmuBucket, "/", toBS _cmuKey]

instance ToQuery CreateMultipartUpload where
  toQuery = const (mconcat ["uploads"])

-- | /See:/ 'createMultipartUploadResponse' smart constructor.
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
  { _cmursRequestCharged ::
      !(Maybe RequestCharged),
    _cmursBucket ::
      !(Maybe BucketName),
    _cmursSSECustomerAlgorithm ::
      !(Maybe Text),
    _cmursAbortDate ::
      !(Maybe ISO8601),
    _cmursAbortRuleId ::
      !(Maybe Text),
    _cmursKey :: !(Maybe ObjectKey),
    _cmursSSECustomerKeyMD5 ::
      !(Maybe Text),
    _cmursSSEKMSKeyId ::
      !(Maybe (Sensitive Text)),
    _cmursSSEKMSEncryptionContext ::
      !(Maybe (Sensitive Text)),
    _cmursUploadId :: !(Maybe Text),
    _cmursServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _cmursResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMultipartUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmursRequestCharged' - Undocumented member.
--
-- * 'cmursBucket' - The name of the bucket to which the multipart upload was initiated.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'cmursSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'cmursAbortDate' - If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, the response includes this header. The header indicates when the initiated multipart upload becomes eligible for an abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> . The response also includes the @x-amz-abort-rule-id@ header that provides the ID of the lifecycle configuration rule that defines this action.
--
-- * 'cmursAbortRuleId' - This header is returned along with the @x-amz-abort-date@ header. It identifies the applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
--
-- * 'cmursKey' - Object key for which the multipart upload was initiated.
--
-- * 'cmursSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- * 'cmursSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- * 'cmursSSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- * 'cmursUploadId' - ID for the initiated multipart upload.
--
-- * 'cmursServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'cmursResponseStatus' - -- | The response status code.
createMultipartUploadResponse ::
  -- | 'cmursResponseStatus'
  Int ->
  CreateMultipartUploadResponse
createMultipartUploadResponse pResponseStatus_ =
  CreateMultipartUploadResponse'
    { _cmursRequestCharged = Nothing,
      _cmursBucket = Nothing,
      _cmursSSECustomerAlgorithm = Nothing,
      _cmursAbortDate = Nothing,
      _cmursAbortRuleId = Nothing,
      _cmursKey = Nothing,
      _cmursSSECustomerKeyMD5 = Nothing,
      _cmursSSEKMSKeyId = Nothing,
      _cmursSSEKMSEncryptionContext = Nothing,
      _cmursUploadId = Nothing,
      _cmursServerSideEncryption = Nothing,
      _cmursResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cmursRequestCharged :: Lens' CreateMultipartUploadResponse (Maybe RequestCharged)
cmursRequestCharged = lens _cmursRequestCharged (\s a -> s {_cmursRequestCharged = a})

-- | The name of the bucket to which the multipart upload was initiated.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
cmursBucket :: Lens' CreateMultipartUploadResponse (Maybe BucketName)
cmursBucket = lens _cmursBucket (\s a -> s {_cmursBucket = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
cmursSSECustomerAlgorithm :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSECustomerAlgorithm = lens _cmursSSECustomerAlgorithm (\s a -> s {_cmursSSECustomerAlgorithm = a})

-- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, the response includes this header. The header indicates when the initiated multipart upload becomes eligible for an abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> . The response also includes the @x-amz-abort-rule-id@ header that provides the ID of the lifecycle configuration rule that defines this action.
cmursAbortDate :: Lens' CreateMultipartUploadResponse (Maybe UTCTime)
cmursAbortDate = lens _cmursAbortDate (\s a -> s {_cmursAbortDate = a}) . mapping _Time

-- | This header is returned along with the @x-amz-abort-date@ header. It identifies the applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
cmursAbortRuleId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursAbortRuleId = lens _cmursAbortRuleId (\s a -> s {_cmursAbortRuleId = a})

-- | Object key for which the multipart upload was initiated.
cmursKey :: Lens' CreateMultipartUploadResponse (Maybe ObjectKey)
cmursKey = lens _cmursKey (\s a -> s {_cmursKey = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
cmursSSECustomerKeyMD5 :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSECustomerKeyMD5 = lens _cmursSSECustomerKeyMD5 (\s a -> s {_cmursSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
cmursSSEKMSKeyId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSEKMSKeyId = lens _cmursSSEKMSKeyId (\s a -> s {_cmursSSEKMSKeyId = a}) . mapping _Sensitive

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
cmursSSEKMSEncryptionContext :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursSSEKMSEncryptionContext = lens _cmursSSEKMSEncryptionContext (\s a -> s {_cmursSSEKMSEncryptionContext = a}) . mapping _Sensitive

-- | ID for the initiated multipart upload.
cmursUploadId :: Lens' CreateMultipartUploadResponse (Maybe Text)
cmursUploadId = lens _cmursUploadId (\s a -> s {_cmursUploadId = a})

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
cmursServerSideEncryption :: Lens' CreateMultipartUploadResponse (Maybe ServerSideEncryption)
cmursServerSideEncryption = lens _cmursServerSideEncryption (\s a -> s {_cmursServerSideEncryption = a})

-- | -- | The response status code.
cmursResponseStatus :: Lens' CreateMultipartUploadResponse Int
cmursResponseStatus = lens _cmursResponseStatus (\s a -> s {_cmursResponseStatus = a})

instance NFData CreateMultipartUploadResponse

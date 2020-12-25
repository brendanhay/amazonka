{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- For more information about multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart Upload Overview> .
-- If you have configured a lifecycle rule to abort incomplete multipart uploads, the upload must complete within the number of days specified in the bucket lifecycle configuration. Otherwise, the incomplete multipart upload becomes eligible for an abort operation and Amazon S3 aborts the multipart upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
-- For information about the permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
-- For request signing, multipart upload is just a series of regular requests. You initiate a multipart upload, send one or more requests to upload parts, and then complete the multipart upload process. You sign each request individually. There is nothing special about signing multipart upload requests. For more information about signing, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> .
-- You can optionally request server-side encryption. For server-side encryption, Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts it when you access it. You can provide your own encryption key, or use AWS Key Management Service (AWS KMS) customer master keys (CMKs) or Amazon S3-managed encryption keys. If you choose to provide your own encryption key, the request headers you provide in <AmazonS3/latest/API/API_UploadPart.html UploadPart> and <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPartCopy.html UploadPartCopy> requests must match the headers you used in the request to initiate the upload by using @CreateMultipartUpload@ .
-- To perform a multipart upload with encryption using an AWS KMS CMK, the requester must have permission to the @kms:Encrypt@ , @kms:Decrypt@ , @kms:ReEncrypt*@ , @kms:GenerateDataKey*@ , and @kms:DescribeKey@ actions on the key. These permissions are required because Amazon S3 must decrypt and read data from the encrypted file parts before it completes the multipart upload.
-- If your AWS Identity and Access Management (IAM) user or role is in the same AWS account as the AWS KMS CMK, then you must have these permissions on the key policy. If your IAM user or role belongs to a different account than the key, then you must have the permissions on both the key policy and your IAM user or role.
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption> .
--
--     * Access Permissions
--
--     * When copying an object, you can optionally specify the accounts or groups that should be granted specific permissions on the new object. There are two ways to grant the permissions using the request headers:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
--
--
-- You can use either a canned ACL or specify access permissions explicitly. You cannot do both.
--
--
--     * Server-Side- Encryption-Specific Request Headers
--
--     * You can optionally tell Amazon S3 to encrypt data at rest using server-side encryption. Server-side encryption is for data encryption at rest. Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts it when you access it. The option you use depends on whether you want to use AWS managed encryption keys or provide your own encryption key.
--
--     * Use encryption keys managed by Amazon S3 or customer master keys (CMKs) stored in AWS Key Management Service (AWS KMS) – If you want AWS to manage the keys used to encrypt data, specify the following headers in the request.
--
--     * x-amz-server-side-encryption
--
--
--     * x-amz-server-side-encryption-aws-kms-key-id
--
--
--     * x-amz-server-side-encryption-context
--
--
-- /Important:/ All GET and PUT requests for an object protected by AWS KMS fail if you don't make them with SSL or by using SigV4.
-- For more information about server-side encryption with CMKs stored in AWS KMS (SSE-KMS), see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs stored in AWS KMS> .
--
--
--     * Use customer-provided encryption keys – If you want to manage your own encryption keys, provide all the following headers in the request.
--
--     * x-amz-server-side-encryption-customer-algorithm
--
--
--     * x-amz-server-side-encryption-customer-key
--
--
--     * x-amz-server-side-encryption-customer-key-MD5
--
--
-- For more information about server-side encryption with CMKs stored in AWS KMS (SSE-KMS), see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs stored in AWS KMS> .
--
--
--
--
--     * Access-Control-List (ACL)-Specific Request Headers
--
--     * You also can use the following access control–related headers with this operation. By default, all objects are private. Only the owner has full access control. When adding a new object, you can grant permissions to individual AWS accounts or to predefined groups defined by Amazon S3. These permissions are then added to the access control list (ACL) on the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs> . With this operation, you can grant access permissions using one of the following two methods:
--
--     * Specify a canned ACL (@x-amz-acl@ ) — Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly — To explicitly grant access permissions to specific AWS accounts or groups, use the following headers. Each header maps to specific permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> . In the header, you specify a list of grantees who get the specific permission. To grant permissions explicitly, use:
--
--     * x-amz-grant-read
--
--
--     * x-amz-grant-write
--
--
--     * x-amz-grant-read-acp
--
--
--     * x-amz-grant-write-acp
--
--
--     * x-amz-grant-full-control
--
--
-- You specify each grantee as a type=value pair, where the type is one of the following:
--
--     * @id@ – if the value specified is the canonical user ID of an AWS account
--
--
--     * @uri@ – if you are granting permissions to a predefined group
--
--
--     * @emailAddress@ – if the value specified is the email address of an AWS account
--
--
-- For example, the following @x-amz-grant-read@ header grants the AWS accounts identified by account IDs permissions to read object data and its metadata:
-- @x-amz-grant-read: id="11112222333", id="444455556666" @
--
--
--
--
-- The following operations are related to @CreateMultipartUpload@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.CreateMultipartUpload
  ( -- * Creating a request
    CreateMultipartUpload (..),
    mkCreateMultipartUpload,

    -- ** Request lenses
    cmuBucket,
    cmuKey,
    cmuACL,
    cmuCacheControl,
    cmuContentDisposition,
    cmuContentEncoding,
    cmuContentLanguage,
    cmuContentType,
    cmuExpectedBucketOwner,
    cmuExpires,
    cmuGrantFullControl,
    cmuGrantRead,
    cmuGrantReadACP,
    cmuGrantWriteACP,
    cmuMetadata,
    cmuObjectLockLegalHoldStatus,
    cmuObjectLockMode,
    cmuObjectLockRetainUntilDate,
    cmuRequestPayer,
    cmuSSECustomerAlgorithm,
    cmuSSECustomerKey,
    cmuSSECustomerKeyMD5,
    cmuSSEKMSEncryptionContext,
    cmuSSEKMSKeyId,
    cmuServerSideEncryption,
    cmuStorageClass,
    cmuTagging,
    cmuWebsiteRedirectLocation,

    -- * Destructuring the response
    CreateMultipartUploadResponse (..),
    mkCreateMultipartUploadResponse,

    -- ** Response lenses
    cmurrsAbortDate,
    cmurrsAbortRuleId,
    cmurrsBucket,
    cmurrsKey,
    cmurrsRequestCharged,
    cmurrsSSECustomerAlgorithm,
    cmurrsSSECustomerKeyMD5,
    cmurrsSSEKMSEncryptionContext,
    cmurrsSSEKMSKeyId,
    cmurrsServerSideEncryption,
    cmurrsUploadId,
    cmurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkCreateMultipartUpload' smart constructor.
data CreateMultipartUpload = CreateMultipartUpload'
  { -- | The name of the bucket to which to initiate the upload
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | Object key for which the multipart upload is to be initiated.
    key :: Types.Key,
    -- | The canned ACL to apply to the object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Core.Maybe Types.ObjectCannedACL,
    -- | Specifies caching behavior along the request/reply chain.
    cacheControl :: Core.Maybe Types.CacheControl,
    -- | Specifies presentational information for the object.
    contentDisposition :: Core.Maybe Types.ContentDisposition,
    -- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
    contentEncoding :: Core.Maybe Types.ContentEncoding,
    -- | The language the content is in.
    contentLanguage :: Core.Maybe Types.ContentLanguage,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Core.Maybe Types.ContentType,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Core.Maybe Core.UTCTime,
    -- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Core.Maybe Types.GrantFullControl,
    -- | Allows grantee to read the object data and its metadata.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Core.Maybe Types.GrantRead,
    -- | Allows grantee to read the object ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Core.Maybe Types.GrantReadACP,
    -- | Allows grantee to write the ACL for the applicable object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Core.Maybe Types.GrantWriteACP,
    -- | A map of metadata to store with the object in S3.
    metadata :: Core.HashMap Types.MetadataKey Types.MetadataValue,
    -- | Specifies whether you want to apply a Legal Hold to the uploaded object.
    objectLockLegalHoldStatus :: Core.Maybe Types.ObjectLockLegalHoldStatus,
    -- | Specifies the Object Lock mode that you want to apply to the uploaded object.
    objectLockMode :: Core.Maybe Types.ObjectLockMode,
    -- | Specifies the date and time when you want the Object Lock to expire.
    objectLockRetainUntilDate :: Core.Maybe Core.UTCTime,
    requestPayer :: Core.Maybe Types.RequestPayer,
    -- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Core.Maybe Types.SSECustomerKey,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Core.Maybe Types.SSEKMSEncryptionContext,
    -- | Specifies the ID of the symmetric customer managed AWS KMS CMK to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
    sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId,
    -- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe Types.ServerSideEncryption,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
    storageClass :: Core.Maybe Types.StorageClass,
    -- | The tag-set for the object. The tag-set must be encoded as URL Query parameters.
    tagging :: Core.Maybe Types.TaggingHeader,
    -- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Core.Maybe Types.WebsiteRedirectLocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateMultipartUpload' value with any optional fields omitted.
mkCreateMultipartUpload ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  CreateMultipartUpload
mkCreateMultipartUpload bucket key =
  CreateMultipartUpload'
    { bucket,
      key,
      acl = Core.Nothing,
      cacheControl = Core.Nothing,
      contentDisposition = Core.Nothing,
      contentEncoding = Core.Nothing,
      contentLanguage = Core.Nothing,
      contentType = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      expires = Core.Nothing,
      grantFullControl = Core.Nothing,
      grantRead = Core.Nothing,
      grantReadACP = Core.Nothing,
      grantWriteACP = Core.Nothing,
      metadata = Core.mempty,
      objectLockLegalHoldStatus = Core.Nothing,
      objectLockMode = Core.Nothing,
      objectLockRetainUntilDate = Core.Nothing,
      requestPayer = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      sSECustomerKey = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      sSEKMSEncryptionContext = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      storageClass = Core.Nothing,
      tagging = Core.Nothing,
      websiteRedirectLocation = Core.Nothing
    }

-- | The name of the bucket to which to initiate the upload
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuBucket :: Lens.Lens' CreateMultipartUpload Types.BucketName
cmuBucket = Lens.field @"bucket"
{-# DEPRECATED cmuBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Object key for which the multipart upload is to be initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuKey :: Lens.Lens' CreateMultipartUpload Types.Key
cmuKey = Lens.field @"key"
{-# DEPRECATED cmuKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuACL :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ObjectCannedACL)
cmuACL = Lens.field @"acl"
{-# DEPRECATED cmuACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuCacheControl :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.CacheControl)
cmuCacheControl = Lens.field @"cacheControl"
{-# DEPRECATED cmuCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuContentDisposition :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ContentDisposition)
cmuContentDisposition = Lens.field @"contentDisposition"
{-# DEPRECATED cmuContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuContentEncoding :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ContentEncoding)
cmuContentEncoding = Lens.field @"contentEncoding"
{-# DEPRECATED cmuContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuContentLanguage :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ContentLanguage)
cmuContentLanguage = Lens.field @"contentLanguage"
{-# DEPRECATED cmuContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuContentType :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ContentType)
cmuContentType = Lens.field @"contentType"
{-# DEPRECATED cmuContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuExpectedBucketOwner :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ExpectedBucketOwner)
cmuExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED cmuExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuExpires :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.UTCTime)
cmuExpires = Lens.field @"expires"
{-# DEPRECATED cmuExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuGrantFullControl :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.GrantFullControl)
cmuGrantFullControl = Lens.field @"grantFullControl"
{-# DEPRECATED cmuGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuGrantRead :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.GrantRead)
cmuGrantRead = Lens.field @"grantRead"
{-# DEPRECATED cmuGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuGrantReadACP :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.GrantReadACP)
cmuGrantReadACP = Lens.field @"grantReadACP"
{-# DEPRECATED cmuGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuGrantWriteACP :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.GrantWriteACP)
cmuGrantWriteACP = Lens.field @"grantWriteACP"
{-# DEPRECATED cmuGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuMetadata :: Lens.Lens' CreateMultipartUpload (Core.HashMap Types.MetadataKey Types.MetadataValue)
cmuMetadata = Lens.field @"metadata"
{-# DEPRECATED cmuMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Specifies whether you want to apply a Legal Hold to the uploaded object.
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuObjectLockLegalHoldStatus :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ObjectLockLegalHoldStatus)
cmuObjectLockLegalHoldStatus = Lens.field @"objectLockLegalHoldStatus"
{-# DEPRECATED cmuObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | Specifies the Object Lock mode that you want to apply to the uploaded object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuObjectLockMode :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ObjectLockMode)
cmuObjectLockMode = Lens.field @"objectLockMode"
{-# DEPRECATED cmuObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | Specifies the date and time when you want the Object Lock to expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuObjectLockRetainUntilDate :: Lens.Lens' CreateMultipartUpload (Core.Maybe Core.UTCTime)
cmuObjectLockRetainUntilDate = Lens.field @"objectLockRetainUntilDate"
{-# DEPRECATED cmuObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuRequestPayer :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.RequestPayer)
cmuRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED cmuRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuSSECustomerAlgorithm :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.SSECustomerAlgorithm)
cmuSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED cmuSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuSSECustomerKey :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.SSECustomerKey)
cmuSSECustomerKey = Lens.field @"sSECustomerKey"
{-# DEPRECATED cmuSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuSSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.SSECustomerKeyMD5)
cmuSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED cmuSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuSSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.SSEKMSEncryptionContext)
cmuSSEKMSEncryptionContext = Lens.field @"sSEKMSEncryptionContext"
{-# DEPRECATED cmuSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | Specifies the ID of the symmetric customer managed AWS KMS CMK to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuSSEKMSKeyId :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.SSEKMSKeyId)
cmuSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED cmuSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuServerSideEncryption :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.ServerSideEncryption)
cmuServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED cmuServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuStorageClass :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.StorageClass)
cmuStorageClass = Lens.field @"storageClass"
{-# DEPRECATED cmuStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuTagging :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.TaggingHeader)
cmuTagging = Lens.field @"tagging"
{-# DEPRECATED cmuTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuWebsiteRedirectLocation :: Lens.Lens' CreateMultipartUpload (Core.Maybe Types.WebsiteRedirectLocation)
cmuWebsiteRedirectLocation = Lens.field @"websiteRedirectLocation"
{-# DEPRECATED cmuWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

instance Core.AWSRequest CreateMultipartUpload where
  type Rs CreateMultipartUpload = CreateMultipartUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery = Core.pure ("uploads", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-acl" acl
            Core.<> (Core.toHeaders "Cache-Control" cacheControl)
            Core.<> (Core.toHeaders "Content-Disposition" contentDisposition)
            Core.<> (Core.toHeaders "Content-Encoding" contentEncoding)
            Core.<> (Core.toHeaders "Content-Language" contentLanguage)
            Core.<> (Core.toHeaders "Content-Type" contentType)
            Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner)
            Core.<> (Core.toHeaders "Expires" expires)
            Core.<> (Core.toHeaders "x-amz-grant-full-control" grantFullControl)
            Core.<> (Core.toHeaders "x-amz-grant-read" grantRead)
            Core.<> (Core.toHeaders "x-amz-grant-read-acp" grantReadACP)
            Core.<> (Core.toHeaders "x-amz-grant-write-acp" grantWriteACP)
            Core.<> (Core.toHeaders "x-amz-meta-" metadata)
            Core.<> ( Core.toHeaders
                        "x-amz-object-lock-legal-hold"
                        objectLockLegalHoldStatus
                    )
            Core.<> (Core.toHeaders "x-amz-object-lock-mode" objectLockMode)
            Core.<> ( Core.toHeaders
                        "x-amz-object-lock-retain-until-date"
                        objectLockRetainUntilDate
                    )
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer)
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-customer-algorithm"
                        sSECustomerAlgorithm
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-customer-key"
                        sSECustomerKey
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-customer-key-MD5"
                        sSECustomerKeyMD5
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-context"
                        sSEKMSEncryptionContext
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption-aws-kms-key-id"
                        sSEKMSKeyId
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-server-side-encryption"
                        serverSideEncryption
                    )
            Core.<> (Core.toHeaders "x-amz-storage-class" storageClass)
            Core.<> (Core.toHeaders "x-amz-tagging" tagging)
            Core.<> ( Core.toHeaders
                        "x-amz-website-redirect-location"
                        websiteRedirectLocation
                    ),
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateMultipartUploadResponse'
            Core.<$> (Core.parseHeaderMaybe "x-amz-abort-date" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-abort-rule-id" h)
            Core.<*> (x Core..@? "Bucket")
            Core.<*> (x Core..@? "Key")
            Core.<*> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> ( Core.parseHeaderMaybe
                         "x-amz-server-side-encryption-customer-algorithm"
                         h
                     )
            Core.<*> ( Core.parseHeaderMaybe
                         "x-amz-server-side-encryption-customer-key-MD5"
                         h
                     )
            Core.<*> (Core.parseHeaderMaybe "x-amz-server-side-encryption-context" h)
            Core.<*> ( Core.parseHeaderMaybe
                         "x-amz-server-side-encryption-aws-kms-key-id"
                         h
                     )
            Core.<*> (Core.parseHeaderMaybe "x-amz-server-side-encryption" h)
            Core.<*> (x Core..@? "UploadId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateMultipartUploadResponse' smart constructor.
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
  { -- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, the response includes this header. The header indicates when the initiated multipart upload becomes eligible for an abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
    --
    -- The response also includes the @x-amz-abort-rule-id@ header that provides the ID of the lifecycle configuration rule that defines this action.
    abortDate :: Core.Maybe Core.UTCTime,
    -- | This header is returned along with the @x-amz-abort-date@ header. It identifies the applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
    abortRuleId :: Core.Maybe Types.AbortRuleId,
    -- | The name of the bucket to which the multipart upload was initiated.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Core.Maybe Types.BucketName,
    -- | Object key for which the multipart upload was initiated.
    key :: Core.Maybe Types.Key,
    requestCharged :: Core.Maybe Types.RequestCharged,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Core.Maybe Types.SSEKMSEncryptionContext,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
    sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId,
    -- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe Types.ServerSideEncryption,
    -- | ID for the initiated multipart upload.
    uploadId :: Core.Maybe Types.MultipartUploadId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateMultipartUploadResponse' value with any optional fields omitted.
mkCreateMultipartUploadResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateMultipartUploadResponse
mkCreateMultipartUploadResponse responseStatus =
  CreateMultipartUploadResponse'
    { abortDate = Core.Nothing,
      abortRuleId = Core.Nothing,
      bucket = Core.Nothing,
      key = Core.Nothing,
      requestCharged = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      sSEKMSEncryptionContext = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      uploadId = Core.Nothing,
      responseStatus
    }

-- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, the response includes this header. The header indicates when the initiated multipart upload becomes eligible for an abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
--
-- The response also includes the @x-amz-abort-rule-id@ header that provides the ID of the lifecycle configuration rule that defines this action.
--
-- /Note:/ Consider using 'abortDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsAbortDate :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Core.UTCTime)
cmurrsAbortDate = Lens.field @"abortDate"
{-# DEPRECATED cmurrsAbortDate "Use generic-lens or generic-optics with 'abortDate' instead." #-}

-- | This header is returned along with the @x-amz-abort-date@ header. It identifies the applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
--
-- /Note:/ Consider using 'abortRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsAbortRuleId :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.AbortRuleId)
cmurrsAbortRuleId = Lens.field @"abortRuleId"
{-# DEPRECATED cmurrsAbortRuleId "Use generic-lens or generic-optics with 'abortRuleId' instead." #-}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsBucket :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.BucketName)
cmurrsBucket = Lens.field @"bucket"
{-# DEPRECATED cmurrsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsKey :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.Key)
cmurrsKey = Lens.field @"key"
{-# DEPRECATED cmurrsKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsRequestCharged :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.RequestCharged)
cmurrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED cmurrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsSSECustomerAlgorithm :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.SSECustomerAlgorithm)
cmurrsSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED cmurrsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsSSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.SSECustomerKeyMD5)
cmurrsSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED cmurrsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsSSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.SSEKMSEncryptionContext)
cmurrsSSEKMSEncryptionContext = Lens.field @"sSEKMSEncryptionContext"
{-# DEPRECATED cmurrsSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsSSEKMSKeyId :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.SSEKMSKeyId)
cmurrsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED cmurrsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsServerSideEncryption :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.ServerSideEncryption)
cmurrsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED cmurrsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | ID for the initiated multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsUploadId :: Lens.Lens' CreateMultipartUploadResponse (Core.Maybe Types.MultipartUploadId)
cmurrsUploadId = Lens.field @"uploadId"
{-# DEPRECATED cmurrsUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmurrsResponseStatus :: Lens.Lens' CreateMultipartUploadResponse Core.Int
cmurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

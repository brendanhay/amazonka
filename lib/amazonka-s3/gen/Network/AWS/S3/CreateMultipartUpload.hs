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
    cObjectLockMode,
    cExpires,
    cGrantReadACP,
    cBucket,
    cSSECustomerAlgorithm,
    cSSECustomerKey,
    cRequestPayer,
    cGrantWriteACP,
    cWebsiteRedirectLocation,
    cGrantRead,
    cKey,
    cStorageClass,
    cSSECustomerKeyMD5,
    cSSEKMSKeyId,
    cGrantFullControl,
    cContentEncoding,
    cTagging,
    cObjectLockRetainUntilDate,
    cMetadata,
    cSSEKMSEncryptionContext,
    cCacheControl,
    cContentLanguage,
    cObjectLockLegalHoldStatus,
    cACL,
    cContentDisposition,
    cExpectedBucketOwner,
    cServerSideEncryption,
    cContentType,

    -- * Destructuring the response
    CreateMultipartUploadResponse (..),
    mkCreateMultipartUploadResponse,

    -- ** Response lenses
    crsRequestCharged,
    crsBucket,
    crsSSECustomerAlgorithm,
    crsAbortDate,
    crsAbortRuleId,
    crsKey,
    crsSSECustomerKeyMD5,
    crsSSEKMSKeyId,
    crsSSEKMSEncryptionContext,
    crsUploadId,
    crsServerSideEncryption,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkCreateMultipartUpload' smart constructor.
data CreateMultipartUpload = CreateMultipartUpload'
  { -- | Specifies the Object Lock mode that you want to apply to the uploaded object.
    objectLockMode :: Lude.Maybe ObjectLockMode,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Lude.Maybe Lude.DateTime,
    -- | Allows grantee to read the object ACL.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantReadACP :: Lude.Maybe Lude.Text,
    -- | The name of the bucket to which to initiate the upload
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    -- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestPayer :: Lude.Maybe RequestPayer,
    -- | Allows grantee to write the ACL for the applicable object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantWriteACP :: Lude.Maybe Lude.Text,
    -- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Lude.Maybe Lude.Text,
    -- | Allows grantee to read the object data and its metadata.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantRead :: Lude.Maybe Lude.Text,
    -- | Object key for which the multipart upload is to be initiated.
    key :: ObjectKey,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
    storageClass :: Lude.Maybe StorageClass,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    -- | Specifies the ID of the symmetric customer managed AWS KMS CMK to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    grantFullControl :: Lude.Maybe Lude.Text,
    -- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
    contentEncoding :: Lude.Maybe Lude.Text,
    -- | The tag-set for the object. The tag-set must be encoded as URL Query parameters.
    tagging :: Lude.Maybe Lude.Text,
    -- | Specifies the date and time when you want the Object Lock to expire.
    objectLockRetainUntilDate :: Lude.Maybe Lude.DateTime,
    -- | A map of metadata to store with the object in S3.
    metadata :: Lude.HashMap Lude.Text (Lude.Text),
    -- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Specifies caching behavior along the request/reply chain.
    cacheControl :: Lude.Maybe Lude.Text,
    -- | The language the content is in.
    contentLanguage :: Lude.Maybe Lude.Text,
    -- | Specifies whether you want to apply a Legal Hold to the uploaded object.
    objectLockLegalHoldStatus :: Lude.Maybe ObjectLockLegalHoldStatus,
    -- | The canned ACL to apply to the object.
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Lude.Maybe ObjectCannedACL,
    -- | Specifies presentational information for the object.
    contentDisposition :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    -- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMultipartUpload' with the minimum fields required to make a request.
--
-- * 'objectLockMode' - Specifies the Object Lock mode that you want to apply to the uploaded object.
-- * 'expires' - The date and time at which the object is no longer cacheable.
-- * 'grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'bucket' - The name of the bucket to which to initiate the upload
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
-- * 'sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
-- * 'requestPayer' -
-- * 'grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
-- * 'grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'key' - Object key for which the multipart upload is to be initiated.
-- * 'storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
-- * 'sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'sSEKMSKeyId' - Specifies the ID of the symmetric customer managed AWS KMS CMK to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
-- * 'grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'contentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
-- * 'tagging' - The tag-set for the object. The tag-set must be encoded as URL Query parameters.
-- * 'objectLockRetainUntilDate' - Specifies the date and time when you want the Object Lock to expire.
-- * 'metadata' - A map of metadata to store with the object in S3.
-- * 'sSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
-- * 'cacheControl' - Specifies caching behavior along the request/reply chain.
-- * 'contentLanguage' - The language the content is in.
-- * 'objectLockLegalHoldStatus' - Specifies whether you want to apply a Legal Hold to the uploaded object.
-- * 'acl' - The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'contentDisposition' - Specifies presentational information for the object.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
-- * 'contentType' - A standard MIME type describing the format of the object data.
mkCreateMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  CreateMultipartUpload
mkCreateMultipartUpload pBucket_ pKey_ =
  CreateMultipartUpload'
    { objectLockMode = Lude.Nothing,
      expires = Lude.Nothing,
      grantReadACP = Lude.Nothing,
      bucket = pBucket_,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKey = Lude.Nothing,
      requestPayer = Lude.Nothing,
      grantWriteACP = Lude.Nothing,
      websiteRedirectLocation = Lude.Nothing,
      grantRead = Lude.Nothing,
      key = pKey_,
      storageClass = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      grantFullControl = Lude.Nothing,
      contentEncoding = Lude.Nothing,
      tagging = Lude.Nothing,
      objectLockRetainUntilDate = Lude.Nothing,
      metadata = Lude.mempty,
      sSEKMSEncryptionContext = Lude.Nothing,
      cacheControl = Lude.Nothing,
      contentLanguage = Lude.Nothing,
      objectLockLegalHoldStatus = Lude.Nothing,
      acl = Lude.Nothing,
      contentDisposition = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      contentType = Lude.Nothing
    }

-- | Specifies the Object Lock mode that you want to apply to the uploaded object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cObjectLockMode :: Lens.Lens' CreateMultipartUpload (Lude.Maybe ObjectLockMode)
cObjectLockMode = Lens.lens (objectLockMode :: CreateMultipartUpload -> Lude.Maybe ObjectLockMode) (\s a -> s {objectLockMode = a} :: CreateMultipartUpload)
{-# DEPRECATED cObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpires :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.DateTime)
cExpires = Lens.lens (expires :: CreateMultipartUpload -> Lude.Maybe Lude.DateTime) (\s a -> s {expires = a} :: CreateMultipartUpload)
{-# DEPRECATED cExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGrantReadACP :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cGrantReadACP = Lens.lens (grantReadACP :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {grantReadACP = a} :: CreateMultipartUpload)
{-# DEPRECATED cGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | The name of the bucket to which to initiate the upload
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBucket :: Lens.Lens' CreateMultipartUpload BucketName
cBucket = Lens.lens (bucket :: CreateMultipartUpload -> BucketName) (\s a -> s {bucket = a} :: CreateMultipartUpload)
{-# DEPRECATED cBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSECustomerAlgorithm :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: CreateMultipartUpload)
{-# DEPRECATED cSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSECustomerKey :: Lens.Lens' CreateMultipartUpload (Lude.Maybe (Lude.Sensitive Lude.Text))
cSSECustomerKey = Lens.lens (sSECustomerKey :: CreateMultipartUpload -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: CreateMultipartUpload)
{-# DEPRECATED cSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRequestPayer :: Lens.Lens' CreateMultipartUpload (Lude.Maybe RequestPayer)
cRequestPayer = Lens.lens (requestPayer :: CreateMultipartUpload -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: CreateMultipartUpload)
{-# DEPRECATED cRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGrantWriteACP :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cGrantWriteACP = Lens.lens (grantWriteACP :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {grantWriteACP = a} :: CreateMultipartUpload)
{-# DEPRECATED cGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWebsiteRedirectLocation :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cWebsiteRedirectLocation = Lens.lens (websiteRedirectLocation :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {websiteRedirectLocation = a} :: CreateMultipartUpload)
{-# DEPRECATED cWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGrantRead :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cGrantRead = Lens.lens (grantRead :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {grantRead = a} :: CreateMultipartUpload)
{-# DEPRECATED cGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | Object key for which the multipart upload is to be initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKey :: Lens.Lens' CreateMultipartUpload ObjectKey
cKey = Lens.lens (key :: CreateMultipartUpload -> ObjectKey) (\s a -> s {key = a} :: CreateMultipartUpload)
{-# DEPRECATED cKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStorageClass :: Lens.Lens' CreateMultipartUpload (Lude.Maybe StorageClass)
cStorageClass = Lens.lens (storageClass :: CreateMultipartUpload -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: CreateMultipartUpload)
{-# DEPRECATED cStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: CreateMultipartUpload)
{-# DEPRECATED cSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Specifies the ID of the symmetric customer managed AWS KMS CMK to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/http:/docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSEKMSKeyId :: Lens.Lens' CreateMultipartUpload (Lude.Maybe (Lude.Sensitive Lude.Text))
cSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: CreateMultipartUpload -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: CreateMultipartUpload)
{-# DEPRECATED cSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGrantFullControl :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cGrantFullControl = Lens.lens (grantFullControl :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {grantFullControl = a} :: CreateMultipartUpload)
{-# DEPRECATED cGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContentEncoding :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cContentEncoding = Lens.lens (contentEncoding :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {contentEncoding = a} :: CreateMultipartUpload)
{-# DEPRECATED cContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTagging :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cTagging = Lens.lens (tagging :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {tagging = a} :: CreateMultipartUpload)
{-# DEPRECATED cTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | Specifies the date and time when you want the Object Lock to expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cObjectLockRetainUntilDate :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.DateTime)
cObjectLockRetainUntilDate = Lens.lens (objectLockRetainUntilDate :: CreateMultipartUpload -> Lude.Maybe Lude.DateTime) (\s a -> s {objectLockRetainUntilDate = a} :: CreateMultipartUpload)
{-# DEPRECATED cObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMetadata :: Lens.Lens' CreateMultipartUpload (Lude.HashMap Lude.Text (Lude.Text))
cMetadata = Lens.lens (metadata :: CreateMultipartUpload -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {metadata = a} :: CreateMultipartUpload)
{-# DEPRECATED cMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUpload (Lude.Maybe (Lude.Sensitive Lude.Text))
cSSEKMSEncryptionContext = Lens.lens (sSEKMSEncryptionContext :: CreateMultipartUpload -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSEncryptionContext = a} :: CreateMultipartUpload)
{-# DEPRECATED cSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheControl :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cCacheControl = Lens.lens (cacheControl :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: CreateMultipartUpload)
{-# DEPRECATED cCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContentLanguage :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cContentLanguage = Lens.lens (contentLanguage :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {contentLanguage = a} :: CreateMultipartUpload)
{-# DEPRECATED cContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Specifies whether you want to apply a Legal Hold to the uploaded object.
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cObjectLockLegalHoldStatus :: Lens.Lens' CreateMultipartUpload (Lude.Maybe ObjectLockLegalHoldStatus)
cObjectLockLegalHoldStatus = Lens.lens (objectLockLegalHoldStatus :: CreateMultipartUpload -> Lude.Maybe ObjectLockLegalHoldStatus) (\s a -> s {objectLockLegalHoldStatus = a} :: CreateMultipartUpload)
{-# DEPRECATED cObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cACL :: Lens.Lens' CreateMultipartUpload (Lude.Maybe ObjectCannedACL)
cACL = Lens.lens (acl :: CreateMultipartUpload -> Lude.Maybe ObjectCannedACL) (\s a -> s {acl = a} :: CreateMultipartUpload)
{-# DEPRECATED cACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContentDisposition :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cContentDisposition = Lens.lens (contentDisposition :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {contentDisposition = a} :: CreateMultipartUpload)
{-# DEPRECATED cContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpectedBucketOwner :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cExpectedBucketOwner = Lens.lens (expectedBucketOwner :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: CreateMultipartUpload)
{-# DEPRECATED cExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cServerSideEncryption :: Lens.Lens' CreateMultipartUpload (Lude.Maybe ServerSideEncryption)
cServerSideEncryption = Lens.lens (serverSideEncryption :: CreateMultipartUpload -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: CreateMultipartUpload)
{-# DEPRECATED cServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContentType :: Lens.Lens' CreateMultipartUpload (Lude.Maybe Lude.Text)
cContentType = Lens.lens (contentType :: CreateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: CreateMultipartUpload)
{-# DEPRECATED cContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.AWSRequest CreateMultipartUpload where
  type Rs CreateMultipartUpload = CreateMultipartUploadResponse
  request = Req.post s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateMultipartUploadResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (x Lude..@? "Bucket")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-algorithm")
            Lude.<*> (h Lude..#? "x-amz-abort-date")
            Lude.<*> (h Lude..#? "x-amz-abort-rule-id")
            Lude.<*> (x Lude..@? "Key")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-key-MD5")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-aws-kms-key-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-context")
            Lude.<*> (x Lude..@? "UploadId")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMultipartUpload where
  toHeaders CreateMultipartUpload' {..} =
    Lude.mconcat
      [ "x-amz-object-lock-mode" Lude.=# objectLockMode,
        "Expires" Lude.=# expires,
        "x-amz-grant-read-acp" Lude.=# grantReadACP,
        "x-amz-server-side-encryption-customer-algorithm"
          Lude.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" Lude.=# sSECustomerKey,
        "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-grant-write-acp" Lude.=# grantWriteACP,
        "x-amz-website-redirect-location" Lude.=# websiteRedirectLocation,
        "x-amz-grant-read" Lude.=# grantRead,
        "x-amz-storage-class" Lude.=# storageClass,
        "x-amz-server-side-encryption-customer-key-MD5"
          Lude.=# sSECustomerKeyMD5,
        "x-amz-server-side-encryption-aws-kms-key-id" Lude.=# sSEKMSKeyId,
        "x-amz-grant-full-control" Lude.=# grantFullControl,
        "Content-Encoding" Lude.=# contentEncoding,
        "x-amz-tagging" Lude.=# tagging,
        "x-amz-object-lock-retain-until-date"
          Lude.=# objectLockRetainUntilDate,
        "x-amz-meta-" Lude.=# metadata,
        "x-amz-server-side-encryption-context"
          Lude.=# sSEKMSEncryptionContext,
        "Cache-Control" Lude.=# cacheControl,
        "Content-Language" Lude.=# contentLanguage,
        "x-amz-object-lock-legal-hold" Lude.=# objectLockLegalHoldStatus,
        "x-amz-acl" Lude.=# acl,
        "Content-Disposition" Lude.=# contentDisposition,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner,
        "x-amz-server-side-encryption" Lude.=# serverSideEncryption,
        "Content-Type" Lude.=# contentType
      ]

instance Lude.ToPath CreateMultipartUpload where
  toPath CreateMultipartUpload' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery CreateMultipartUpload where
  toQuery = Lude.const (Lude.mconcat ["uploads"])

-- | /See:/ 'mkCreateMultipartUploadResponse' smart constructor.
data CreateMultipartUploadResponse = CreateMultipartUploadResponse'
  { requestCharged :: Lude.Maybe RequestCharged,
    -- | The name of the bucket to which the multipart upload was initiated.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Lude.Maybe BucketName,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    -- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, the response includes this header. The header indicates when the initiated multipart upload becomes eligible for an abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
    --
    -- The response also includes the @x-amz-abort-rule-id@ header that provides the ID of the lifecycle configuration rule that defines this action.
    abortDate :: Lude.Maybe Lude.DateTime,
    -- | This header is returned along with the @x-amz-abort-date@ header. It identifies the applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
    abortRuleId :: Lude.Maybe Lude.Text,
    -- | Object key for which the multipart upload was initiated.
    key :: Lude.Maybe ObjectKey,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | ID for the initiated multipart upload.
    uploadId :: Lude.Maybe Lude.Text,
    -- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMultipartUploadResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' -
-- * 'bucket' - The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
-- * 'abortDate' - If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, the response includes this header. The header indicates when the initiated multipart upload becomes eligible for an abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
--
-- The response also includes the @x-amz-abort-rule-id@ header that provides the ID of the lifecycle configuration rule that defines this action.
-- * 'abortRuleId' - This header is returned along with the @x-amz-abort-date@ header. It identifies the applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
-- * 'key' - Object key for which the multipart upload was initiated.
-- * 'sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
-- * 'sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
-- * 'sSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
-- * 'uploadId' - ID for the initiated multipart upload.
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
-- * 'responseStatus' - The response status code.
mkCreateMultipartUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMultipartUploadResponse
mkCreateMultipartUploadResponse pResponseStatus_ =
  CreateMultipartUploadResponse'
    { requestCharged = Lude.Nothing,
      bucket = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      abortDate = Lude.Nothing,
      abortRuleId = Lude.Nothing,
      key = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      sSEKMSEncryptionContext = Lude.Nothing,
      uploadId = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRequestCharged :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe RequestCharged)
crsRequestCharged = Lens.lens (requestCharged :: CreateMultipartUploadResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsBucket :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe BucketName)
crsBucket = Lens.lens (bucket :: CreateMultipartUploadResponse -> Lude.Maybe BucketName) (\s a -> s {bucket = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSSECustomerAlgorithm :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe Lude.Text)
crsSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: CreateMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, the response includes this header. The header indicates when the initiated multipart upload becomes eligible for an abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
--
-- The response also includes the @x-amz-abort-rule-id@ header that provides the ID of the lifecycle configuration rule that defines this action.
--
-- /Note:/ Consider using 'abortDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsAbortDate :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe Lude.DateTime)
crsAbortDate = Lens.lens (abortDate :: CreateMultipartUploadResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {abortDate = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsAbortDate "Use generic-lens or generic-optics with 'abortDate' instead." #-}

-- | This header is returned along with the @x-amz-abort-date@ header. It identifies the applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
--
-- /Note:/ Consider using 'abortRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsAbortRuleId :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe Lude.Text)
crsAbortRuleId = Lens.lens (abortRuleId :: CreateMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {abortRuleId = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsAbortRuleId "Use generic-lens or generic-optics with 'abortRuleId' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsKey :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe ObjectKey)
crsKey = Lens.lens (key :: CreateMultipartUploadResponse -> Lude.Maybe ObjectKey) (\s a -> s {key = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSSECustomerKeyMD5 :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe Lude.Text)
crsSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: CreateMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSSEKMSKeyId :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
crsSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: CreateMultipartUploadResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSSEKMSEncryptionContext :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
crsSSEKMSEncryptionContext = Lens.lens (sSEKMSEncryptionContext :: CreateMultipartUploadResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSEncryptionContext = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | ID for the initiated multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsUploadId :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe Lude.Text)
crsUploadId = Lens.lens (uploadId :: CreateMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {uploadId = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsServerSideEncryption :: Lens.Lens' CreateMultipartUploadResponse (Lude.Maybe ServerSideEncryption)
crsServerSideEncryption = Lens.lens (serverSideEncryption :: CreateMultipartUploadResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateMultipartUploadResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateMultipartUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMultipartUploadResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

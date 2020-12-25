{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an object that is already stored in Amazon S3.
--
-- All copy requests must be authenticated. Additionally, you must have /read/ access to the source object and /write/ access to the destination bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> . Both the Region that you want to copy the object from and the Region that you want to copy the object to must be enabled for your account.
-- A copy request might return an error when Amazon S3 receives the copy request or while Amazon S3 is copying the files. If the error occurs before the copy operation starts, you receive a standard Amazon S3 error. If the error occurs during the copy operation, the error response is embedded in the @200 OK@ response. This means that a @200 OK@ response can contain either a success or an error. Design your application to parse the contents of the response and handle it appropriately.
-- If the copy is successful, you receive a response with information about the copied object.
-- The copy request charge is based on the storage class and Region that you specify for the destination object. For pricing information, see <https://aws.amazon.com/s3/pricing/ Amazon S3 pricing> .
-- /Important:/ Amazon S3 transfer acceleration does not support cross-Region copies. If you request a cross-Region copy using a transfer acceleration endpoint, you get a 400 @Bad Request@ error. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> .
-- __Metadata__
-- When copying an object, you can preserve all metadata (default) or specify new metadata. However, the ACL is not preserved and is set to private for the user making the request. To override the default ACL setting, specify a new ACL when generating a copy request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs> .
-- To specify whether you want the object metadata copied from the source object or replaced with metadata provided in the request, you can optionally add the @x-amz-metadata-directive@ header. When you grant permissions, you can use the @s3:x-amz-metadata-directive@ condition key to enforce certain metadata behavior when objects are uploaded. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/amazon-s3-policy-keys.html Specifying Conditions in a Policy> in the /Amazon S3 Developer Guide/ . For a complete list of Amazon S3-specific condition keys, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3> .
-- __@x-amz-copy-source-if@ Headers__
-- To only copy an object under certain conditions, such as whether the @Etag@ matches or whether the object was modified before or after a specified date, use the following request parameters:
--
--     * @x-amz-copy-source-if-match@
--
--
--     * @x-amz-copy-source-if-none-match@
--
--
--     * @x-amz-copy-source-if-unmodified-since@
--
--
--     * @x-amz-copy-source-if-modified-since@
--
--
-- If both the @x-amz-copy-source-if-match@ and @x-amz-copy-source-if-unmodified-since@ headers are present in the request and evaluate as follows, Amazon S3 returns @200 OK@ and copies the data:
--
--     * @x-amz-copy-source-if-match@ condition evaluates to true
--
--
--     * @x-amz-copy-source-if-unmodified-since@ condition evaluates to false
--
--
-- If both the @x-amz-copy-source-if-none-match@ and @x-amz-copy-source-if-modified-since@ headers are present in the request and evaluate as follows, Amazon S3 returns the @412 Precondition Failed@ response code:
--
--     * @x-amz-copy-source-if-none-match@ condition evaluates to false
--
--
--     * @x-amz-copy-source-if-modified-since@ condition evaluates to true
--
--
-- __Encryption__
-- The source object that you are copying can be encrypted or unencrypted. The source object can be encrypted with server-side encryption using AWS managed encryption keys (SSE-S3 or SSE-KMS) or by using a customer-provided encryption key. With server-side encryption, Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts the data when you access it.
-- You can optionally use the appropriate encryption-related headers to request server-side encryption for the target object. You have the option to provide your own encryption key or use SSE-S3 or SSE-KMS, regardless of the form of server-side encryption that was used to encrypt the source object. You can even request encryption if the source object was not encrypted. For more information about server-side encryption, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Using Server-Side Encryption> .
-- __Access Control List (ACL)-Specific Request Headers__
-- When copying an object, you can optionally use headers to grant ACL-based permissions. By default, all objects are private. Only the owner has full access control. When adding a new object, you can grant permissions to individual AWS accounts or to predefined groups defined by Amazon S3. These permissions are then added to the ACL on the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-using-rest-api.html Managing ACLs Using the REST API> .
-- __Storage Class Options__
-- You can use the @CopyObject@ operation to change the storage class of an object that is already stored in Amazon S3 using the @StorageClass@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
-- __Versioning__
-- By default, @x-amz-copy-source@ identifies the current version of an object to copy. If the current version is a delete marker, Amazon S3 behaves as if the object was deleted. To copy a different version, use the @versionId@ subresource.
-- If you enable versioning on the target bucket, Amazon S3 generates a unique version ID for the object being copied. This version ID is different from the version ID of the source object. Amazon S3 returns the version ID of the copied object in the @x-amz-version-id@ response header in the response.
-- If you do not enable versioning or suspend it on the target bucket, the version ID that Amazon S3 generates is always null.
-- If the source object's storage class is GLACIER, you must restore a copy of this object before you can use it as a source object for the copy operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> .
-- The following operations are related to @CopyObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/CopyingObjectsExamples.html Copying Objects> .
module Network.AWS.S3.CopyObject
  ( -- * Creating a request
    CopyObject (..),
    mkCopyObject,

    -- ** Request lenses
    coBucket,
    coCopySource,
    coKey,
    coACL,
    coCacheControl,
    coContentDisposition,
    coContentEncoding,
    coContentLanguage,
    coContentType,
    coCopySourceIfMatch,
    coCopySourceIfModifiedSince,
    coCopySourceIfNoneMatch,
    coCopySourceIfUnmodifiedSince,
    coCopySourceSSECustomerAlgorithm,
    coCopySourceSSECustomerKey,
    coCopySourceSSECustomerKeyMD5,
    coExpectedBucketOwner,
    coExpectedSourceBucketOwner,
    coExpires,
    coGrantFullControl,
    coGrantRead,
    coGrantReadACP,
    coGrantWriteACP,
    coMetadata,
    coMetadataDirective,
    coObjectLockLegalHoldStatus,
    coObjectLockMode,
    coObjectLockRetainUntilDate,
    coRequestPayer,
    coSSECustomerAlgorithm,
    coSSECustomerKey,
    coSSECustomerKeyMD5,
    coSSEKMSEncryptionContext,
    coSSEKMSKeyId,
    coServerSideEncryption,
    coStorageClass,
    coTagging,
    coTaggingDirective,
    coWebsiteRedirectLocation,

    -- * Destructuring the response
    CopyObjectResponse (..),
    mkCopyObjectResponse,

    -- ** Response lenses
    corrsCopyObjectResult,
    corrsCopySourceVersionId,
    corrsExpiration,
    corrsRequestCharged,
    corrsSSECustomerAlgorithm,
    corrsSSECustomerKeyMD5,
    corrsSSEKMSEncryptionContext,
    corrsSSEKMSKeyId,
    corrsServerSideEncryption,
    corrsVersionId,
    corrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkCopyObject' smart constructor.
data CopyObject = CopyObject'
  { -- | The name of the destination bucket.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :
    --
    --
    --     * For objects not accessed through an access point, specify the name of the source bucket and the key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.
    --
    --
    --     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded.
    -- Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded.
    --
    --
    -- To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
    copySource :: Types.CopySource,
    -- | The key of the destination object.
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
    -- | Copies the object if its entity tag (ETag) matches the specified tag.
    copySourceIfMatch :: Core.Maybe Types.CopySourceIfMatch,
    -- | Copies the object if it has been modified since the specified time.
    copySourceIfModifiedSince :: Core.Maybe Core.UTCTime,
    -- | Copies the object if its entity tag (ETag) is different than the specified ETag.
    copySourceIfNoneMatch :: Core.Maybe Types.CopySourceIfNoneMatch,
    -- | Copies the object if it hasn't been modified since the specified time.
    copySourceIfUnmodifiedSince :: Core.Maybe Core.UTCTime,
    -- | Specifies the algorithm to use when decrypting the source object (for example, AES256).
    copySourceSSECustomerAlgorithm :: Core.Maybe Types.CopySourceSSECustomerAlgorithm,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
    copySourceSSECustomerKey :: Core.Maybe Types.CopySourceSSECustomerKey,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
    copySourceSSECustomerKeyMD5 :: Core.Maybe Types.CopySourceSSECustomerKeyMD5,
    -- | The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedSourceBucketOwner :: Core.Maybe Types.ExpectedSourceBucketOwner,
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
    -- | Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
    metadataDirective :: Core.Maybe Types.MetadataDirective,
    -- | Specifies whether you want to apply a Legal Hold to the copied object.
    objectLockLegalHoldStatus :: Core.Maybe Types.ObjectLockLegalHoldStatus,
    -- | The Object Lock mode that you want to apply to the copied object.
    objectLockMode :: Core.Maybe Types.ObjectLockMode,
    -- | The date and time when you want the copied object's Object Lock to expire.
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
    -- | Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
    sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId,
    -- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe Types.ServerSideEncryption,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
    storageClass :: Core.Maybe Types.StorageClass,
    -- | The tag-set for the object destination object this value must be used in conjunction with the @TaggingDirective@ . The tag-set must be encoded as URL Query parameters.
    tagging :: Core.Maybe Types.TaggingHeader,
    -- | Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
    taggingDirective :: Core.Maybe Types.TaggingDirective,
    -- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Core.Maybe Types.WebsiteRedirectLocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopyObject' value with any optional fields omitted.
mkCopyObject ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'copySource'
  Types.CopySource ->
  -- | 'key'
  Types.Key ->
  CopyObject
mkCopyObject bucket copySource key =
  CopyObject'
    { bucket,
      copySource,
      key,
      acl = Core.Nothing,
      cacheControl = Core.Nothing,
      contentDisposition = Core.Nothing,
      contentEncoding = Core.Nothing,
      contentLanguage = Core.Nothing,
      contentType = Core.Nothing,
      copySourceIfMatch = Core.Nothing,
      copySourceIfModifiedSince = Core.Nothing,
      copySourceIfNoneMatch = Core.Nothing,
      copySourceIfUnmodifiedSince = Core.Nothing,
      copySourceSSECustomerAlgorithm = Core.Nothing,
      copySourceSSECustomerKey = Core.Nothing,
      copySourceSSECustomerKeyMD5 = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      expectedSourceBucketOwner = Core.Nothing,
      expires = Core.Nothing,
      grantFullControl = Core.Nothing,
      grantRead = Core.Nothing,
      grantReadACP = Core.Nothing,
      grantWriteACP = Core.Nothing,
      metadata = Core.mempty,
      metadataDirective = Core.Nothing,
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
      taggingDirective = Core.Nothing,
      websiteRedirectLocation = Core.Nothing
    }

-- | The name of the destination bucket.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coBucket :: Lens.Lens' CopyObject Types.BucketName
coBucket = Lens.field @"bucket"
{-# DEPRECATED coBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :
--
--
--     * For objects not accessed through an access point, specify the name of the source bucket and the key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.
--
--
--     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded.
-- Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded.
--
--
-- To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
--
-- /Note:/ Consider using 'copySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySource :: Lens.Lens' CopyObject Types.CopySource
coCopySource = Lens.field @"copySource"
{-# DEPRECATED coCopySource "Use generic-lens or generic-optics with 'copySource' instead." #-}

-- | The key of the destination object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coKey :: Lens.Lens' CopyObject Types.Key
coKey = Lens.field @"key"
{-# DEPRECATED coKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coACL :: Lens.Lens' CopyObject (Core.Maybe Types.ObjectCannedACL)
coACL = Lens.field @"acl"
{-# DEPRECATED coACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCacheControl :: Lens.Lens' CopyObject (Core.Maybe Types.CacheControl)
coCacheControl = Lens.field @"cacheControl"
{-# DEPRECATED coCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentDisposition :: Lens.Lens' CopyObject (Core.Maybe Types.ContentDisposition)
coContentDisposition = Lens.field @"contentDisposition"
{-# DEPRECATED coContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentEncoding :: Lens.Lens' CopyObject (Core.Maybe Types.ContentEncoding)
coContentEncoding = Lens.field @"contentEncoding"
{-# DEPRECATED coContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentLanguage :: Lens.Lens' CopyObject (Core.Maybe Types.ContentLanguage)
coContentLanguage = Lens.field @"contentLanguage"
{-# DEPRECATED coContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentType :: Lens.Lens' CopyObject (Core.Maybe Types.ContentType)
coContentType = Lens.field @"contentType"
{-# DEPRECATED coContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
--
-- /Note:/ Consider using 'copySourceIfMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfMatch :: Lens.Lens' CopyObject (Core.Maybe Types.CopySourceIfMatch)
coCopySourceIfMatch = Lens.field @"copySourceIfMatch"
{-# DEPRECATED coCopySourceIfMatch "Use generic-lens or generic-optics with 'copySourceIfMatch' instead." #-}

-- | Copies the object if it has been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfModifiedSince :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
coCopySourceIfModifiedSince = Lens.field @"copySourceIfModifiedSince"
{-# DEPRECATED coCopySourceIfModifiedSince "Use generic-lens or generic-optics with 'copySourceIfModifiedSince' instead." #-}

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- /Note:/ Consider using 'copySourceIfNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfNoneMatch :: Lens.Lens' CopyObject (Core.Maybe Types.CopySourceIfNoneMatch)
coCopySourceIfNoneMatch = Lens.field @"copySourceIfNoneMatch"
{-# DEPRECATED coCopySourceIfNoneMatch "Use generic-lens or generic-optics with 'copySourceIfNoneMatch' instead." #-}

-- | Copies the object if it hasn't been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfUnmodifiedSince :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
coCopySourceIfUnmodifiedSince = Lens.field @"copySourceIfUnmodifiedSince"
{-# DEPRECATED coCopySourceIfUnmodifiedSince "Use generic-lens or generic-optics with 'copySourceIfUnmodifiedSince' instead." #-}

-- | Specifies the algorithm to use when decrypting the source object (for example, AES256).
--
-- /Note:/ Consider using 'copySourceSSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceSSECustomerAlgorithm :: Lens.Lens' CopyObject (Core.Maybe Types.CopySourceSSECustomerAlgorithm)
coCopySourceSSECustomerAlgorithm = Lens.field @"copySourceSSECustomerAlgorithm"
{-# DEPRECATED coCopySourceSSECustomerAlgorithm "Use generic-lens or generic-optics with 'copySourceSSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- /Note:/ Consider using 'copySourceSSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceSSECustomerKey :: Lens.Lens' CopyObject (Core.Maybe Types.CopySourceSSECustomerKey)
coCopySourceSSECustomerKey = Lens.field @"copySourceSSECustomerKey"
{-# DEPRECATED coCopySourceSSECustomerKey "Use generic-lens or generic-optics with 'copySourceSSECustomerKey' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'copySourceSSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceSSECustomerKeyMD5 :: Lens.Lens' CopyObject (Core.Maybe Types.CopySourceSSECustomerKeyMD5)
coCopySourceSSECustomerKeyMD5 = Lens.field @"copySourceSSECustomerKeyMD5"
{-# DEPRECATED coCopySourceSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'copySourceSSECustomerKeyMD5' instead." #-}

-- | The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coExpectedBucketOwner :: Lens.Lens' CopyObject (Core.Maybe Types.ExpectedBucketOwner)
coExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED coExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedSourceBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coExpectedSourceBucketOwner :: Lens.Lens' CopyObject (Core.Maybe Types.ExpectedSourceBucketOwner)
coExpectedSourceBucketOwner = Lens.field @"expectedSourceBucketOwner"
{-# DEPRECATED coExpectedSourceBucketOwner "Use generic-lens or generic-optics with 'expectedSourceBucketOwner' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coExpires :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
coExpires = Lens.field @"expires"
{-# DEPRECATED coExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantFullControl :: Lens.Lens' CopyObject (Core.Maybe Types.GrantFullControl)
coGrantFullControl = Lens.field @"grantFullControl"
{-# DEPRECATED coGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantRead :: Lens.Lens' CopyObject (Core.Maybe Types.GrantRead)
coGrantRead = Lens.field @"grantRead"
{-# DEPRECATED coGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantReadACP :: Lens.Lens' CopyObject (Core.Maybe Types.GrantReadACP)
coGrantReadACP = Lens.field @"grantReadACP"
{-# DEPRECATED coGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantWriteACP :: Lens.Lens' CopyObject (Core.Maybe Types.GrantWriteACP)
coGrantWriteACP = Lens.field @"grantWriteACP"
{-# DEPRECATED coGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMetadata :: Lens.Lens' CopyObject (Core.HashMap Types.MetadataKey Types.MetadataValue)
coMetadata = Lens.field @"metadata"
{-# DEPRECATED coMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
--
-- /Note:/ Consider using 'metadataDirective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMetadataDirective :: Lens.Lens' CopyObject (Core.Maybe Types.MetadataDirective)
coMetadataDirective = Lens.field @"metadataDirective"
{-# DEPRECATED coMetadataDirective "Use generic-lens or generic-optics with 'metadataDirective' instead." #-}

-- | Specifies whether you want to apply a Legal Hold to the copied object.
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectLockLegalHoldStatus :: Lens.Lens' CopyObject (Core.Maybe Types.ObjectLockLegalHoldStatus)
coObjectLockLegalHoldStatus = Lens.field @"objectLockLegalHoldStatus"
{-# DEPRECATED coObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | The Object Lock mode that you want to apply to the copied object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectLockMode :: Lens.Lens' CopyObject (Core.Maybe Types.ObjectLockMode)
coObjectLockMode = Lens.field @"objectLockMode"
{-# DEPRECATED coObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time when you want the copied object's Object Lock to expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectLockRetainUntilDate :: Lens.Lens' CopyObject (Core.Maybe Core.UTCTime)
coObjectLockRetainUntilDate = Lens.field @"objectLockRetainUntilDate"
{-# DEPRECATED coObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coRequestPayer :: Lens.Lens' CopyObject (Core.Maybe Types.RequestPayer)
coRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED coRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSECustomerAlgorithm :: Lens.Lens' CopyObject (Core.Maybe Types.SSECustomerAlgorithm)
coSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED coSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSECustomerKey :: Lens.Lens' CopyObject (Core.Maybe Types.SSECustomerKey)
coSSECustomerKey = Lens.field @"sSECustomerKey"
{-# DEPRECATED coSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSECustomerKeyMD5 :: Lens.Lens' CopyObject (Core.Maybe Types.SSECustomerKeyMD5)
coSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED coSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSEKMSEncryptionContext :: Lens.Lens' CopyObject (Core.Maybe Types.SSEKMSEncryptionContext)
coSSEKMSEncryptionContext = Lens.field @"sSEKMSEncryptionContext"
{-# DEPRECATED coSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSEKMSKeyId :: Lens.Lens' CopyObject (Core.Maybe Types.SSEKMSKeyId)
coSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED coSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coServerSideEncryption :: Lens.Lens' CopyObject (Core.Maybe Types.ServerSideEncryption)
coServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED coServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coStorageClass :: Lens.Lens' CopyObject (Core.Maybe Types.StorageClass)
coStorageClass = Lens.field @"storageClass"
{-# DEPRECATED coStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The tag-set for the object destination object this value must be used in conjunction with the @TaggingDirective@ . The tag-set must be encoded as URL Query parameters.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coTagging :: Lens.Lens' CopyObject (Core.Maybe Types.TaggingHeader)
coTagging = Lens.field @"tagging"
{-# DEPRECATED coTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
--
-- /Note:/ Consider using 'taggingDirective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coTaggingDirective :: Lens.Lens' CopyObject (Core.Maybe Types.TaggingDirective)
coTaggingDirective = Lens.field @"taggingDirective"
{-# DEPRECATED coTaggingDirective "Use generic-lens or generic-optics with 'taggingDirective' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coWebsiteRedirectLocation :: Lens.Lens' CopyObject (Core.Maybe Types.WebsiteRedirectLocation)
coWebsiteRedirectLocation = Lens.field @"websiteRedirectLocation"
{-# DEPRECATED coWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

instance Core.AWSRequest CopyObject where
  type Rs CopyObject = CopyObjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-copy-source" copySource
            Core.<> (Core.toHeaders "x-amz-acl" acl)
            Core.<> (Core.toHeaders "Cache-Control" cacheControl)
            Core.<> (Core.toHeaders "Content-Disposition" contentDisposition)
            Core.<> (Core.toHeaders "Content-Encoding" contentEncoding)
            Core.<> (Core.toHeaders "Content-Language" contentLanguage)
            Core.<> (Core.toHeaders "Content-Type" contentType)
            Core.<> (Core.toHeaders "x-amz-copy-source-if-match" copySourceIfMatch)
            Core.<> ( Core.toHeaders
                        "x-amz-copy-source-if-modified-since"
                        copySourceIfModifiedSince
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-copy-source-if-none-match"
                        copySourceIfNoneMatch
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-copy-source-if-unmodified-since"
                        copySourceIfUnmodifiedSince
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-copy-source-server-side-encryption-customer-algorithm"
                        copySourceSSECustomerAlgorithm
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-copy-source-server-side-encryption-customer-key"
                        copySourceSSECustomerKey
                    )
            Core.<> ( Core.toHeaders
                        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                        copySourceSSECustomerKeyMD5
                    )
            Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner)
            Core.<> ( Core.toHeaders
                        "x-amz-source-expected-bucket-owner"
                        expectedSourceBucketOwner
                    )
            Core.<> (Core.toHeaders "Expires" expires)
            Core.<> (Core.toHeaders "x-amz-grant-full-control" grantFullControl)
            Core.<> (Core.toHeaders "x-amz-grant-read" grantRead)
            Core.<> (Core.toHeaders "x-amz-grant-read-acp" grantReadACP)
            Core.<> (Core.toHeaders "x-amz-grant-write-acp" grantWriteACP)
            Core.<> (Core.toHeaders "x-amz-meta-" metadata)
            Core.<> (Core.toHeaders "x-amz-metadata-directive" metadataDirective)
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
            Core.<> (Core.toHeaders "x-amz-tagging-directive" taggingDirective)
            Core.<> ( Core.toHeaders
                        "x-amz-website-redirect-location"
                        websiteRedirectLocation
                    ),
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CopyObjectResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "x-amz-copy-source-version-id" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-expiration" h)
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
            Core.<*> (Core.parseHeaderMaybe "x-amz-version-id" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyObjectResponse' smart constructor.
data CopyObjectResponse = CopyObjectResponse'
  { -- | Container for all response elements.
    copyObjectResult :: Core.Maybe Types.CopyObjectResult,
    -- | Version of the copied object in the destination bucket.
    copySourceVersionId :: Core.Maybe Types.CopySourceVersionId,
    -- | If the object expiration is configured, the response includes this header.
    expiration :: Core.Maybe Types.Expiration,
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
    -- | Version ID of the newly created copy.
    versionId :: Core.Maybe Types.VersionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopyObjectResponse' value with any optional fields omitted.
mkCopyObjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyObjectResponse
mkCopyObjectResponse responseStatus =
  CopyObjectResponse'
    { copyObjectResult = Core.Nothing,
      copySourceVersionId = Core.Nothing,
      expiration = Core.Nothing,
      requestCharged = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      sSEKMSEncryptionContext = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      versionId = Core.Nothing,
      responseStatus
    }

-- | Container for all response elements.
--
-- /Note:/ Consider using 'copyObjectResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsCopyObjectResult :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.CopyObjectResult)
corrsCopyObjectResult = Lens.field @"copyObjectResult"
{-# DEPRECATED corrsCopyObjectResult "Use generic-lens or generic-optics with 'copyObjectResult' instead." #-}

-- | Version of the copied object in the destination bucket.
--
-- /Note:/ Consider using 'copySourceVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsCopySourceVersionId :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.CopySourceVersionId)
corrsCopySourceVersionId = Lens.field @"copySourceVersionId"
{-# DEPRECATED corrsCopySourceVersionId "Use generic-lens or generic-optics with 'copySourceVersionId' instead." #-}

-- | If the object expiration is configured, the response includes this header.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsExpiration :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.Expiration)
corrsExpiration = Lens.field @"expiration"
{-# DEPRECATED corrsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsRequestCharged :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.RequestCharged)
corrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED corrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsSSECustomerAlgorithm :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.SSECustomerAlgorithm)
corrsSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED corrsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsSSECustomerKeyMD5 :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.SSECustomerKeyMD5)
corrsSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED corrsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsSSEKMSEncryptionContext :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.SSEKMSEncryptionContext)
corrsSSEKMSEncryptionContext = Lens.field @"sSEKMSEncryptionContext"
{-# DEPRECATED corrsSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsSSEKMSKeyId :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.SSEKMSKeyId)
corrsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED corrsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsServerSideEncryption :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.ServerSideEncryption)
corrsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED corrsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | Version ID of the newly created copy.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsVersionId :: Lens.Lens' CopyObjectResponse (Core.Maybe Types.VersionId)
corrsVersionId = Lens.field @"versionId"
{-# DEPRECATED corrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsResponseStatus :: Lens.Lens' CopyObjectResponse Core.Int
corrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED corrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

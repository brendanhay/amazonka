{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    coCopySourceIfModifiedSince,
    coCopySourceIfUnmodifiedSince,
    coCopySourceSSECustomerKeyMD5,
    coTaggingDirective,
    coMetadataDirective,
    coObjectLockMode,
    coExpires,
    coGrantReadACP,
    coCopySourceIfNoneMatch,
    coSSECustomerAlgorithm,
    coSSECustomerKey,
    coRequestPayer,
    coGrantWriteACP,
    coCopySourceIfMatch,
    coWebsiteRedirectLocation,
    coGrantRead,
    coExpectedSourceBucketOwner,
    coStorageClass,
    coSSECustomerKeyMD5,
    coSSEKMSKeyId,
    coGrantFullControl,
    coContentEncoding,
    coTagging,
    coObjectLockRetainUntilDate,
    coMetadata,
    coSSEKMSEncryptionContext,
    coCacheControl,
    coContentLanguage,
    coCopySourceSSECustomerKey,
    coObjectLockLegalHoldStatus,
    coCopySourceSSECustomerAlgorithm,
    coACL,
    coContentDisposition,
    coExpectedBucketOwner,
    coServerSideEncryption,
    coContentType,
    coBucket,
    coCopySource,
    coKey,

    -- * Destructuring the response
    CopyObjectResponse (..),
    mkCopyObjectResponse,

    -- ** Response lenses
    corsRequestCharged,
    corsVersionId,
    corsExpiration,
    corsSSECustomerAlgorithm,
    corsCopySourceVersionId,
    corsSSECustomerKeyMD5,
    corsSSEKMSKeyId,
    corsSSEKMSEncryptionContext,
    corsServerSideEncryption,
    corsCopyObjectResult,
    corsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkCopyObject' smart constructor.
data CopyObject = CopyObject'
  { copySourceIfModifiedSince ::
      Lude.Maybe Lude.ISO8601,
    copySourceIfUnmodifiedSince :: Lude.Maybe Lude.ISO8601,
    copySourceSSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    taggingDirective :: Lude.Maybe TaggingDirective,
    metadataDirective :: Lude.Maybe MetadataDirective,
    objectLockMode :: Lude.Maybe ObjectLockMode,
    expires :: Lude.Maybe Lude.ISO8601,
    grantReadACP :: Lude.Maybe Lude.Text,
    copySourceIfNoneMatch :: Lude.Maybe Lude.Text,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    sSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestPayer :: Lude.Maybe RequestPayer,
    grantWriteACP :: Lude.Maybe Lude.Text,
    copySourceIfMatch :: Lude.Maybe Lude.Text,
    websiteRedirectLocation :: Lude.Maybe Lude.Text,
    grantRead :: Lude.Maybe Lude.Text,
    expectedSourceBucketOwner :: Lude.Maybe Lude.Text,
    storageClass :: Lude.Maybe StorageClass,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    grantFullControl :: Lude.Maybe Lude.Text,
    contentEncoding :: Lude.Maybe Lude.Text,
    tagging :: Lude.Maybe Lude.Text,
    objectLockRetainUntilDate :: Lude.Maybe Lude.ISO8601,
    metadata :: Lude.HashMap Lude.Text (Lude.Text),
    sSEKMSEncryptionContext :: Lude.Maybe (Lude.Sensitive Lude.Text),
    cacheControl :: Lude.Maybe Lude.Text,
    contentLanguage :: Lude.Maybe Lude.Text,
    copySourceSSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    objectLockLegalHoldStatus :: Lude.Maybe ObjectLockLegalHoldStatus,
    copySourceSSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    acl :: Lude.Maybe ObjectCannedACL,
    contentDisposition :: Lude.Maybe Lude.Text,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    contentType :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    copySource :: Lude.Text,
    key :: ObjectKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyObject' with the minimum fields required to make a request.
--
-- * 'acl' - The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'bucket' - The name of the destination bucket.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'cacheControl' - Specifies caching behavior along the request/reply chain.
-- * 'contentDisposition' - Specifies presentational information for the object.
-- * 'contentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
-- * 'contentLanguage' - The language the content is in.
-- * 'contentType' - A standard MIME type describing the format of the object data.
-- * 'copySource' - Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :
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
-- * 'copySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
-- * 'copySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
-- * 'copySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
-- * 'copySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
-- * 'copySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for example, AES256).
-- * 'copySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
-- * 'copySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'expectedBucketOwner' - The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'expectedSourceBucketOwner' - The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'expires' - The date and time at which the object is no longer cacheable.
-- * 'grantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'grantRead' - Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'grantReadACP' - Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'grantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'key' - The key of the destination object.
-- * 'metadata' - A map of metadata to store with the object in S3.
-- * 'metadataDirective' - Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
-- * 'objectLockLegalHoldStatus' - Specifies whether you want to apply a Legal Hold to the copied object.
-- * 'objectLockMode' - The Object Lock mode that you want to apply to the copied object.
-- * 'objectLockRetainUntilDate' - The date and time when you want the copied object's Object Lock to expire.
-- * 'requestPayer' - Undocumented field.
-- * 'sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
-- * 'sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
-- * 'sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'sSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
-- * 'sSEKMSKeyId' - Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
-- * 'storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
-- * 'tagging' - The tag-set for the object destination object this value must be used in conjunction with the @TaggingDirective@ . The tag-set must be encoded as URL Query parameters.
-- * 'taggingDirective' - Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
-- * 'websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
mkCopyObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'copySource'
  Lude.Text ->
  -- | 'key'
  ObjectKey ->
  CopyObject
mkCopyObject pBucket_ pCopySource_ pKey_ =
  CopyObject'
    { copySourceIfModifiedSince = Lude.Nothing,
      copySourceIfUnmodifiedSince = Lude.Nothing,
      copySourceSSECustomerKeyMD5 = Lude.Nothing,
      taggingDirective = Lude.Nothing,
      metadataDirective = Lude.Nothing,
      objectLockMode = Lude.Nothing,
      expires = Lude.Nothing,
      grantReadACP = Lude.Nothing,
      copySourceIfNoneMatch = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKey = Lude.Nothing,
      requestPayer = Lude.Nothing,
      grantWriteACP = Lude.Nothing,
      copySourceIfMatch = Lude.Nothing,
      websiteRedirectLocation = Lude.Nothing,
      grantRead = Lude.Nothing,
      expectedSourceBucketOwner = Lude.Nothing,
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
      copySourceSSECustomerKey = Lude.Nothing,
      objectLockLegalHoldStatus = Lude.Nothing,
      copySourceSSECustomerAlgorithm = Lude.Nothing,
      acl = Lude.Nothing,
      contentDisposition = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      contentType = Lude.Nothing,
      bucket = pBucket_,
      copySource = pCopySource_,
      key = pKey_
    }

-- | Copies the object if it has been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfModifiedSince :: Lens.Lens' CopyObject (Lude.Maybe Lude.ISO8601)
coCopySourceIfModifiedSince = Lens.lens (copySourceIfModifiedSince :: CopyObject -> Lude.Maybe Lude.ISO8601) (\s a -> s {copySourceIfModifiedSince = a} :: CopyObject)
{-# DEPRECATED coCopySourceIfModifiedSince "Use generic-lens or generic-optics with 'copySourceIfModifiedSince' instead." #-}

-- | Copies the object if it hasn't been modified since the specified time.
--
-- /Note:/ Consider using 'copySourceIfUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfUnmodifiedSince :: Lens.Lens' CopyObject (Lude.Maybe Lude.ISO8601)
coCopySourceIfUnmodifiedSince = Lens.lens (copySourceIfUnmodifiedSince :: CopyObject -> Lude.Maybe Lude.ISO8601) (\s a -> s {copySourceIfUnmodifiedSince = a} :: CopyObject)
{-# DEPRECATED coCopySourceIfUnmodifiedSince "Use generic-lens or generic-optics with 'copySourceIfUnmodifiedSince' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'copySourceSSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceSSECustomerKeyMD5 :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coCopySourceSSECustomerKeyMD5 = Lens.lens (copySourceSSECustomerKeyMD5 :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {copySourceSSECustomerKeyMD5 = a} :: CopyObject)
{-# DEPRECATED coCopySourceSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'copySourceSSECustomerKeyMD5' instead." #-}

-- | Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
--
-- /Note:/ Consider using 'taggingDirective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coTaggingDirective :: Lens.Lens' CopyObject (Lude.Maybe TaggingDirective)
coTaggingDirective = Lens.lens (taggingDirective :: CopyObject -> Lude.Maybe TaggingDirective) (\s a -> s {taggingDirective = a} :: CopyObject)
{-# DEPRECATED coTaggingDirective "Use generic-lens or generic-optics with 'taggingDirective' instead." #-}

-- | Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
--
-- /Note:/ Consider using 'metadataDirective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMetadataDirective :: Lens.Lens' CopyObject (Lude.Maybe MetadataDirective)
coMetadataDirective = Lens.lens (metadataDirective :: CopyObject -> Lude.Maybe MetadataDirective) (\s a -> s {metadataDirective = a} :: CopyObject)
{-# DEPRECATED coMetadataDirective "Use generic-lens or generic-optics with 'metadataDirective' instead." #-}

-- | The Object Lock mode that you want to apply to the copied object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectLockMode :: Lens.Lens' CopyObject (Lude.Maybe ObjectLockMode)
coObjectLockMode = Lens.lens (objectLockMode :: CopyObject -> Lude.Maybe ObjectLockMode) (\s a -> s {objectLockMode = a} :: CopyObject)
{-# DEPRECATED coObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coExpires :: Lens.Lens' CopyObject (Lude.Maybe Lude.ISO8601)
coExpires = Lens.lens (expires :: CopyObject -> Lude.Maybe Lude.ISO8601) (\s a -> s {expires = a} :: CopyObject)
{-# DEPRECATED coExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantReadACP :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coGrantReadACP = Lens.lens (grantReadACP :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {grantReadACP = a} :: CopyObject)
{-# DEPRECATED coGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- /Note:/ Consider using 'copySourceIfNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfNoneMatch :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coCopySourceIfNoneMatch = Lens.lens (copySourceIfNoneMatch :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {copySourceIfNoneMatch = a} :: CopyObject)
{-# DEPRECATED coCopySourceIfNoneMatch "Use generic-lens or generic-optics with 'copySourceIfNoneMatch' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSECustomerAlgorithm :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: CopyObject)
{-# DEPRECATED coSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSECustomerKey :: Lens.Lens' CopyObject (Lude.Maybe (Lude.Sensitive Lude.Text))
coSSECustomerKey = Lens.lens (sSECustomerKey :: CopyObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: CopyObject)
{-# DEPRECATED coSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coRequestPayer :: Lens.Lens' CopyObject (Lude.Maybe RequestPayer)
coRequestPayer = Lens.lens (requestPayer :: CopyObject -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: CopyObject)
{-# DEPRECATED coRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantWriteACP :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coGrantWriteACP = Lens.lens (grantWriteACP :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {grantWriteACP = a} :: CopyObject)
{-# DEPRECATED coGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
--
-- /Note:/ Consider using 'copySourceIfMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceIfMatch :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coCopySourceIfMatch = Lens.lens (copySourceIfMatch :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {copySourceIfMatch = a} :: CopyObject)
{-# DEPRECATED coCopySourceIfMatch "Use generic-lens or generic-optics with 'copySourceIfMatch' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coWebsiteRedirectLocation :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coWebsiteRedirectLocation = Lens.lens (websiteRedirectLocation :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {websiteRedirectLocation = a} :: CopyObject)
{-# DEPRECATED coWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantRead :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coGrantRead = Lens.lens (grantRead :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {grantRead = a} :: CopyObject)
{-# DEPRECATED coGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedSourceBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coExpectedSourceBucketOwner :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coExpectedSourceBucketOwner = Lens.lens (expectedSourceBucketOwner :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {expectedSourceBucketOwner = a} :: CopyObject)
{-# DEPRECATED coExpectedSourceBucketOwner "Use generic-lens or generic-optics with 'expectedSourceBucketOwner' instead." #-}

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coStorageClass :: Lens.Lens' CopyObject (Lude.Maybe StorageClass)
coStorageClass = Lens.lens (storageClass :: CopyObject -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: CopyObject)
{-# DEPRECATED coStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSECustomerKeyMD5 :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: CopyObject)
{-# DEPRECATED coSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSEKMSKeyId :: Lens.Lens' CopyObject (Lude.Maybe (Lude.Sensitive Lude.Text))
coSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: CopyObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: CopyObject)
{-# DEPRECATED coSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coGrantFullControl :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coGrantFullControl = Lens.lens (grantFullControl :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {grantFullControl = a} :: CopyObject)
{-# DEPRECATED coGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentEncoding :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coContentEncoding = Lens.lens (contentEncoding :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {contentEncoding = a} :: CopyObject)
{-# DEPRECATED coContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The tag-set for the object destination object this value must be used in conjunction with the @TaggingDirective@ . The tag-set must be encoded as URL Query parameters.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coTagging :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coTagging = Lens.lens (tagging :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {tagging = a} :: CopyObject)
{-# DEPRECATED coTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | The date and time when you want the copied object's Object Lock to expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectLockRetainUntilDate :: Lens.Lens' CopyObject (Lude.Maybe Lude.ISO8601)
coObjectLockRetainUntilDate = Lens.lens (objectLockRetainUntilDate :: CopyObject -> Lude.Maybe Lude.ISO8601) (\s a -> s {objectLockRetainUntilDate = a} :: CopyObject)
{-# DEPRECATED coObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMetadata :: Lens.Lens' CopyObject (Lude.HashMap Lude.Text (Lude.Text))
coMetadata = Lens.lens (metadata :: CopyObject -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {metadata = a} :: CopyObject)
{-# DEPRECATED coMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSSEKMSEncryptionContext :: Lens.Lens' CopyObject (Lude.Maybe (Lude.Sensitive Lude.Text))
coSSEKMSEncryptionContext = Lens.lens (sSEKMSEncryptionContext :: CopyObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSEncryptionContext = a} :: CopyObject)
{-# DEPRECATED coSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCacheControl :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coCacheControl = Lens.lens (cacheControl :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: CopyObject)
{-# DEPRECATED coCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentLanguage :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coContentLanguage = Lens.lens (contentLanguage :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {contentLanguage = a} :: CopyObject)
{-# DEPRECATED coContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- /Note:/ Consider using 'copySourceSSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceSSECustomerKey :: Lens.Lens' CopyObject (Lude.Maybe (Lude.Sensitive Lude.Text))
coCopySourceSSECustomerKey = Lens.lens (copySourceSSECustomerKey :: CopyObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {copySourceSSECustomerKey = a} :: CopyObject)
{-# DEPRECATED coCopySourceSSECustomerKey "Use generic-lens or generic-optics with 'copySourceSSECustomerKey' instead." #-}

-- | Specifies whether you want to apply a Legal Hold to the copied object.
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectLockLegalHoldStatus :: Lens.Lens' CopyObject (Lude.Maybe ObjectLockLegalHoldStatus)
coObjectLockLegalHoldStatus = Lens.lens (objectLockLegalHoldStatus :: CopyObject -> Lude.Maybe ObjectLockLegalHoldStatus) (\s a -> s {objectLockLegalHoldStatus = a} :: CopyObject)
{-# DEPRECATED coObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | Specifies the algorithm to use when decrypting the source object (for example, AES256).
--
-- /Note:/ Consider using 'copySourceSSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCopySourceSSECustomerAlgorithm :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coCopySourceSSECustomerAlgorithm = Lens.lens (copySourceSSECustomerAlgorithm :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {copySourceSSECustomerAlgorithm = a} :: CopyObject)
{-# DEPRECATED coCopySourceSSECustomerAlgorithm "Use generic-lens or generic-optics with 'copySourceSSECustomerAlgorithm' instead." #-}

-- | The canned ACL to apply to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coACL :: Lens.Lens' CopyObject (Lude.Maybe ObjectCannedACL)
coACL = Lens.lens (acl :: CopyObject -> Lude.Maybe ObjectCannedACL) (\s a -> s {acl = a} :: CopyObject)
{-# DEPRECATED coACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentDisposition :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coContentDisposition = Lens.lens (contentDisposition :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {contentDisposition = a} :: CopyObject)
{-# DEPRECATED coContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coExpectedBucketOwner :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coExpectedBucketOwner = Lens.lens (expectedBucketOwner :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: CopyObject)
{-# DEPRECATED coExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coServerSideEncryption :: Lens.Lens' CopyObject (Lude.Maybe ServerSideEncryption)
coServerSideEncryption = Lens.lens (serverSideEncryption :: CopyObject -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: CopyObject)
{-# DEPRECATED coServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coContentType :: Lens.Lens' CopyObject (Lude.Maybe Lude.Text)
coContentType = Lens.lens (contentType :: CopyObject -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: CopyObject)
{-# DEPRECATED coContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The name of the destination bucket.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coBucket :: Lens.Lens' CopyObject BucketName
coBucket = Lens.lens (bucket :: CopyObject -> BucketName) (\s a -> s {bucket = a} :: CopyObject)
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
coCopySource :: Lens.Lens' CopyObject Lude.Text
coCopySource = Lens.lens (copySource :: CopyObject -> Lude.Text) (\s a -> s {copySource = a} :: CopyObject)
{-# DEPRECATED coCopySource "Use generic-lens or generic-optics with 'copySource' instead." #-}

-- | The key of the destination object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coKey :: Lens.Lens' CopyObject ObjectKey
coKey = Lens.lens (key :: CopyObject -> ObjectKey) (\s a -> s {key = a} :: CopyObject)
{-# DEPRECATED coKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest CopyObject where
  type Rs CopyObject = CopyObjectResponse
  request = Req.put s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          CopyObjectResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (h Lude..#? "x-amz-version-id")
            Lude.<*> (h Lude..#? "x-amz-expiration")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-algorithm")
            Lude.<*> (h Lude..#? "x-amz-copy-source-version-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-key-MD5")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-aws-kms-key-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-context")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyObject where
  toHeaders CopyObject' {..} =
    Lude.mconcat
      [ "x-amz-copy-source-if-modified-since"
          Lude.=# copySourceIfModifiedSince,
        "x-amz-copy-source-if-unmodified-since"
          Lude.=# copySourceIfUnmodifiedSince,
        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
          Lude.=# copySourceSSECustomerKeyMD5,
        "x-amz-tagging-directive" Lude.=# taggingDirective,
        "x-amz-metadata-directive" Lude.=# metadataDirective,
        "x-amz-object-lock-mode" Lude.=# objectLockMode,
        "Expires" Lude.=# expires,
        "x-amz-grant-read-acp" Lude.=# grantReadACP,
        "x-amz-copy-source-if-none-match" Lude.=# copySourceIfNoneMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          Lude.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" Lude.=# sSECustomerKey,
        "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-grant-write-acp" Lude.=# grantWriteACP,
        "x-amz-copy-source-if-match" Lude.=# copySourceIfMatch,
        "x-amz-website-redirect-location" Lude.=# websiteRedirectLocation,
        "x-amz-grant-read" Lude.=# grantRead,
        "x-amz-source-expected-bucket-owner"
          Lude.=# expectedSourceBucketOwner,
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
        "x-amz-copy-source-server-side-encryption-customer-key"
          Lude.=# copySourceSSECustomerKey,
        "x-amz-object-lock-legal-hold" Lude.=# objectLockLegalHoldStatus,
        "x-amz-copy-source-server-side-encryption-customer-algorithm"
          Lude.=# copySourceSSECustomerAlgorithm,
        "x-amz-acl" Lude.=# acl,
        "Content-Disposition" Lude.=# contentDisposition,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner,
        "x-amz-server-side-encryption" Lude.=# serverSideEncryption,
        "Content-Type" Lude.=# contentType,
        "x-amz-copy-source" Lude.=# copySource
      ]

instance Lude.ToPath CopyObject where
  toPath CopyObject' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery CopyObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCopyObjectResponse' smart constructor.
data CopyObjectResponse = CopyObjectResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    versionId :: Lude.Maybe ObjectVersionId,
    expiration :: Lude.Maybe Lude.Text,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    copySourceVersionId :: Lude.Maybe Lude.Text,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sSEKMSEncryptionContext ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    serverSideEncryption ::
      Lude.Maybe ServerSideEncryption,
    copyObjectResult :: Lude.Maybe CopyObjectResult,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyObjectResponse' with the minimum fields required to make a request.
--
-- * 'copyObjectResult' - Container for all response elements.
-- * 'copySourceVersionId' - Version of the copied object in the destination bucket.
-- * 'expiration' - If the object expiration is configured, the response includes this header.
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
-- * 'sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
-- * 'sSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
-- * 'sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
-- * 'versionId' - Version ID of the newly created copy.
mkCopyObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyObjectResponse
mkCopyObjectResponse pResponseStatus_ =
  CopyObjectResponse'
    { requestCharged = Lude.Nothing,
      versionId = Lude.Nothing,
      expiration = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      copySourceVersionId = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      sSEKMSEncryptionContext = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      copyObjectResult = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsRequestCharged :: Lens.Lens' CopyObjectResponse (Lude.Maybe RequestCharged)
corsRequestCharged = Lens.lens (requestCharged :: CopyObjectResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: CopyObjectResponse)
{-# DEPRECATED corsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Version ID of the newly created copy.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsVersionId :: Lens.Lens' CopyObjectResponse (Lude.Maybe ObjectVersionId)
corsVersionId = Lens.lens (versionId :: CopyObjectResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: CopyObjectResponse)
{-# DEPRECATED corsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | If the object expiration is configured, the response includes this header.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsExpiration :: Lens.Lens' CopyObjectResponse (Lude.Maybe Lude.Text)
corsExpiration = Lens.lens (expiration :: CopyObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {expiration = a} :: CopyObjectResponse)
{-# DEPRECATED corsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsSSECustomerAlgorithm :: Lens.Lens' CopyObjectResponse (Lude.Maybe Lude.Text)
corsSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: CopyObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: CopyObjectResponse)
{-# DEPRECATED corsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Version of the copied object in the destination bucket.
--
-- /Note:/ Consider using 'copySourceVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsCopySourceVersionId :: Lens.Lens' CopyObjectResponse (Lude.Maybe Lude.Text)
corsCopySourceVersionId = Lens.lens (copySourceVersionId :: CopyObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {copySourceVersionId = a} :: CopyObjectResponse)
{-# DEPRECATED corsCopySourceVersionId "Use generic-lens or generic-optics with 'copySourceVersionId' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsSSECustomerKeyMD5 :: Lens.Lens' CopyObjectResponse (Lude.Maybe Lude.Text)
corsSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: CopyObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: CopyObjectResponse)
{-# DEPRECATED corsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsSSEKMSKeyId :: Lens.Lens' CopyObjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
corsSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: CopyObjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: CopyObjectResponse)
{-# DEPRECATED corsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsSSEKMSEncryptionContext :: Lens.Lens' CopyObjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
corsSSEKMSEncryptionContext = Lens.lens (sSEKMSEncryptionContext :: CopyObjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSEncryptionContext = a} :: CopyObjectResponse)
{-# DEPRECATED corsSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsServerSideEncryption :: Lens.Lens' CopyObjectResponse (Lude.Maybe ServerSideEncryption)
corsServerSideEncryption = Lens.lens (serverSideEncryption :: CopyObjectResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: CopyObjectResponse)
{-# DEPRECATED corsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | Container for all response elements.
--
-- /Note:/ Consider using 'copyObjectResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsCopyObjectResult :: Lens.Lens' CopyObjectResponse (Lude.Maybe CopyObjectResult)
corsCopyObjectResult = Lens.lens (copyObjectResult :: CopyObjectResponse -> Lude.Maybe CopyObjectResult) (\s a -> s {copyObjectResult = a} :: CopyObjectResponse)
{-# DEPRECATED corsCopyObjectResult "Use generic-lens or generic-optics with 'copyObjectResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsResponseStatus :: Lens.Lens' CopyObjectResponse Lude.Int
corsResponseStatus = Lens.lens (responseStatus :: CopyObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyObjectResponse)
{-# DEPRECATED corsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

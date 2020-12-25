{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an object to a bucket. You must have WRITE permissions on a bucket to add an object to it.
--
-- Amazon S3 never adds partial objects; if you receive a success response, Amazon S3 added the entire object to the bucket.
-- Amazon S3 is a distributed system. If it receives multiple write requests for the same object simultaneously, it overwrites all but the last object written. Amazon S3 does not provide object locking; if you need this, make sure to build it into your application layer or use versioning instead.
-- To ensure that data is not corrupted traversing the network, use the @Content-MD5@ header. When you use this header, Amazon S3 checks the object against the provided MD5 value and, if they do not match, returns an error. Additionally, you can calculate the MD5 while putting an object to Amazon S3 and compare the returned ETag to the calculated MD5 value.
-- __Server-side Encryption__
-- You can optionally request server-side encryption. With server-side encryption, Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts the data when you access it. You have the option to provide your own encryption key or use AWS managed encryption keys. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption> .
-- __Access Control List (ACL)-Specific Request Headers__
-- You can use headers to grant ACL- based permissions. By default, all objects are private. Only the owner has full access control. When adding a new object, you can grant permissions to individual AWS accounts or to predefined groups defined by Amazon S3. These permissions are then added to the ACL on the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-using-rest-api.html Managing ACLs Using the REST API> .
-- __Storage Class Options__
-- By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
-- __Versioning__
-- If you enable versioning for a bucket, Amazon S3 automatically generates a unique version ID for the object being stored. Amazon S3 returns this ID in the response. When you enable versioning for a bucket, if Amazon S3 receives multiple write requests for the same object simultaneously, it stores all of the objects.
-- For more information about versioning, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/AddingObjectstoVersioningEnabledBuckets.html Adding Objects to Versioning Enabled Buckets> . For information about returning the versioning state of a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning> .
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.PutObject
  ( -- * Creating a request
    PutObject (..),
    mkPutObject,

    -- ** Request lenses
    poBucket,
    poKey,
    poACL,
    poBody,
    poCacheControl,
    poContentDisposition,
    poContentEncoding,
    poContentLanguage,
    poContentLength,
    poContentMD5,
    poContentType,
    poExpectedBucketOwner,
    poExpires,
    poGrantFullControl,
    poGrantRead,
    poGrantReadACP,
    poGrantWriteACP,
    poMetadata,
    poObjectLockLegalHoldStatus,
    poObjectLockMode,
    poObjectLockRetainUntilDate,
    poRequestPayer,
    poSSECustomerAlgorithm,
    poSSECustomerKey,
    poSSECustomerKeyMD5,
    poSSEKMSEncryptionContext,
    poSSEKMSKeyId,
    poServerSideEncryption,
    poStorageClass,
    poTagging,
    poWebsiteRedirectLocation,

    -- * Destructuring the response
    PutObjectResponse (..),
    mkPutObjectResponse,

    -- ** Response lenses
    porrsETag,
    porrsExpiration,
    porrsRequestCharged,
    porrsSSECustomerAlgorithm,
    porrsSSECustomerKeyMD5,
    porrsSSEKMSEncryptionContext,
    porrsSSEKMSKeyId,
    porrsServerSideEncryption,
    porrsVersionId,
    porrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutObject' smart constructor.
data PutObject = PutObject'
  { -- | The bucket name to which the PUT operation was initiated.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | Object key for which the PUT operation was initiated.
    key :: Types.ObjectKey,
    -- | The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
    --
    -- This action is not supported by Amazon S3 on Outposts.
    acl :: Core.Maybe Types.ObjectCannedACL,
    -- | Object data.
    body :: Core.RqBody,
    -- | Can be used to specify caching behavior along the request/reply chain. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
    cacheControl :: Core.Maybe Types.CacheControl,
    -- | Specifies presentational information for the object. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1 http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1> .
    contentDisposition :: Core.Maybe Types.ContentDisposition,
    -- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11> .
    contentEncoding :: Core.Maybe Types.ContentEncoding,
    -- | The language the content is in.
    contentLanguage :: Core.Maybe Types.ContentLanguage,
    -- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13> .
    contentLength :: Core.Maybe Core.Integer,
    -- | The base64-encoded 128-bit MD5 digest of the message (without the headers) according to RFC 1864. This header can be used as a message integrity check to verify that the data is the same data that was originally sent. Although it is optional, we recommend using the Content-MD5 mechanism as an end-to-end integrity check. For more information about REST request authentication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> .
    contentMD5 :: Core.Maybe Types.ContentMD5,
    -- | A standard MIME type describing the format of the contents. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17> .
    contentType :: Core.Maybe Types.ContentType,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId,
    -- | The date and time at which the object is no longer cacheable. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21> .
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
    -- | Specifies whether a legal hold will be applied to this object. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
    objectLockLegalHoldStatus :: Core.Maybe Types.ObjectLockLegalHoldStatus,
    -- | The Object Lock mode that you want to apply to this object.
    objectLockMode :: Core.Maybe Types.ObjectLockMode,
    -- | The date and time when you want this object's Object Lock to expire.
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
    -- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetrical customer managed customer master key (CMK) that was used for the object.
    --
    -- If the value of @x-amz-server-side-encryption@ is @aws:kms@ , this header specifies the ID of the symmetric customer managed AWS KMS CMK that will be used for the object. If you specify @x-amz-server-side-encryption:aws:kms@ , but do not provide@x-amz-server-side-encryption-aws-kms-key-id@ , Amazon S3 uses the AWS managed CMK in AWS to protect the data.
    sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId,
    -- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe Types.ServerSideEncryption,
    -- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
    storageClass :: Core.Maybe Types.StorageClass,
    -- | The tag-set for the object. The tag-set must be encoded as URL Query parameters. (For example, "Key1=Value1")
    tagging :: Core.Maybe Types.TaggingHeader,
    -- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata. For information about object metadata, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata> .
    --
    -- In the following example, the request header sets the redirect to an object (anotherPage.html) in the same bucket:
    -- @x-amz-website-redirect-location: /anotherPage.html@
    -- In the following example, the request header sets the object redirect to another website:
    -- @x-amz-website-redirect-location: http://www.example.com/@
    -- For more information about website hosting in Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects> .
    websiteRedirectLocation :: Core.Maybe Types.WebsiteRedirectLocation
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'PutObject' value with any optional fields omitted.
mkPutObject ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.ObjectKey ->
  -- | 'body'
  Core.RqBody ->
  PutObject
mkPutObject bucket key body =
  PutObject'
    { bucket,
      key,
      acl = Core.Nothing,
      body,
      cacheControl = Core.Nothing,
      contentDisposition = Core.Nothing,
      contentEncoding = Core.Nothing,
      contentLanguage = Core.Nothing,
      contentLength = Core.Nothing,
      contentMD5 = Core.Nothing,
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

-- | The bucket name to which the PUT operation was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poBucket :: Lens.Lens' PutObject Types.BucketName
poBucket = Lens.field @"bucket"
{-# DEPRECATED poBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Object key for which the PUT operation was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poKey :: Lens.Lens' PutObject Types.ObjectKey
poKey = Lens.field @"key"
{-# DEPRECATED poKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poACL :: Lens.Lens' PutObject (Core.Maybe Types.ObjectCannedACL)
poACL = Lens.field @"acl"
{-# DEPRECATED poACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | Object data.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poBody :: Lens.Lens' PutObject Core.RqBody
poBody = Lens.field @"body"
{-# DEPRECATED poBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | Can be used to specify caching behavior along the request/reply chain. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poCacheControl :: Lens.Lens' PutObject (Core.Maybe Types.CacheControl)
poCacheControl = Lens.field @"cacheControl"
{-# DEPRECATED poCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | Specifies presentational information for the object. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1 http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1> .
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentDisposition :: Lens.Lens' PutObject (Core.Maybe Types.ContentDisposition)
poContentDisposition = Lens.field @"contentDisposition"
{-# DEPRECATED poContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11> .
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentEncoding :: Lens.Lens' PutObject (Core.Maybe Types.ContentEncoding)
poContentEncoding = Lens.field @"contentEncoding"
{-# DEPRECATED poContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentLanguage :: Lens.Lens' PutObject (Core.Maybe Types.ContentLanguage)
poContentLanguage = Lens.field @"contentLanguage"
{-# DEPRECATED poContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13> .
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentLength :: Lens.Lens' PutObject (Core.Maybe Core.Integer)
poContentLength = Lens.field @"contentLength"
{-# DEPRECATED poContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the message (without the headers) according to RFC 1864. This header can be used as a message integrity check to verify that the data is the same data that was originally sent. Although it is optional, we recommend using the Content-MD5 mechanism as an end-to-end integrity check. For more information about REST request authentication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> .
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentMD5 :: Lens.Lens' PutObject (Core.Maybe Types.ContentMD5)
poContentMD5 = Lens.field @"contentMD5"
{-# DEPRECATED poContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | A standard MIME type describing the format of the contents. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17> .
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentType :: Lens.Lens' PutObject (Core.Maybe Types.ContentType)
poContentType = Lens.field @"contentType"
{-# DEPRECATED poContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poExpectedBucketOwner :: Lens.Lens' PutObject (Core.Maybe Types.AccountId)
poExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED poExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The date and time at which the object is no longer cacheable. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21> .
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poExpires :: Lens.Lens' PutObject (Core.Maybe Core.UTCTime)
poExpires = Lens.field @"expires"
{-# DEPRECATED poExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantFullControl :: Lens.Lens' PutObject (Core.Maybe Types.GrantFullControl)
poGrantFullControl = Lens.field @"grantFullControl"
{-# DEPRECATED poGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantRead :: Lens.Lens' PutObject (Core.Maybe Types.GrantRead)
poGrantRead = Lens.field @"grantRead"
{-# DEPRECATED poGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantReadACP :: Lens.Lens' PutObject (Core.Maybe Types.GrantReadACP)
poGrantReadACP = Lens.field @"grantReadACP"
{-# DEPRECATED poGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantWriteACP :: Lens.Lens' PutObject (Core.Maybe Types.GrantWriteACP)
poGrantWriteACP = Lens.field @"grantWriteACP"
{-# DEPRECATED poGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poMetadata :: Lens.Lens' PutObject (Core.HashMap Types.MetadataKey Types.MetadataValue)
poMetadata = Lens.field @"metadata"
{-# DEPRECATED poMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Specifies whether a legal hold will be applied to this object. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poObjectLockLegalHoldStatus :: Lens.Lens' PutObject (Core.Maybe Types.ObjectLockLegalHoldStatus)
poObjectLockLegalHoldStatus = Lens.field @"objectLockLegalHoldStatus"
{-# DEPRECATED poObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | The Object Lock mode that you want to apply to this object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poObjectLockMode :: Lens.Lens' PutObject (Core.Maybe Types.ObjectLockMode)
poObjectLockMode = Lens.field @"objectLockMode"
{-# DEPRECATED poObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time when you want this object's Object Lock to expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poObjectLockRetainUntilDate :: Lens.Lens' PutObject (Core.Maybe Core.UTCTime)
poObjectLockRetainUntilDate = Lens.field @"objectLockRetainUntilDate"
{-# DEPRECATED poObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poRequestPayer :: Lens.Lens' PutObject (Core.Maybe Types.RequestPayer)
poRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED poRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSECustomerAlgorithm :: Lens.Lens' PutObject (Core.Maybe Types.SSECustomerAlgorithm)
poSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED poSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSECustomerKey :: Lens.Lens' PutObject (Core.Maybe Types.SSECustomerKey)
poSSECustomerKey = Lens.field @"sSECustomerKey"
{-# DEPRECATED poSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSECustomerKeyMD5 :: Lens.Lens' PutObject (Core.Maybe Types.SSECustomerKeyMD5)
poSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED poSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSEKMSEncryptionContext :: Lens.Lens' PutObject (Core.Maybe Types.SSEKMSEncryptionContext)
poSSEKMSEncryptionContext = Lens.field @"sSEKMSEncryptionContext"
{-# DEPRECATED poSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetrical customer managed customer master key (CMK) that was used for the object.
--
-- If the value of @x-amz-server-side-encryption@ is @aws:kms@ , this header specifies the ID of the symmetric customer managed AWS KMS CMK that will be used for the object. If you specify @x-amz-server-side-encryption:aws:kms@ , but do not provide@x-amz-server-side-encryption-aws-kms-key-id@ , Amazon S3 uses the AWS managed CMK in AWS to protect the data.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSEKMSKeyId :: Lens.Lens' PutObject (Core.Maybe Types.SSEKMSKeyId)
poSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED poSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poServerSideEncryption :: Lens.Lens' PutObject (Core.Maybe Types.ServerSideEncryption)
poServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED poServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poStorageClass :: Lens.Lens' PutObject (Core.Maybe Types.StorageClass)
poStorageClass = Lens.field @"storageClass"
{-# DEPRECATED poStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters. (For example, "Key1=Value1")
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poTagging :: Lens.Lens' PutObject (Core.Maybe Types.TaggingHeader)
poTagging = Lens.field @"tagging"
{-# DEPRECATED poTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata. For information about object metadata, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata> .
--
-- In the following example, the request header sets the redirect to an object (anotherPage.html) in the same bucket:
-- @x-amz-website-redirect-location: /anotherPage.html@
-- In the following example, the request header sets the object redirect to another website:
-- @x-amz-website-redirect-location: http://www.example.com/@
-- For more information about website hosting in Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects> .
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poWebsiteRedirectLocation :: Lens.Lens' PutObject (Core.Maybe Types.WebsiteRedirectLocation)
poWebsiteRedirectLocation = Lens.field @"websiteRedirectLocation"
{-# DEPRECATED poWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

instance Core.AWSRequest PutObject where
  type Rs PutObject = PutObjectResponse
  request x@Core.Request {..} =
    Request.expectHeader Core.$
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
            Core.toHeaders "x-amz-acl" acl
              Core.<> (Core.toHeaders "Cache-Control" cacheControl)
              Core.<> (Core.toHeaders "Content-Disposition" contentDisposition)
              Core.<> (Core.toHeaders "Content-Encoding" contentEncoding)
              Core.<> (Core.toHeaders "Content-Language" contentLanguage)
              Core.<> (Core.toHeaders "Content-Length" contentLength)
              Core.<> (Core.toHeaders "Content-MD5" contentMD5)
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
          Core._rqBody = Core.toBody body
        }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
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

-- | /See:/ 'mkPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { -- | Entity tag for the uploaded object.
    eTag :: Core.Maybe Types.ETag,
    -- | If the expiration is configured for the object (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration> ), the response includes this header. It includes the expiry-date and rule-id key-value pairs that provide information about object expiration. The value of the rule-id is URL encoded.
    expiration :: Core.Maybe Types.Expiration,
    requestCharged :: Core.Maybe Types.RequestCharged,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
    sSEKMSEncryptionContext :: Core.Maybe Types.SSEKMSEncryptionContext,
    -- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
    sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId,
    -- | If you specified server-side encryption either with an AWS KMS customer master key (CMK) or Amazon S3-managed encryption key in your PUT request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
    serverSideEncryption :: Core.Maybe Types.ServerSideEncryption,
    -- | Version of the object.
    versionId :: Core.Maybe Types.ObjectVersionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectResponse' value with any optional fields omitted.
mkPutObjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutObjectResponse
mkPutObjectResponse responseStatus =
  PutObjectResponse'
    { eTag = Core.Nothing,
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

-- | Entity tag for the uploaded object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsETag :: Lens.Lens' PutObjectResponse (Core.Maybe Types.ETag)
porrsETag = Lens.field @"eTag"
{-# DEPRECATED porrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | If the expiration is configured for the object (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration> ), the response includes this header. It includes the expiry-date and rule-id key-value pairs that provide information about object expiration. The value of the rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsExpiration :: Lens.Lens' PutObjectResponse (Core.Maybe Types.Expiration)
porrsExpiration = Lens.field @"expiration"
{-# DEPRECATED porrsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsRequestCharged :: Lens.Lens' PutObjectResponse (Core.Maybe Types.RequestCharged)
porrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED porrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsSSECustomerAlgorithm :: Lens.Lens' PutObjectResponse (Core.Maybe Types.SSECustomerAlgorithm)
porrsSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED porrsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsSSECustomerKeyMD5 :: Lens.Lens' PutObjectResponse (Core.Maybe Types.SSECustomerKeyMD5)
porrsSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED porrsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsSSEKMSEncryptionContext :: Lens.Lens' PutObjectResponse (Core.Maybe Types.SSEKMSEncryptionContext)
porrsSSEKMSEncryptionContext = Lens.field @"sSEKMSEncryptionContext"
{-# DEPRECATED porrsSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsSSEKMSKeyId :: Lens.Lens' PutObjectResponse (Core.Maybe Types.SSEKMSKeyId)
porrsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED porrsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | If you specified server-side encryption either with an AWS KMS customer master key (CMK) or Amazon S3-managed encryption key in your PUT request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsServerSideEncryption :: Lens.Lens' PutObjectResponse (Core.Maybe Types.ServerSideEncryption)
porrsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED porrsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | Version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsVersionId :: Lens.Lens' PutObjectResponse (Core.Maybe Types.ObjectVersionId)
porrsVersionId = Lens.field @"versionId"
{-# DEPRECATED porrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsResponseStatus :: Lens.Lens' PutObjectResponse Core.Int
porrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED porrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

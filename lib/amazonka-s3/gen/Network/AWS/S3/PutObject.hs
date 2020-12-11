{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    poContentLength,
    poObjectLockMode,
    poExpires,
    poGrantReadACP,
    poSSECustomerAlgorithm,
    poSSECustomerKey,
    poRequestPayer,
    poGrantWriteACP,
    poWebsiteRedirectLocation,
    poGrantRead,
    poStorageClass,
    poSSECustomerKeyMD5,
    poSSEKMSKeyId,
    poGrantFullControl,
    poContentEncoding,
    poTagging,
    poContentMD5,
    poObjectLockRetainUntilDate,
    poMetadata,
    poSSEKMSEncryptionContext,
    poCacheControl,
    poContentLanguage,
    poObjectLockLegalHoldStatus,
    poACL,
    poContentDisposition,
    poExpectedBucketOwner,
    poServerSideEncryption,
    poContentType,
    poBucket,
    poKey,
    poBody,

    -- * Destructuring the response
    PutObjectResponse (..),
    mkPutObjectResponse,

    -- ** Response lenses
    porsRequestCharged,
    porsETag,
    porsVersionId,
    porsExpiration,
    porsSSECustomerAlgorithm,
    porsSSECustomerKeyMD5,
    porsSSEKMSKeyId,
    porsSSEKMSEncryptionContext,
    porsServerSideEncryption,
    porsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutObject' smart constructor.
data PutObject = PutObject'
  { contentLength ::
      Lude.Maybe Lude.Integer,
    objectLockMode :: Lude.Maybe ObjectLockMode,
    expires :: Lude.Maybe Lude.ISO8601,
    grantReadACP :: Lude.Maybe Lude.Text,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    sSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestPayer :: Lude.Maybe RequestPayer,
    grantWriteACP :: Lude.Maybe Lude.Text,
    websiteRedirectLocation :: Lude.Maybe Lude.Text,
    grantRead :: Lude.Maybe Lude.Text,
    storageClass :: Lude.Maybe StorageClass,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    grantFullControl :: Lude.Maybe Lude.Text,
    contentEncoding :: Lude.Maybe Lude.Text,
    tagging :: Lude.Maybe Lude.Text,
    contentMD5 :: Lude.Maybe Lude.Text,
    objectLockRetainUntilDate :: Lude.Maybe Lude.ISO8601,
    metadata :: Lude.HashMap Lude.Text (Lude.Text),
    sSEKMSEncryptionContext :: Lude.Maybe (Lude.Sensitive Lude.Text),
    cacheControl :: Lude.Maybe Lude.Text,
    contentLanguage :: Lude.Maybe Lude.Text,
    objectLockLegalHoldStatus :: Lude.Maybe ObjectLockLegalHoldStatus,
    acl :: Lude.Maybe ObjectCannedACL,
    contentDisposition :: Lude.Maybe Lude.Text,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    contentType :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    key :: ObjectKey,
    body :: Lude.RqBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'PutObject' with the minimum fields required to make a request.
--
-- * 'acl' - The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
-- This action is not supported by Amazon S3 on Outposts.
-- * 'body' - Object data.
-- * 'bucket' - The bucket name to which the PUT operation was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'cacheControl' - Can be used to specify caching behavior along the request/reply chain. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
-- * 'contentDisposition' - Specifies presentational information for the object. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1 http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1> .
-- * 'contentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11> .
-- * 'contentLanguage' - The language the content is in.
-- * 'contentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13> .
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the message (without the headers) according to RFC 1864. This header can be used as a message integrity check to verify that the data is the same data that was originally sent. Although it is optional, we recommend using the Content-MD5 mechanism as an end-to-end integrity check. For more information about REST request authentication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> .
-- * 'contentType' - A standard MIME type describing the format of the contents. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17> .
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'expires' - The date and time at which the object is no longer cacheable. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21> .
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
-- * 'key' - Object key for which the PUT operation was initiated.
-- * 'metadata' - A map of metadata to store with the object in S3.
-- * 'objectLockLegalHoldStatus' - Specifies whether a legal hold will be applied to this object. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
-- * 'objectLockMode' - The Object Lock mode that you want to apply to this object.
-- * 'objectLockRetainUntilDate' - The date and time when you want this object's Object Lock to expire.
-- * 'requestPayer' - Undocumented field.
-- * 'sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
-- * 'sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
-- * 'sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'sSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
-- * 'sSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetrical customer managed customer master key (CMK) that was used for the object.
--
-- If the value of @x-amz-server-side-encryption@ is @aws:kms@ , this header specifies the ID of the symmetric customer managed AWS KMS CMK that will be used for the object. If you specify @x-amz-server-side-encryption:aws:kms@ , but do not provide@x-amz-server-side-encryption-aws-kms-key-id@ , Amazon S3 uses the AWS managed CMK in AWS to protect the data.
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
-- * 'storageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
-- * 'tagging' - The tag-set for the object. The tag-set must be encoded as URL Query parameters. (For example, "Key1=Value1")
-- * 'websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata. For information about object metadata, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata> .
--
-- In the following example, the request header sets the redirect to an object (anotherPage.html) in the same bucket:
-- @x-amz-website-redirect-location: /anotherPage.html@
-- In the following example, the request header sets the object redirect to another website:
-- @x-amz-website-redirect-location: http://www.example.com/@
-- For more information about website hosting in Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects> .
mkPutObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'body'
  Lude.RqBody ->
  PutObject
mkPutObject pBucket_ pKey_ pBody_ =
  PutObject'
    { contentLength = Lude.Nothing,
      objectLockMode = Lude.Nothing,
      expires = Lude.Nothing,
      grantReadACP = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKey = Lude.Nothing,
      requestPayer = Lude.Nothing,
      grantWriteACP = Lude.Nothing,
      websiteRedirectLocation = Lude.Nothing,
      grantRead = Lude.Nothing,
      storageClass = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      grantFullControl = Lude.Nothing,
      contentEncoding = Lude.Nothing,
      tagging = Lude.Nothing,
      contentMD5 = Lude.Nothing,
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
      contentType = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      body = pBody_
    }

-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13> .
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentLength :: Lens.Lens' PutObject (Lude.Maybe Lude.Integer)
poContentLength = Lens.lens (contentLength :: PutObject -> Lude.Maybe Lude.Integer) (\s a -> s {contentLength = a} :: PutObject)
{-# DEPRECATED poContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The Object Lock mode that you want to apply to this object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poObjectLockMode :: Lens.Lens' PutObject (Lude.Maybe ObjectLockMode)
poObjectLockMode = Lens.lens (objectLockMode :: PutObject -> Lude.Maybe ObjectLockMode) (\s a -> s {objectLockMode = a} :: PutObject)
{-# DEPRECATED poObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time at which the object is no longer cacheable. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21> .
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poExpires :: Lens.Lens' PutObject (Lude.Maybe Lude.ISO8601)
poExpires = Lens.lens (expires :: PutObject -> Lude.Maybe Lude.ISO8601) (\s a -> s {expires = a} :: PutObject)
{-# DEPRECATED poExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Allows grantee to read the object ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantReadACP :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poGrantReadACP = Lens.lens (grantReadACP :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {grantReadACP = a} :: PutObject)
{-# DEPRECATED poGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSECustomerAlgorithm :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: PutObject)
{-# DEPRECATED poSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSECustomerKey :: Lens.Lens' PutObject (Lude.Maybe (Lude.Sensitive Lude.Text))
poSSECustomerKey = Lens.lens (sSECustomerKey :: PutObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: PutObject)
{-# DEPRECATED poSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poRequestPayer :: Lens.Lens' PutObject (Lude.Maybe RequestPayer)
poRequestPayer = Lens.lens (requestPayer :: PutObject -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: PutObject)
{-# DEPRECATED poRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Allows grantee to write the ACL for the applicable object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantWriteACP :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poGrantWriteACP = Lens.lens (grantWriteACP :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {grantWriteACP = a} :: PutObject)
{-# DEPRECATED poGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata. For information about object metadata, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata> .
--
-- In the following example, the request header sets the redirect to an object (anotherPage.html) in the same bucket:
-- @x-amz-website-redirect-location: /anotherPage.html@
-- In the following example, the request header sets the object redirect to another website:
-- @x-amz-website-redirect-location: http://www.example.com/@
-- For more information about website hosting in Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects> .
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poWebsiteRedirectLocation :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poWebsiteRedirectLocation = Lens.lens (websiteRedirectLocation :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {websiteRedirectLocation = a} :: PutObject)
{-# DEPRECATED poWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

-- | Allows grantee to read the object data and its metadata.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantRead :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poGrantRead = Lens.lens (grantRead :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {grantRead = a} :: PutObject)
{-# DEPRECATED poGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poStorageClass :: Lens.Lens' PutObject (Lude.Maybe StorageClass)
poStorageClass = Lens.lens (storageClass :: PutObject -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: PutObject)
{-# DEPRECATED poStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSECustomerKeyMD5 :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: PutObject)
{-# DEPRECATED poSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetrical customer managed customer master key (CMK) that was used for the object.
--
-- If the value of @x-amz-server-side-encryption@ is @aws:kms@ , this header specifies the ID of the symmetric customer managed AWS KMS CMK that will be used for the object. If you specify @x-amz-server-side-encryption:aws:kms@ , but do not provide@x-amz-server-side-encryption-aws-kms-key-id@ , Amazon S3 uses the AWS managed CMK in AWS to protect the data.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSEKMSKeyId :: Lens.Lens' PutObject (Lude.Maybe (Lude.Sensitive Lude.Text))
poSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: PutObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: PutObject)
{-# DEPRECATED poSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poGrantFullControl :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poGrantFullControl = Lens.lens (grantFullControl :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {grantFullControl = a} :: PutObject)
{-# DEPRECATED poGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11> .
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentEncoding :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poContentEncoding = Lens.lens (contentEncoding :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {contentEncoding = a} :: PutObject)
{-# DEPRECATED poContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters. (For example, "Key1=Value1")
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poTagging :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poTagging = Lens.lens (tagging :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {tagging = a} :: PutObject)
{-# DEPRECATED poTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the message (without the headers) according to RFC 1864. This header can be used as a message integrity check to verify that the data is the same data that was originally sent. Although it is optional, we recommend using the Content-MD5 mechanism as an end-to-end integrity check. For more information about REST request authentication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> .
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentMD5 :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poContentMD5 = Lens.lens (contentMD5 :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutObject)
{-# DEPRECATED poContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The date and time when you want this object's Object Lock to expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poObjectLockRetainUntilDate :: Lens.Lens' PutObject (Lude.Maybe Lude.ISO8601)
poObjectLockRetainUntilDate = Lens.lens (objectLockRetainUntilDate :: PutObject -> Lude.Maybe Lude.ISO8601) (\s a -> s {objectLockRetainUntilDate = a} :: PutObject)
{-# DEPRECATED poObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poMetadata :: Lens.Lens' PutObject (Lude.HashMap Lude.Text (Lude.Text))
poMetadata = Lens.lens (metadata :: PutObject -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {metadata = a} :: PutObject)
{-# DEPRECATED poMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poSSEKMSEncryptionContext :: Lens.Lens' PutObject (Lude.Maybe (Lude.Sensitive Lude.Text))
poSSEKMSEncryptionContext = Lens.lens (sSEKMSEncryptionContext :: PutObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSEncryptionContext = a} :: PutObject)
{-# DEPRECATED poSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | Can be used to specify caching behavior along the request/reply chain. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poCacheControl :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poCacheControl = Lens.lens (cacheControl :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: PutObject)
{-# DEPRECATED poCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentLanguage :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poContentLanguage = Lens.lens (contentLanguage :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {contentLanguage = a} :: PutObject)
{-# DEPRECATED poContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Specifies whether a legal hold will be applied to this object. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poObjectLockLegalHoldStatus :: Lens.Lens' PutObject (Lude.Maybe ObjectLockLegalHoldStatus)
poObjectLockLegalHoldStatus = Lens.lens (objectLockLegalHoldStatus :: PutObject -> Lude.Maybe ObjectLockLegalHoldStatus) (\s a -> s {objectLockLegalHoldStatus = a} :: PutObject)
{-# DEPRECATED poObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poACL :: Lens.Lens' PutObject (Lude.Maybe ObjectCannedACL)
poACL = Lens.lens (acl :: PutObject -> Lude.Maybe ObjectCannedACL) (\s a -> s {acl = a} :: PutObject)
{-# DEPRECATED poACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | Specifies presentational information for the object. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1 http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1> .
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentDisposition :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poContentDisposition = Lens.lens (contentDisposition :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {contentDisposition = a} :: PutObject)
{-# DEPRECATED poContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poExpectedBucketOwner :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutObject)
{-# DEPRECATED poExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poServerSideEncryption :: Lens.Lens' PutObject (Lude.Maybe ServerSideEncryption)
poServerSideEncryption = Lens.lens (serverSideEncryption :: PutObject -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: PutObject)
{-# DEPRECATED poServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | A standard MIME type describing the format of the contents. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17> .
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentType :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poContentType = Lens.lens (contentType :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: PutObject)
{-# DEPRECATED poContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The bucket name to which the PUT operation was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poBucket :: Lens.Lens' PutObject BucketName
poBucket = Lens.lens (bucket :: PutObject -> BucketName) (\s a -> s {bucket = a} :: PutObject)
{-# DEPRECATED poBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Object key for which the PUT operation was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poKey :: Lens.Lens' PutObject ObjectKey
poKey = Lens.lens (key :: PutObject -> ObjectKey) (\s a -> s {key = a} :: PutObject)
{-# DEPRECATED poKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Object data.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poBody :: Lens.Lens' PutObject Lude.RqBody
poBody = Lens.lens (body :: PutObject -> Lude.RqBody) (\s a -> s {body = a} :: PutObject)
{-# DEPRECATED poBody "Use generic-lens or generic-optics with 'body' instead." #-}

instance Lude.AWSRequest PutObject where
  type Rs PutObject = PutObjectResponse
  request = expectHeader Lude.. Req.putBody s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutObjectResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "x-amz-version-id")
            Lude.<*> (h Lude..#? "x-amz-expiration")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-algorithm")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-key-MD5")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-aws-kms-key-id")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-context")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody PutObject where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders PutObject where
  toHeaders PutObject' {..} =
    Lude.mconcat
      [ "Content-Length" Lude.=# contentLength,
        "x-amz-object-lock-mode" Lude.=# objectLockMode,
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
        "Content-MD5" Lude.=# contentMD5,
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

instance Lude.ToPath PutObject where
  toPath PutObject' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery PutObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    eTag :: Lude.Maybe ETag,
    versionId :: Lude.Maybe ObjectVersionId,
    expiration :: Lude.Maybe Lude.Text,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sSEKMSEncryptionContext ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - Entity tag for the uploaded object.
-- * 'expiration' - If the expiration is configured for the object (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration> ), the response includes this header. It includes the expiry-date and rule-id key-value pairs that provide information about object expiration. The value of the rule-id is URL encoded.
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
-- * 'sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
-- * 'sSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
-- * 'sSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
-- * 'serverSideEncryption' - If you specified server-side encryption either with an AWS KMS customer master key (CMK) or Amazon S3-managed encryption key in your PUT request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
-- * 'versionId' - Version of the object.
mkPutObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutObjectResponse
mkPutObjectResponse pResponseStatus_ =
  PutObjectResponse'
    { requestCharged = Lude.Nothing,
      eTag = Lude.Nothing,
      versionId = Lude.Nothing,
      expiration = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      sSEKMSEncryptionContext = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsRequestCharged :: Lens.Lens' PutObjectResponse (Lude.Maybe RequestCharged)
porsRequestCharged = Lens.lens (requestCharged :: PutObjectResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: PutObjectResponse)
{-# DEPRECATED porsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Entity tag for the uploaded object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsETag :: Lens.Lens' PutObjectResponse (Lude.Maybe ETag)
porsETag = Lens.lens (eTag :: PutObjectResponse -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: PutObjectResponse)
{-# DEPRECATED porsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsVersionId :: Lens.Lens' PutObjectResponse (Lude.Maybe ObjectVersionId)
porsVersionId = Lens.lens (versionId :: PutObjectResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: PutObjectResponse)
{-# DEPRECATED porsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | If the expiration is configured for the object (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration> ), the response includes this header. It includes the expiry-date and rule-id key-value pairs that provide information about object expiration. The value of the rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsExpiration :: Lens.Lens' PutObjectResponse (Lude.Maybe Lude.Text)
porsExpiration = Lens.lens (expiration :: PutObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {expiration = a} :: PutObjectResponse)
{-# DEPRECATED porsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsSSECustomerAlgorithm :: Lens.Lens' PutObjectResponse (Lude.Maybe Lude.Text)
porsSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: PutObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: PutObjectResponse)
{-# DEPRECATED porsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsSSECustomerKeyMD5 :: Lens.Lens' PutObjectResponse (Lude.Maybe Lude.Text)
porsSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: PutObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: PutObjectResponse)
{-# DEPRECATED porsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsSSEKMSKeyId :: Lens.Lens' PutObjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
porsSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: PutObjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: PutObjectResponse)
{-# DEPRECATED porsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- /Note:/ Consider using 'sSEKMSEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsSSEKMSEncryptionContext :: Lens.Lens' PutObjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
porsSSEKMSEncryptionContext = Lens.lens (sSEKMSEncryptionContext :: PutObjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSEncryptionContext = a} :: PutObjectResponse)
{-# DEPRECATED porsSSEKMSEncryptionContext "Use generic-lens or generic-optics with 'sSEKMSEncryptionContext' instead." #-}

-- | If you specified server-side encryption either with an AWS KMS customer master key (CMK) or Amazon S3-managed encryption key in your PUT request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsServerSideEncryption :: Lens.Lens' PutObjectResponse (Lude.Maybe ServerSideEncryption)
porsServerSideEncryption = Lens.lens (serverSideEncryption :: PutObjectResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: PutObjectResponse)
{-# DEPRECATED porsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsResponseStatus :: Lens.Lens' PutObjectResponse Lude.Int
porsResponseStatus = Lens.lens (responseStatus :: PutObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutObjectResponse)
{-# DEPRECATED porsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

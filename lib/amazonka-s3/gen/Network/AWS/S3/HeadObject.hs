{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.HeadObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The HEAD operation retrieves metadata from an object without returning the object itself. This operation is useful if you're only interested in an object's metadata. To use HEAD, you must have READ access to the object.
--
-- A @HEAD@ request has the same options as a @GET@ operation on an object. The response is identical to the @GET@ response except that there is no response body.
-- If you encrypt an object by using server-side encryption with customer-provided encryption keys (SSE-C) when you store the object in Amazon S3, then when you retrieve the metadata from the object, you must use the following headers:
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
-- For more information about SSE-C, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys)> .
-- Request headers are limited to 8 KB in size. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTCommonRequestHeaders.html Common Request Headers> .
-- Consider the following when using request headers:
--
--     * Consideration 1 – If both of the @If-Match@ and @If-Unmodified-Since@ headers are present in the request as follows:
--
--     * @If-Match@ condition evaluates to @true@ , and;
--
--
--     * @If-Unmodified-Since@ condition evaluates to @false@ ;
--
--
-- Then Amazon S3 returns @200 OK@ and the data requested.
--
--
--     * Consideration 2 – If both of the @If-None-Match@ and @If-Modified-Since@ headers are present in the request as follows:
--
--     * @If-None-Match@ condition evaluates to @false@ , and;
--
--
--     * @If-Modified-Since@ condition evaluates to @true@ ;
--
--
-- Then Amazon S3 returns the @304 Not Modified@ response code.
--
--
-- For more information about conditional requests, see <https://tools.ietf.org/html/rfc7232 RFC 7232> .
-- __Permissions__
-- You need the @s3:GetObject@ permission for this operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> . If the object you request does not exist, the error Amazon S3 returns depends on whether you also have the s3:ListBucket permission.
--
--     * If you have the @s3:ListBucket@ permission on the bucket, Amazon S3 returns an HTTP status code 404 ("no such key") error.
--
--
--     * If you don’t have the @s3:ListBucket@ permission, Amazon S3 returns an HTTP status code 403 ("access denied") error.
--
--
-- The following operation is related to @HeadObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.HeadObject
  ( -- * Creating a request
    HeadObject (..),
    mkHeadObject,

    -- ** Request lenses
    hoIfMatch,
    hoVersionId,
    hoBucket,
    hoSSECustomerAlgorithm,
    hoSSECustomerKey,
    hoRequestPayer,
    hoIfModifiedSince,
    hoPartNumber,
    hoRange,
    hoKey,
    hoIfUnmodifiedSince,
    hoSSECustomerKeyMD5,
    hoIfNoneMatch,
    hoExpectedBucketOwner,

    -- * Destructuring the response
    HeadObjectResponse (..),
    mkHeadObjectResponse,

    -- ** Response lenses
    horsRequestCharged,
    horsPartsCount,
    horsETag,
    horsVersionId,
    horsContentLength,
    horsObjectLockMode,
    horsExpires,
    horsRestore,
    horsExpiration,
    horsDeleteMarker,
    horsArchiveStatus,
    horsSSECustomerAlgorithm,
    horsMissingMeta,
    horsWebsiteRedirectLocation,
    horsAcceptRanges,
    horsStorageClass,
    horsSSECustomerKeyMD5,
    horsSSEKMSKeyId,
    horsContentEncoding,
    horsObjectLockRetainUntilDate,
    horsMetadata,
    horsReplicationStatus,
    horsCacheControl,
    horsContentLanguage,
    horsLastModified,
    horsObjectLockLegalHoldStatus,
    horsContentDisposition,
    horsServerSideEncryption,
    horsContentType,
    horsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkHeadObject' smart constructor.
data HeadObject = HeadObject'
  { -- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
    ifMatch :: Lude.Maybe Lude.Text,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | The name of the bucket containing the object.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    -- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestPayer :: Lude.Maybe RequestPayer,
    -- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
    ifModifiedSince :: Lude.Maybe Lude.DateTime,
    -- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' HEAD request for the part specified. Useful querying about the size of the part and the number of parts in this object.
    partNumber :: Lude.Maybe Lude.Int,
    -- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
    range :: Lude.Maybe Lude.Text,
    -- | The object key.
    key :: ObjectKey,
    -- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
    ifUnmodifiedSince :: Lude.Maybe Lude.DateTime,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    -- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
    ifNoneMatch :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HeadObject' with the minimum fields required to make a request.
--
-- * 'ifMatch' - Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
-- * 'versionId' - VersionId used to reference a specific version of the object.
-- * 'bucket' - The name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
-- * 'sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
-- * 'requestPayer' -
-- * 'ifModifiedSince' - Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
-- * 'partNumber' - Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' HEAD request for the part specified. Useful querying about the size of the part and the number of parts in this object.
-- * 'range' - Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
-- * 'key' - The object key.
-- * 'ifUnmodifiedSince' - Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
-- * 'sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'ifNoneMatch' - Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkHeadObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  HeadObject
mkHeadObject pBucket_ pKey_ =
  HeadObject'
    { ifMatch = Lude.Nothing,
      versionId = Lude.Nothing,
      bucket = pBucket_,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKey = Lude.Nothing,
      requestPayer = Lude.Nothing,
      ifModifiedSince = Lude.Nothing,
      partNumber = Lude.Nothing,
      range = Lude.Nothing,
      key = pKey_,
      ifUnmodifiedSince = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      ifNoneMatch = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfMatch :: Lens.Lens' HeadObject (Lude.Maybe Lude.Text)
hoIfMatch = Lens.lens (ifMatch :: HeadObject -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: HeadObject)
{-# DEPRECATED hoIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoVersionId :: Lens.Lens' HeadObject (Lude.Maybe ObjectVersionId)
hoVersionId = Lens.lens (versionId :: HeadObject -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: HeadObject)
{-# DEPRECATED hoVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoBucket :: Lens.Lens' HeadObject BucketName
hoBucket = Lens.lens (bucket :: HeadObject -> BucketName) (\s a -> s {bucket = a} :: HeadObject)
{-# DEPRECATED hoBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoSSECustomerAlgorithm :: Lens.Lens' HeadObject (Lude.Maybe Lude.Text)
hoSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: HeadObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: HeadObject)
{-# DEPRECATED hoSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoSSECustomerKey :: Lens.Lens' HeadObject (Lude.Maybe (Lude.Sensitive Lude.Text))
hoSSECustomerKey = Lens.lens (sSECustomerKey :: HeadObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: HeadObject)
{-# DEPRECATED hoSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoRequestPayer :: Lens.Lens' HeadObject (Lude.Maybe RequestPayer)
hoRequestPayer = Lens.lens (requestPayer :: HeadObject -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: HeadObject)
{-# DEPRECATED hoRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfModifiedSince :: Lens.Lens' HeadObject (Lude.Maybe Lude.DateTime)
hoIfModifiedSince = Lens.lens (ifModifiedSince :: HeadObject -> Lude.Maybe Lude.DateTime) (\s a -> s {ifModifiedSince = a} :: HeadObject)
{-# DEPRECATED hoIfModifiedSince "Use generic-lens or generic-optics with 'ifModifiedSince' instead." #-}

-- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' HEAD request for the part specified. Useful querying about the size of the part and the number of parts in this object.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoPartNumber :: Lens.Lens' HeadObject (Lude.Maybe Lude.Int)
hoPartNumber = Lens.lens (partNumber :: HeadObject -> Lude.Maybe Lude.Int) (\s a -> s {partNumber = a} :: HeadObject)
{-# DEPRECATED hoPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

-- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoRange :: Lens.Lens' HeadObject (Lude.Maybe Lude.Text)
hoRange = Lens.lens (range :: HeadObject -> Lude.Maybe Lude.Text) (\s a -> s {range = a} :: HeadObject)
{-# DEPRECATED hoRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoKey :: Lens.Lens' HeadObject ObjectKey
hoKey = Lens.lens (key :: HeadObject -> ObjectKey) (\s a -> s {key = a} :: HeadObject)
{-# DEPRECATED hoKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfUnmodifiedSince :: Lens.Lens' HeadObject (Lude.Maybe Lude.DateTime)
hoIfUnmodifiedSince = Lens.lens (ifUnmodifiedSince :: HeadObject -> Lude.Maybe Lude.DateTime) (\s a -> s {ifUnmodifiedSince = a} :: HeadObject)
{-# DEPRECATED hoIfUnmodifiedSince "Use generic-lens or generic-optics with 'ifUnmodifiedSince' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoSSECustomerKeyMD5 :: Lens.Lens' HeadObject (Lude.Maybe Lude.Text)
hoSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: HeadObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: HeadObject)
{-# DEPRECATED hoSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfNoneMatch :: Lens.Lens' HeadObject (Lude.Maybe Lude.Text)
hoIfNoneMatch = Lens.lens (ifNoneMatch :: HeadObject -> Lude.Maybe Lude.Text) (\s a -> s {ifNoneMatch = a} :: HeadObject)
{-# DEPRECATED hoIfNoneMatch "Use generic-lens or generic-optics with 'ifNoneMatch' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoExpectedBucketOwner :: Lens.Lens' HeadObject (Lude.Maybe Lude.Text)
hoExpectedBucketOwner = Lens.lens (expectedBucketOwner :: HeadObject -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: HeadObject)
{-# DEPRECATED hoExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest HeadObject where
  type Rs HeadObject = HeadObjectResponse
  request = Req.head' s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          HeadObjectResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (h Lude..#? "x-amz-mp-parts-count")
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "x-amz-version-id")
            Lude.<*> (h Lude..#? "Content-Length")
            Lude.<*> (h Lude..#? "x-amz-object-lock-mode")
            Lude.<*> (h Lude..#? "Expires")
            Lude.<*> (h Lude..#? "x-amz-restore")
            Lude.<*> (h Lude..#? "x-amz-expiration")
            Lude.<*> (h Lude..#? "x-amz-delete-marker")
            Lude.<*> (h Lude..#? "x-amz-archive-status")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-algorithm")
            Lude.<*> (h Lude..#? "x-amz-missing-meta")
            Lude.<*> (h Lude..#? "x-amz-website-redirect-location")
            Lude.<*> (h Lude..#? "accept-ranges")
            Lude.<*> (h Lude..#? "x-amz-storage-class")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-key-MD5")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-aws-kms-key-id")
            Lude.<*> (h Lude..#? "Content-Encoding")
            Lude.<*> (h Lude..#? "x-amz-object-lock-retain-until-date")
            Lude.<*> (Lude.parseHeadersMap "x-amz-meta-" h)
            Lude.<*> (h Lude..#? "x-amz-replication-status")
            Lude.<*> (h Lude..#? "Cache-Control")
            Lude.<*> (h Lude..#? "Content-Language")
            Lude.<*> (h Lude..#? "Last-Modified")
            Lude.<*> (h Lude..#? "x-amz-object-lock-legal-hold")
            Lude.<*> (h Lude..#? "Content-Disposition")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders HeadObject where
  toHeaders HeadObject' {..} =
    Lude.mconcat
      [ "If-Match" Lude.=# ifMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          Lude.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" Lude.=# sSECustomerKey,
        "x-amz-request-payer" Lude.=# requestPayer,
        "If-Modified-Since" Lude.=# ifModifiedSince,
        "Range" Lude.=# range,
        "If-Unmodified-Since" Lude.=# ifUnmodifiedSince,
        "x-amz-server-side-encryption-customer-key-MD5"
          Lude.=# sSECustomerKeyMD5,
        "If-None-Match" Lude.=# ifNoneMatch,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath HeadObject where
  toPath HeadObject' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery HeadObject where
  toQuery HeadObject' {..} =
    Lude.mconcat
      ["versionId" Lude.=: versionId, "partNumber" Lude.=: partNumber]

-- | /See:/ 'mkHeadObjectResponse' smart constructor.
data HeadObjectResponse = HeadObjectResponse'
  { requestCharged :: Lude.Maybe RequestCharged,
    -- | The count of parts this object has.
    partsCount :: Lude.Maybe Lude.Int,
    -- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
    eTag :: Lude.Maybe ETag,
    -- | Version of the object.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | Size of the body in bytes.
    contentLength :: Lude.Maybe Lude.Integer,
    -- | The Object Lock mode, if any, that's in effect for this object. This header is only returned if the requester has the @s3:GetObjectRetention@ permission. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
    objectLockMode :: Lude.Maybe ObjectLockMode,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Lude.Maybe Lude.DateTime,
    -- | If the object is an archived object (an object whose storage class is GLACIER), the response includes this header if either the archive restoration is in progress (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> or an archive copy is already restored.
    --
    -- If an archive copy is already restored, the header value indicates when Amazon S3 is scheduled to delete the object copy. For example:
    -- @x-amz-restore: ongoing-request="false", expiry-date="Fri, 23 Dec 2012 00:00:00 GMT"@
    -- If the object restoration is in progress, the header returns the value @ongoing-request="true"@ .
    -- For more information about archiving objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations> .
    restore :: Lude.Maybe Lude.Text,
    -- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
    expiration :: Lude.Maybe Lude.Text,
    -- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
    deleteMarker :: Lude.Maybe Lude.Bool,
    -- | The archive state of the head object.
    archiveStatus :: Lude.Maybe ArchiveStatus,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    -- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
    missingMeta :: Lude.Maybe Lude.Int,
    -- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Lude.Maybe Lude.Text,
    -- | Indicates that a range of bytes was specified.
    acceptRanges :: Lude.Maybe Lude.Text,
    -- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> .
    storageClass :: Lude.Maybe StorageClass,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
    contentEncoding :: Lude.Maybe Lude.Text,
    -- | The date and time when the Object Lock retention period expires. This header is only returned if the requester has the @s3:GetObjectRetention@ permission.
    objectLockRetainUntilDate :: Lude.Maybe Lude.DateTime,
    -- | A map of metadata to store with the object in S3.
    metadata :: Lude.HashMap Lude.Text (Lude.Text),
    -- | Amazon S3 can return this header if your request involves a bucket that is either a source or destination in a replication rule.
    --
    -- In replication, you have a source bucket on which you configure replication and destination bucket where Amazon S3 stores object replicas. When you request an object (@GetObject@ ) or object metadata (@HeadObject@ ) from these buckets, Amazon S3 will return the @x-amz-replication-status@ header in the response as follows:
    --
    --     * If requesting an object from the source bucket — Amazon S3 will return the @x-amz-replication-status@ header if the object in your request is eligible for replication.
    -- For example, suppose that in your replication configuration, you specify object prefix @TaxDocs@ requesting Amazon S3 to replicate objects with key prefix @TaxDocs@ . Any objects you upload with this key name prefix, for example @TaxDocs/document1.pdf@ , are eligible for replication. For any object request with this key name prefix, Amazon S3 will return the @x-amz-replication-status@ header with value PENDING, COMPLETED or FAILED indicating object replication status.
    --
    --
    --     * If requesting an object from the destination bucket — Amazon S3 will return the @x-amz-replication-status@ header with value REPLICA if the object in your request is a replica that Amazon S3 created.
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication> .
    replicationStatus :: Lude.Maybe ReplicationStatus,
    -- | Specifies caching behavior along the request/reply chain.
    cacheControl :: Lude.Maybe Lude.Text,
    -- | The language the content is in.
    contentLanguage :: Lude.Maybe Lude.Text,
    -- | Last modified date of the object
    lastModified :: Lude.Maybe Lude.DateTime,
    -- | Specifies whether a legal hold is in effect for this object. This header is only returned if the requester has the @s3:GetObjectLegalHold@ permission. This header is not returned if the specified version of this object has never had a legal hold applied. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
    objectLockLegalHoldStatus :: Lude.Maybe ObjectLockLegalHoldStatus,
    -- | Specifies presentational information for the object.
    contentDisposition :: Lude.Maybe Lude.Text,
    -- | If the object is stored using server-side encryption either with an AWS KMS customer master key (CMK) or an Amazon S3-managed encryption key, the response includes this header with the value of the server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HeadObjectResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' -
-- * 'partsCount' - The count of parts this object has.
-- * 'eTag' - An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
-- * 'versionId' - Version of the object.
-- * 'contentLength' - Size of the body in bytes.
-- * 'objectLockMode' - The Object Lock mode, if any, that's in effect for this object. This header is only returned if the requester has the @s3:GetObjectRetention@ permission. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
-- * 'expires' - The date and time at which the object is no longer cacheable.
-- * 'restore' - If the object is an archived object (an object whose storage class is GLACIER), the response includes this header if either the archive restoration is in progress (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> or an archive copy is already restored.
--
-- If an archive copy is already restored, the header value indicates when Amazon S3 is scheduled to delete the object copy. For example:
-- @x-amz-restore: ongoing-request="false", expiry-date="Fri, 23 Dec 2012 00:00:00 GMT"@
-- If the object restoration is in progress, the header returns the value @ongoing-request="true"@ .
-- For more information about archiving objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations> .
-- * 'expiration' - If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
-- * 'deleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
-- * 'archiveStatus' - The archive state of the head object.
-- * 'sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
-- * 'missingMeta' - This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
-- * 'websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
-- * 'acceptRanges' - Indicates that a range of bytes was specified.
-- * 'storageClass' - Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> .
-- * 'sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
-- * 'sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
-- * 'contentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
-- * 'objectLockRetainUntilDate' - The date and time when the Object Lock retention period expires. This header is only returned if the requester has the @s3:GetObjectRetention@ permission.
-- * 'metadata' - A map of metadata to store with the object in S3.
-- * 'replicationStatus' - Amazon S3 can return this header if your request involves a bucket that is either a source or destination in a replication rule.
--
-- In replication, you have a source bucket on which you configure replication and destination bucket where Amazon S3 stores object replicas. When you request an object (@GetObject@ ) or object metadata (@HeadObject@ ) from these buckets, Amazon S3 will return the @x-amz-replication-status@ header in the response as follows:
--
--     * If requesting an object from the source bucket — Amazon S3 will return the @x-amz-replication-status@ header if the object in your request is eligible for replication.
-- For example, suppose that in your replication configuration, you specify object prefix @TaxDocs@ requesting Amazon S3 to replicate objects with key prefix @TaxDocs@ . Any objects you upload with this key name prefix, for example @TaxDocs/document1.pdf@ , are eligible for replication. For any object request with this key name prefix, Amazon S3 will return the @x-amz-replication-status@ header with value PENDING, COMPLETED or FAILED indicating object replication status.
--
--
--     * If requesting an object from the destination bucket — Amazon S3 will return the @x-amz-replication-status@ header with value REPLICA if the object in your request is a replica that Amazon S3 created.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication> .
-- * 'cacheControl' - Specifies caching behavior along the request/reply chain.
-- * 'contentLanguage' - The language the content is in.
-- * 'lastModified' - Last modified date of the object
-- * 'objectLockLegalHoldStatus' - Specifies whether a legal hold is in effect for this object. This header is only returned if the requester has the @s3:GetObjectLegalHold@ permission. This header is not returned if the specified version of this object has never had a legal hold applied. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
-- * 'contentDisposition' - Specifies presentational information for the object.
-- * 'serverSideEncryption' - If the object is stored using server-side encryption either with an AWS KMS customer master key (CMK) or an Amazon S3-managed encryption key, the response includes this header with the value of the server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
-- * 'contentType' - A standard MIME type describing the format of the object data.
-- * 'responseStatus' - The response status code.
mkHeadObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  HeadObjectResponse
mkHeadObjectResponse pResponseStatus_ =
  HeadObjectResponse'
    { requestCharged = Lude.Nothing,
      partsCount = Lude.Nothing,
      eTag = Lude.Nothing,
      versionId = Lude.Nothing,
      contentLength = Lude.Nothing,
      objectLockMode = Lude.Nothing,
      expires = Lude.Nothing,
      restore = Lude.Nothing,
      expiration = Lude.Nothing,
      deleteMarker = Lude.Nothing,
      archiveStatus = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      missingMeta = Lude.Nothing,
      websiteRedirectLocation = Lude.Nothing,
      acceptRanges = Lude.Nothing,
      storageClass = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      sSEKMSKeyId = Lude.Nothing,
      contentEncoding = Lude.Nothing,
      objectLockRetainUntilDate = Lude.Nothing,
      metadata = Lude.mempty,
      replicationStatus = Lude.Nothing,
      cacheControl = Lude.Nothing,
      contentLanguage = Lude.Nothing,
      lastModified = Lude.Nothing,
      objectLockLegalHoldStatus = Lude.Nothing,
      contentDisposition = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsRequestCharged :: Lens.Lens' HeadObjectResponse (Lude.Maybe RequestCharged)
horsRequestCharged = Lens.lens (requestCharged :: HeadObjectResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: HeadObjectResponse)
{-# DEPRECATED horsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The count of parts this object has.
--
-- /Note:/ Consider using 'partsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsPartsCount :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Int)
horsPartsCount = Lens.lens (partsCount :: HeadObjectResponse -> Lude.Maybe Lude.Int) (\s a -> s {partsCount = a} :: HeadObjectResponse)
{-# DEPRECATED horsPartsCount "Use generic-lens or generic-optics with 'partsCount' instead." #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsETag :: Lens.Lens' HeadObjectResponse (Lude.Maybe ETag)
horsETag = Lens.lens (eTag :: HeadObjectResponse -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: HeadObjectResponse)
{-# DEPRECATED horsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsVersionId :: Lens.Lens' HeadObjectResponse (Lude.Maybe ObjectVersionId)
horsVersionId = Lens.lens (versionId :: HeadObjectResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: HeadObjectResponse)
{-# DEPRECATED horsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Size of the body in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsContentLength :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Integer)
horsContentLength = Lens.lens (contentLength :: HeadObjectResponse -> Lude.Maybe Lude.Integer) (\s a -> s {contentLength = a} :: HeadObjectResponse)
{-# DEPRECATED horsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The Object Lock mode, if any, that's in effect for this object. This header is only returned if the requester has the @s3:GetObjectRetention@ permission. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsObjectLockMode :: Lens.Lens' HeadObjectResponse (Lude.Maybe ObjectLockMode)
horsObjectLockMode = Lens.lens (objectLockMode :: HeadObjectResponse -> Lude.Maybe ObjectLockMode) (\s a -> s {objectLockMode = a} :: HeadObjectResponse)
{-# DEPRECATED horsObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsExpires :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.DateTime)
horsExpires = Lens.lens (expires :: HeadObjectResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {expires = a} :: HeadObjectResponse)
{-# DEPRECATED horsExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | If the object is an archived object (an object whose storage class is GLACIER), the response includes this header if either the archive restoration is in progress (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> or an archive copy is already restored.
--
-- If an archive copy is already restored, the header value indicates when Amazon S3 is scheduled to delete the object copy. For example:
-- @x-amz-restore: ongoing-request="false", expiry-date="Fri, 23 Dec 2012 00:00:00 GMT"@
-- If the object restoration is in progress, the header returns the value @ongoing-request="true"@ .
-- For more information about archiving objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations> .
--
-- /Note:/ Consider using 'restore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsRestore :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsRestore = Lens.lens (restore :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {restore = a} :: HeadObjectResponse)
{-# DEPRECATED horsRestore "Use generic-lens or generic-optics with 'restore' instead." #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsExpiration :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsExpiration = Lens.lens (expiration :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {expiration = a} :: HeadObjectResponse)
{-# DEPRECATED horsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsDeleteMarker :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Bool)
horsDeleteMarker = Lens.lens (deleteMarker :: HeadObjectResponse -> Lude.Maybe Lude.Bool) (\s a -> s {deleteMarker = a} :: HeadObjectResponse)
{-# DEPRECATED horsDeleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead." #-}

-- | The archive state of the head object.
--
-- /Note:/ Consider using 'archiveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsArchiveStatus :: Lens.Lens' HeadObjectResponse (Lude.Maybe ArchiveStatus)
horsArchiveStatus = Lens.lens (archiveStatus :: HeadObjectResponse -> Lude.Maybe ArchiveStatus) (\s a -> s {archiveStatus = a} :: HeadObjectResponse)
{-# DEPRECATED horsArchiveStatus "Use generic-lens or generic-optics with 'archiveStatus' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsSSECustomerAlgorithm :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: HeadObjectResponse)
{-# DEPRECATED horsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
--
-- /Note:/ Consider using 'missingMeta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsMissingMeta :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Int)
horsMissingMeta = Lens.lens (missingMeta :: HeadObjectResponse -> Lude.Maybe Lude.Int) (\s a -> s {missingMeta = a} :: HeadObjectResponse)
{-# DEPRECATED horsMissingMeta "Use generic-lens or generic-optics with 'missingMeta' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsWebsiteRedirectLocation :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsWebsiteRedirectLocation = Lens.lens (websiteRedirectLocation :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {websiteRedirectLocation = a} :: HeadObjectResponse)
{-# DEPRECATED horsWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

-- | Indicates that a range of bytes was specified.
--
-- /Note:/ Consider using 'acceptRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsAcceptRanges :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsAcceptRanges = Lens.lens (acceptRanges :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {acceptRanges = a} :: HeadObjectResponse)
{-# DEPRECATED horsAcceptRanges "Use generic-lens or generic-optics with 'acceptRanges' instead." #-}

-- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsStorageClass :: Lens.Lens' HeadObjectResponse (Lude.Maybe StorageClass)
horsStorageClass = Lens.lens (storageClass :: HeadObjectResponse -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: HeadObjectResponse)
{-# DEPRECATED horsStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsSSECustomerKeyMD5 :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: HeadObjectResponse)
{-# DEPRECATED horsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsSSEKMSKeyId :: Lens.Lens' HeadObjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
horsSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: HeadObjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: HeadObjectResponse)
{-# DEPRECATED horsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsContentEncoding :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsContentEncoding = Lens.lens (contentEncoding :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentEncoding = a} :: HeadObjectResponse)
{-# DEPRECATED horsContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The date and time when the Object Lock retention period expires. This header is only returned if the requester has the @s3:GetObjectRetention@ permission.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsObjectLockRetainUntilDate :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.DateTime)
horsObjectLockRetainUntilDate = Lens.lens (objectLockRetainUntilDate :: HeadObjectResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {objectLockRetainUntilDate = a} :: HeadObjectResponse)
{-# DEPRECATED horsObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsMetadata :: Lens.Lens' HeadObjectResponse (Lude.HashMap Lude.Text (Lude.Text))
horsMetadata = Lens.lens (metadata :: HeadObjectResponse -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {metadata = a} :: HeadObjectResponse)
{-# DEPRECATED horsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Amazon S3 can return this header if your request involves a bucket that is either a source or destination in a replication rule.
--
-- In replication, you have a source bucket on which you configure replication and destination bucket where Amazon S3 stores object replicas. When you request an object (@GetObject@ ) or object metadata (@HeadObject@ ) from these buckets, Amazon S3 will return the @x-amz-replication-status@ header in the response as follows:
--
--     * If requesting an object from the source bucket — Amazon S3 will return the @x-amz-replication-status@ header if the object in your request is eligible for replication.
-- For example, suppose that in your replication configuration, you specify object prefix @TaxDocs@ requesting Amazon S3 to replicate objects with key prefix @TaxDocs@ . Any objects you upload with this key name prefix, for example @TaxDocs/document1.pdf@ , are eligible for replication. For any object request with this key name prefix, Amazon S3 will return the @x-amz-replication-status@ header with value PENDING, COMPLETED or FAILED indicating object replication status.
--
--
--     * If requesting an object from the destination bucket — Amazon S3 will return the @x-amz-replication-status@ header with value REPLICA if the object in your request is a replica that Amazon S3 created.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication> .
--
-- /Note:/ Consider using 'replicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsReplicationStatus :: Lens.Lens' HeadObjectResponse (Lude.Maybe ReplicationStatus)
horsReplicationStatus = Lens.lens (replicationStatus :: HeadObjectResponse -> Lude.Maybe ReplicationStatus) (\s a -> s {replicationStatus = a} :: HeadObjectResponse)
{-# DEPRECATED horsReplicationStatus "Use generic-lens or generic-optics with 'replicationStatus' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsCacheControl :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsCacheControl = Lens.lens (cacheControl :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: HeadObjectResponse)
{-# DEPRECATED horsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsContentLanguage :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsContentLanguage = Lens.lens (contentLanguage :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentLanguage = a} :: HeadObjectResponse)
{-# DEPRECATED horsContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Last modified date of the object
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsLastModified :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.DateTime)
horsLastModified = Lens.lens (lastModified :: HeadObjectResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: HeadObjectResponse)
{-# DEPRECATED horsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Specifies whether a legal hold is in effect for this object. This header is only returned if the requester has the @s3:GetObjectLegalHold@ permission. This header is not returned if the specified version of this object has never had a legal hold applied. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsObjectLockLegalHoldStatus :: Lens.Lens' HeadObjectResponse (Lude.Maybe ObjectLockLegalHoldStatus)
horsObjectLockLegalHoldStatus = Lens.lens (objectLockLegalHoldStatus :: HeadObjectResponse -> Lude.Maybe ObjectLockLegalHoldStatus) (\s a -> s {objectLockLegalHoldStatus = a} :: HeadObjectResponse)
{-# DEPRECATED horsObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsContentDisposition :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsContentDisposition = Lens.lens (contentDisposition :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentDisposition = a} :: HeadObjectResponse)
{-# DEPRECATED horsContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | If the object is stored using server-side encryption either with an AWS KMS customer master key (CMK) or an Amazon S3-managed encryption key, the response includes this header with the value of the server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsServerSideEncryption :: Lens.Lens' HeadObjectResponse (Lude.Maybe ServerSideEncryption)
horsServerSideEncryption = Lens.lens (serverSideEncryption :: HeadObjectResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: HeadObjectResponse)
{-# DEPRECATED horsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsContentType :: Lens.Lens' HeadObjectResponse (Lude.Maybe Lude.Text)
horsContentType = Lens.lens (contentType :: HeadObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: HeadObjectResponse)
{-# DEPRECATED horsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horsResponseStatus :: Lens.Lens' HeadObjectResponse Lude.Int
horsResponseStatus = Lens.lens (responseStatus :: HeadObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: HeadObjectResponse)
{-# DEPRECATED horsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

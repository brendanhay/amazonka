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
    hoBucket,
    hoKey,
    hoExpectedBucketOwner,
    hoIfMatch,
    hoIfModifiedSince,
    hoIfNoneMatch,
    hoIfUnmodifiedSince,
    hoPartNumber,
    hoRange,
    hoRequestPayer,
    hoSSECustomerAlgorithm,
    hoSSECustomerKey,
    hoSSECustomerKeyMD5,
    hoVersionId,

    -- * Destructuring the response
    HeadObjectResponse (..),
    mkHeadObjectResponse,

    -- ** Response lenses
    horrsAcceptRanges,
    horrsArchiveStatus,
    horrsCacheControl,
    horrsContentDisposition,
    horrsContentEncoding,
    horrsContentLanguage,
    horrsContentLength,
    horrsContentType,
    horrsDeleteMarker,
    horrsETag,
    horrsExpiration,
    horrsExpires,
    horrsLastModified,
    horrsMetadata,
    horrsMissingMeta,
    horrsObjectLockLegalHoldStatus,
    horrsObjectLockMode,
    horrsObjectLockRetainUntilDate,
    horrsPartsCount,
    horrsReplicationStatus,
    horrsRequestCharged,
    horrsRestore,
    horrsSSECustomerAlgorithm,
    horrsSSECustomerKeyMD5,
    horrsSSEKMSKeyId,
    horrsServerSideEncryption,
    horrsStorageClass,
    horrsVersionId,
    horrsWebsiteRedirectLocation,
    horrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkHeadObject' smart constructor.
data HeadObject = HeadObject'
  { -- | The name of the bucket containing the object.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | The object key.
    key :: Types.Key,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId,
    -- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
    ifMatch :: Core.Maybe Types.IfMatch,
    -- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
    ifModifiedSince :: Core.Maybe Core.UTCTime,
    -- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
    ifNoneMatch :: Core.Maybe Types.IfNoneMatch,
    -- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
    ifUnmodifiedSince :: Core.Maybe Core.UTCTime,
    -- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' HEAD request for the part specified. Useful querying about the size of the part and the number of parts in this object.
    partNumber :: Core.Maybe Core.Int,
    -- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
    range :: Core.Maybe Types.Range,
    requestPayer :: Core.Maybe Types.RequestPayer,
    -- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Core.Maybe Types.SSECustomerKey,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Core.Maybe Types.ObjectVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'HeadObject' value with any optional fields omitted.
mkHeadObject ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  HeadObject
mkHeadObject bucket key =
  HeadObject'
    { bucket,
      key,
      expectedBucketOwner = Core.Nothing,
      ifMatch = Core.Nothing,
      ifModifiedSince = Core.Nothing,
      ifNoneMatch = Core.Nothing,
      ifUnmodifiedSince = Core.Nothing,
      partNumber = Core.Nothing,
      range = Core.Nothing,
      requestPayer = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      sSECustomerKey = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoBucket :: Lens.Lens' HeadObject Types.BucketName
hoBucket = Lens.field @"bucket"
{-# DEPRECATED hoBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoKey :: Lens.Lens' HeadObject Types.Key
hoKey = Lens.field @"key"
{-# DEPRECATED hoKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoExpectedBucketOwner :: Lens.Lens' HeadObject (Core.Maybe Types.AccountId)
hoExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED hoExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfMatch :: Lens.Lens' HeadObject (Core.Maybe Types.IfMatch)
hoIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED hoIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfModifiedSince :: Lens.Lens' HeadObject (Core.Maybe Core.UTCTime)
hoIfModifiedSince = Lens.field @"ifModifiedSince"
{-# DEPRECATED hoIfModifiedSince "Use generic-lens or generic-optics with 'ifModifiedSince' instead." #-}

-- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfNoneMatch :: Lens.Lens' HeadObject (Core.Maybe Types.IfNoneMatch)
hoIfNoneMatch = Lens.field @"ifNoneMatch"
{-# DEPRECATED hoIfNoneMatch "Use generic-lens or generic-optics with 'ifNoneMatch' instead." #-}

-- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoIfUnmodifiedSince :: Lens.Lens' HeadObject (Core.Maybe Core.UTCTime)
hoIfUnmodifiedSince = Lens.field @"ifUnmodifiedSince"
{-# DEPRECATED hoIfUnmodifiedSince "Use generic-lens or generic-optics with 'ifUnmodifiedSince' instead." #-}

-- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' HEAD request for the part specified. Useful querying about the size of the part and the number of parts in this object.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoPartNumber :: Lens.Lens' HeadObject (Core.Maybe Core.Int)
hoPartNumber = Lens.field @"partNumber"
{-# DEPRECATED hoPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

-- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoRange :: Lens.Lens' HeadObject (Core.Maybe Types.Range)
hoRange = Lens.field @"range"
{-# DEPRECATED hoRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoRequestPayer :: Lens.Lens' HeadObject (Core.Maybe Types.RequestPayer)
hoRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED hoRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoSSECustomerAlgorithm :: Lens.Lens' HeadObject (Core.Maybe Types.SSECustomerAlgorithm)
hoSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED hoSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoSSECustomerKey :: Lens.Lens' HeadObject (Core.Maybe Types.SSECustomerKey)
hoSSECustomerKey = Lens.field @"sSECustomerKey"
{-# DEPRECATED hoSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoSSECustomerKeyMD5 :: Lens.Lens' HeadObject (Core.Maybe Types.SSECustomerKeyMD5)
hoSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED hoSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoVersionId :: Lens.Lens' HeadObject (Core.Maybe Types.ObjectVersionId)
hoVersionId = Lens.field @"versionId"
{-# DEPRECATED hoVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest HeadObject where
  type Rs HeadObject = HeadObjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.HEAD,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery =
          Core.toQueryValue "partNumber" Core.<$> partNumber
            Core.<> (Core.toQueryValue "versionId" Core.<$> versionId),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> (Core.toHeaders "If-Match" ifMatch)
            Core.<> (Core.toHeaders "If-Modified-Since" ifModifiedSince)
            Core.<> (Core.toHeaders "If-None-Match" ifNoneMatch)
            Core.<> (Core.toHeaders "If-Unmodified-Since" ifUnmodifiedSince)
            Core.<> (Core.toHeaders "Range" range)
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
                    ),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          HeadObjectResponse'
            Core.<$> (Core.parseHeaderMaybe "accept-ranges" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-archive-status" h)
            Core.<*> (Core.parseHeaderMaybe "Cache-Control" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Disposition" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Encoding" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Language" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Length" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Type" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-delete-marker" h)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-expiration" h)
            Core.<*> (Core.parseHeaderMaybe "Expires" h)
            Core.<*> (Core.parseHeaderMaybe "Last-Modified" h)
            Core.<*> (Core.parseHeaderMap "x-amz-meta-" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-missing-meta" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-object-lock-legal-hold" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-object-lock-mode" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-object-lock-retain-until-date" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-mp-parts-count" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-replication-status" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-restore" h)
            Core.<*> ( Core.parseHeaderMaybe
                         "x-amz-server-side-encryption-customer-algorithm"
                         h
                     )
            Core.<*> ( Core.parseHeaderMaybe
                         "x-amz-server-side-encryption-customer-key-MD5"
                         h
                     )
            Core.<*> ( Core.parseHeaderMaybe
                         "x-amz-server-side-encryption-aws-kms-key-id"
                         h
                     )
            Core.<*> (Core.parseHeaderMaybe "x-amz-server-side-encryption" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-storage-class" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-version-id" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-website-redirect-location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkHeadObjectResponse' smart constructor.
data HeadObjectResponse = HeadObjectResponse'
  { -- | Indicates that a range of bytes was specified.
    acceptRanges :: Core.Maybe Types.AcceptRanges,
    -- | The archive state of the head object.
    archiveStatus :: Core.Maybe Types.ArchiveStatus,
    -- | Specifies caching behavior along the request/reply chain.
    cacheControl :: Core.Maybe Types.CacheControl,
    -- | Specifies presentational information for the object.
    contentDisposition :: Core.Maybe Types.ContentDisposition,
    -- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
    contentEncoding :: Core.Maybe Types.ContentEncoding,
    -- | The language the content is in.
    contentLanguage :: Core.Maybe Types.ContentLanguage,
    -- | Size of the body in bytes.
    contentLength :: Core.Maybe Core.Integer,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Core.Maybe Types.ContentType,
    -- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
    deleteMarker :: Core.Maybe Core.Bool,
    -- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
    eTag :: Core.Maybe Types.ETag,
    -- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
    expiration :: Core.Maybe Types.Expiration,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Core.Maybe Core.UTCTime,
    -- | Last modified date of the object
    lastModified :: Core.Maybe Core.UTCTime,
    -- | A map of metadata to store with the object in S3.
    metadata :: Core.HashMap Types.MetadataKey Types.MetadataValue,
    -- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
    missingMeta :: Core.Maybe Core.Int,
    -- | Specifies whether a legal hold is in effect for this object. This header is only returned if the requester has the @s3:GetObjectLegalHold@ permission. This header is not returned if the specified version of this object has never had a legal hold applied. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
    objectLockLegalHoldStatus :: Core.Maybe Types.ObjectLockLegalHoldStatus,
    -- | The Object Lock mode, if any, that's in effect for this object. This header is only returned if the requester has the @s3:GetObjectRetention@ permission. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
    objectLockMode :: Core.Maybe Types.ObjectLockMode,
    -- | The date and time when the Object Lock retention period expires. This header is only returned if the requester has the @s3:GetObjectRetention@ permission.
    objectLockRetainUntilDate :: Core.Maybe Core.UTCTime,
    -- | The count of parts this object has.
    partsCount :: Core.Maybe Core.Int,
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
    replicationStatus :: Core.Maybe Types.ReplicationStatus,
    requestCharged :: Core.Maybe Types.RequestCharged,
    -- | If the object is an archived object (an object whose storage class is GLACIER), the response includes this header if either the archive restoration is in progress (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> or an archive copy is already restored.
    --
    -- If an archive copy is already restored, the header value indicates when Amazon S3 is scheduled to delete the object copy. For example:
    -- @x-amz-restore: ongoing-request="false", expiry-date="Fri, 23 Dec 2012 00:00:00 GMT"@
    -- If the object restoration is in progress, the header returns the value @ongoing-request="true"@ .
    -- For more information about archiving objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations> .
    restore :: Core.Maybe Types.Restore,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
    sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId,
    -- | If the object is stored using server-side encryption either with an AWS KMS customer master key (CMK) or an Amazon S3-managed encryption key, the response includes this header with the value of the server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe Types.ServerSideEncryption,
    -- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> .
    storageClass :: Core.Maybe Types.StorageClass,
    -- | Version of the object.
    versionId :: Core.Maybe Types.VersionId,
    -- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Core.Maybe Types.WebsiteRedirectLocation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'HeadObjectResponse' value with any optional fields omitted.
mkHeadObjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  HeadObjectResponse
mkHeadObjectResponse responseStatus =
  HeadObjectResponse'
    { acceptRanges = Core.Nothing,
      archiveStatus = Core.Nothing,
      cacheControl = Core.Nothing,
      contentDisposition = Core.Nothing,
      contentEncoding = Core.Nothing,
      contentLanguage = Core.Nothing,
      contentLength = Core.Nothing,
      contentType = Core.Nothing,
      deleteMarker = Core.Nothing,
      eTag = Core.Nothing,
      expiration = Core.Nothing,
      expires = Core.Nothing,
      lastModified = Core.Nothing,
      metadata = Core.mempty,
      missingMeta = Core.Nothing,
      objectLockLegalHoldStatus = Core.Nothing,
      objectLockMode = Core.Nothing,
      objectLockRetainUntilDate = Core.Nothing,
      partsCount = Core.Nothing,
      replicationStatus = Core.Nothing,
      requestCharged = Core.Nothing,
      restore = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      storageClass = Core.Nothing,
      versionId = Core.Nothing,
      websiteRedirectLocation = Core.Nothing,
      responseStatus
    }

-- | Indicates that a range of bytes was specified.
--
-- /Note:/ Consider using 'acceptRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsAcceptRanges :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.AcceptRanges)
horrsAcceptRanges = Lens.field @"acceptRanges"
{-# DEPRECATED horrsAcceptRanges "Use generic-lens or generic-optics with 'acceptRanges' instead." #-}

-- | The archive state of the head object.
--
-- /Note:/ Consider using 'archiveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsArchiveStatus :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ArchiveStatus)
horrsArchiveStatus = Lens.field @"archiveStatus"
{-# DEPRECATED horrsArchiveStatus "Use generic-lens or generic-optics with 'archiveStatus' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsCacheControl :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.CacheControl)
horrsCacheControl = Lens.field @"cacheControl"
{-# DEPRECATED horrsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsContentDisposition :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ContentDisposition)
horrsContentDisposition = Lens.field @"contentDisposition"
{-# DEPRECATED horrsContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsContentEncoding :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ContentEncoding)
horrsContentEncoding = Lens.field @"contentEncoding"
{-# DEPRECATED horrsContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsContentLanguage :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ContentLanguage)
horrsContentLanguage = Lens.field @"contentLanguage"
{-# DEPRECATED horrsContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Size of the body in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsContentLength :: Lens.Lens' HeadObjectResponse (Core.Maybe Core.Integer)
horrsContentLength = Lens.field @"contentLength"
{-# DEPRECATED horrsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsContentType :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ContentType)
horrsContentType = Lens.field @"contentType"
{-# DEPRECATED horrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsDeleteMarker :: Lens.Lens' HeadObjectResponse (Core.Maybe Core.Bool)
horrsDeleteMarker = Lens.field @"deleteMarker"
{-# DEPRECATED horrsDeleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead." #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsETag :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ETag)
horrsETag = Lens.field @"eTag"
{-# DEPRECATED horrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsExpiration :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.Expiration)
horrsExpiration = Lens.field @"expiration"
{-# DEPRECATED horrsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsExpires :: Lens.Lens' HeadObjectResponse (Core.Maybe Core.UTCTime)
horrsExpires = Lens.field @"expires"
{-# DEPRECATED horrsExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Last modified date of the object
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsLastModified :: Lens.Lens' HeadObjectResponse (Core.Maybe Core.UTCTime)
horrsLastModified = Lens.field @"lastModified"
{-# DEPRECATED horrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsMetadata :: Lens.Lens' HeadObjectResponse (Core.HashMap Types.MetadataKey Types.MetadataValue)
horrsMetadata = Lens.field @"metadata"
{-# DEPRECATED horrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
--
-- /Note:/ Consider using 'missingMeta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsMissingMeta :: Lens.Lens' HeadObjectResponse (Core.Maybe Core.Int)
horrsMissingMeta = Lens.field @"missingMeta"
{-# DEPRECATED horrsMissingMeta "Use generic-lens or generic-optics with 'missingMeta' instead." #-}

-- | Specifies whether a legal hold is in effect for this object. This header is only returned if the requester has the @s3:GetObjectLegalHold@ permission. This header is not returned if the specified version of this object has never had a legal hold applied. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsObjectLockLegalHoldStatus :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ObjectLockLegalHoldStatus)
horrsObjectLockLegalHoldStatus = Lens.field @"objectLockLegalHoldStatus"
{-# DEPRECATED horrsObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | The Object Lock mode, if any, that's in effect for this object. This header is only returned if the requester has the @s3:GetObjectRetention@ permission. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsObjectLockMode :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ObjectLockMode)
horrsObjectLockMode = Lens.field @"objectLockMode"
{-# DEPRECATED horrsObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time when the Object Lock retention period expires. This header is only returned if the requester has the @s3:GetObjectRetention@ permission.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsObjectLockRetainUntilDate :: Lens.Lens' HeadObjectResponse (Core.Maybe Core.UTCTime)
horrsObjectLockRetainUntilDate = Lens.field @"objectLockRetainUntilDate"
{-# DEPRECATED horrsObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | The count of parts this object has.
--
-- /Note:/ Consider using 'partsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsPartsCount :: Lens.Lens' HeadObjectResponse (Core.Maybe Core.Int)
horrsPartsCount = Lens.field @"partsCount"
{-# DEPRECATED horrsPartsCount "Use generic-lens or generic-optics with 'partsCount' instead." #-}

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
horrsReplicationStatus :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ReplicationStatus)
horrsReplicationStatus = Lens.field @"replicationStatus"
{-# DEPRECATED horrsReplicationStatus "Use generic-lens or generic-optics with 'replicationStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsRequestCharged :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.RequestCharged)
horrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED horrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | If the object is an archived object (an object whose storage class is GLACIER), the response includes this header if either the archive restoration is in progress (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> or an archive copy is already restored.
--
-- If an archive copy is already restored, the header value indicates when Amazon S3 is scheduled to delete the object copy. For example:
-- @x-amz-restore: ongoing-request="false", expiry-date="Fri, 23 Dec 2012 00:00:00 GMT"@
-- If the object restoration is in progress, the header returns the value @ongoing-request="true"@ .
-- For more information about archiving objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations> .
--
-- /Note:/ Consider using 'restore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsRestore :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.Restore)
horrsRestore = Lens.field @"restore"
{-# DEPRECATED horrsRestore "Use generic-lens or generic-optics with 'restore' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsSSECustomerAlgorithm :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.SSECustomerAlgorithm)
horrsSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED horrsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsSSECustomerKeyMD5 :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.SSECustomerKeyMD5)
horrsSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED horrsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsSSEKMSKeyId :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.SSEKMSKeyId)
horrsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED horrsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | If the object is stored using server-side encryption either with an AWS KMS customer master key (CMK) or an Amazon S3-managed encryption key, the response includes this header with the value of the server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsServerSideEncryption :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.ServerSideEncryption)
horrsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED horrsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsStorageClass :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.StorageClass)
horrsStorageClass = Lens.field @"storageClass"
{-# DEPRECATED horrsStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsVersionId :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.VersionId)
horrsVersionId = Lens.field @"versionId"
{-# DEPRECATED horrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsWebsiteRedirectLocation :: Lens.Lens' HeadObjectResponse (Core.Maybe Types.WebsiteRedirectLocation)
horrsWebsiteRedirectLocation = Lens.field @"websiteRedirectLocation"
{-# DEPRECATED horrsWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horrsResponseStatus :: Lens.Lens' HeadObjectResponse Core.Int
horrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED horrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

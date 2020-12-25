{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves objects from Amazon S3. To use @GET@ , you must have @READ@ access to the object. If you grant @READ@ access to the anonymous user, you can return the object without using an authorization header.
--
-- An Amazon S3 bucket has no directory hierarchy such as you would find in a typical computer file system. You can, however, create a logical hierarchy by using object key names that imply a folder structure. For example, instead of naming an object @sample.jpg@ , you can name it @photos/2006/February/sample.jpg@ .
-- To get an object from such a logical hierarchy, specify the full key name for the object in the @GET@ operation. For a virtual hosted-style request example, if you have the object @photos/2006/February/sample.jpg@ , specify the resource as @/photos/2006/February/sample.jpg@ . For a path-style request example, if you have the object @photos/2006/February/sample.jpg@ in the bucket named @examplebucket@ , specify the resource as @/examplebucket/photos/2006/February/sample.jpg@ . For more information about request types, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html#VirtualHostingSpecifyBucket HTTP Host Header Bucket Specification> .
-- To distribute large files to many people, you can save bandwidth costs by using BitTorrent. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3Torrent.html Amazon S3 Torrent> . For more information about returning the ACL of an object, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl> .
-- If the object you are retrieving is stored in the S3 Glacier or S3 Glacier Deep Archive storage class, or S3 Intelligent-Tiering Archive or S3 Intelligent-Tiering Deep Archive tiers, before you can retrieve the object you must first restore a copy using <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> . Otherwise, this operation returns an @InvalidObjectStateError@ error. For information about restoring archived objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/restoring-objects.html Restoring Archived Objects> .
-- Encryption request headers, like @x-amz-server-side-encryption@ , should not be sent for GET requests if your object uses server-side encryption with CMKs stored in AWS KMS (SSE-KMS) or server-side encryption with Amazon S3–managed encryption keys (SSE-S3). If your object does use these types of keys, you’ll get an HTTP 400 BadRequest error.
-- If you encrypt an object by using server-side encryption with customer-provided encryption keys (SSE-C) when you store the object in Amazon S3, then when you GET the object, you must use the following headers:
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
-- Assuming you have permission to read object tags (permission for the @s3:GetObjectVersionTagging@ action), the response also returns the @x-amz-tagging-count@ header that provides the count of number of tags associated with the object. You can use <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging> to retrieve the tag set associated with an object.
-- __Permissions__
-- You need the @s3:GetObject@ permission for this operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> . If the object you request does not exist, the error Amazon S3 returns depends on whether you also have the @s3:ListBucket@ permission.
--
--     * If you have the @s3:ListBucket@ permission on the bucket, Amazon S3 will return an HTTP status code 404 ("no such key") error.
--
--
--     * If you don’t have the @s3:ListBucket@ permission, Amazon S3 will return an HTTP status code 403 ("access denied") error.
--
--
-- __Versioning__
-- By default, the GET operation returns the current version of an object. To return a different version, use the @versionId@ subresource.
-- For more information about versioning, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketVersioning.html PutBucketVersioning> .
-- __Overriding Response Header Values__
-- There are times when you want to override certain response header values in a GET response. For example, you might override the Content-Disposition response header value in your GET request.
-- You can override values for a set of response headers using the following query parameters. These response header values are sent only on a successful request, that is, when status code 200 OK is returned. The set of headers you can override using these parameters is a subset of the headers that Amazon S3 accepts when you create an object. The response headers that you can override for the GET response are @Content-Type@ , @Content-Language@ , @Expires@ , @Cache-Control@ , @Content-Disposition@ , and @Content-Encoding@ . To override these header values in the GET response, you use the following request parameters.
--
--     * @response-content-type@
--
--
--     * @response-content-language@
--
--
--     * @response-expires@
--
--
--     * @response-cache-control@
--
--
--     * @response-content-disposition@
--
--
--     * @response-content-encoding@
--
--
-- __Additional Considerations about Request Headers__
-- If both of the @If-Match@ and @If-Unmodified-Since@ headers are present in the request as follows: @If-Match@ condition evaluates to @true@ , and; @If-Unmodified-Since@ condition evaluates to @false@ ; then, S3 returns 200 OK and the data requested.
-- If both of the @If-None-Match@ and @If-Modified-Since@ headers are present in the request as follows:@If-None-Match@ condition evaluates to @false@ , and; @If-Modified-Since@ condition evaluates to @true@ ; then, S3 returns 304 Not Modified response code.
-- For more information about conditional requests, see <https://tools.ietf.org/html/rfc7232 RFC 7232> .
-- The following operations are related to @GetObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
module Network.AWS.S3.GetObject
  ( -- * Creating a request
    GetObject (..),
    mkGetObject,

    -- ** Request lenses
    goBucket,
    goKey,
    goExpectedBucketOwner,
    goIfMatch,
    goIfModifiedSince,
    goIfNoneMatch,
    goIfUnmodifiedSince,
    goPartNumber,
    goRange,
    goRequestPayer,
    goResponseCacheControl,
    goResponseContentDisposition,
    goResponseContentEncoding,
    goResponseContentLanguage,
    goResponseContentType,
    goResponseExpires,
    goSSECustomerAlgorithm,
    goSSECustomerKey,
    goSSECustomerKeyMD5,
    goVersionId,

    -- * Destructuring the response
    GetObjectResponse (..),
    mkGetObjectResponse,

    -- ** Response lenses
    gorrsAcceptRanges,
    gorrsBody,
    gorrsCacheControl,
    gorrsContentDisposition,
    gorrsContentEncoding,
    gorrsContentLanguage,
    gorrsContentLength,
    gorrsContentRange,
    gorrsContentType,
    gorrsDeleteMarker,
    gorrsETag,
    gorrsExpiration,
    gorrsExpires,
    gorrsLastModified,
    gorrsMetadata,
    gorrsMissingMeta,
    gorrsObjectLockLegalHoldStatus,
    gorrsObjectLockMode,
    gorrsObjectLockRetainUntilDate,
    gorrsPartsCount,
    gorrsReplicationStatus,
    gorrsRequestCharged,
    gorrsRestore,
    gorrsSSECustomerAlgorithm,
    gorrsSSECustomerKeyMD5,
    gorrsSSEKMSKeyId,
    gorrsServerSideEncryption,
    gorrsStorageClass,
    gorrsTagCount,
    gorrsVersionId,
    gorrsWebsiteRedirectLocation,
    gorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetObject' smart constructor.
data GetObject = GetObject'
  { -- | The bucket name containing the object.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | Key of the object to get.
    key :: Types.Key,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
    ifMatch :: Core.Maybe Types.IfMatch,
    -- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
    ifModifiedSince :: Core.Maybe Core.UTCTime,
    -- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
    ifNoneMatch :: Core.Maybe Types.IfNoneMatch,
    -- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
    ifUnmodifiedSince :: Core.Maybe Core.UTCTime,
    -- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
    partNumber :: Core.Maybe Core.Int,
    -- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
    range :: Core.Maybe Types.Range,
    requestPayer :: Core.Maybe Types.RequestPayer,
    -- | Sets the @Cache-Control@ header of the response.
    responseCacheControl :: Core.Maybe Types.ResponseCacheControl,
    -- | Sets the @Content-Disposition@ header of the response
    responseContentDisposition :: Core.Maybe Types.ResponseContentDisposition,
    -- | Sets the @Content-Encoding@ header of the response.
    responseContentEncoding :: Core.Maybe Types.ResponseContentEncoding,
    -- | Sets the @Content-Language@ header of the response.
    responseContentLanguage :: Core.Maybe Types.ResponseContentLanguage,
    -- | Sets the @Content-Type@ header of the response.
    responseContentType :: Core.Maybe Types.ResponseContentType,
    -- | Sets the @Expires@ header of the response.
    responseExpires :: Core.Maybe Core.UTCTime,
    -- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Core.Maybe Types.SSECustomerKey,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Core.Maybe Types.VersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetObject' value with any optional fields omitted.
mkGetObject ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  GetObject
mkGetObject bucket key =
  GetObject'
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
      responseCacheControl = Core.Nothing,
      responseContentDisposition = Core.Nothing,
      responseContentEncoding = Core.Nothing,
      responseContentLanguage = Core.Nothing,
      responseContentType = Core.Nothing,
      responseExpires = Core.Nothing,
      sSECustomerAlgorithm = Core.Nothing,
      sSECustomerKey = Core.Nothing,
      sSECustomerKeyMD5 = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The bucket name containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goBucket :: Lens.Lens' GetObject Types.BucketName
goBucket = Lens.field @"bucket"
{-# DEPRECATED goBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Key of the object to get.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goKey :: Lens.Lens' GetObject Types.Key
goKey = Lens.field @"key"
{-# DEPRECATED goKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goExpectedBucketOwner :: Lens.Lens' GetObject (Core.Maybe Types.ExpectedBucketOwner)
goExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED goExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfMatch :: Lens.Lens' GetObject (Core.Maybe Types.IfMatch)
goIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED goIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfModifiedSince :: Lens.Lens' GetObject (Core.Maybe Core.UTCTime)
goIfModifiedSince = Lens.field @"ifModifiedSince"
{-# DEPRECATED goIfModifiedSince "Use generic-lens or generic-optics with 'ifModifiedSince' instead." #-}

-- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfNoneMatch :: Lens.Lens' GetObject (Core.Maybe Types.IfNoneMatch)
goIfNoneMatch = Lens.field @"ifNoneMatch"
{-# DEPRECATED goIfNoneMatch "Use generic-lens or generic-optics with 'ifNoneMatch' instead." #-}

-- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfUnmodifiedSince :: Lens.Lens' GetObject (Core.Maybe Core.UTCTime)
goIfUnmodifiedSince = Lens.field @"ifUnmodifiedSince"
{-# DEPRECATED goIfUnmodifiedSince "Use generic-lens or generic-optics with 'ifUnmodifiedSince' instead." #-}

-- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goPartNumber :: Lens.Lens' GetObject (Core.Maybe Core.Int)
goPartNumber = Lens.field @"partNumber"
{-# DEPRECATED goPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

-- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goRange :: Lens.Lens' GetObject (Core.Maybe Types.Range)
goRange = Lens.field @"range"
{-# DEPRECATED goRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goRequestPayer :: Lens.Lens' GetObject (Core.Maybe Types.RequestPayer)
goRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED goRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Sets the @Cache-Control@ header of the response.
--
-- /Note:/ Consider using 'responseCacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseCacheControl :: Lens.Lens' GetObject (Core.Maybe Types.ResponseCacheControl)
goResponseCacheControl = Lens.field @"responseCacheControl"
{-# DEPRECATED goResponseCacheControl "Use generic-lens or generic-optics with 'responseCacheControl' instead." #-}

-- | Sets the @Content-Disposition@ header of the response
--
-- /Note:/ Consider using 'responseContentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentDisposition :: Lens.Lens' GetObject (Core.Maybe Types.ResponseContentDisposition)
goResponseContentDisposition = Lens.field @"responseContentDisposition"
{-# DEPRECATED goResponseContentDisposition "Use generic-lens or generic-optics with 'responseContentDisposition' instead." #-}

-- | Sets the @Content-Encoding@ header of the response.
--
-- /Note:/ Consider using 'responseContentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentEncoding :: Lens.Lens' GetObject (Core.Maybe Types.ResponseContentEncoding)
goResponseContentEncoding = Lens.field @"responseContentEncoding"
{-# DEPRECATED goResponseContentEncoding "Use generic-lens or generic-optics with 'responseContentEncoding' instead." #-}

-- | Sets the @Content-Language@ header of the response.
--
-- /Note:/ Consider using 'responseContentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentLanguage :: Lens.Lens' GetObject (Core.Maybe Types.ResponseContentLanguage)
goResponseContentLanguage = Lens.field @"responseContentLanguage"
{-# DEPRECATED goResponseContentLanguage "Use generic-lens or generic-optics with 'responseContentLanguage' instead." #-}

-- | Sets the @Content-Type@ header of the response.
--
-- /Note:/ Consider using 'responseContentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentType :: Lens.Lens' GetObject (Core.Maybe Types.ResponseContentType)
goResponseContentType = Lens.field @"responseContentType"
{-# DEPRECATED goResponseContentType "Use generic-lens or generic-optics with 'responseContentType' instead." #-}

-- | Sets the @Expires@ header of the response.
--
-- /Note:/ Consider using 'responseExpires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseExpires :: Lens.Lens' GetObject (Core.Maybe Core.UTCTime)
goResponseExpires = Lens.field @"responseExpires"
{-# DEPRECATED goResponseExpires "Use generic-lens or generic-optics with 'responseExpires' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goSSECustomerAlgorithm :: Lens.Lens' GetObject (Core.Maybe Types.SSECustomerAlgorithm)
goSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED goSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goSSECustomerKey :: Lens.Lens' GetObject (Core.Maybe Types.SSECustomerKey)
goSSECustomerKey = Lens.field @"sSECustomerKey"
{-# DEPRECATED goSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goSSECustomerKeyMD5 :: Lens.Lens' GetObject (Core.Maybe Types.SSECustomerKeyMD5)
goSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED goSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goVersionId :: Lens.Lens' GetObject (Core.Maybe Types.VersionId)
goVersionId = Lens.field @"versionId"
{-# DEPRECATED goVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest GetObject where
  type Rs GetObject = GetObjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery =
          Core.toQueryValue "partNumber" Core.<$> partNumber
            Core.<> ( Core.toQueryValue "response-cache-control"
                        Core.<$> responseCacheControl
                    )
            Core.<> ( Core.toQueryValue "response-content-disposition"
                        Core.<$> responseContentDisposition
                    )
            Core.<> ( Core.toQueryValue "response-content-encoding"
                        Core.<$> responseContentEncoding
                    )
            Core.<> ( Core.toQueryValue "response-content-language"
                        Core.<$> responseContentLanguage
                    )
            Core.<> ( Core.toQueryValue "response-content-type"
                        Core.<$> responseContentType
                    )
            Core.<> (Core.toQueryValue "response-expires" Core.<$> responseExpires)
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
    Response.receiveBody
      ( \s h x ->
          GetObjectResponse'
            Core.<$> (Core.parseHeaderMaybe "accept-ranges" h)
            Core.<*> (Core.pure x)
            Core.<*> (Core.parseHeaderMaybe "Cache-Control" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Disposition" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Encoding" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Language" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Length" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Range" h)
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
            Core.<*> (Core.parseHeaderMaybe "x-amz-tagging-count" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-version-id" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-website-redirect-location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { -- | Indicates that a range of bytes was specified.
    acceptRanges :: Core.Maybe Types.AcceptRanges,
    -- | Object data.
    body :: Core.RsBody,
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
    -- | The portion of the object returned in the response.
    contentRange :: Core.Maybe Types.ContentRange,
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
    -- | Indicates whether this object has an active legal hold. This field is only returned if you have permission to view an object's legal hold status.
    objectLockLegalHoldStatus :: Core.Maybe Types.ObjectLockLegalHoldStatus,
    -- | The Object Lock mode currently in place for this object.
    objectLockMode :: Core.Maybe Types.ObjectLockMode,
    -- | The date and time when this object's Object Lock will expire.
    objectLockRetainUntilDate :: Core.Maybe Core.UTCTime,
    -- | The count of parts this object has.
    partsCount :: Core.Maybe Core.Int,
    -- | Amazon S3 can return this if your request involves a bucket that is either a source or destination in a replication rule.
    replicationStatus :: Core.Maybe Types.ReplicationStatus,
    requestCharged :: Core.Maybe Types.RequestCharged,
    -- | Provides information about object restoration operation and expiration time of the restored object copy.
    restore :: Core.Maybe Types.Restore,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
    sSECustomerAlgorithm :: Core.Maybe Types.SSECustomerAlgorithm,
    -- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Core.Maybe Types.SSECustomerKeyMD5,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
    sSEKMSKeyId :: Core.Maybe Types.SSEKMSKeyId,
    -- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Core.Maybe Types.ServerSideEncryption,
    -- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
    storageClass :: Core.Maybe Types.StorageClass,
    -- | The number of tags, if any, on the object.
    tagCount :: Core.Maybe Core.Int,
    -- | Version of the object.
    versionId :: Core.Maybe Types.ObjectVersionId,
    -- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Core.Maybe Types.WebsiteRedirectLocation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'GetObjectResponse' value with any optional fields omitted.
mkGetObjectResponse ::
  -- | 'body'
  Core.RsBody ->
  -- | 'responseStatus'
  Core.Int ->
  GetObjectResponse
mkGetObjectResponse body responseStatus =
  GetObjectResponse'
    { acceptRanges = Core.Nothing,
      body,
      cacheControl = Core.Nothing,
      contentDisposition = Core.Nothing,
      contentEncoding = Core.Nothing,
      contentLanguage = Core.Nothing,
      contentLength = Core.Nothing,
      contentRange = Core.Nothing,
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
      tagCount = Core.Nothing,
      versionId = Core.Nothing,
      websiteRedirectLocation = Core.Nothing,
      responseStatus
    }

-- | Indicates that a range of bytes was specified.
--
-- /Note:/ Consider using 'acceptRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsAcceptRanges :: Lens.Lens' GetObjectResponse (Core.Maybe Types.AcceptRanges)
gorrsAcceptRanges = Lens.field @"acceptRanges"
{-# DEPRECATED gorrsAcceptRanges "Use generic-lens or generic-optics with 'acceptRanges' instead." #-}

-- | Object data.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsBody :: Lens.Lens' GetObjectResponse Core.RsBody
gorrsBody = Lens.field @"body"
{-# DEPRECATED gorrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsCacheControl :: Lens.Lens' GetObjectResponse (Core.Maybe Types.CacheControl)
gorrsCacheControl = Lens.field @"cacheControl"
{-# DEPRECATED gorrsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentDisposition :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ContentDisposition)
gorrsContentDisposition = Lens.field @"contentDisposition"
{-# DEPRECATED gorrsContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentEncoding :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ContentEncoding)
gorrsContentEncoding = Lens.field @"contentEncoding"
{-# DEPRECATED gorrsContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentLanguage :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ContentLanguage)
gorrsContentLanguage = Lens.field @"contentLanguage"
{-# DEPRECATED gorrsContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Size of the body in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentLength :: Lens.Lens' GetObjectResponse (Core.Maybe Core.Integer)
gorrsContentLength = Lens.field @"contentLength"
{-# DEPRECATED gorrsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The portion of the object returned in the response.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentRange :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ContentRange)
gorrsContentRange = Lens.field @"contentRange"
{-# DEPRECATED gorrsContentRange "Use generic-lens or generic-optics with 'contentRange' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentType :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ContentType)
gorrsContentType = Lens.field @"contentType"
{-# DEPRECATED gorrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsDeleteMarker :: Lens.Lens' GetObjectResponse (Core.Maybe Core.Bool)
gorrsDeleteMarker = Lens.field @"deleteMarker"
{-# DEPRECATED gorrsDeleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead." #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsETag :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ETag)
gorrsETag = Lens.field @"eTag"
{-# DEPRECATED gorrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsExpiration :: Lens.Lens' GetObjectResponse (Core.Maybe Types.Expiration)
gorrsExpiration = Lens.field @"expiration"
{-# DEPRECATED gorrsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsExpires :: Lens.Lens' GetObjectResponse (Core.Maybe Core.UTCTime)
gorrsExpires = Lens.field @"expires"
{-# DEPRECATED gorrsExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Last modified date of the object
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsLastModified :: Lens.Lens' GetObjectResponse (Core.Maybe Core.UTCTime)
gorrsLastModified = Lens.field @"lastModified"
{-# DEPRECATED gorrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsMetadata :: Lens.Lens' GetObjectResponse (Core.HashMap Types.MetadataKey Types.MetadataValue)
gorrsMetadata = Lens.field @"metadata"
{-# DEPRECATED gorrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
--
-- /Note:/ Consider using 'missingMeta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsMissingMeta :: Lens.Lens' GetObjectResponse (Core.Maybe Core.Int)
gorrsMissingMeta = Lens.field @"missingMeta"
{-# DEPRECATED gorrsMissingMeta "Use generic-lens or generic-optics with 'missingMeta' instead." #-}

-- | Indicates whether this object has an active legal hold. This field is only returned if you have permission to view an object's legal hold status.
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsObjectLockLegalHoldStatus :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ObjectLockLegalHoldStatus)
gorrsObjectLockLegalHoldStatus = Lens.field @"objectLockLegalHoldStatus"
{-# DEPRECATED gorrsObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | The Object Lock mode currently in place for this object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsObjectLockMode :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ObjectLockMode)
gorrsObjectLockMode = Lens.field @"objectLockMode"
{-# DEPRECATED gorrsObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time when this object's Object Lock will expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsObjectLockRetainUntilDate :: Lens.Lens' GetObjectResponse (Core.Maybe Core.UTCTime)
gorrsObjectLockRetainUntilDate = Lens.field @"objectLockRetainUntilDate"
{-# DEPRECATED gorrsObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | The count of parts this object has.
--
-- /Note:/ Consider using 'partsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsPartsCount :: Lens.Lens' GetObjectResponse (Core.Maybe Core.Int)
gorrsPartsCount = Lens.field @"partsCount"
{-# DEPRECATED gorrsPartsCount "Use generic-lens or generic-optics with 'partsCount' instead." #-}

-- | Amazon S3 can return this if your request involves a bucket that is either a source or destination in a replication rule.
--
-- /Note:/ Consider using 'replicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsReplicationStatus :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ReplicationStatus)
gorrsReplicationStatus = Lens.field @"replicationStatus"
{-# DEPRECATED gorrsReplicationStatus "Use generic-lens or generic-optics with 'replicationStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsRequestCharged :: Lens.Lens' GetObjectResponse (Core.Maybe Types.RequestCharged)
gorrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED gorrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Provides information about object restoration operation and expiration time of the restored object copy.
--
-- /Note:/ Consider using 'restore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsRestore :: Lens.Lens' GetObjectResponse (Core.Maybe Types.Restore)
gorrsRestore = Lens.field @"restore"
{-# DEPRECATED gorrsRestore "Use generic-lens or generic-optics with 'restore' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsSSECustomerAlgorithm :: Lens.Lens' GetObjectResponse (Core.Maybe Types.SSECustomerAlgorithm)
gorrsSSECustomerAlgorithm = Lens.field @"sSECustomerAlgorithm"
{-# DEPRECATED gorrsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsSSECustomerKeyMD5 :: Lens.Lens' GetObjectResponse (Core.Maybe Types.SSECustomerKeyMD5)
gorrsSSECustomerKeyMD5 = Lens.field @"sSECustomerKeyMD5"
{-# DEPRECATED gorrsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsSSEKMSKeyId :: Lens.Lens' GetObjectResponse (Core.Maybe Types.SSEKMSKeyId)
gorrsSSEKMSKeyId = Lens.field @"sSEKMSKeyId"
{-# DEPRECATED gorrsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsServerSideEncryption :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ServerSideEncryption)
gorrsServerSideEncryption = Lens.field @"serverSideEncryption"
{-# DEPRECATED gorrsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsStorageClass :: Lens.Lens' GetObjectResponse (Core.Maybe Types.StorageClass)
gorrsStorageClass = Lens.field @"storageClass"
{-# DEPRECATED gorrsStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The number of tags, if any, on the object.
--
-- /Note:/ Consider using 'tagCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsTagCount :: Lens.Lens' GetObjectResponse (Core.Maybe Core.Int)
gorrsTagCount = Lens.field @"tagCount"
{-# DEPRECATED gorrsTagCount "Use generic-lens or generic-optics with 'tagCount' instead." #-}

-- | Version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsVersionId :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ObjectVersionId)
gorrsVersionId = Lens.field @"versionId"
{-# DEPRECATED gorrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsWebsiteRedirectLocation :: Lens.Lens' GetObjectResponse (Core.Maybe Types.WebsiteRedirectLocation)
gorrsWebsiteRedirectLocation = Lens.field @"websiteRedirectLocation"
{-# DEPRECATED gorrsWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsResponseStatus :: Lens.Lens' GetObjectResponse Core.Int
gorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

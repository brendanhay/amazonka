{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    goIfMatch,
    goVersionId,
    goResponseContentType,
    goResponseContentDisposition,
    goResponseContentLanguage,
    goSSECustomerAlgorithm,
    goSSECustomerKey,
    goRequestPayer,
    goResponseContentEncoding,
    goIfModifiedSince,
    goPartNumber,
    goRange,
    goIfUnmodifiedSince,
    goSSECustomerKeyMD5,
    goResponseCacheControl,
    goResponseExpires,
    goIfNoneMatch,
    goExpectedBucketOwner,
    goBucket,
    goKey,

    -- * Destructuring the response
    GetObjectResponse (..),
    mkGetObjectResponse,

    -- ** Response lenses
    gorsRequestCharged,
    gorsPartsCount,
    gorsETag,
    gorsVersionId,
    gorsContentLength,
    gorsObjectLockMode,
    gorsExpires,
    gorsRestore,
    gorsExpiration,
    gorsDeleteMarker,
    gorsSSECustomerAlgorithm,
    gorsTagCount,
    gorsMissingMeta,
    gorsWebsiteRedirectLocation,
    gorsAcceptRanges,
    gorsStorageClass,
    gorsSSECustomerKeyMD5,
    gorsSSEKMSKeyId,
    gorsContentEncoding,
    gorsObjectLockRetainUntilDate,
    gorsMetadata,
    gorsReplicationStatus,
    gorsCacheControl,
    gorsContentLanguage,
    gorsLastModified,
    gorsObjectLockLegalHoldStatus,
    gorsContentDisposition,
    gorsContentRange,
    gorsServerSideEncryption,
    gorsContentType,
    gorsResponseStatus,
    gorsBody,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetObject' smart constructor.
data GetObject = GetObject'
  { ifMatch :: Lude.Maybe Lude.Text,
    versionId :: Lude.Maybe ObjectVersionId,
    responseContentType :: Lude.Maybe Lude.Text,
    responseContentDisposition :: Lude.Maybe Lude.Text,
    responseContentLanguage :: Lude.Maybe Lude.Text,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    sSECustomerKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestPayer :: Lude.Maybe RequestPayer,
    responseContentEncoding :: Lude.Maybe Lude.Text,
    ifModifiedSince :: Lude.Maybe Lude.DateTime,
    partNumber :: Lude.Maybe Lude.Int,
    range :: Lude.Maybe Lude.Text,
    ifUnmodifiedSince :: Lude.Maybe Lude.DateTime,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    responseCacheControl :: Lude.Maybe Lude.Text,
    responseExpires :: Lude.Maybe Lude.DateTime,
    ifNoneMatch :: Lude.Maybe Lude.Text,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    key :: ObjectKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObject' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'ifMatch' - Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
-- * 'ifModifiedSince' - Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
-- * 'ifNoneMatch' - Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
-- * 'ifUnmodifiedSince' - Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
-- * 'key' - Key of the object to get.
-- * 'partNumber' - Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
-- * 'range' - Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
-- * 'requestPayer' - Undocumented field.
-- * 'responseCacheControl' - Sets the @Cache-Control@ header of the response.
-- * 'responseContentDisposition' - Sets the @Content-Disposition@ header of the response
-- * 'responseContentEncoding' - Sets the @Content-Encoding@ header of the response.
-- * 'responseContentLanguage' - Sets the @Content-Language@ header of the response.
-- * 'responseContentType' - Sets the @Content-Type@ header of the response.
-- * 'responseExpires' - Sets the @Expires@ header of the response.
-- * 'sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
-- * 'sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
-- * 'sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
-- * 'versionId' - VersionId used to reference a specific version of the object.
mkGetObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObject
mkGetObject pBucket_ pKey_ =
  GetObject'
    { ifMatch = Lude.Nothing,
      versionId = Lude.Nothing,
      responseContentType = Lude.Nothing,
      responseContentDisposition = Lude.Nothing,
      responseContentLanguage = Lude.Nothing,
      sSECustomerAlgorithm = Lude.Nothing,
      sSECustomerKey = Lude.Nothing,
      requestPayer = Lude.Nothing,
      responseContentEncoding = Lude.Nothing,
      ifModifiedSince = Lude.Nothing,
      partNumber = Lude.Nothing,
      range = Lude.Nothing,
      ifUnmodifiedSince = Lude.Nothing,
      sSECustomerKeyMD5 = Lude.Nothing,
      responseCacheControl = Lude.Nothing,
      responseExpires = Lude.Nothing,
      ifNoneMatch = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfMatch :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goIfMatch = Lens.lens (ifMatch :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: GetObject)
{-# DEPRECATED goIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goVersionId :: Lens.Lens' GetObject (Lude.Maybe ObjectVersionId)
goVersionId = Lens.lens (versionId :: GetObject -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: GetObject)
{-# DEPRECATED goVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Sets the @Content-Type@ header of the response.
--
-- /Note:/ Consider using 'responseContentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentType :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goResponseContentType = Lens.lens (responseContentType :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {responseContentType = a} :: GetObject)
{-# DEPRECATED goResponseContentType "Use generic-lens or generic-optics with 'responseContentType' instead." #-}

-- | Sets the @Content-Disposition@ header of the response
--
-- /Note:/ Consider using 'responseContentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentDisposition :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goResponseContentDisposition = Lens.lens (responseContentDisposition :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {responseContentDisposition = a} :: GetObject)
{-# DEPRECATED goResponseContentDisposition "Use generic-lens or generic-optics with 'responseContentDisposition' instead." #-}

-- | Sets the @Content-Language@ header of the response.
--
-- /Note:/ Consider using 'responseContentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentLanguage :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goResponseContentLanguage = Lens.lens (responseContentLanguage :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {responseContentLanguage = a} :: GetObject)
{-# DEPRECATED goResponseContentLanguage "Use generic-lens or generic-optics with 'responseContentLanguage' instead." #-}

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goSSECustomerAlgorithm :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: GetObject)
{-# DEPRECATED goSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- /Note:/ Consider using 'sSECustomerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goSSECustomerKey :: Lens.Lens' GetObject (Lude.Maybe (Lude.Sensitive Lude.Text))
goSSECustomerKey = Lens.lens (sSECustomerKey :: GetObject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSECustomerKey = a} :: GetObject)
{-# DEPRECATED goSSECustomerKey "Use generic-lens or generic-optics with 'sSECustomerKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goRequestPayer :: Lens.Lens' GetObject (Lude.Maybe RequestPayer)
goRequestPayer = Lens.lens (requestPayer :: GetObject -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: GetObject)
{-# DEPRECATED goRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Sets the @Content-Encoding@ header of the response.
--
-- /Note:/ Consider using 'responseContentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseContentEncoding :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goResponseContentEncoding = Lens.lens (responseContentEncoding :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {responseContentEncoding = a} :: GetObject)
{-# DEPRECATED goResponseContentEncoding "Use generic-lens or generic-optics with 'responseContentEncoding' instead." #-}

-- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifModifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfModifiedSince :: Lens.Lens' GetObject (Lude.Maybe Lude.DateTime)
goIfModifiedSince = Lens.lens (ifModifiedSince :: GetObject -> Lude.Maybe Lude.DateTime) (\s a -> s {ifModifiedSince = a} :: GetObject)
{-# DEPRECATED goIfModifiedSince "Use generic-lens or generic-optics with 'ifModifiedSince' instead." #-}

-- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goPartNumber :: Lens.Lens' GetObject (Lude.Maybe Lude.Int)
goPartNumber = Lens.lens (partNumber :: GetObject -> Lude.Maybe Lude.Int) (\s a -> s {partNumber = a} :: GetObject)
{-# DEPRECATED goPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

-- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goRange :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goRange = Lens.lens (range :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {range = a} :: GetObject)
{-# DEPRECATED goRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
--
-- /Note:/ Consider using 'ifUnmodifiedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfUnmodifiedSince :: Lens.Lens' GetObject (Lude.Maybe Lude.DateTime)
goIfUnmodifiedSince = Lens.lens (ifUnmodifiedSince :: GetObject -> Lude.Maybe Lude.DateTime) (\s a -> s {ifUnmodifiedSince = a} :: GetObject)
{-# DEPRECATED goIfUnmodifiedSince "Use generic-lens or generic-optics with 'ifUnmodifiedSince' instead." #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goSSECustomerKeyMD5 :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: GetObject)
{-# DEPRECATED goSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | Sets the @Cache-Control@ header of the response.
--
-- /Note:/ Consider using 'responseCacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseCacheControl :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goResponseCacheControl = Lens.lens (responseCacheControl :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {responseCacheControl = a} :: GetObject)
{-# DEPRECATED goResponseCacheControl "Use generic-lens or generic-optics with 'responseCacheControl' instead." #-}

-- | Sets the @Expires@ header of the response.
--
-- /Note:/ Consider using 'responseExpires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goResponseExpires :: Lens.Lens' GetObject (Lude.Maybe Lude.DateTime)
goResponseExpires = Lens.lens (responseExpires :: GetObject -> Lude.Maybe Lude.DateTime) (\s a -> s {responseExpires = a} :: GetObject)
{-# DEPRECATED goResponseExpires "Use generic-lens or generic-optics with 'responseExpires' instead." #-}

-- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
--
-- /Note:/ Consider using 'ifNoneMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goIfNoneMatch :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goIfNoneMatch = Lens.lens (ifNoneMatch :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {ifNoneMatch = a} :: GetObject)
{-# DEPRECATED goIfNoneMatch "Use generic-lens or generic-optics with 'ifNoneMatch' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goExpectedBucketOwner :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetObject)
{-# DEPRECATED goExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name containing the object.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goBucket :: Lens.Lens' GetObject BucketName
goBucket = Lens.lens (bucket :: GetObject -> BucketName) (\s a -> s {bucket = a} :: GetObject)
{-# DEPRECATED goBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Key of the object to get.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goKey :: Lens.Lens' GetObject ObjectKey
goKey = Lens.lens (key :: GetObject -> ObjectKey) (\s a -> s {key = a} :: GetObject)
{-# DEPRECATED goKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest GetObject where
  type Rs GetObject = GetObjectResponse
  request = Req.get s3Service
  response =
    Res.receiveBody
      ( \s h x ->
          GetObjectResponse'
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
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption-customer-algorithm")
            Lude.<*> (h Lude..#? "x-amz-tagging-count")
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
            Lude.<*> (h Lude..#? "Content-Range")
            Lude.<*> (h Lude..#? "x-amz-server-side-encryption")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (Lude.pure x)
      )

instance Lude.ToHeaders GetObject where
  toHeaders GetObject' {..} =
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

instance Lude.ToPath GetObject where
  toPath GetObject' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery GetObject where
  toQuery GetObject' {..} =
    Lude.mconcat
      [ "versionId" Lude.=: versionId,
        "response-content-type" Lude.=: responseContentType,
        "response-content-disposition" Lude.=: responseContentDisposition,
        "response-content-language" Lude.=: responseContentLanguage,
        "response-content-encoding" Lude.=: responseContentEncoding,
        "partNumber" Lude.=: partNumber,
        "response-cache-control" Lude.=: responseCacheControl,
        "response-expires" Lude.=: responseExpires
      ]

-- | /See:/ 'mkGetObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    partsCount :: Lude.Maybe Lude.Int,
    eTag :: Lude.Maybe ETag,
    versionId :: Lude.Maybe ObjectVersionId,
    contentLength :: Lude.Maybe Lude.Integer,
    objectLockMode :: Lude.Maybe ObjectLockMode,
    expires :: Lude.Maybe Lude.DateTime,
    restore :: Lude.Maybe Lude.Text,
    expiration :: Lude.Maybe Lude.Text,
    deleteMarker :: Lude.Maybe Lude.Bool,
    sSECustomerAlgorithm :: Lude.Maybe Lude.Text,
    tagCount :: Lude.Maybe Lude.Int,
    missingMeta :: Lude.Maybe Lude.Int,
    websiteRedirectLocation :: Lude.Maybe Lude.Text,
    acceptRanges :: Lude.Maybe Lude.Text,
    storageClass :: Lude.Maybe StorageClass,
    sSECustomerKeyMD5 :: Lude.Maybe Lude.Text,
    sSEKMSKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    contentEncoding :: Lude.Maybe Lude.Text,
    objectLockRetainUntilDate :: Lude.Maybe Lude.DateTime,
    metadata :: Lude.HashMap Lude.Text (Lude.Text),
    replicationStatus :: Lude.Maybe ReplicationStatus,
    cacheControl :: Lude.Maybe Lude.Text,
    contentLanguage :: Lude.Maybe Lude.Text,
    lastModified :: Lude.Maybe Lude.DateTime,
    objectLockLegalHoldStatus ::
      Lude.Maybe ObjectLockLegalHoldStatus,
    contentDisposition :: Lude.Maybe Lude.Text,
    contentRange :: Lude.Maybe Lude.Text,
    serverSideEncryption :: Lude.Maybe ServerSideEncryption,
    contentType :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    body :: Lude.RsBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'GetObjectResponse' with the minimum fields required to make a request.
--
-- * 'acceptRanges' - Indicates that a range of bytes was specified.
-- * 'body' - Object data.
-- * 'cacheControl' - Specifies caching behavior along the request/reply chain.
-- * 'contentDisposition' - Specifies presentational information for the object.
-- * 'contentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
-- * 'contentLanguage' - The language the content is in.
-- * 'contentLength' - Size of the body in bytes.
-- * 'contentRange' - The portion of the object returned in the response.
-- * 'contentType' - A standard MIME type describing the format of the object data.
-- * 'deleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
-- * 'eTag' - An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
-- * 'expiration' - If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
-- * 'expires' - The date and time at which the object is no longer cacheable.
-- * 'lastModified' - Last modified date of the object
-- * 'metadata' - A map of metadata to store with the object in S3.
-- * 'missingMeta' - This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
-- * 'objectLockLegalHoldStatus' - Indicates whether this object has an active legal hold. This field is only returned if you have permission to view an object's legal hold status.
-- * 'objectLockMode' - The Object Lock mode currently in place for this object.
-- * 'objectLockRetainUntilDate' - The date and time when this object's Object Lock will expire.
-- * 'partsCount' - The count of parts this object has.
-- * 'replicationStatus' - Amazon S3 can return this if your request involves a bucket that is either a source or destination in a replication rule.
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'restore' - Provides information about object restoration operation and expiration time of the restored object copy.
-- * 'sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
-- * 'sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
-- * 'sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
-- * 'serverSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
-- * 'storageClass' - Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
-- * 'tagCount' - The number of tags, if any, on the object.
-- * 'versionId' - Version of the object.
-- * 'websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
mkGetObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'body'
  Lude.RsBody ->
  GetObjectResponse
mkGetObjectResponse pResponseStatus_ pBody_ =
  GetObjectResponse'
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
      sSECustomerAlgorithm = Lude.Nothing,
      tagCount = Lude.Nothing,
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
      contentRange = Lude.Nothing,
      serverSideEncryption = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_,
      body = pBody_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsRequestCharged :: Lens.Lens' GetObjectResponse (Lude.Maybe RequestCharged)
gorsRequestCharged = Lens.lens (requestCharged :: GetObjectResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: GetObjectResponse)
{-# DEPRECATED gorsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The count of parts this object has.
--
-- /Note:/ Consider using 'partsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsPartsCount :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Int)
gorsPartsCount = Lens.lens (partsCount :: GetObjectResponse -> Lude.Maybe Lude.Int) (\s a -> s {partsCount = a} :: GetObjectResponse)
{-# DEPRECATED gorsPartsCount "Use generic-lens or generic-optics with 'partsCount' instead." #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsETag :: Lens.Lens' GetObjectResponse (Lude.Maybe ETag)
gorsETag = Lens.lens (eTag :: GetObjectResponse -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: GetObjectResponse)
{-# DEPRECATED gorsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsVersionId :: Lens.Lens' GetObjectResponse (Lude.Maybe ObjectVersionId)
gorsVersionId = Lens.lens (versionId :: GetObjectResponse -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: GetObjectResponse)
{-# DEPRECATED gorsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Size of the body in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentLength :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Integer)
gorsContentLength = Lens.lens (contentLength :: GetObjectResponse -> Lude.Maybe Lude.Integer) (\s a -> s {contentLength = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The Object Lock mode currently in place for this object.
--
-- /Note:/ Consider using 'objectLockMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsObjectLockMode :: Lens.Lens' GetObjectResponse (Lude.Maybe ObjectLockMode)
gorsObjectLockMode = Lens.lens (objectLockMode :: GetObjectResponse -> Lude.Maybe ObjectLockMode) (\s a -> s {objectLockMode = a} :: GetObjectResponse)
{-# DEPRECATED gorsObjectLockMode "Use generic-lens or generic-optics with 'objectLockMode' instead." #-}

-- | The date and time at which the object is no longer cacheable.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsExpires :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.DateTime)
gorsExpires = Lens.lens (expires :: GetObjectResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {expires = a} :: GetObjectResponse)
{-# DEPRECATED gorsExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | Provides information about object restoration operation and expiration time of the restored object copy.
--
-- /Note:/ Consider using 'restore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsRestore :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsRestore = Lens.lens (restore :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {restore = a} :: GetObjectResponse)
{-# DEPRECATED gorsRestore "Use generic-lens or generic-optics with 'restore' instead." #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsExpiration :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsExpiration = Lens.lens (expiration :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {expiration = a} :: GetObjectResponse)
{-# DEPRECATED gorsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsDeleteMarker :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Bool)
gorsDeleteMarker = Lens.lens (deleteMarker :: GetObjectResponse -> Lude.Maybe Lude.Bool) (\s a -> s {deleteMarker = a} :: GetObjectResponse)
{-# DEPRECATED gorsDeleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- /Note:/ Consider using 'sSECustomerAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsSSECustomerAlgorithm :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsSSECustomerAlgorithm = Lens.lens (sSECustomerAlgorithm :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerAlgorithm = a} :: GetObjectResponse)
{-# DEPRECATED gorsSSECustomerAlgorithm "Use generic-lens or generic-optics with 'sSECustomerAlgorithm' instead." #-}

-- | The number of tags, if any, on the object.
--
-- /Note:/ Consider using 'tagCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsTagCount :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Int)
gorsTagCount = Lens.lens (tagCount :: GetObjectResponse -> Lude.Maybe Lude.Int) (\s a -> s {tagCount = a} :: GetObjectResponse)
{-# DEPRECATED gorsTagCount "Use generic-lens or generic-optics with 'tagCount' instead." #-}

-- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
--
-- /Note:/ Consider using 'missingMeta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsMissingMeta :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Int)
gorsMissingMeta = Lens.lens (missingMeta :: GetObjectResponse -> Lude.Maybe Lude.Int) (\s a -> s {missingMeta = a} :: GetObjectResponse)
{-# DEPRECATED gorsMissingMeta "Use generic-lens or generic-optics with 'missingMeta' instead." #-}

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- /Note:/ Consider using 'websiteRedirectLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsWebsiteRedirectLocation :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsWebsiteRedirectLocation = Lens.lens (websiteRedirectLocation :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {websiteRedirectLocation = a} :: GetObjectResponse)
{-# DEPRECATED gorsWebsiteRedirectLocation "Use generic-lens or generic-optics with 'websiteRedirectLocation' instead." #-}

-- | Indicates that a range of bytes was specified.
--
-- /Note:/ Consider using 'acceptRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsAcceptRanges :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsAcceptRanges = Lens.lens (acceptRanges :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {acceptRanges = a} :: GetObjectResponse)
{-# DEPRECATED gorsAcceptRanges "Use generic-lens or generic-optics with 'acceptRanges' instead." #-}

-- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsStorageClass :: Lens.Lens' GetObjectResponse (Lude.Maybe StorageClass)
gorsStorageClass = Lens.lens (storageClass :: GetObjectResponse -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: GetObjectResponse)
{-# DEPRECATED gorsStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- /Note:/ Consider using 'sSECustomerKeyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsSSECustomerKeyMD5 :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsSSECustomerKeyMD5 = Lens.lens (sSECustomerKeyMD5 :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {sSECustomerKeyMD5 = a} :: GetObjectResponse)
{-# DEPRECATED gorsSSECustomerKeyMD5 "Use generic-lens or generic-optics with 'sSECustomerKeyMD5' instead." #-}

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- /Note:/ Consider using 'sSEKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsSSEKMSKeyId :: Lens.Lens' GetObjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gorsSSEKMSKeyId = Lens.lens (sSEKMSKeyId :: GetObjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sSEKMSKeyId = a} :: GetObjectResponse)
{-# DEPRECATED gorsSSEKMSKeyId "Use generic-lens or generic-optics with 'sSEKMSKeyId' instead." #-}

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentEncoding :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsContentEncoding = Lens.lens (contentEncoding :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentEncoding = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

-- | The date and time when this object's Object Lock will expire.
--
-- /Note:/ Consider using 'objectLockRetainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsObjectLockRetainUntilDate :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.DateTime)
gorsObjectLockRetainUntilDate = Lens.lens (objectLockRetainUntilDate :: GetObjectResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {objectLockRetainUntilDate = a} :: GetObjectResponse)
{-# DEPRECATED gorsObjectLockRetainUntilDate "Use generic-lens or generic-optics with 'objectLockRetainUntilDate' instead." #-}

-- | A map of metadata to store with the object in S3.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsMetadata :: Lens.Lens' GetObjectResponse (Lude.HashMap Lude.Text (Lude.Text))
gorsMetadata = Lens.lens (metadata :: GetObjectResponse -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {metadata = a} :: GetObjectResponse)
{-# DEPRECATED gorsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Amazon S3 can return this if your request involves a bucket that is either a source or destination in a replication rule.
--
-- /Note:/ Consider using 'replicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsReplicationStatus :: Lens.Lens' GetObjectResponse (Lude.Maybe ReplicationStatus)
gorsReplicationStatus = Lens.lens (replicationStatus :: GetObjectResponse -> Lude.Maybe ReplicationStatus) (\s a -> s {replicationStatus = a} :: GetObjectResponse)
{-# DEPRECATED gorsReplicationStatus "Use generic-lens or generic-optics with 'replicationStatus' instead." #-}

-- | Specifies caching behavior along the request/reply chain.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsCacheControl :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsCacheControl = Lens.lens (cacheControl :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: GetObjectResponse)
{-# DEPRECATED gorsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The language the content is in.
--
-- /Note:/ Consider using 'contentLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentLanguage :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsContentLanguage = Lens.lens (contentLanguage :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentLanguage = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentLanguage "Use generic-lens or generic-optics with 'contentLanguage' instead." #-}

-- | Last modified date of the object
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsLastModified :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.DateTime)
gorsLastModified = Lens.lens (lastModified :: GetObjectResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: GetObjectResponse)
{-# DEPRECATED gorsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Indicates whether this object has an active legal hold. This field is only returned if you have permission to view an object's legal hold status.
--
-- /Note:/ Consider using 'objectLockLegalHoldStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsObjectLockLegalHoldStatus :: Lens.Lens' GetObjectResponse (Lude.Maybe ObjectLockLegalHoldStatus)
gorsObjectLockLegalHoldStatus = Lens.lens (objectLockLegalHoldStatus :: GetObjectResponse -> Lude.Maybe ObjectLockLegalHoldStatus) (\s a -> s {objectLockLegalHoldStatus = a} :: GetObjectResponse)
{-# DEPRECATED gorsObjectLockLegalHoldStatus "Use generic-lens or generic-optics with 'objectLockLegalHoldStatus' instead." #-}

-- | Specifies presentational information for the object.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentDisposition :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsContentDisposition = Lens.lens (contentDisposition :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentDisposition = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead." #-}

-- | The portion of the object returned in the response.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentRange :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsContentRange = Lens.lens (contentRange :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentRange = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentRange "Use generic-lens or generic-optics with 'contentRange' instead." #-}

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'serverSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsServerSideEncryption :: Lens.Lens' GetObjectResponse (Lude.Maybe ServerSideEncryption)
gorsServerSideEncryption = Lens.lens (serverSideEncryption :: GetObjectResponse -> Lude.Maybe ServerSideEncryption) (\s a -> s {serverSideEncryption = a} :: GetObjectResponse)
{-# DEPRECATED gorsServerSideEncryption "Use generic-lens or generic-optics with 'serverSideEncryption' instead." #-}

-- | A standard MIME type describing the format of the object data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentType :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsContentType = Lens.lens (contentType :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsResponseStatus :: Lens.Lens' GetObjectResponse Lude.Int
gorsResponseStatus = Lens.lens (responseStatus :: GetObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectResponse)
{-# DEPRECATED gorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Object data.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsBody :: Lens.Lens' GetObjectResponse Lude.RsBody
gorsBody = Lens.lens (body :: GetObjectResponse -> Lude.RsBody) (\s a -> s {body = a} :: GetObjectResponse)
{-# DEPRECATED gorsBody "Use generic-lens or generic-optics with 'body' instead." #-}

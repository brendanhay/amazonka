{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves objects from Amazon S3. To use @GET@, you must have @READ@
-- access to the object. If you grant @READ@ access to the anonymous user,
-- you can return the object without using an authorization header.
--
-- An Amazon S3 bucket has no directory hierarchy such as you would find in
-- a typical computer file system. You can, however, create a logical
-- hierarchy by using object key names that imply a folder structure. For
-- example, instead of naming an object @sample.jpg@, you can name it
-- @photos\/2006\/February\/sample.jpg@.
--
-- To get an object from such a logical hierarchy, specify the full key
-- name for the object in the @GET@ operation. For a virtual hosted-style
-- request example, if you have the object
-- @photos\/2006\/February\/sample.jpg@, specify the resource as
-- @\/photos\/2006\/February\/sample.jpg@. For a path-style request
-- example, if you have the object @photos\/2006\/February\/sample.jpg@ in
-- the bucket named @examplebucket@, specify the resource as
-- @\/examplebucket\/photos\/2006\/February\/sample.jpg@. For more
-- information about request types, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html#VirtualHostingSpecifyBucket HTTP Host Header Bucket Specification>.
--
-- To distribute large files to many people, you can save bandwidth costs
-- by using BitTorrent. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3Torrent.html Amazon S3 Torrent>.
-- For more information about returning the ACL of an object, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>.
--
-- If the object you are retrieving is stored in the S3 Glacier or S3
-- Glacier Deep Archive storage class, or S3 Intelligent-Tiering Archive or
-- S3 Intelligent-Tiering Deep Archive tiers, before you can retrieve the
-- object you must first restore a copy using
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject>.
-- Otherwise, this action returns an @InvalidObjectStateError@ error. For
-- information about restoring archived objects, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/restoring-objects.html Restoring Archived Objects>.
--
-- Encryption request headers, like @x-amz-server-side-encryption@, should
-- not be sent for GET requests if your object uses server-side encryption
-- with KMS keys (SSE-KMS) or server-side encryption with Amazon S3–managed
-- encryption keys (SSE-S3). If your object does use these types of keys,
-- you’ll get an HTTP 400 BadRequest error.
--
-- If you encrypt an object by using server-side encryption with
-- customer-provided encryption keys (SSE-C) when you store the object in
-- Amazon S3, then when you GET the object, you must use the following
-- headers:
--
-- -   x-amz-server-side-encryption-customer-algorithm
--
-- -   x-amz-server-side-encryption-customer-key
--
-- -   x-amz-server-side-encryption-customer-key-MD5
--
-- For more information about SSE-C, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys)>.
--
-- Assuming you have the relevant permission to read object tags, the
-- response also returns the @x-amz-tagging-count@ header that provides the
-- count of number of tags associated with the object. You can use
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>
-- to retrieve the tag set associated with an object.
--
-- __Permissions__
--
-- You need the relevant read object (or version) permission for this
-- operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
-- If the object you request does not exist, the error Amazon S3 returns
-- depends on whether you also have the @s3:ListBucket@ permission.
--
-- -   If you have the @s3:ListBucket@ permission on the bucket, Amazon S3
--     will return an HTTP status code 404 (\"no such key\") error.
--
-- -   If you don’t have the @s3:ListBucket@ permission, Amazon S3 will
--     return an HTTP status code 403 (\"access denied\") error.
--
-- __Versioning__
--
-- By default, the GET action returns the current version of an object. To
-- return a different version, use the @versionId@ subresource.
--
-- -   You need the @s3:GetObjectVersion@ permission to access a specific
--     version of an object.
--
-- -   If the current version of the object is a delete marker, Amazon S3
--     behaves as if the object was deleted and includes
--     @x-amz-delete-marker: true@ in the response.
--
-- For more information about versioning, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketVersioning.html PutBucketVersioning>.
--
-- __Overriding Response Header Values__
--
-- There are times when you want to override certain response header values
-- in a GET response. For example, you might override the
-- Content-Disposition response header value in your GET request.
--
-- You can override values for a set of response headers using the
-- following query parameters. These response header values are sent only
-- on a successful request, that is, when status code 200 OK is returned.
-- The set of headers you can override using these parameters is a subset
-- of the headers that Amazon S3 accepts when you create an object. The
-- response headers that you can override for the GET response are
-- @Content-Type@, @Content-Language@, @Expires@, @Cache-Control@,
-- @Content-Disposition@, and @Content-Encoding@. To override these header
-- values in the GET response, you use the following request parameters.
--
-- You must sign the request, either using an Authorization header or a
-- presigned URL, when using these parameters. They cannot be used with an
-- unsigned (anonymous) request.
--
-- -   @response-content-type@
--
-- -   @response-content-language@
--
-- -   @response-expires@
--
-- -   @response-cache-control@
--
-- -   @response-content-disposition@
--
-- -   @response-content-encoding@
--
-- __Additional Considerations about Request Headers__
--
-- If both of the @If-Match@ and @If-Unmodified-Since@ headers are present
-- in the request as follows: @If-Match@ condition evaluates to @true@,
-- and; @If-Unmodified-Since@ condition evaluates to @false@; then, S3
-- returns 200 OK and the data requested.
--
-- If both of the @If-None-Match@ and @If-Modified-Since@ headers are
-- present in the request as follows:@ If-None-Match@ condition evaluates
-- to @false@, and; @If-Modified-Since@ condition evaluates to @true@;
-- then, S3 returns 304 Not Modified response code.
--
-- For more information about conditional requests, see
-- <https://tools.ietf.org/html/rfc7232 RFC 7232>.
--
-- The following operations are related to @GetObject@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
module Network.AWS.S3.GetObject
  ( -- * Creating a Request
    GetObject (..),
    newGetObject,

    -- * Request Lenses
    getObject_ifUnmodifiedSince,
    getObject_range,
    getObject_ifModifiedSince,
    getObject_expectedBucketOwner,
    getObject_responseExpires,
    getObject_sSECustomerKeyMD5,
    getObject_versionId,
    getObject_ifMatch,
    getObject_partNumber,
    getObject_responseContentEncoding,
    getObject_sSECustomerKey,
    getObject_sSECustomerAlgorithm,
    getObject_ifNoneMatch,
    getObject_requestPayer,
    getObject_responseContentLanguage,
    getObject_responseCacheControl,
    getObject_responseContentDisposition,
    getObject_responseContentType,
    getObject_bucket,
    getObject_key,

    -- * Destructuring the Response
    GetObjectResponse (..),
    newGetObjectResponse,

    -- * Response Lenses
    getObjectResponse_partsCount,
    getObjectResponse_requestCharged,
    getObjectResponse_eTag,
    getObjectResponse_websiteRedirectLocation,
    getObjectResponse_tagCount,
    getObjectResponse_contentType,
    getObjectResponse_contentDisposition,
    getObjectResponse_contentRange,
    getObjectResponse_deleteMarker,
    getObjectResponse_contentLanguage,
    getObjectResponse_expiration,
    getObjectResponse_replicationStatus,
    getObjectResponse_contentLength,
    getObjectResponse_metadata,
    getObjectResponse_sSECustomerKeyMD5,
    getObjectResponse_sSEKMSKeyId,
    getObjectResponse_contentEncoding,
    getObjectResponse_versionId,
    getObjectResponse_storageClass,
    getObjectResponse_bucketKeyEnabled,
    getObjectResponse_acceptRanges,
    getObjectResponse_missingMeta,
    getObjectResponse_serverSideEncryption,
    getObjectResponse_sSECustomerAlgorithm,
    getObjectResponse_objectLockLegalHoldStatus,
    getObjectResponse_cacheControl,
    getObjectResponse_lastModified,
    getObjectResponse_expires,
    getObjectResponse_restore,
    getObjectResponse_objectLockMode,
    getObjectResponse_objectLockRetainUntilDate,
    getObjectResponse_httpStatus,
    getObjectResponse_body,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetObject' smart constructor.
data GetObject = GetObject'
  { -- | Return the object only if it has not been modified since the specified
    -- time, otherwise return a 412 (precondition failed).
    ifUnmodifiedSince :: Prelude.Maybe Core.ISO8601,
    -- | Downloads the specified range bytes of an object. For more information
    -- about the HTTP Range header, see
    -- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
    --
    -- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
    -- request.
    range :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if it has been modified since the specified time,
    -- otherwise return a 304 (not modified).
    ifModifiedSince :: Prelude.Maybe Core.ISO8601,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Expires@ header of the response.
    responseExpires :: Prelude.Maybe Core.ISO8601,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Return the object only if its entity tag (ETag) is the same as the one
    -- specified, otherwise return a 412 (precondition failed).
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | Part number of the object being read. This is a positive integer between
    -- 1 and 10,000. Effectively performs a \'ranged\' GET request for the part
    -- specified. Useful for downloading just a part of an object.
    partNumber :: Prelude.Maybe Prelude.Int,
    -- | Sets the @Content-Encoding@ header of the response.
    responseContentEncoding :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 used to
    -- encrypt the data. This value is used to decrypt the object when
    -- recovering it and must match the one used when storing the data. The key
    -- must be appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Specifies the algorithm to use to when decrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if its entity tag (ETag) is different from the
    -- one specified, otherwise return a 304 (not modified).
    ifNoneMatch :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Sets the @Content-Language@ header of the response.
    responseContentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Cache-Control@ header of the response.
    responseCacheControl :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Content-Disposition@ header of the response
    responseContentDisposition :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Content-Type@ header of the response.
    responseContentType :: Prelude.Maybe Prelude.Text,
    -- | The bucket name containing the object.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- When using an Object Lambda access point the hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-object-lambda./Region/.amazonaws.com.
    --
    -- When using this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this action using S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | Key of the object to get.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifUnmodifiedSince', 'getObject_ifUnmodifiedSince' - Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
--
-- 'range', 'getObject_range' - Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
--
-- 'ifModifiedSince', 'getObject_ifModifiedSince' - Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
--
-- 'expectedBucketOwner', 'getObject_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'responseExpires', 'getObject_responseExpires' - Sets the @Expires@ header of the response.
--
-- 'sSECustomerKeyMD5', 'getObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'versionId', 'getObject_versionId' - VersionId used to reference a specific version of the object.
--
-- 'ifMatch', 'getObject_ifMatch' - Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
--
-- 'partNumber', 'getObject_partNumber' - Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' GET request for the part
-- specified. Useful for downloading just a part of an object.
--
-- 'responseContentEncoding', 'getObject_responseContentEncoding' - Sets the @Content-Encoding@ header of the response.
--
-- 'sSECustomerKey', 'getObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 used to
-- encrypt the data. This value is used to decrypt the object when
-- recovering it and must match the one used when storing the data. The key
-- must be appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'sSECustomerAlgorithm', 'getObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when decrypting the object (for
-- example, AES256).
--
-- 'ifNoneMatch', 'getObject_ifNoneMatch' - Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
--
-- 'requestPayer', 'getObject_requestPayer' - Undocumented member.
--
-- 'responseContentLanguage', 'getObject_responseContentLanguage' - Sets the @Content-Language@ header of the response.
--
-- 'responseCacheControl', 'getObject_responseCacheControl' - Sets the @Cache-Control@ header of the response.
--
-- 'responseContentDisposition', 'getObject_responseContentDisposition' - Sets the @Content-Disposition@ header of the response
--
-- 'responseContentType', 'getObject_responseContentType' - Sets the @Content-Type@ header of the response.
--
-- 'bucket', 'getObject_bucket' - The bucket name containing the object.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using an Object Lambda access point the hostname takes the form
-- /AccessPointName/-/AccountId/.s3-object-lambda./Region/.amazonaws.com.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'key', 'getObject_key' - Key of the object to get.
newGetObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObject
newGetObject pBucket_ pKey_ =
  GetObject'
    { ifUnmodifiedSince = Prelude.Nothing,
      range = Prelude.Nothing,
      ifModifiedSince = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      responseExpires = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      versionId = Prelude.Nothing,
      ifMatch = Prelude.Nothing,
      partNumber = Prelude.Nothing,
      responseContentEncoding = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      ifNoneMatch = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      responseContentLanguage = Prelude.Nothing,
      responseCacheControl = Prelude.Nothing,
      responseContentDisposition = Prelude.Nothing,
      responseContentType = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
getObject_ifUnmodifiedSince :: Lens.Lens' GetObject (Prelude.Maybe Prelude.UTCTime)
getObject_ifUnmodifiedSince = Lens.lens (\GetObject' {ifUnmodifiedSince} -> ifUnmodifiedSince) (\s@GetObject' {} a -> s {ifUnmodifiedSince = a} :: GetObject) Prelude.. Lens.mapping Core._Time

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
getObject_range :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_range = Lens.lens (\GetObject' {range} -> range) (\s@GetObject' {} a -> s {range = a} :: GetObject)

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
getObject_ifModifiedSince :: Lens.Lens' GetObject (Prelude.Maybe Prelude.UTCTime)
getObject_ifModifiedSince = Lens.lens (\GetObject' {ifModifiedSince} -> ifModifiedSince) (\s@GetObject' {} a -> s {ifModifiedSince = a} :: GetObject) Prelude.. Lens.mapping Core._Time

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getObject_expectedBucketOwner :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_expectedBucketOwner = Lens.lens (\GetObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObject' {} a -> s {expectedBucketOwner = a} :: GetObject)

-- | Sets the @Expires@ header of the response.
getObject_responseExpires :: Lens.Lens' GetObject (Prelude.Maybe Prelude.UTCTime)
getObject_responseExpires = Lens.lens (\GetObject' {responseExpires} -> responseExpires) (\s@GetObject' {} a -> s {responseExpires = a} :: GetObject) Prelude.. Lens.mapping Core._Time

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
getObject_sSECustomerKeyMD5 :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_sSECustomerKeyMD5 = Lens.lens (\GetObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@GetObject' {} a -> s {sSECustomerKeyMD5 = a} :: GetObject)

-- | VersionId used to reference a specific version of the object.
getObject_versionId :: Lens.Lens' GetObject (Prelude.Maybe ObjectVersionId)
getObject_versionId = Lens.lens (\GetObject' {versionId} -> versionId) (\s@GetObject' {} a -> s {versionId = a} :: GetObject)

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
getObject_ifMatch :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_ifMatch = Lens.lens (\GetObject' {ifMatch} -> ifMatch) (\s@GetObject' {} a -> s {ifMatch = a} :: GetObject)

-- | Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' GET request for the part
-- specified. Useful for downloading just a part of an object.
getObject_partNumber :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Int)
getObject_partNumber = Lens.lens (\GetObject' {partNumber} -> partNumber) (\s@GetObject' {} a -> s {partNumber = a} :: GetObject)

-- | Sets the @Content-Encoding@ header of the response.
getObject_responseContentEncoding :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentEncoding = Lens.lens (\GetObject' {responseContentEncoding} -> responseContentEncoding) (\s@GetObject' {} a -> s {responseContentEncoding = a} :: GetObject)

-- | Specifies the customer-provided encryption key for Amazon S3 used to
-- encrypt the data. This value is used to decrypt the object when
-- recovering it and must match the one used when storing the data. The key
-- must be appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
getObject_sSECustomerKey :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_sSECustomerKey = Lens.lens (\GetObject' {sSECustomerKey} -> sSECustomerKey) (\s@GetObject' {} a -> s {sSECustomerKey = a} :: GetObject) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies the algorithm to use to when decrypting the object (for
-- example, AES256).
getObject_sSECustomerAlgorithm :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_sSECustomerAlgorithm = Lens.lens (\GetObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@GetObject' {} a -> s {sSECustomerAlgorithm = a} :: GetObject)

-- | Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
getObject_ifNoneMatch :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_ifNoneMatch = Lens.lens (\GetObject' {ifNoneMatch} -> ifNoneMatch) (\s@GetObject' {} a -> s {ifNoneMatch = a} :: GetObject)

-- | Undocumented member.
getObject_requestPayer :: Lens.Lens' GetObject (Prelude.Maybe RequestPayer)
getObject_requestPayer = Lens.lens (\GetObject' {requestPayer} -> requestPayer) (\s@GetObject' {} a -> s {requestPayer = a} :: GetObject)

-- | Sets the @Content-Language@ header of the response.
getObject_responseContentLanguage :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentLanguage = Lens.lens (\GetObject' {responseContentLanguage} -> responseContentLanguage) (\s@GetObject' {} a -> s {responseContentLanguage = a} :: GetObject)

-- | Sets the @Cache-Control@ header of the response.
getObject_responseCacheControl :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseCacheControl = Lens.lens (\GetObject' {responseCacheControl} -> responseCacheControl) (\s@GetObject' {} a -> s {responseCacheControl = a} :: GetObject)

-- | Sets the @Content-Disposition@ header of the response
getObject_responseContentDisposition :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentDisposition = Lens.lens (\GetObject' {responseContentDisposition} -> responseContentDisposition) (\s@GetObject' {} a -> s {responseContentDisposition = a} :: GetObject)

-- | Sets the @Content-Type@ header of the response.
getObject_responseContentType :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentType = Lens.lens (\GetObject' {responseContentType} -> responseContentType) (\s@GetObject' {} a -> s {responseContentType = a} :: GetObject)

-- | The bucket name containing the object.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using an Object Lambda access point the hostname takes the form
-- /AccessPointName/-/AccountId/.s3-object-lambda./Region/.amazonaws.com.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
getObject_bucket :: Lens.Lens' GetObject BucketName
getObject_bucket = Lens.lens (\GetObject' {bucket} -> bucket) (\s@GetObject' {} a -> s {bucket = a} :: GetObject)

-- | Key of the object to get.
getObject_key :: Lens.Lens' GetObject ObjectKey
getObject_key = Lens.lens (\GetObject' {key} -> key) (\s@GetObject' {} a -> s {key = a} :: GetObject)

instance Core.AWSRequest GetObject where
  type AWSResponse GetObject = GetObjectResponse
  request = Request.get defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          GetObjectResponse'
            Prelude.<$> (h Core..#? "x-amz-mp-parts-count")
            Prelude.<*> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> (h Core..#? "x-amz-website-redirect-location")
            Prelude.<*> (h Core..#? "x-amz-tagging-count")
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (h Core..#? "Content-Disposition")
            Prelude.<*> (h Core..#? "Content-Range")
            Prelude.<*> (h Core..#? "x-amz-delete-marker")
            Prelude.<*> (h Core..#? "Content-Language")
            Prelude.<*> (h Core..#? "x-amz-expiration")
            Prelude.<*> (h Core..#? "x-amz-replication-status")
            Prelude.<*> (h Core..#? "Content-Length")
            Prelude.<*> (Core.parseHeadersMap "x-amz-meta-" h)
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Core..#? "Content-Encoding")
            Prelude.<*> (h Core..#? "x-amz-version-id")
            Prelude.<*> (h Core..#? "x-amz-storage-class")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Core..#? "accept-ranges")
            Prelude.<*> (h Core..#? "x-amz-missing-meta")
            Prelude.<*> (h Core..#? "x-amz-server-side-encryption")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> (h Core..#? "x-amz-object-lock-legal-hold")
            Prelude.<*> (h Core..#? "Cache-Control")
            Prelude.<*> (h Core..#? "Last-Modified")
            Prelude.<*> (h Core..#? "Expires")
            Prelude.<*> (h Core..#? "x-amz-restore")
            Prelude.<*> (h Core..#? "x-amz-object-lock-mode")
            Prelude.<*> (h Core..#? "x-amz-object-lock-retain-until-date")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetObject

instance Prelude.NFData GetObject

instance Core.ToHeaders GetObject where
  toHeaders GetObject' {..} =
    Prelude.mconcat
      [ "If-Unmodified-Since" Core.=# ifUnmodifiedSince,
        "Range" Core.=# range,
        "If-Modified-Since" Core.=# ifModifiedSince,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "If-Match" Core.=# ifMatch,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "If-None-Match" Core.=# ifNoneMatch,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath GetObject where
  toPath GetObject' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery GetObject where
  toQuery GetObject' {..} =
    Prelude.mconcat
      [ "response-expires" Core.=: responseExpires,
        "versionId" Core.=: versionId,
        "partNumber" Core.=: partNumber,
        "response-content-encoding"
          Core.=: responseContentEncoding,
        "response-content-language"
          Core.=: responseContentLanguage,
        "response-cache-control"
          Core.=: responseCacheControl,
        "response-content-disposition"
          Core.=: responseContentDisposition,
        "response-content-type" Core.=: responseContentType
      ]

-- | /See:/ 'newGetObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { -- | The count of parts this object has.
    partsCount :: Prelude.Maybe Prelude.Int,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | An ETag is an opaque identifier assigned by a web server to a specific
    -- version of a resource found at a URL.
    eTag :: Prelude.Maybe ETag,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Prelude.Maybe Prelude.Text,
    -- | The number of tags, if any, on the object.
    tagCount :: Prelude.Maybe Prelude.Int,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | Specifies presentational information for the object.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The portion of the object returned in the response.
    contentRange :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the object retrieved was (true) or was not (false) a
    -- Delete Marker. If false, this response header does not appear in the
    -- response.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | If the object expiration is configured (see PUT Bucket lifecycle), the
    -- response includes this header. It includes the expiry-date and rule-id
    -- key-value pairs providing object expiration information. The value of
    -- the rule-id is URL encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | Amazon S3 can return this if your request involves a bucket that is
    -- either a source or destination in a replication rule.
    replicationStatus :: Prelude.Maybe ReplicationStatus,
    -- | Size of the body in bytes.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the ID of the Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) symmetric customer managed key that
    -- was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | Version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Provides storage class information of the object. Amazon S3 returns this
    -- header for all objects except for S3 Standard storage class objects.
    storageClass :: Prelude.Maybe StorageClass,
    -- | Indicates whether the object uses an S3 Bucket Key for server-side
    -- encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that a range of bytes was specified.
    acceptRanges :: Prelude.Maybe Prelude.Text,
    -- | This is set to the number of metadata entries not returned in
    -- @x-amz-meta@ headers. This can happen if you create metadata using an
    -- API like SOAP that supports more flexible metadata than the REST API.
    -- For example, using SOAP, you can create metadata whose values are not
    -- legal HTTP headers.
    missingMeta :: Prelude.Maybe Prelude.Int,
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this object has an active legal hold. This field is
    -- only returned if you have permission to view an object\'s legal hold
    -- status.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | Specifies caching behavior along the request\/reply chain.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | Creation date of the object.
    lastModified :: Prelude.Maybe Core.ISO8601,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Prelude.Maybe Core.ISO8601,
    -- | Provides information about object restoration action and expiration time
    -- of the restored object copy.
    restore :: Prelude.Maybe Prelude.Text,
    -- | The Object Lock mode currently in place for this object.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time when this object\'s Object Lock will expire.
    objectLockRetainUntilDate :: Prelude.Maybe Core.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Object data.
    body :: Core.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partsCount', 'getObjectResponse_partsCount' - The count of parts this object has.
--
-- 'requestCharged', 'getObjectResponse_requestCharged' - Undocumented member.
--
-- 'eTag', 'getObjectResponse_eTag' - An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
--
-- 'websiteRedirectLocation', 'getObjectResponse_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'tagCount', 'getObjectResponse_tagCount' - The number of tags, if any, on the object.
--
-- 'contentType', 'getObjectResponse_contentType' - A standard MIME type describing the format of the object data.
--
-- 'contentDisposition', 'getObjectResponse_contentDisposition' - Specifies presentational information for the object.
--
-- 'contentRange', 'getObjectResponse_contentRange' - The portion of the object returned in the response.
--
-- 'deleteMarker', 'getObjectResponse_deleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
--
-- 'contentLanguage', 'getObjectResponse_contentLanguage' - The language the content is in.
--
-- 'expiration', 'getObjectResponse_expiration' - If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key-value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
--
-- 'replicationStatus', 'getObjectResponse_replicationStatus' - Amazon S3 can return this if your request involves a bucket that is
-- either a source or destination in a replication rule.
--
-- 'contentLength', 'getObjectResponse_contentLength' - Size of the body in bytes.
--
-- 'metadata', 'getObjectResponse_metadata' - A map of metadata to store with the object in S3.
--
-- 'sSECustomerKeyMD5', 'getObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSKeyId', 'getObjectResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
--
-- 'contentEncoding', 'getObjectResponse_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'versionId', 'getObjectResponse_versionId' - Version of the object.
--
-- 'storageClass', 'getObjectResponse_storageClass' - Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
--
-- 'bucketKeyEnabled', 'getObjectResponse_bucketKeyEnabled' - Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'acceptRanges', 'getObjectResponse_acceptRanges' - Indicates that a range of bytes was specified.
--
-- 'missingMeta', 'getObjectResponse_missingMeta' - This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
--
-- 'serverSideEncryption', 'getObjectResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'sSECustomerAlgorithm', 'getObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'objectLockLegalHoldStatus', 'getObjectResponse_objectLockLegalHoldStatus' - Indicates whether this object has an active legal hold. This field is
-- only returned if you have permission to view an object\'s legal hold
-- status.
--
-- 'cacheControl', 'getObjectResponse_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'lastModified', 'getObjectResponse_lastModified' - Creation date of the object.
--
-- 'expires', 'getObjectResponse_expires' - The date and time at which the object is no longer cacheable.
--
-- 'restore', 'getObjectResponse_restore' - Provides information about object restoration action and expiration time
-- of the restored object copy.
--
-- 'objectLockMode', 'getObjectResponse_objectLockMode' - The Object Lock mode currently in place for this object.
--
-- 'objectLockRetainUntilDate', 'getObjectResponse_objectLockRetainUntilDate' - The date and time when this object\'s Object Lock will expire.
--
-- 'httpStatus', 'getObjectResponse_httpStatus' - The response's http status code.
--
-- 'body', 'getObjectResponse_body' - Object data.
newGetObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'body'
  Core.ResponseBody ->
  GetObjectResponse
newGetObjectResponse pHttpStatus_ pBody_ =
  GetObjectResponse'
    { partsCount = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      eTag = Prelude.Nothing,
      websiteRedirectLocation = Prelude.Nothing,
      tagCount = Prelude.Nothing,
      contentType = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentRange = Prelude.Nothing,
      deleteMarker = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      expiration = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      contentLength = Prelude.Nothing,
      metadata = Prelude.mempty,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      versionId = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      acceptRanges = Prelude.Nothing,
      missingMeta = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      objectLockLegalHoldStatus = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      expires = Prelude.Nothing,
      restore = Prelude.Nothing,
      objectLockMode = Prelude.Nothing,
      objectLockRetainUntilDate = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      body = pBody_
    }

-- | The count of parts this object has.
getObjectResponse_partsCount :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Int)
getObjectResponse_partsCount = Lens.lens (\GetObjectResponse' {partsCount} -> partsCount) (\s@GetObjectResponse' {} a -> s {partsCount = a} :: GetObjectResponse)

-- | Undocumented member.
getObjectResponse_requestCharged :: Lens.Lens' GetObjectResponse (Prelude.Maybe RequestCharged)
getObjectResponse_requestCharged = Lens.lens (\GetObjectResponse' {requestCharged} -> requestCharged) (\s@GetObjectResponse' {} a -> s {requestCharged = a} :: GetObjectResponse)

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
getObjectResponse_eTag :: Lens.Lens' GetObjectResponse (Prelude.Maybe ETag)
getObjectResponse_eTag = Lens.lens (\GetObjectResponse' {eTag} -> eTag) (\s@GetObjectResponse' {} a -> s {eTag = a} :: GetObjectResponse)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
getObjectResponse_websiteRedirectLocation :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_websiteRedirectLocation = Lens.lens (\GetObjectResponse' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@GetObjectResponse' {} a -> s {websiteRedirectLocation = a} :: GetObjectResponse)

-- | The number of tags, if any, on the object.
getObjectResponse_tagCount :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Int)
getObjectResponse_tagCount = Lens.lens (\GetObjectResponse' {tagCount} -> tagCount) (\s@GetObjectResponse' {} a -> s {tagCount = a} :: GetObjectResponse)

-- | A standard MIME type describing the format of the object data.
getObjectResponse_contentType :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentType = Lens.lens (\GetObjectResponse' {contentType} -> contentType) (\s@GetObjectResponse' {} a -> s {contentType = a} :: GetObjectResponse)

-- | Specifies presentational information for the object.
getObjectResponse_contentDisposition :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentDisposition = Lens.lens (\GetObjectResponse' {contentDisposition} -> contentDisposition) (\s@GetObjectResponse' {} a -> s {contentDisposition = a} :: GetObjectResponse)

-- | The portion of the object returned in the response.
getObjectResponse_contentRange :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentRange = Lens.lens (\GetObjectResponse' {contentRange} -> contentRange) (\s@GetObjectResponse' {} a -> s {contentRange = a} :: GetObjectResponse)

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
getObjectResponse_deleteMarker :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Bool)
getObjectResponse_deleteMarker = Lens.lens (\GetObjectResponse' {deleteMarker} -> deleteMarker) (\s@GetObjectResponse' {} a -> s {deleteMarker = a} :: GetObjectResponse)

-- | The language the content is in.
getObjectResponse_contentLanguage :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentLanguage = Lens.lens (\GetObjectResponse' {contentLanguage} -> contentLanguage) (\s@GetObjectResponse' {} a -> s {contentLanguage = a} :: GetObjectResponse)

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key-value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
getObjectResponse_expiration :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_expiration = Lens.lens (\GetObjectResponse' {expiration} -> expiration) (\s@GetObjectResponse' {} a -> s {expiration = a} :: GetObjectResponse)

-- | Amazon S3 can return this if your request involves a bucket that is
-- either a source or destination in a replication rule.
getObjectResponse_replicationStatus :: Lens.Lens' GetObjectResponse (Prelude.Maybe ReplicationStatus)
getObjectResponse_replicationStatus = Lens.lens (\GetObjectResponse' {replicationStatus} -> replicationStatus) (\s@GetObjectResponse' {} a -> s {replicationStatus = a} :: GetObjectResponse)

-- | Size of the body in bytes.
getObjectResponse_contentLength :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Integer)
getObjectResponse_contentLength = Lens.lens (\GetObjectResponse' {contentLength} -> contentLength) (\s@GetObjectResponse' {} a -> s {contentLength = a} :: GetObjectResponse)

-- | A map of metadata to store with the object in S3.
getObjectResponse_metadata :: Lens.Lens' GetObjectResponse (Prelude.HashMap Prelude.Text Prelude.Text)
getObjectResponse_metadata = Lens.lens (\GetObjectResponse' {metadata} -> metadata) (\s@GetObjectResponse' {} a -> s {metadata = a} :: GetObjectResponse) Prelude.. Lens._Coerce

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
getObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_sSECustomerKeyMD5 = Lens.lens (\GetObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@GetObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: GetObjectResponse)

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
getObjectResponse_sSEKMSKeyId :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_sSEKMSKeyId = Lens.lens (\GetObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@GetObjectResponse' {} a -> s {sSEKMSKeyId = a} :: GetObjectResponse) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
getObjectResponse_contentEncoding :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentEncoding = Lens.lens (\GetObjectResponse' {contentEncoding} -> contentEncoding) (\s@GetObjectResponse' {} a -> s {contentEncoding = a} :: GetObjectResponse)

-- | Version of the object.
getObjectResponse_versionId :: Lens.Lens' GetObjectResponse (Prelude.Maybe ObjectVersionId)
getObjectResponse_versionId = Lens.lens (\GetObjectResponse' {versionId} -> versionId) (\s@GetObjectResponse' {} a -> s {versionId = a} :: GetObjectResponse)

-- | Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
getObjectResponse_storageClass :: Lens.Lens' GetObjectResponse (Prelude.Maybe StorageClass)
getObjectResponse_storageClass = Lens.lens (\GetObjectResponse' {storageClass} -> storageClass) (\s@GetObjectResponse' {} a -> s {storageClass = a} :: GetObjectResponse)

-- | Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with Amazon Web Services KMS (SSE-KMS).
getObjectResponse_bucketKeyEnabled :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Bool)
getObjectResponse_bucketKeyEnabled = Lens.lens (\GetObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@GetObjectResponse' {} a -> s {bucketKeyEnabled = a} :: GetObjectResponse)

-- | Indicates that a range of bytes was specified.
getObjectResponse_acceptRanges :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_acceptRanges = Lens.lens (\GetObjectResponse' {acceptRanges} -> acceptRanges) (\s@GetObjectResponse' {} a -> s {acceptRanges = a} :: GetObjectResponse)

-- | This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
getObjectResponse_missingMeta :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Int)
getObjectResponse_missingMeta = Lens.lens (\GetObjectResponse' {missingMeta} -> missingMeta) (\s@GetObjectResponse' {} a -> s {missingMeta = a} :: GetObjectResponse)

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
getObjectResponse_serverSideEncryption :: Lens.Lens' GetObjectResponse (Prelude.Maybe ServerSideEncryption)
getObjectResponse_serverSideEncryption = Lens.lens (\GetObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@GetObjectResponse' {} a -> s {serverSideEncryption = a} :: GetObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
getObjectResponse_sSECustomerAlgorithm :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_sSECustomerAlgorithm = Lens.lens (\GetObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@GetObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: GetObjectResponse)

-- | Indicates whether this object has an active legal hold. This field is
-- only returned if you have permission to view an object\'s legal hold
-- status.
getObjectResponse_objectLockLegalHoldStatus :: Lens.Lens' GetObjectResponse (Prelude.Maybe ObjectLockLegalHoldStatus)
getObjectResponse_objectLockLegalHoldStatus = Lens.lens (\GetObjectResponse' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@GetObjectResponse' {} a -> s {objectLockLegalHoldStatus = a} :: GetObjectResponse)

-- | Specifies caching behavior along the request\/reply chain.
getObjectResponse_cacheControl :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_cacheControl = Lens.lens (\GetObjectResponse' {cacheControl} -> cacheControl) (\s@GetObjectResponse' {} a -> s {cacheControl = a} :: GetObjectResponse)

-- | Creation date of the object.
getObjectResponse_lastModified :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.UTCTime)
getObjectResponse_lastModified = Lens.lens (\GetObjectResponse' {lastModified} -> lastModified) (\s@GetObjectResponse' {} a -> s {lastModified = a} :: GetObjectResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time at which the object is no longer cacheable.
getObjectResponse_expires :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.UTCTime)
getObjectResponse_expires = Lens.lens (\GetObjectResponse' {expires} -> expires) (\s@GetObjectResponse' {} a -> s {expires = a} :: GetObjectResponse) Prelude.. Lens.mapping Core._Time

-- | Provides information about object restoration action and expiration time
-- of the restored object copy.
getObjectResponse_restore :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_restore = Lens.lens (\GetObjectResponse' {restore} -> restore) (\s@GetObjectResponse' {} a -> s {restore = a} :: GetObjectResponse)

-- | The Object Lock mode currently in place for this object.
getObjectResponse_objectLockMode :: Lens.Lens' GetObjectResponse (Prelude.Maybe ObjectLockMode)
getObjectResponse_objectLockMode = Lens.lens (\GetObjectResponse' {objectLockMode} -> objectLockMode) (\s@GetObjectResponse' {} a -> s {objectLockMode = a} :: GetObjectResponse)

-- | The date and time when this object\'s Object Lock will expire.
getObjectResponse_objectLockRetainUntilDate :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.UTCTime)
getObjectResponse_objectLockRetainUntilDate = Lens.lens (\GetObjectResponse' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@GetObjectResponse' {} a -> s {objectLockRetainUntilDate = a} :: GetObjectResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getObjectResponse_httpStatus :: Lens.Lens' GetObjectResponse Prelude.Int
getObjectResponse_httpStatus = Lens.lens (\GetObjectResponse' {httpStatus} -> httpStatus) (\s@GetObjectResponse' {} a -> s {httpStatus = a} :: GetObjectResponse)

-- | Object data.
getObjectResponse_body :: Lens.Lens' GetObjectResponse Core.ResponseBody
getObjectResponse_body = Lens.lens (\GetObjectResponse' {body} -> body) (\s@GetObjectResponse' {} a -> s {body = a} :: GetObjectResponse)

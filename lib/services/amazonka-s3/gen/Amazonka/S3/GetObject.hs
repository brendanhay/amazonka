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
-- Module      : Amazonka.S3.GetObject
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- For more information about returning the ACL of an object, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>.
--
-- If the object you are retrieving is stored in the S3 Glacier Flexible
-- Retrieval or S3 Glacier Deep Archive storage class, or S3
-- Intelligent-Tiering Archive or S3 Intelligent-Tiering Deep Archive
-- tiers, before you can retrieve the object you must first restore a copy
-- using
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject>.
-- Otherwise, this action returns an @InvalidObjectState@ error. For
-- information about restoring archived objects, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/restoring-objects.html Restoring Archived Objects>.
--
-- Encryption request headers, like @x-amz-server-side-encryption@, should
-- not be sent for GET requests if your object uses server-side encryption
-- with Key Management Service (KMS) keys (SSE-KMS), dual-layer server-side
-- encryption with Amazon Web Services KMS keys (DSSE-KMS), or server-side
-- encryption with Amazon S3 managed encryption keys (SSE-S3). If your
-- object does use these types of keys, you’ll get an HTTP 400 Bad Request
-- error.
--
-- If you encrypt an object by using server-side encryption with
-- customer-provided encryption keys (SSE-C) when you store the object in
-- Amazon S3, then when you GET the object, you must use the following
-- headers:
--
-- -   @x-amz-server-side-encryption-customer-algorithm@
--
-- -   @x-amz-server-side-encryption-customer-key@
--
-- -   @x-amz-server-side-encryption-customer-key-MD5@
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
-- [Permissions]
--     You need the relevant read object (or version) permission for this
--     operation. For more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
--     If the object that you request doesn’t exist, the error that Amazon
--     S3 returns depends on whether you also have the @s3:ListBucket@
--     permission.
--
--     If you have the @s3:ListBucket@ permission on the bucket, Amazon S3
--     returns an HTTP status code 404 (Not Found) error.
--
--     If you don’t have the @s3:ListBucket@ permission, Amazon S3 returns
--     an HTTP status code 403 (\"access denied\") error.
--
-- [Versioning]
--     By default, the @GET@ action returns the current version of an
--     object. To return a different version, use the @versionId@
--     subresource.
--
--     -   If you supply a @versionId@, you need the @s3:GetObjectVersion@
--         permission to access a specific version of an object. If you
--         request a specific version, you do not need to have the
--         @s3:GetObject@ permission. If you request the current version
--         without a specific version ID, only @s3:GetObject@ permission is
--         required. @s3:GetObjectVersion@ permission won\'t be required.
--
--     -   If the current version of the object is a delete marker, Amazon
--         S3 behaves as if the object was deleted and includes
--         @x-amz-delete-marker: true@ in the response.
--
--     For more information about versioning, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketVersioning.html PutBucketVersioning>.
--
-- [Overriding Response Header Values]
--     There are times when you want to override certain response header
--     values in a @GET@ response. For example, you might override the
--     @Content-Disposition@ response header value in your @GET@ request.
--
--     You can override values for a set of response headers using the
--     following query parameters. These response header values are sent
--     only on a successful request, that is, when status code 200 OK is
--     returned. The set of headers you can override using these parameters
--     is a subset of the headers that Amazon S3 accepts when you create an
--     object. The response headers that you can override for the @GET@
--     response are @Content-Type@, @Content-Language@, @Expires@,
--     @Cache-Control@, @Content-Disposition@, and @Content-Encoding@. To
--     override these header values in the @GET@ response, you use the
--     following request parameters.
--
--     You must sign the request, either using an Authorization header or a
--     presigned URL, when using these parameters. They cannot be used with
--     an unsigned (anonymous) request.
--
--     -   @response-content-type@
--
--     -   @response-content-language@
--
--     -   @response-expires@
--
--     -   @response-cache-control@
--
--     -   @response-content-disposition@
--
--     -   @response-content-encoding@
--
-- [Overriding Response Header Values]
--     If both of the @If-Match@ and @If-Unmodified-Since@ headers are
--     present in the request as follows: @If-Match@ condition evaluates to
--     @true@, and; @If-Unmodified-Since@ condition evaluates to @false@;
--     then, S3 returns 200 OK and the data requested.
--
--     If both of the @If-None-Match@ and @If-Modified-Since@ headers are
--     present in the request as follows:@ If-None-Match@ condition
--     evaluates to @false@, and; @If-Modified-Since@ condition evaluates
--     to @true@; then, S3 returns 304 Not Modified response code.
--
--     For more information about conditional requests, see
--     <https://tools.ietf.org/html/rfc7232 RFC 7232>.
--
-- The following operations are related to @GetObject@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
module Amazonka.S3.GetObject
  ( -- * Creating a Request
    GetObject (..),
    newGetObject,

    -- * Request Lenses
    getObject_checksumMode,
    getObject_expectedBucketOwner,
    getObject_ifMatch,
    getObject_ifModifiedSince,
    getObject_ifNoneMatch,
    getObject_ifUnmodifiedSince,
    getObject_partNumber,
    getObject_range,
    getObject_requestPayer,
    getObject_responseCacheControl,
    getObject_responseContentDisposition,
    getObject_responseContentEncoding,
    getObject_responseContentLanguage,
    getObject_responseContentType,
    getObject_responseExpires,
    getObject_sSECustomerAlgorithm,
    getObject_sSECustomerKey,
    getObject_sSECustomerKeyMD5,
    getObject_versionId,
    getObject_bucket,
    getObject_key,

    -- * Destructuring the Response
    GetObjectResponse (..),
    newGetObjectResponse,

    -- * Response Lenses
    getObjectResponse_acceptRanges,
    getObjectResponse_bucketKeyEnabled,
    getObjectResponse_cacheControl,
    getObjectResponse_checksumCRC32,
    getObjectResponse_checksumCRC32C,
    getObjectResponse_checksumSHA1,
    getObjectResponse_checksumSHA256,
    getObjectResponse_contentDisposition,
    getObjectResponse_contentEncoding,
    getObjectResponse_contentLanguage,
    getObjectResponse_contentLength,
    getObjectResponse_contentRange,
    getObjectResponse_contentType,
    getObjectResponse_deleteMarker,
    getObjectResponse_eTag,
    getObjectResponse_expiration,
    getObjectResponse_expires,
    getObjectResponse_lastModified,
    getObjectResponse_metadata,
    getObjectResponse_missingMeta,
    getObjectResponse_objectLockLegalHoldStatus,
    getObjectResponse_objectLockMode,
    getObjectResponse_objectLockRetainUntilDate,
    getObjectResponse_partsCount,
    getObjectResponse_replicationStatus,
    getObjectResponse_requestCharged,
    getObjectResponse_restore,
    getObjectResponse_sSECustomerAlgorithm,
    getObjectResponse_sSECustomerKeyMD5,
    getObjectResponse_sSEKMSKeyId,
    getObjectResponse_serverSideEncryption,
    getObjectResponse_storageClass,
    getObjectResponse_tagCount,
    getObjectResponse_versionId,
    getObjectResponse_websiteRedirectLocation,
    getObjectResponse_httpStatus,
    getObjectResponse_body,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObject' smart constructor.
data GetObject = GetObject'
  { -- | To retrieve the checksum, this mode must be enabled.
    checksumMode :: Prelude.Maybe ChecksumMode,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if its entity tag (ETag) is the same as the one
    -- specified; otherwise, return a 412 (precondition failed) error.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if it has been modified since the specified time;
    -- otherwise, return a 304 (not modified) error.
    ifModifiedSince :: Prelude.Maybe Data.RFC822,
    -- | Return the object only if its entity tag (ETag) is different from the
    -- one specified; otherwise, return a 304 (not modified) error.
    ifNoneMatch :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if it has not been modified since the specified
    -- time; otherwise, return a 412 (precondition failed) error.
    ifUnmodifiedSince :: Prelude.Maybe Data.RFC822,
    -- | Part number of the object being read. This is a positive integer between
    -- 1 and 10,000. Effectively performs a \'ranged\' GET request for the part
    -- specified. Useful for downloading just a part of an object.
    partNumber :: Prelude.Maybe Prelude.Int,
    -- | Downloads the specified range bytes of an object. For more information
    -- about the HTTP Range header, see
    -- <https://www.rfc-editor.org/rfc/rfc9110.html#name-range>.
    --
    -- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
    -- request.
    range :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Sets the @Cache-Control@ header of the response.
    responseCacheControl :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Content-Disposition@ header of the response
    responseContentDisposition :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Content-Encoding@ header of the response.
    responseContentEncoding :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Content-Language@ header of the response.
    responseContentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Content-Type@ header of the response.
    responseContentType :: Prelude.Maybe Prelude.Text,
    -- | Sets the @Expires@ header of the response.
    responseExpires :: Prelude.Maybe Data.RFC822,
    -- | Specifies the algorithm to use to when decrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 used to
    -- encrypt the data. This value is used to decrypt the object when
    -- recovering it and must match the one used when storing the data. The key
    -- must be appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
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
    -- When you use this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
    -- When you use this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts access point ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
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
-- 'checksumMode', 'getObject_checksumMode' - To retrieve the checksum, this mode must be enabled.
--
-- 'expectedBucketOwner', 'getObject_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'ifMatch', 'getObject_ifMatch' - Return the object only if its entity tag (ETag) is the same as the one
-- specified; otherwise, return a 412 (precondition failed) error.
--
-- 'ifModifiedSince', 'getObject_ifModifiedSince' - Return the object only if it has been modified since the specified time;
-- otherwise, return a 304 (not modified) error.
--
-- 'ifNoneMatch', 'getObject_ifNoneMatch' - Return the object only if its entity tag (ETag) is different from the
-- one specified; otherwise, return a 304 (not modified) error.
--
-- 'ifUnmodifiedSince', 'getObject_ifUnmodifiedSince' - Return the object only if it has not been modified since the specified
-- time; otherwise, return a 412 (precondition failed) error.
--
-- 'partNumber', 'getObject_partNumber' - Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' GET request for the part
-- specified. Useful for downloading just a part of an object.
--
-- 'range', 'getObject_range' - Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <https://www.rfc-editor.org/rfc/rfc9110.html#name-range>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
--
-- 'requestPayer', 'getObject_requestPayer' - Undocumented member.
--
-- 'responseCacheControl', 'getObject_responseCacheControl' - Sets the @Cache-Control@ header of the response.
--
-- 'responseContentDisposition', 'getObject_responseContentDisposition' - Sets the @Content-Disposition@ header of the response
--
-- 'responseContentEncoding', 'getObject_responseContentEncoding' - Sets the @Content-Encoding@ header of the response.
--
-- 'responseContentLanguage', 'getObject_responseContentLanguage' - Sets the @Content-Language@ header of the response.
--
-- 'responseContentType', 'getObject_responseContentType' - Sets the @Content-Type@ header of the response.
--
-- 'responseExpires', 'getObject_responseExpires' - Sets the @Expires@ header of the response.
--
-- 'sSECustomerAlgorithm', 'getObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when decrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKey', 'getObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 used to
-- encrypt the data. This value is used to decrypt the object when
-- recovering it and must match the one used when storing the data. The key
-- must be appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'sSECustomerKeyMD5', 'getObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'versionId', 'getObject_versionId' - VersionId used to reference a specific version of the object.
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
-- When you use this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When you use this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts access point ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
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
    { checksumMode = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      ifMatch = Prelude.Nothing,
      ifModifiedSince = Prelude.Nothing,
      ifNoneMatch = Prelude.Nothing,
      ifUnmodifiedSince = Prelude.Nothing,
      partNumber = Prelude.Nothing,
      range = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      responseCacheControl = Prelude.Nothing,
      responseContentDisposition = Prelude.Nothing,
      responseContentEncoding = Prelude.Nothing,
      responseContentLanguage = Prelude.Nothing,
      responseContentType = Prelude.Nothing,
      responseExpires = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | To retrieve the checksum, this mode must be enabled.
getObject_checksumMode :: Lens.Lens' GetObject (Prelude.Maybe ChecksumMode)
getObject_checksumMode = Lens.lens (\GetObject' {checksumMode} -> checksumMode) (\s@GetObject' {} a -> s {checksumMode = a} :: GetObject)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getObject_expectedBucketOwner :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_expectedBucketOwner = Lens.lens (\GetObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObject' {} a -> s {expectedBucketOwner = a} :: GetObject)

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified; otherwise, return a 412 (precondition failed) error.
getObject_ifMatch :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_ifMatch = Lens.lens (\GetObject' {ifMatch} -> ifMatch) (\s@GetObject' {} a -> s {ifMatch = a} :: GetObject)

-- | Return the object only if it has been modified since the specified time;
-- otherwise, return a 304 (not modified) error.
getObject_ifModifiedSince :: Lens.Lens' GetObject (Prelude.Maybe Prelude.UTCTime)
getObject_ifModifiedSince = Lens.lens (\GetObject' {ifModifiedSince} -> ifModifiedSince) (\s@GetObject' {} a -> s {ifModifiedSince = a} :: GetObject) Prelude.. Lens.mapping Data._Time

-- | Return the object only if its entity tag (ETag) is different from the
-- one specified; otherwise, return a 304 (not modified) error.
getObject_ifNoneMatch :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_ifNoneMatch = Lens.lens (\GetObject' {ifNoneMatch} -> ifNoneMatch) (\s@GetObject' {} a -> s {ifNoneMatch = a} :: GetObject)

-- | Return the object only if it has not been modified since the specified
-- time; otherwise, return a 412 (precondition failed) error.
getObject_ifUnmodifiedSince :: Lens.Lens' GetObject (Prelude.Maybe Prelude.UTCTime)
getObject_ifUnmodifiedSince = Lens.lens (\GetObject' {ifUnmodifiedSince} -> ifUnmodifiedSince) (\s@GetObject' {} a -> s {ifUnmodifiedSince = a} :: GetObject) Prelude.. Lens.mapping Data._Time

-- | Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' GET request for the part
-- specified. Useful for downloading just a part of an object.
getObject_partNumber :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Int)
getObject_partNumber = Lens.lens (\GetObject' {partNumber} -> partNumber) (\s@GetObject' {} a -> s {partNumber = a} :: GetObject)

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <https://www.rfc-editor.org/rfc/rfc9110.html#name-range>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
getObject_range :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_range = Lens.lens (\GetObject' {range} -> range) (\s@GetObject' {} a -> s {range = a} :: GetObject)

-- | Undocumented member.
getObject_requestPayer :: Lens.Lens' GetObject (Prelude.Maybe RequestPayer)
getObject_requestPayer = Lens.lens (\GetObject' {requestPayer} -> requestPayer) (\s@GetObject' {} a -> s {requestPayer = a} :: GetObject)

-- | Sets the @Cache-Control@ header of the response.
getObject_responseCacheControl :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseCacheControl = Lens.lens (\GetObject' {responseCacheControl} -> responseCacheControl) (\s@GetObject' {} a -> s {responseCacheControl = a} :: GetObject)

-- | Sets the @Content-Disposition@ header of the response
getObject_responseContentDisposition :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentDisposition = Lens.lens (\GetObject' {responseContentDisposition} -> responseContentDisposition) (\s@GetObject' {} a -> s {responseContentDisposition = a} :: GetObject)

-- | Sets the @Content-Encoding@ header of the response.
getObject_responseContentEncoding :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentEncoding = Lens.lens (\GetObject' {responseContentEncoding} -> responseContentEncoding) (\s@GetObject' {} a -> s {responseContentEncoding = a} :: GetObject)

-- | Sets the @Content-Language@ header of the response.
getObject_responseContentLanguage :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentLanguage = Lens.lens (\GetObject' {responseContentLanguage} -> responseContentLanguage) (\s@GetObject' {} a -> s {responseContentLanguage = a} :: GetObject)

-- | Sets the @Content-Type@ header of the response.
getObject_responseContentType :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_responseContentType = Lens.lens (\GetObject' {responseContentType} -> responseContentType) (\s@GetObject' {} a -> s {responseContentType = a} :: GetObject)

-- | Sets the @Expires@ header of the response.
getObject_responseExpires :: Lens.Lens' GetObject (Prelude.Maybe Prelude.UTCTime)
getObject_responseExpires = Lens.lens (\GetObject' {responseExpires} -> responseExpires) (\s@GetObject' {} a -> s {responseExpires = a} :: GetObject) Prelude.. Lens.mapping Data._Time

-- | Specifies the algorithm to use to when decrypting the object (for
-- example, AES256).
getObject_sSECustomerAlgorithm :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_sSECustomerAlgorithm = Lens.lens (\GetObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@GetObject' {} a -> s {sSECustomerAlgorithm = a} :: GetObject)

-- | Specifies the customer-provided encryption key for Amazon S3 used to
-- encrypt the data. This value is used to decrypt the object when
-- recovering it and must match the one used when storing the data. The key
-- must be appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
getObject_sSECustomerKey :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_sSECustomerKey = Lens.lens (\GetObject' {sSECustomerKey} -> sSECustomerKey) (\s@GetObject' {} a -> s {sSECustomerKey = a} :: GetObject) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
getObject_sSECustomerKeyMD5 :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_sSECustomerKeyMD5 = Lens.lens (\GetObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@GetObject' {} a -> s {sSECustomerKeyMD5 = a} :: GetObject)

-- | VersionId used to reference a specific version of the object.
getObject_versionId :: Lens.Lens' GetObject (Prelude.Maybe ObjectVersionId)
getObject_versionId = Lens.lens (\GetObject' {versionId} -> versionId) (\s@GetObject' {} a -> s {versionId = a} :: GetObject)

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
-- When you use this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When you use this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts access point ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
-- in the /Amazon S3 User Guide/.
getObject_bucket :: Lens.Lens' GetObject BucketName
getObject_bucket = Lens.lens (\GetObject' {bucket} -> bucket) (\s@GetObject' {} a -> s {bucket = a} :: GetObject)

-- | Key of the object to get.
getObject_key :: Lens.Lens' GetObject ObjectKey
getObject_key = Lens.lens (\GetObject' {key} -> key) (\s@GetObject' {} a -> s {key = a} :: GetObject)

instance Core.AWSRequest GetObject where
  type AWSResponse GetObject = GetObjectResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetObjectResponse'
            Prelude.<$> (h Data..#? "accept-ranges")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Data..#? "Cache-Control")
            Prelude.<*> (h Data..#? "x-amz-checksum-crc32")
            Prelude.<*> (h Data..#? "x-amz-checksum-crc32c")
            Prelude.<*> (h Data..#? "x-amz-checksum-sha1")
            Prelude.<*> (h Data..#? "x-amz-checksum-sha256")
            Prelude.<*> (h Data..#? "Content-Disposition")
            Prelude.<*> (h Data..#? "Content-Encoding")
            Prelude.<*> (h Data..#? "Content-Language")
            Prelude.<*> (h Data..#? "Content-Length")
            Prelude.<*> (h Data..#? "Content-Range")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "x-amz-delete-marker")
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "x-amz-expiration")
            Prelude.<*> (h Data..#? "Expires")
            Prelude.<*> (h Data..#? "Last-Modified")
            Prelude.<*> (Data.parseHeadersMap "x-amz-meta-" h)
            Prelude.<*> (h Data..#? "x-amz-missing-meta")
            Prelude.<*> (h Data..#? "x-amz-object-lock-legal-hold")
            Prelude.<*> (h Data..#? "x-amz-object-lock-mode")
            Prelude.<*> (h Data..#? "x-amz-object-lock-retain-until-date")
            Prelude.<*> (h Data..#? "x-amz-mp-parts-count")
            Prelude.<*> (h Data..#? "x-amz-replication-status")
            Prelude.<*> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (h Data..#? "x-amz-restore")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Data..#? "x-amz-server-side-encryption")
            Prelude.<*> (h Data..#? "x-amz-storage-class")
            Prelude.<*> (h Data..#? "x-amz-tagging-count")
            Prelude.<*> (h Data..#? "x-amz-version-id")
            Prelude.<*> (h Data..#? "x-amz-website-redirect-location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetObject where
  hashWithSalt _salt GetObject' {..} =
    _salt
      `Prelude.hashWithSalt` checksumMode
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` ifModifiedSince
      `Prelude.hashWithSalt` ifNoneMatch
      `Prelude.hashWithSalt` ifUnmodifiedSince
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` responseCacheControl
      `Prelude.hashWithSalt` responseContentDisposition
      `Prelude.hashWithSalt` responseContentEncoding
      `Prelude.hashWithSalt` responseContentLanguage
      `Prelude.hashWithSalt` responseContentType
      `Prelude.hashWithSalt` responseExpires
      `Prelude.hashWithSalt` sSECustomerAlgorithm
      `Prelude.hashWithSalt` sSECustomerKey
      `Prelude.hashWithSalt` sSECustomerKeyMD5
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData GetObject where
  rnf GetObject' {..} =
    Prelude.rnf checksumMode
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf ifModifiedSince
      `Prelude.seq` Prelude.rnf ifNoneMatch
      `Prelude.seq` Prelude.rnf ifUnmodifiedSince
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf range
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf responseCacheControl
      `Prelude.seq` Prelude.rnf responseContentDisposition
      `Prelude.seq` Prelude.rnf responseContentEncoding
      `Prelude.seq` Prelude.rnf responseContentLanguage
      `Prelude.seq` Prelude.rnf responseContentType
      `Prelude.seq` Prelude.rnf responseExpires
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKey
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders GetObject where
  toHeaders GetObject' {..} =
    Prelude.mconcat
      [ "x-amz-checksum-mode" Data.=# checksumMode,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "If-Match" Data.=# ifMatch,
        "If-Modified-Since" Data.=# ifModifiedSince,
        "If-None-Match" Data.=# ifNoneMatch,
        "If-Unmodified-Since" Data.=# ifUnmodifiedSince,
        "Range" Data.=# range,
        "x-amz-request-payer" Data.=# requestPayer,
        "x-amz-server-side-encryption-customer-algorithm"
          Data.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key"
          Data.=# sSECustomerKey,
        "x-amz-server-side-encryption-customer-key-MD5"
          Data.=# sSECustomerKeyMD5
      ]

instance Data.ToPath GetObject where
  toPath GetObject' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery GetObject where
  toQuery GetObject' {..} =
    Prelude.mconcat
      [ "partNumber" Data.=: partNumber,
        "response-cache-control"
          Data.=: responseCacheControl,
        "response-content-disposition"
          Data.=: responseContentDisposition,
        "response-content-encoding"
          Data.=: responseContentEncoding,
        "response-content-language"
          Data.=: responseContentLanguage,
        "response-content-type" Data.=: responseContentType,
        "response-expires" Data.=: responseExpires,
        "versionId" Data.=: versionId
      ]

-- | /See:/ 'newGetObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { -- | Indicates that a range of bytes was specified.
    acceptRanges :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the object uses an S3 Bucket Key for server-side
    -- encryption with Key Management Service (KMS) keys (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies caching behavior along the request\/reply chain.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32C :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA1 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text,
    -- | Specifies presentational information for the object.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Size of the body in bytes.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | The portion of the object returned in the response.
    contentRange :: Prelude.Maybe Prelude.Text,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the object retrieved was (true) or was not (false) a
    -- Delete Marker. If false, this response header does not appear in the
    -- response.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | An entity tag (ETag) is an opaque identifier assigned by a web server to
    -- a specific version of a resource found at a URL.
    eTag :: Prelude.Maybe ETag,
    -- | If the object expiration is configured (see PUT Bucket lifecycle), the
    -- response includes this header. It includes the @expiry-date@ and
    -- @rule-id@ key-value pairs providing object expiration information. The
    -- value of the @rule-id@ is URL-encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Prelude.Maybe Data.RFC822,
    -- | Creation date of the object.
    lastModified :: Prelude.Maybe Data.RFC822,
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | This is set to the number of metadata entries not returned in
    -- @x-amz-meta@ headers. This can happen if you create metadata using an
    -- API like SOAP that supports more flexible metadata than the REST API.
    -- For example, using SOAP, you can create metadata whose values are not
    -- legal HTTP headers.
    missingMeta :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether this object has an active legal hold. This field is
    -- only returned if you have permission to view an object\'s legal hold
    -- status.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | The Object Lock mode currently in place for this object.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time when this object\'s Object Lock will expire.
    objectLockRetainUntilDate :: Prelude.Maybe Data.ISO8601,
    -- | The count of parts this object has. This value is only returned if you
    -- specify @partNumber@ in your request and the object was uploaded as a
    -- multipart upload.
    partsCount :: Prelude.Maybe Prelude.Int,
    -- | Amazon S3 can return this if your request involves a bucket that is
    -- either a source or destination in a replication rule.
    replicationStatus :: Prelude.Maybe ReplicationStatus,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | Provides information about object restoration action and expiration time
    -- of the restored object copy.
    restore :: Prelude.Maybe Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the ID of the Key Management Service (KMS)
    -- symmetric encryption customer managed key that was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, @AES256@, @aws:kms@, @aws:kms:dsse@).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | Provides storage class information of the object. Amazon S3 returns this
    -- header for all objects except for S3 Standard storage class objects.
    storageClass :: Prelude.Maybe StorageClass,
    -- | The number of tags, if any, on the object.
    tagCount :: Prelude.Maybe Prelude.Int,
    -- | Version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Object data.
    body :: Data.ResponseBody
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
-- 'acceptRanges', 'getObjectResponse_acceptRanges' - Indicates that a range of bytes was specified.
--
-- 'bucketKeyEnabled', 'getObjectResponse_bucketKeyEnabled' - Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with Key Management Service (KMS) keys (SSE-KMS).
--
-- 'cacheControl', 'getObjectResponse_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'checksumCRC32', 'getObjectResponse_checksumCRC32' - The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32C', 'getObjectResponse_checksumCRC32C' - The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'getObjectResponse_checksumSHA1' - The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA256', 'getObjectResponse_checksumSHA256' - The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'contentDisposition', 'getObjectResponse_contentDisposition' - Specifies presentational information for the object.
--
-- 'contentEncoding', 'getObjectResponse_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'contentLanguage', 'getObjectResponse_contentLanguage' - The language the content is in.
--
-- 'contentLength', 'getObjectResponse_contentLength' - Size of the body in bytes.
--
-- 'contentRange', 'getObjectResponse_contentRange' - The portion of the object returned in the response.
--
-- 'contentType', 'getObjectResponse_contentType' - A standard MIME type describing the format of the object data.
--
-- 'deleteMarker', 'getObjectResponse_deleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
--
-- 'eTag', 'getObjectResponse_eTag' - An entity tag (ETag) is an opaque identifier assigned by a web server to
-- a specific version of a resource found at a URL.
--
-- 'expiration', 'getObjectResponse_expiration' - If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the @expiry-date@ and
-- @rule-id@ key-value pairs providing object expiration information. The
-- value of the @rule-id@ is URL-encoded.
--
-- 'expires', 'getObjectResponse_expires' - The date and time at which the object is no longer cacheable.
--
-- 'lastModified', 'getObjectResponse_lastModified' - Creation date of the object.
--
-- 'metadata', 'getObjectResponse_metadata' - A map of metadata to store with the object in S3.
--
-- 'missingMeta', 'getObjectResponse_missingMeta' - This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
--
-- 'objectLockLegalHoldStatus', 'getObjectResponse_objectLockLegalHoldStatus' - Indicates whether this object has an active legal hold. This field is
-- only returned if you have permission to view an object\'s legal hold
-- status.
--
-- 'objectLockMode', 'getObjectResponse_objectLockMode' - The Object Lock mode currently in place for this object.
--
-- 'objectLockRetainUntilDate', 'getObjectResponse_objectLockRetainUntilDate' - The date and time when this object\'s Object Lock will expire.
--
-- 'partsCount', 'getObjectResponse_partsCount' - The count of parts this object has. This value is only returned if you
-- specify @partNumber@ in your request and the object was uploaded as a
-- multipart upload.
--
-- 'replicationStatus', 'getObjectResponse_replicationStatus' - Amazon S3 can return this if your request involves a bucket that is
-- either a source or destination in a replication rule.
--
-- 'requestCharged', 'getObjectResponse_requestCharged' - Undocumented member.
--
-- 'restore', 'getObjectResponse_restore' - Provides information about object restoration action and expiration time
-- of the restored object copy.
--
-- 'sSECustomerAlgorithm', 'getObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'sSECustomerKeyMD5', 'getObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSKeyId', 'getObjectResponse_sSEKMSKeyId' - If present, specifies the ID of the Key Management Service (KMS)
-- symmetric encryption customer managed key that was used for the object.
--
-- 'serverSideEncryption', 'getObjectResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, @AES256@, @aws:kms@, @aws:kms:dsse@).
--
-- 'storageClass', 'getObjectResponse_storageClass' - Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
--
-- 'tagCount', 'getObjectResponse_tagCount' - The number of tags, if any, on the object.
--
-- 'versionId', 'getObjectResponse_versionId' - Version of the object.
--
-- 'websiteRedirectLocation', 'getObjectResponse_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'httpStatus', 'getObjectResponse_httpStatus' - The response's http status code.
--
-- 'body', 'getObjectResponse_body' - Object data.
newGetObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'body'
  Data.ResponseBody ->
  GetObjectResponse
newGetObjectResponse pHttpStatus_ pBody_ =
  GetObjectResponse'
    { acceptRanges = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      checksumCRC32 = Prelude.Nothing,
      checksumCRC32C = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      contentLength = Prelude.Nothing,
      contentRange = Prelude.Nothing,
      contentType = Prelude.Nothing,
      deleteMarker = Prelude.Nothing,
      eTag = Prelude.Nothing,
      expiration = Prelude.Nothing,
      expires = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      metadata = Prelude.mempty,
      missingMeta = Prelude.Nothing,
      objectLockLegalHoldStatus = Prelude.Nothing,
      objectLockMode = Prelude.Nothing,
      objectLockRetainUntilDate = Prelude.Nothing,
      partsCount = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      restore = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      tagCount = Prelude.Nothing,
      versionId = Prelude.Nothing,
      websiteRedirectLocation = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      body = pBody_
    }

-- | Indicates that a range of bytes was specified.
getObjectResponse_acceptRanges :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_acceptRanges = Lens.lens (\GetObjectResponse' {acceptRanges} -> acceptRanges) (\s@GetObjectResponse' {} a -> s {acceptRanges = a} :: GetObjectResponse)

-- | Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with Key Management Service (KMS) keys (SSE-KMS).
getObjectResponse_bucketKeyEnabled :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Bool)
getObjectResponse_bucketKeyEnabled = Lens.lens (\GetObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@GetObjectResponse' {} a -> s {bucketKeyEnabled = a} :: GetObjectResponse)

-- | Specifies caching behavior along the request\/reply chain.
getObjectResponse_cacheControl :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_cacheControl = Lens.lens (\GetObjectResponse' {cacheControl} -> cacheControl) (\s@GetObjectResponse' {} a -> s {cacheControl = a} :: GetObjectResponse)

-- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
getObjectResponse_checksumCRC32 :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_checksumCRC32 = Lens.lens (\GetObjectResponse' {checksumCRC32} -> checksumCRC32) (\s@GetObjectResponse' {} a -> s {checksumCRC32 = a} :: GetObjectResponse)

-- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
getObjectResponse_checksumCRC32C :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_checksumCRC32C = Lens.lens (\GetObjectResponse' {checksumCRC32C} -> checksumCRC32C) (\s@GetObjectResponse' {} a -> s {checksumCRC32C = a} :: GetObjectResponse)

-- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
getObjectResponse_checksumSHA1 :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_checksumSHA1 = Lens.lens (\GetObjectResponse' {checksumSHA1} -> checksumSHA1) (\s@GetObjectResponse' {} a -> s {checksumSHA1 = a} :: GetObjectResponse)

-- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
getObjectResponse_checksumSHA256 :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_checksumSHA256 = Lens.lens (\GetObjectResponse' {checksumSHA256} -> checksumSHA256) (\s@GetObjectResponse' {} a -> s {checksumSHA256 = a} :: GetObjectResponse)

-- | Specifies presentational information for the object.
getObjectResponse_contentDisposition :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentDisposition = Lens.lens (\GetObjectResponse' {contentDisposition} -> contentDisposition) (\s@GetObjectResponse' {} a -> s {contentDisposition = a} :: GetObjectResponse)

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
getObjectResponse_contentEncoding :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentEncoding = Lens.lens (\GetObjectResponse' {contentEncoding} -> contentEncoding) (\s@GetObjectResponse' {} a -> s {contentEncoding = a} :: GetObjectResponse)

-- | The language the content is in.
getObjectResponse_contentLanguage :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentLanguage = Lens.lens (\GetObjectResponse' {contentLanguage} -> contentLanguage) (\s@GetObjectResponse' {} a -> s {contentLanguage = a} :: GetObjectResponse)

-- | Size of the body in bytes.
getObjectResponse_contentLength :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Integer)
getObjectResponse_contentLength = Lens.lens (\GetObjectResponse' {contentLength} -> contentLength) (\s@GetObjectResponse' {} a -> s {contentLength = a} :: GetObjectResponse)

-- | The portion of the object returned in the response.
getObjectResponse_contentRange :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentRange = Lens.lens (\GetObjectResponse' {contentRange} -> contentRange) (\s@GetObjectResponse' {} a -> s {contentRange = a} :: GetObjectResponse)

-- | A standard MIME type describing the format of the object data.
getObjectResponse_contentType :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentType = Lens.lens (\GetObjectResponse' {contentType} -> contentType) (\s@GetObjectResponse' {} a -> s {contentType = a} :: GetObjectResponse)

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
getObjectResponse_deleteMarker :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Bool)
getObjectResponse_deleteMarker = Lens.lens (\GetObjectResponse' {deleteMarker} -> deleteMarker) (\s@GetObjectResponse' {} a -> s {deleteMarker = a} :: GetObjectResponse)

-- | An entity tag (ETag) is an opaque identifier assigned by a web server to
-- a specific version of a resource found at a URL.
getObjectResponse_eTag :: Lens.Lens' GetObjectResponse (Prelude.Maybe ETag)
getObjectResponse_eTag = Lens.lens (\GetObjectResponse' {eTag} -> eTag) (\s@GetObjectResponse' {} a -> s {eTag = a} :: GetObjectResponse)

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the @expiry-date@ and
-- @rule-id@ key-value pairs providing object expiration information. The
-- value of the @rule-id@ is URL-encoded.
getObjectResponse_expiration :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_expiration = Lens.lens (\GetObjectResponse' {expiration} -> expiration) (\s@GetObjectResponse' {} a -> s {expiration = a} :: GetObjectResponse)

-- | The date and time at which the object is no longer cacheable.
getObjectResponse_expires :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.UTCTime)
getObjectResponse_expires = Lens.lens (\GetObjectResponse' {expires} -> expires) (\s@GetObjectResponse' {} a -> s {expires = a} :: GetObjectResponse) Prelude.. Lens.mapping Data._Time

-- | Creation date of the object.
getObjectResponse_lastModified :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.UTCTime)
getObjectResponse_lastModified = Lens.lens (\GetObjectResponse' {lastModified} -> lastModified) (\s@GetObjectResponse' {} a -> s {lastModified = a} :: GetObjectResponse) Prelude.. Lens.mapping Data._Time

-- | A map of metadata to store with the object in S3.
getObjectResponse_metadata :: Lens.Lens' GetObjectResponse (Prelude.HashMap Prelude.Text Prelude.Text)
getObjectResponse_metadata = Lens.lens (\GetObjectResponse' {metadata} -> metadata) (\s@GetObjectResponse' {} a -> s {metadata = a} :: GetObjectResponse) Prelude.. Lens.coerced

-- | This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
getObjectResponse_missingMeta :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Int)
getObjectResponse_missingMeta = Lens.lens (\GetObjectResponse' {missingMeta} -> missingMeta) (\s@GetObjectResponse' {} a -> s {missingMeta = a} :: GetObjectResponse)

-- | Indicates whether this object has an active legal hold. This field is
-- only returned if you have permission to view an object\'s legal hold
-- status.
getObjectResponse_objectLockLegalHoldStatus :: Lens.Lens' GetObjectResponse (Prelude.Maybe ObjectLockLegalHoldStatus)
getObjectResponse_objectLockLegalHoldStatus = Lens.lens (\GetObjectResponse' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@GetObjectResponse' {} a -> s {objectLockLegalHoldStatus = a} :: GetObjectResponse)

-- | The Object Lock mode currently in place for this object.
getObjectResponse_objectLockMode :: Lens.Lens' GetObjectResponse (Prelude.Maybe ObjectLockMode)
getObjectResponse_objectLockMode = Lens.lens (\GetObjectResponse' {objectLockMode} -> objectLockMode) (\s@GetObjectResponse' {} a -> s {objectLockMode = a} :: GetObjectResponse)

-- | The date and time when this object\'s Object Lock will expire.
getObjectResponse_objectLockRetainUntilDate :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.UTCTime)
getObjectResponse_objectLockRetainUntilDate = Lens.lens (\GetObjectResponse' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@GetObjectResponse' {} a -> s {objectLockRetainUntilDate = a} :: GetObjectResponse) Prelude.. Lens.mapping Data._Time

-- | The count of parts this object has. This value is only returned if you
-- specify @partNumber@ in your request and the object was uploaded as a
-- multipart upload.
getObjectResponse_partsCount :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Int)
getObjectResponse_partsCount = Lens.lens (\GetObjectResponse' {partsCount} -> partsCount) (\s@GetObjectResponse' {} a -> s {partsCount = a} :: GetObjectResponse)

-- | Amazon S3 can return this if your request involves a bucket that is
-- either a source or destination in a replication rule.
getObjectResponse_replicationStatus :: Lens.Lens' GetObjectResponse (Prelude.Maybe ReplicationStatus)
getObjectResponse_replicationStatus = Lens.lens (\GetObjectResponse' {replicationStatus} -> replicationStatus) (\s@GetObjectResponse' {} a -> s {replicationStatus = a} :: GetObjectResponse)

-- | Undocumented member.
getObjectResponse_requestCharged :: Lens.Lens' GetObjectResponse (Prelude.Maybe RequestCharged)
getObjectResponse_requestCharged = Lens.lens (\GetObjectResponse' {requestCharged} -> requestCharged) (\s@GetObjectResponse' {} a -> s {requestCharged = a} :: GetObjectResponse)

-- | Provides information about object restoration action and expiration time
-- of the restored object copy.
getObjectResponse_restore :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_restore = Lens.lens (\GetObjectResponse' {restore} -> restore) (\s@GetObjectResponse' {} a -> s {restore = a} :: GetObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
getObjectResponse_sSECustomerAlgorithm :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_sSECustomerAlgorithm = Lens.lens (\GetObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@GetObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: GetObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
getObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_sSECustomerKeyMD5 = Lens.lens (\GetObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@GetObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: GetObjectResponse)

-- | If present, specifies the ID of the Key Management Service (KMS)
-- symmetric encryption customer managed key that was used for the object.
getObjectResponse_sSEKMSKeyId :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_sSEKMSKeyId = Lens.lens (\GetObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@GetObjectResponse' {} a -> s {sSEKMSKeyId = a} :: GetObjectResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, @AES256@, @aws:kms@, @aws:kms:dsse@).
getObjectResponse_serverSideEncryption :: Lens.Lens' GetObjectResponse (Prelude.Maybe ServerSideEncryption)
getObjectResponse_serverSideEncryption = Lens.lens (\GetObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@GetObjectResponse' {} a -> s {serverSideEncryption = a} :: GetObjectResponse)

-- | Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
getObjectResponse_storageClass :: Lens.Lens' GetObjectResponse (Prelude.Maybe StorageClass)
getObjectResponse_storageClass = Lens.lens (\GetObjectResponse' {storageClass} -> storageClass) (\s@GetObjectResponse' {} a -> s {storageClass = a} :: GetObjectResponse)

-- | The number of tags, if any, on the object.
getObjectResponse_tagCount :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Int)
getObjectResponse_tagCount = Lens.lens (\GetObjectResponse' {tagCount} -> tagCount) (\s@GetObjectResponse' {} a -> s {tagCount = a} :: GetObjectResponse)

-- | Version of the object.
getObjectResponse_versionId :: Lens.Lens' GetObjectResponse (Prelude.Maybe ObjectVersionId)
getObjectResponse_versionId = Lens.lens (\GetObjectResponse' {versionId} -> versionId) (\s@GetObjectResponse' {} a -> s {versionId = a} :: GetObjectResponse)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
getObjectResponse_websiteRedirectLocation :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_websiteRedirectLocation = Lens.lens (\GetObjectResponse' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@GetObjectResponse' {} a -> s {websiteRedirectLocation = a} :: GetObjectResponse)

-- | The response's http status code.
getObjectResponse_httpStatus :: Lens.Lens' GetObjectResponse Prelude.Int
getObjectResponse_httpStatus = Lens.lens (\GetObjectResponse' {httpStatus} -> httpStatus) (\s@GetObjectResponse' {} a -> s {httpStatus = a} :: GetObjectResponse)

-- | Object data.
getObjectResponse_body :: Lens.Lens' GetObjectResponse Data.ResponseBody
getObjectResponse_body = Lens.lens (\GetObjectResponse' {body} -> body) (\s@GetObjectResponse' {} a -> s {body = a} :: GetObjectResponse)

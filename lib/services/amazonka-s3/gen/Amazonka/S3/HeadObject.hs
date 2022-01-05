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
-- Module      : Amazonka.S3.HeadObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The HEAD action retrieves metadata from an object without returning the
-- object itself. This action is useful if you\'re only interested in an
-- object\'s metadata. To use HEAD, you must have READ access to the
-- object.
--
-- A @HEAD@ request has the same options as a @GET@ action on an object.
-- The response is identical to the @GET@ response except that there is no
-- response body. Because of this, if the @HEAD@ request generates an
-- error, it returns a generic @404 Not Found@ or @403 Forbidden@ code. It
-- is not possible to retrieve the exact exception beyond these error
-- codes.
--
-- If you encrypt an object by using server-side encryption with
-- customer-provided encryption keys (SSE-C) when you store the object in
-- Amazon S3, then when you retrieve the metadata from the object, you must
-- use the following headers:
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
-- -   Encryption request headers, like @x-amz-server-side-encryption@,
--     should not be sent for GET requests if your object uses server-side
--     encryption with KMS keys (SSE-KMS) or server-side encryption with
--     Amazon S3–managed encryption keys (SSE-S3). If your object does use
--     these types of keys, you’ll get an HTTP 400 BadRequest error.
--
-- -   The last modified property in this case is the creation date of the
--     object.
--
-- Request headers are limited to 8 KB in size. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTCommonRequestHeaders.html Common Request Headers>.
--
-- Consider the following when using request headers:
--
-- -   Consideration 1 – If both of the @If-Match@ and
--     @If-Unmodified-Since@ headers are present in the request as follows:
--
--     -   @If-Match@ condition evaluates to @true@, and;
--
--     -   @If-Unmodified-Since@ condition evaluates to @false@;
--
--     Then Amazon S3 returns @200 OK@ and the data requested.
--
-- -   Consideration 2 – If both of the @If-None-Match@ and
--     @If-Modified-Since@ headers are present in the request as follows:
--
--     -   @If-None-Match@ condition evaluates to @false@, and;
--
--     -   @If-Modified-Since@ condition evaluates to @true@;
--
--     Then Amazon S3 returns the @304 Not Modified@ response code.
--
-- For more information about conditional requests, see
-- <https://tools.ietf.org/html/rfc7232 RFC 7232>.
--
-- __Permissions__
--
-- You need the relevant read object (or version) permission for this
-- operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
-- If the object you request does not exist, the error Amazon S3 returns
-- depends on whether you also have the s3:ListBucket permission.
--
-- -   If you have the @s3:ListBucket@ permission on the bucket, Amazon S3
--     returns an HTTP status code 404 (\"no such key\") error.
--
-- -   If you don’t have the @s3:ListBucket@ permission, Amazon S3 returns
--     an HTTP status code 403 (\"access denied\") error.
--
-- The following action is related to @HeadObject@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Amazonka.S3.HeadObject
  ( -- * Creating a Request
    HeadObject (..),
    newHeadObject,

    -- * Request Lenses
    headObject_ifMatch,
    headObject_versionId,
    headObject_sSECustomerAlgorithm,
    headObject_sSECustomerKey,
    headObject_requestPayer,
    headObject_ifModifiedSince,
    headObject_partNumber,
    headObject_range,
    headObject_ifUnmodifiedSince,
    headObject_sSECustomerKeyMD5,
    headObject_ifNoneMatch,
    headObject_expectedBucketOwner,
    headObject_bucket,
    headObject_key,

    -- * Destructuring the Response
    HeadObjectResponse (..),
    newHeadObjectResponse,

    -- * Response Lenses
    headObjectResponse_requestCharged,
    headObjectResponse_partsCount,
    headObjectResponse_eTag,
    headObjectResponse_versionId,
    headObjectResponse_contentLength,
    headObjectResponse_objectLockMode,
    headObjectResponse_expires,
    headObjectResponse_restore,
    headObjectResponse_expiration,
    headObjectResponse_deleteMarker,
    headObjectResponse_archiveStatus,
    headObjectResponse_sSECustomerAlgorithm,
    headObjectResponse_missingMeta,
    headObjectResponse_bucketKeyEnabled,
    headObjectResponse_websiteRedirectLocation,
    headObjectResponse_acceptRanges,
    headObjectResponse_storageClass,
    headObjectResponse_sSECustomerKeyMD5,
    headObjectResponse_sSEKMSKeyId,
    headObjectResponse_contentEncoding,
    headObjectResponse_objectLockRetainUntilDate,
    headObjectResponse_metadata,
    headObjectResponse_replicationStatus,
    headObjectResponse_cacheControl,
    headObjectResponse_contentLanguage,
    headObjectResponse_lastModified,
    headObjectResponse_objectLockLegalHoldStatus,
    headObjectResponse_contentDisposition,
    headObjectResponse_serverSideEncryption,
    headObjectResponse_contentType,
    headObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newHeadObject' smart constructor.
data HeadObject = HeadObject'
  { -- | Return the object only if its entity tag (ETag) is the same as the one
    -- specified, otherwise return a 412 (precondition failed).
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | VersionId used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Return the object only if it has been modified since the specified time,
    -- otherwise return a 304 (not modified).
    ifModifiedSince :: Prelude.Maybe Core.ISO8601,
    -- | Part number of the object being read. This is a positive integer between
    -- 1 and 10,000. Effectively performs a \'ranged\' HEAD request for the
    -- part specified. Useful querying about the size of the part and the
    -- number of parts in this object.
    partNumber :: Prelude.Maybe Prelude.Int,
    -- | Downloads the specified range bytes of an object. For more information
    -- about the HTTP Range header, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
    --
    -- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
    -- request.
    range :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if it has not been modified since the specified
    -- time, otherwise return a 412 (precondition failed).
    ifUnmodifiedSince :: Prelude.Maybe Core.ISO8601,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if its entity tag (ETag) is different from the
    -- one specified, otherwise return a 304 (not modified).
    ifNoneMatch :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the object.
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
    -- | The object key.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeadObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'headObject_ifMatch' - Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
--
-- 'versionId', 'headObject_versionId' - VersionId used to reference a specific version of the object.
--
-- 'sSECustomerAlgorithm', 'headObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKey', 'headObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'requestPayer', 'headObject_requestPayer' - Undocumented member.
--
-- 'ifModifiedSince', 'headObject_ifModifiedSince' - Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
--
-- 'partNumber', 'headObject_partNumber' - Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' HEAD request for the
-- part specified. Useful querying about the size of the part and the
-- number of parts in this object.
--
-- 'range', 'headObject_range' - Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
--
-- 'ifUnmodifiedSince', 'headObject_ifUnmodifiedSince' - Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
--
-- 'sSECustomerKeyMD5', 'headObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'ifNoneMatch', 'headObject_ifNoneMatch' - Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
--
-- 'expectedBucketOwner', 'headObject_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'headObject_bucket' - The name of the bucket containing the object.
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
-- 'key', 'headObject_key' - The object key.
newHeadObject ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  HeadObject
newHeadObject pBucket_ pKey_ =
  HeadObject'
    { ifMatch = Prelude.Nothing,
      versionId = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      ifModifiedSince = Prelude.Nothing,
      partNumber = Prelude.Nothing,
      range = Prelude.Nothing,
      ifUnmodifiedSince = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      ifNoneMatch = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
headObject_ifMatch :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_ifMatch = Lens.lens (\HeadObject' {ifMatch} -> ifMatch) (\s@HeadObject' {} a -> s {ifMatch = a} :: HeadObject)

-- | VersionId used to reference a specific version of the object.
headObject_versionId :: Lens.Lens' HeadObject (Prelude.Maybe ObjectVersionId)
headObject_versionId = Lens.lens (\HeadObject' {versionId} -> versionId) (\s@HeadObject' {} a -> s {versionId = a} :: HeadObject)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
headObject_sSECustomerAlgorithm :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_sSECustomerAlgorithm = Lens.lens (\HeadObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@HeadObject' {} a -> s {sSECustomerAlgorithm = a} :: HeadObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
headObject_sSECustomerKey :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_sSECustomerKey = Lens.lens (\HeadObject' {sSECustomerKey} -> sSECustomerKey) (\s@HeadObject' {} a -> s {sSECustomerKey = a} :: HeadObject) Prelude.. Lens.mapping Core._Sensitive

-- | Undocumented member.
headObject_requestPayer :: Lens.Lens' HeadObject (Prelude.Maybe RequestPayer)
headObject_requestPayer = Lens.lens (\HeadObject' {requestPayer} -> requestPayer) (\s@HeadObject' {} a -> s {requestPayer = a} :: HeadObject)

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
headObject_ifModifiedSince :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.UTCTime)
headObject_ifModifiedSince = Lens.lens (\HeadObject' {ifModifiedSince} -> ifModifiedSince) (\s@HeadObject' {} a -> s {ifModifiedSince = a} :: HeadObject) Prelude.. Lens.mapping Core._Time

-- | Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' HEAD request for the
-- part specified. Useful querying about the size of the part and the
-- number of parts in this object.
headObject_partNumber :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Int)
headObject_partNumber = Lens.lens (\HeadObject' {partNumber} -> partNumber) (\s@HeadObject' {} a -> s {partNumber = a} :: HeadObject)

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
headObject_range :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_range = Lens.lens (\HeadObject' {range} -> range) (\s@HeadObject' {} a -> s {range = a} :: HeadObject)

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
headObject_ifUnmodifiedSince :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.UTCTime)
headObject_ifUnmodifiedSince = Lens.lens (\HeadObject' {ifUnmodifiedSince} -> ifUnmodifiedSince) (\s@HeadObject' {} a -> s {ifUnmodifiedSince = a} :: HeadObject) Prelude.. Lens.mapping Core._Time

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
headObject_sSECustomerKeyMD5 :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_sSECustomerKeyMD5 = Lens.lens (\HeadObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@HeadObject' {} a -> s {sSECustomerKeyMD5 = a} :: HeadObject)

-- | Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
headObject_ifNoneMatch :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_ifNoneMatch = Lens.lens (\HeadObject' {ifNoneMatch} -> ifNoneMatch) (\s@HeadObject' {} a -> s {ifNoneMatch = a} :: HeadObject)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
headObject_expectedBucketOwner :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_expectedBucketOwner = Lens.lens (\HeadObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@HeadObject' {} a -> s {expectedBucketOwner = a} :: HeadObject)

-- | The name of the bucket containing the object.
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
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
headObject_bucket :: Lens.Lens' HeadObject BucketName
headObject_bucket = Lens.lens (\HeadObject' {bucket} -> bucket) (\s@HeadObject' {} a -> s {bucket = a} :: HeadObject)

-- | The object key.
headObject_key :: Lens.Lens' HeadObject ObjectKey
headObject_key = Lens.lens (\HeadObject' {key} -> key) (\s@HeadObject' {} a -> s {key = a} :: HeadObject)

instance Core.AWSRequest HeadObject where
  type AWSResponse HeadObject = HeadObjectResponse
  request =
    Request.s3vhost
      Prelude.. Request.head' defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          HeadObjectResponse'
            Prelude.<$> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (h Core..#? "x-amz-mp-parts-count")
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> (h Core..#? "x-amz-version-id")
            Prelude.<*> (h Core..#? "Content-Length")
            Prelude.<*> (h Core..#? "x-amz-object-lock-mode")
            Prelude.<*> (h Core..#? "Expires")
            Prelude.<*> (h Core..#? "x-amz-restore")
            Prelude.<*> (h Core..#? "x-amz-expiration")
            Prelude.<*> (h Core..#? "x-amz-delete-marker")
            Prelude.<*> (h Core..#? "x-amz-archive-status")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> (h Core..#? "x-amz-missing-meta")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Core..#? "x-amz-website-redirect-location")
            Prelude.<*> (h Core..#? "accept-ranges")
            Prelude.<*> (h Core..#? "x-amz-storage-class")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Core..#? "Content-Encoding")
            Prelude.<*> (h Core..#? "x-amz-object-lock-retain-until-date")
            Prelude.<*> (Core.parseHeadersMap "x-amz-meta-" h)
            Prelude.<*> (h Core..#? "x-amz-replication-status")
            Prelude.<*> (h Core..#? "Cache-Control")
            Prelude.<*> (h Core..#? "Content-Language")
            Prelude.<*> (h Core..#? "Last-Modified")
            Prelude.<*> (h Core..#? "x-amz-object-lock-legal-hold")
            Prelude.<*> (h Core..#? "Content-Disposition")
            Prelude.<*> (h Core..#? "x-amz-server-side-encryption")
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable HeadObject where
  hashWithSalt _salt HeadObject' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` sSECustomerAlgorithm
      `Prelude.hashWithSalt` sSECustomerKey
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` ifModifiedSince
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` ifUnmodifiedSince
      `Prelude.hashWithSalt` sSECustomerKeyMD5
      `Prelude.hashWithSalt` ifNoneMatch
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData HeadObject where
  rnf HeadObject' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKey
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf ifModifiedSince
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf range
      `Prelude.seq` Prelude.rnf ifUnmodifiedSince
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf ifNoneMatch
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Core.ToHeaders HeadObject where
  toHeaders HeadObject' {..} =
    Prelude.mconcat
      [ "If-Match" Core.=# ifMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey,
        "x-amz-request-payer" Core.=# requestPayer,
        "If-Modified-Since" Core.=# ifModifiedSince,
        "Range" Core.=# range,
        "If-Unmodified-Since" Core.=# ifUnmodifiedSince,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "If-None-Match" Core.=# ifNoneMatch,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath HeadObject where
  toPath HeadObject' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery HeadObject where
  toQuery HeadObject' {..} =
    Prelude.mconcat
      [ "versionId" Core.=: versionId,
        "partNumber" Core.=: partNumber
      ]

-- | /See:/ 'newHeadObjectResponse' smart constructor.
data HeadObjectResponse = HeadObjectResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The count of parts this object has.
    partsCount :: Prelude.Maybe Prelude.Int,
    -- | An ETag is an opaque identifier assigned by a web server to a specific
    -- version of a resource found at a URL.
    eTag :: Prelude.Maybe ETag,
    -- | Version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Size of the body in bytes.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | The Object Lock mode, if any, that\'s in effect for this object. This
    -- header is only returned if the requester has the @s3:GetObjectRetention@
    -- permission. For more information about S3 Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time at which the object is no longer cacheable.
    expires :: Prelude.Maybe Core.ISO8601,
    -- | If the object is an archived object (an object whose storage class is
    -- GLACIER), the response includes this header if either the archive
    -- restoration is in progress (see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject>
    -- or an archive copy is already restored.
    --
    -- If an archive copy is already restored, the header value indicates when
    -- Amazon S3 is scheduled to delete the object copy. For example:
    --
    -- @x-amz-restore: ongoing-request=\"false\", expiry-date=\"Fri, 21 Dec 2012 00:00:00 GMT\"@
    --
    -- If the object restoration is in progress, the header returns the value
    -- @ongoing-request=\"true\"@.
    --
    -- For more information about archiving objects, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations>.
    restore :: Prelude.Maybe Prelude.Text,
    -- | If the object expiration is configured (see PUT Bucket lifecycle), the
    -- response includes this header. It includes the expiry-date and rule-id
    -- key-value pairs providing object expiration information. The value of
    -- the rule-id is URL encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the object retrieved was (true) or was not (false) a
    -- Delete Marker. If false, this response header does not appear in the
    -- response.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | The archive state of the head object.
    archiveStatus :: Prelude.Maybe ArchiveStatus,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | This is set to the number of metadata entries not returned in
    -- @x-amz-meta@ headers. This can happen if you create metadata using an
    -- API like SOAP that supports more flexible metadata than the REST API.
    -- For example, using SOAP, you can create metadata whose values are not
    -- legal HTTP headers.
    missingMeta :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the object uses an S3 Bucket Key for server-side
    -- encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Prelude.Maybe Prelude.Text,
    -- | Indicates that a range of bytes was specified.
    acceptRanges :: Prelude.Maybe Prelude.Text,
    -- | Provides storage class information of the object. Amazon S3 returns this
    -- header for all objects except for S3 Standard storage class objects.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
    storageClass :: Prelude.Maybe StorageClass,
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
    -- | The date and time when the Object Lock retention period expires. This
    -- header is only returned if the requester has the @s3:GetObjectRetention@
    -- permission.
    objectLockRetainUntilDate :: Prelude.Maybe Core.ISO8601,
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Amazon S3 can return this header if your request involves a bucket that
    -- is either a source or a destination in a replication rule.
    --
    -- In replication, you have a source bucket on which you configure
    -- replication and destination bucket or buckets where Amazon S3 stores
    -- object replicas. When you request an object (@GetObject@) or object
    -- metadata (@HeadObject@) from these buckets, Amazon S3 will return the
    -- @x-amz-replication-status@ header in the response as follows:
    --
    -- -   If requesting an object from the source bucket — Amazon S3 will
    --     return the @x-amz-replication-status@ header if the object in your
    --     request is eligible for replication.
    --
    --     For example, suppose that in your replication configuration, you
    --     specify object prefix @TaxDocs@ requesting Amazon S3 to replicate
    --     objects with key prefix @TaxDocs@. Any objects you upload with this
    --     key name prefix, for example @TaxDocs\/document1.pdf@, are eligible
    --     for replication. For any object request with this key name prefix,
    --     Amazon S3 will return the @x-amz-replication-status@ header with
    --     value PENDING, COMPLETED or FAILED indicating object replication
    --     status.
    --
    -- -   If requesting an object from a destination bucket — Amazon S3 will
    --     return the @x-amz-replication-status@ header with value REPLICA if
    --     the object in your request is a replica that Amazon S3 created and
    --     there is no replica modification replication in progress.
    --
    -- -   When replicating objects to multiple destination buckets the
    --     @x-amz-replication-status@ header acts differently. The header of
    --     the source object will only return a value of COMPLETED when
    --     replication is successful to all destinations. The header will
    --     remain at value PENDING until replication has completed for all
    --     destinations. If one or more destinations fails replication the
    --     header will return FAILED.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication>.
    replicationStatus :: Prelude.Maybe ReplicationStatus,
    -- | Specifies caching behavior along the request\/reply chain.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
    -- | Creation date of the object.
    lastModified :: Prelude.Maybe Core.ISO8601,
    -- | Specifies whether a legal hold is in effect for this object. This header
    -- is only returned if the requester has the @s3:GetObjectLegalHold@
    -- permission. This header is not returned if the specified version of this
    -- object has never had a legal hold applied. For more information about S3
    -- Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | Specifies presentational information for the object.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | If the object is stored using server-side encryption either with an
    -- Amazon Web Services KMS key or an Amazon S3-managed encryption key, the
    -- response includes this header with the value of the server-side
    -- encryption algorithm used when storing this object in Amazon S3 (for
    -- example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeadObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'headObjectResponse_requestCharged' - Undocumented member.
--
-- 'partsCount', 'headObjectResponse_partsCount' - The count of parts this object has.
--
-- 'eTag', 'headObjectResponse_eTag' - An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
--
-- 'versionId', 'headObjectResponse_versionId' - Version of the object.
--
-- 'contentLength', 'headObjectResponse_contentLength' - Size of the body in bytes.
--
-- 'objectLockMode', 'headObjectResponse_objectLockMode' - The Object Lock mode, if any, that\'s in effect for this object. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission. For more information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
--
-- 'expires', 'headObjectResponse_expires' - The date and time at which the object is no longer cacheable.
--
-- 'restore', 'headObjectResponse_restore' - If the object is an archived object (an object whose storage class is
-- GLACIER), the response includes this header if either the archive
-- restoration is in progress (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject>
-- or an archive copy is already restored.
--
-- If an archive copy is already restored, the header value indicates when
-- Amazon S3 is scheduled to delete the object copy. For example:
--
-- @x-amz-restore: ongoing-request=\"false\", expiry-date=\"Fri, 21 Dec 2012 00:00:00 GMT\"@
--
-- If the object restoration is in progress, the header returns the value
-- @ongoing-request=\"true\"@.
--
-- For more information about archiving objects, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations>.
--
-- 'expiration', 'headObjectResponse_expiration' - If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key-value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
--
-- 'deleteMarker', 'headObjectResponse_deleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
--
-- 'archiveStatus', 'headObjectResponse_archiveStatus' - The archive state of the head object.
--
-- 'sSECustomerAlgorithm', 'headObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'missingMeta', 'headObjectResponse_missingMeta' - This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
--
-- 'bucketKeyEnabled', 'headObjectResponse_bucketKeyEnabled' - Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'websiteRedirectLocation', 'headObjectResponse_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'acceptRanges', 'headObjectResponse_acceptRanges' - Indicates that a range of bytes was specified.
--
-- 'storageClass', 'headObjectResponse_storageClass' - Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
--
-- 'sSECustomerKeyMD5', 'headObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSKeyId', 'headObjectResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
--
-- 'contentEncoding', 'headObjectResponse_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'objectLockRetainUntilDate', 'headObjectResponse_objectLockRetainUntilDate' - The date and time when the Object Lock retention period expires. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission.
--
-- 'metadata', 'headObjectResponse_metadata' - A map of metadata to store with the object in S3.
--
-- 'replicationStatus', 'headObjectResponse_replicationStatus' - Amazon S3 can return this header if your request involves a bucket that
-- is either a source or a destination in a replication rule.
--
-- In replication, you have a source bucket on which you configure
-- replication and destination bucket or buckets where Amazon S3 stores
-- object replicas. When you request an object (@GetObject@) or object
-- metadata (@HeadObject@) from these buckets, Amazon S3 will return the
-- @x-amz-replication-status@ header in the response as follows:
--
-- -   If requesting an object from the source bucket — Amazon S3 will
--     return the @x-amz-replication-status@ header if the object in your
--     request is eligible for replication.
--
--     For example, suppose that in your replication configuration, you
--     specify object prefix @TaxDocs@ requesting Amazon S3 to replicate
--     objects with key prefix @TaxDocs@. Any objects you upload with this
--     key name prefix, for example @TaxDocs\/document1.pdf@, are eligible
--     for replication. For any object request with this key name prefix,
--     Amazon S3 will return the @x-amz-replication-status@ header with
--     value PENDING, COMPLETED or FAILED indicating object replication
--     status.
--
-- -   If requesting an object from a destination bucket — Amazon S3 will
--     return the @x-amz-replication-status@ header with value REPLICA if
--     the object in your request is a replica that Amazon S3 created and
--     there is no replica modification replication in progress.
--
-- -   When replicating objects to multiple destination buckets the
--     @x-amz-replication-status@ header acts differently. The header of
--     the source object will only return a value of COMPLETED when
--     replication is successful to all destinations. The header will
--     remain at value PENDING until replication has completed for all
--     destinations. If one or more destinations fails replication the
--     header will return FAILED.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication>.
--
-- 'cacheControl', 'headObjectResponse_cacheControl' - Specifies caching behavior along the request\/reply chain.
--
-- 'contentLanguage', 'headObjectResponse_contentLanguage' - The language the content is in.
--
-- 'lastModified', 'headObjectResponse_lastModified' - Creation date of the object.
--
-- 'objectLockLegalHoldStatus', 'headObjectResponse_objectLockLegalHoldStatus' - Specifies whether a legal hold is in effect for this object. This header
-- is only returned if the requester has the @s3:GetObjectLegalHold@
-- permission. This header is not returned if the specified version of this
-- object has never had a legal hold applied. For more information about S3
-- Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
--
-- 'contentDisposition', 'headObjectResponse_contentDisposition' - Specifies presentational information for the object.
--
-- 'serverSideEncryption', 'headObjectResponse_serverSideEncryption' - If the object is stored using server-side encryption either with an
-- Amazon Web Services KMS key or an Amazon S3-managed encryption key, the
-- response includes this header with the value of the server-side
-- encryption algorithm used when storing this object in Amazon S3 (for
-- example, AES256, aws:kms).
--
-- 'contentType', 'headObjectResponse_contentType' - A standard MIME type describing the format of the object data.
--
-- 'httpStatus', 'headObjectResponse_httpStatus' - The response's http status code.
newHeadObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  HeadObjectResponse
newHeadObjectResponse pHttpStatus_ =
  HeadObjectResponse'
    { requestCharged =
        Prelude.Nothing,
      partsCount = Prelude.Nothing,
      eTag = Prelude.Nothing,
      versionId = Prelude.Nothing,
      contentLength = Prelude.Nothing,
      objectLockMode = Prelude.Nothing,
      expires = Prelude.Nothing,
      restore = Prelude.Nothing,
      expiration = Prelude.Nothing,
      deleteMarker = Prelude.Nothing,
      archiveStatus = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      missingMeta = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      websiteRedirectLocation = Prelude.Nothing,
      acceptRanges = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      objectLockRetainUntilDate = Prelude.Nothing,
      metadata = Prelude.mempty,
      replicationStatus = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      objectLockLegalHoldStatus = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
headObjectResponse_requestCharged :: Lens.Lens' HeadObjectResponse (Prelude.Maybe RequestCharged)
headObjectResponse_requestCharged = Lens.lens (\HeadObjectResponse' {requestCharged} -> requestCharged) (\s@HeadObjectResponse' {} a -> s {requestCharged = a} :: HeadObjectResponse)

-- | The count of parts this object has.
headObjectResponse_partsCount :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Int)
headObjectResponse_partsCount = Lens.lens (\HeadObjectResponse' {partsCount} -> partsCount) (\s@HeadObjectResponse' {} a -> s {partsCount = a} :: HeadObjectResponse)

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
headObjectResponse_eTag :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ETag)
headObjectResponse_eTag = Lens.lens (\HeadObjectResponse' {eTag} -> eTag) (\s@HeadObjectResponse' {} a -> s {eTag = a} :: HeadObjectResponse)

-- | Version of the object.
headObjectResponse_versionId :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ObjectVersionId)
headObjectResponse_versionId = Lens.lens (\HeadObjectResponse' {versionId} -> versionId) (\s@HeadObjectResponse' {} a -> s {versionId = a} :: HeadObjectResponse)

-- | Size of the body in bytes.
headObjectResponse_contentLength :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Integer)
headObjectResponse_contentLength = Lens.lens (\HeadObjectResponse' {contentLength} -> contentLength) (\s@HeadObjectResponse' {} a -> s {contentLength = a} :: HeadObjectResponse)

-- | The Object Lock mode, if any, that\'s in effect for this object. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission. For more information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
headObjectResponse_objectLockMode :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ObjectLockMode)
headObjectResponse_objectLockMode = Lens.lens (\HeadObjectResponse' {objectLockMode} -> objectLockMode) (\s@HeadObjectResponse' {} a -> s {objectLockMode = a} :: HeadObjectResponse)

-- | The date and time at which the object is no longer cacheable.
headObjectResponse_expires :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.UTCTime)
headObjectResponse_expires = Lens.lens (\HeadObjectResponse' {expires} -> expires) (\s@HeadObjectResponse' {} a -> s {expires = a} :: HeadObjectResponse) Prelude.. Lens.mapping Core._Time

-- | If the object is an archived object (an object whose storage class is
-- GLACIER), the response includes this header if either the archive
-- restoration is in progress (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject>
-- or an archive copy is already restored.
--
-- If an archive copy is already restored, the header value indicates when
-- Amazon S3 is scheduled to delete the object copy. For example:
--
-- @x-amz-restore: ongoing-request=\"false\", expiry-date=\"Fri, 21 Dec 2012 00:00:00 GMT\"@
--
-- If the object restoration is in progress, the header returns the value
-- @ongoing-request=\"true\"@.
--
-- For more information about archiving objects, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations>.
headObjectResponse_restore :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_restore = Lens.lens (\HeadObjectResponse' {restore} -> restore) (\s@HeadObjectResponse' {} a -> s {restore = a} :: HeadObjectResponse)

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key-value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
headObjectResponse_expiration :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_expiration = Lens.lens (\HeadObjectResponse' {expiration} -> expiration) (\s@HeadObjectResponse' {} a -> s {expiration = a} :: HeadObjectResponse)

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
headObjectResponse_deleteMarker :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Bool)
headObjectResponse_deleteMarker = Lens.lens (\HeadObjectResponse' {deleteMarker} -> deleteMarker) (\s@HeadObjectResponse' {} a -> s {deleteMarker = a} :: HeadObjectResponse)

-- | The archive state of the head object.
headObjectResponse_archiveStatus :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ArchiveStatus)
headObjectResponse_archiveStatus = Lens.lens (\HeadObjectResponse' {archiveStatus} -> archiveStatus) (\s@HeadObjectResponse' {} a -> s {archiveStatus = a} :: HeadObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
headObjectResponse_sSECustomerAlgorithm :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_sSECustomerAlgorithm = Lens.lens (\HeadObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@HeadObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: HeadObjectResponse)

-- | This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
headObjectResponse_missingMeta :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Int)
headObjectResponse_missingMeta = Lens.lens (\HeadObjectResponse' {missingMeta} -> missingMeta) (\s@HeadObjectResponse' {} a -> s {missingMeta = a} :: HeadObjectResponse)

-- | Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with Amazon Web Services KMS (SSE-KMS).
headObjectResponse_bucketKeyEnabled :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Bool)
headObjectResponse_bucketKeyEnabled = Lens.lens (\HeadObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@HeadObjectResponse' {} a -> s {bucketKeyEnabled = a} :: HeadObjectResponse)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
headObjectResponse_websiteRedirectLocation :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_websiteRedirectLocation = Lens.lens (\HeadObjectResponse' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@HeadObjectResponse' {} a -> s {websiteRedirectLocation = a} :: HeadObjectResponse)

-- | Indicates that a range of bytes was specified.
headObjectResponse_acceptRanges :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_acceptRanges = Lens.lens (\HeadObjectResponse' {acceptRanges} -> acceptRanges) (\s@HeadObjectResponse' {} a -> s {acceptRanges = a} :: HeadObjectResponse)

-- | Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
headObjectResponse_storageClass :: Lens.Lens' HeadObjectResponse (Prelude.Maybe StorageClass)
headObjectResponse_storageClass = Lens.lens (\HeadObjectResponse' {storageClass} -> storageClass) (\s@HeadObjectResponse' {} a -> s {storageClass = a} :: HeadObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
headObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_sSECustomerKeyMD5 = Lens.lens (\HeadObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@HeadObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: HeadObjectResponse)

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
headObjectResponse_sSEKMSKeyId :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_sSEKMSKeyId = Lens.lens (\HeadObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@HeadObjectResponse' {} a -> s {sSEKMSKeyId = a} :: HeadObjectResponse) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
headObjectResponse_contentEncoding :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentEncoding = Lens.lens (\HeadObjectResponse' {contentEncoding} -> contentEncoding) (\s@HeadObjectResponse' {} a -> s {contentEncoding = a} :: HeadObjectResponse)

-- | The date and time when the Object Lock retention period expires. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission.
headObjectResponse_objectLockRetainUntilDate :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.UTCTime)
headObjectResponse_objectLockRetainUntilDate = Lens.lens (\HeadObjectResponse' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@HeadObjectResponse' {} a -> s {objectLockRetainUntilDate = a} :: HeadObjectResponse) Prelude.. Lens.mapping Core._Time

-- | A map of metadata to store with the object in S3.
headObjectResponse_metadata :: Lens.Lens' HeadObjectResponse (Prelude.HashMap Prelude.Text Prelude.Text)
headObjectResponse_metadata = Lens.lens (\HeadObjectResponse' {metadata} -> metadata) (\s@HeadObjectResponse' {} a -> s {metadata = a} :: HeadObjectResponse) Prelude.. Lens.coerced

-- | Amazon S3 can return this header if your request involves a bucket that
-- is either a source or a destination in a replication rule.
--
-- In replication, you have a source bucket on which you configure
-- replication and destination bucket or buckets where Amazon S3 stores
-- object replicas. When you request an object (@GetObject@) or object
-- metadata (@HeadObject@) from these buckets, Amazon S3 will return the
-- @x-amz-replication-status@ header in the response as follows:
--
-- -   If requesting an object from the source bucket — Amazon S3 will
--     return the @x-amz-replication-status@ header if the object in your
--     request is eligible for replication.
--
--     For example, suppose that in your replication configuration, you
--     specify object prefix @TaxDocs@ requesting Amazon S3 to replicate
--     objects with key prefix @TaxDocs@. Any objects you upload with this
--     key name prefix, for example @TaxDocs\/document1.pdf@, are eligible
--     for replication. For any object request with this key name prefix,
--     Amazon S3 will return the @x-amz-replication-status@ header with
--     value PENDING, COMPLETED or FAILED indicating object replication
--     status.
--
-- -   If requesting an object from a destination bucket — Amazon S3 will
--     return the @x-amz-replication-status@ header with value REPLICA if
--     the object in your request is a replica that Amazon S3 created and
--     there is no replica modification replication in progress.
--
-- -   When replicating objects to multiple destination buckets the
--     @x-amz-replication-status@ header acts differently. The header of
--     the source object will only return a value of COMPLETED when
--     replication is successful to all destinations. The header will
--     remain at value PENDING until replication has completed for all
--     destinations. If one or more destinations fails replication the
--     header will return FAILED.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication>.
headObjectResponse_replicationStatus :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ReplicationStatus)
headObjectResponse_replicationStatus = Lens.lens (\HeadObjectResponse' {replicationStatus} -> replicationStatus) (\s@HeadObjectResponse' {} a -> s {replicationStatus = a} :: HeadObjectResponse)

-- | Specifies caching behavior along the request\/reply chain.
headObjectResponse_cacheControl :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_cacheControl = Lens.lens (\HeadObjectResponse' {cacheControl} -> cacheControl) (\s@HeadObjectResponse' {} a -> s {cacheControl = a} :: HeadObjectResponse)

-- | The language the content is in.
headObjectResponse_contentLanguage :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentLanguage = Lens.lens (\HeadObjectResponse' {contentLanguage} -> contentLanguage) (\s@HeadObjectResponse' {} a -> s {contentLanguage = a} :: HeadObjectResponse)

-- | Creation date of the object.
headObjectResponse_lastModified :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.UTCTime)
headObjectResponse_lastModified = Lens.lens (\HeadObjectResponse' {lastModified} -> lastModified) (\s@HeadObjectResponse' {} a -> s {lastModified = a} :: HeadObjectResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies whether a legal hold is in effect for this object. This header
-- is only returned if the requester has the @s3:GetObjectLegalHold@
-- permission. This header is not returned if the specified version of this
-- object has never had a legal hold applied. For more information about S3
-- Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
headObjectResponse_objectLockLegalHoldStatus :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ObjectLockLegalHoldStatus)
headObjectResponse_objectLockLegalHoldStatus = Lens.lens (\HeadObjectResponse' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@HeadObjectResponse' {} a -> s {objectLockLegalHoldStatus = a} :: HeadObjectResponse)

-- | Specifies presentational information for the object.
headObjectResponse_contentDisposition :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentDisposition = Lens.lens (\HeadObjectResponse' {contentDisposition} -> contentDisposition) (\s@HeadObjectResponse' {} a -> s {contentDisposition = a} :: HeadObjectResponse)

-- | If the object is stored using server-side encryption either with an
-- Amazon Web Services KMS key or an Amazon S3-managed encryption key, the
-- response includes this header with the value of the server-side
-- encryption algorithm used when storing this object in Amazon S3 (for
-- example, AES256, aws:kms).
headObjectResponse_serverSideEncryption :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ServerSideEncryption)
headObjectResponse_serverSideEncryption = Lens.lens (\HeadObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@HeadObjectResponse' {} a -> s {serverSideEncryption = a} :: HeadObjectResponse)

-- | A standard MIME type describing the format of the object data.
headObjectResponse_contentType :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentType = Lens.lens (\HeadObjectResponse' {contentType} -> contentType) (\s@HeadObjectResponse' {} a -> s {contentType = a} :: HeadObjectResponse)

-- | The response's http status code.
headObjectResponse_httpStatus :: Lens.Lens' HeadObjectResponse Prelude.Int
headObjectResponse_httpStatus = Lens.lens (\HeadObjectResponse' {httpStatus} -> httpStatus) (\s@HeadObjectResponse' {} a -> s {httpStatus = a} :: HeadObjectResponse)

instance Prelude.NFData HeadObjectResponse where
  rnf HeadObjectResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf partsCount
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf contentLength
      `Prelude.seq` Prelude.rnf objectLockMode
      `Prelude.seq` Prelude.rnf expires
      `Prelude.seq` Prelude.rnf restore
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf deleteMarker
      `Prelude.seq` Prelude.rnf archiveStatus
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf missingMeta
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf websiteRedirectLocation
      `Prelude.seq` Prelude.rnf acceptRanges
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf contentEncoding
      `Prelude.seq` Prelude.rnf
        objectLockRetainUntilDate
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf
        replicationStatus
      `Prelude.seq` Prelude.rnf
        cacheControl
      `Prelude.seq` Prelude.rnf
        contentLanguage
      `Prelude.seq` Prelude.rnf
        lastModified
      `Prelude.seq` Prelude.rnf
        objectLockLegalHoldStatus
      `Prelude.seq` Prelude.rnf
        contentDisposition
      `Prelude.seq` Prelude.rnf
        serverSideEncryption
      `Prelude.seq` Prelude.rnf
        contentType
      `Prelude.seq` Prelude.rnf
        httpStatus

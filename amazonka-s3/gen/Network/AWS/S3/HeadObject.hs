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
-- Module      : Network.AWS.S3.HeadObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The HEAD operation retrieves metadata from an object without returning
-- the object itself. This operation is useful if you\'re only interested
-- in an object\'s metadata. To use HEAD, you must have READ access to the
-- object.
--
-- A @HEAD@ request has the same options as a @GET@ operation on an object.
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
--     encryption with CMKs stored in AWS KMS (SSE-KMS) or server-side
--     encryption with Amazon S3–managed encryption keys (SSE-S3). If your
--     object does use these types of keys, you’ll get an HTTP 400
--     BadRequest error.
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
-- You need the @s3:GetObject@ permission for this operation. For more
-- information, see
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
-- The following operation is related to @HeadObject@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.HeadObject
  ( -- * Creating a Request
    HeadObject (..),
    newHeadObject,

    -- * Request Lenses
    headObject_ifUnmodifiedSince,
    headObject_range,
    headObject_ifModifiedSince,
    headObject_expectedBucketOwner,
    headObject_sSECustomerKeyMD5,
    headObject_versionId,
    headObject_ifMatch,
    headObject_partNumber,
    headObject_ifNoneMatch,
    headObject_sSECustomerAlgorithm,
    headObject_requestPayer,
    headObject_sSECustomerKey,
    headObject_bucket,
    headObject_key,

    -- * Destructuring the Response
    HeadObjectResponse (..),
    newHeadObjectResponse,

    -- * Response Lenses
    headObjectResponse_eTag,
    headObjectResponse_requestCharged,
    headObjectResponse_partsCount,
    headObjectResponse_websiteRedirectLocation,
    headObjectResponse_contentType,
    headObjectResponse_contentDisposition,
    headObjectResponse_archiveStatus,
    headObjectResponse_deleteMarker,
    headObjectResponse_expiration,
    headObjectResponse_contentLanguage,
    headObjectResponse_replicationStatus,
    headObjectResponse_metadata,
    headObjectResponse_contentLength,
    headObjectResponse_contentEncoding,
    headObjectResponse_sSEKMSKeyId,
    headObjectResponse_sSECustomerKeyMD5,
    headObjectResponse_storageClass,
    headObjectResponse_versionId,
    headObjectResponse_acceptRanges,
    headObjectResponse_bucketKeyEnabled,
    headObjectResponse_serverSideEncryption,
    headObjectResponse_missingMeta,
    headObjectResponse_objectLockLegalHoldStatus,
    headObjectResponse_sSECustomerAlgorithm,
    headObjectResponse_lastModified,
    headObjectResponse_cacheControl,
    headObjectResponse_expires,
    headObjectResponse_restore,
    headObjectResponse_objectLockMode,
    headObjectResponse_objectLockRetainUntilDate,
    headObjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newHeadObject' smart constructor.
data HeadObject = HeadObject'
  { -- | Return the object only if it has not been modified since the specified
    -- time, otherwise return a 412 (precondition failed).
    ifUnmodifiedSince :: Prelude.Maybe Core.ISO8601,
    -- | Downloads the specified range bytes of an object. For more information
    -- about the HTTP Range header, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
    --
    -- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
    -- request.
    range :: Prelude.Maybe Prelude.Text,
    -- | Return the object only if it has been modified since the specified time,
    -- otherwise return a 304 (not modified).
    ifModifiedSince :: Prelude.Maybe Core.ISO8601,
    -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
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
    -- 1 and 10,000. Effectively performs a \'ranged\' HEAD request for the
    -- part specified. Useful querying about the size of the part and the
    -- number of parts in this object.
    partNumber :: Prelude.Maybe Prelude.Int,
    -- | Return the object only if its entity tag (ETag) is different from the
    -- one specified, otherwise return a 304 (not modified).
    ifNoneMatch :: Prelude.Maybe Prelude.Text,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the bucket containing the object.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- When using this API with Amazon S3 on Outposts, you must direct requests
    -- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
    -- form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this operation using S3 on Outposts through the AWS SDKs, you
    -- provide the Outposts bucket ARN in place of the bucket name. For more
    -- information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon Simple Storage Service Developer Guide/.
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
-- 'ifUnmodifiedSince', 'headObject_ifUnmodifiedSince' - Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
--
-- 'range', 'headObject_range' - Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
--
-- 'ifModifiedSince', 'headObject_ifModifiedSince' - Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
--
-- 'expectedBucketOwner', 'headObject_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'sSECustomerKeyMD5', 'headObject_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'versionId', 'headObject_versionId' - VersionId used to reference a specific version of the object.
--
-- 'ifMatch', 'headObject_ifMatch' - Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
--
-- 'partNumber', 'headObject_partNumber' - Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' HEAD request for the
-- part specified. Useful querying about the size of the part and the
-- number of parts in this object.
--
-- 'ifNoneMatch', 'headObject_ifNoneMatch' - Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
--
-- 'sSECustomerAlgorithm', 'headObject_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'requestPayer', 'headObject_requestPayer' - Undocumented member.
--
-- 'sSECustomerKey', 'headObject_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'bucket', 'headObject_bucket' - The name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
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
    { ifUnmodifiedSince = Prelude.Nothing,
      range = Prelude.Nothing,
      ifModifiedSince = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      versionId = Prelude.Nothing,
      ifMatch = Prelude.Nothing,
      partNumber = Prelude.Nothing,
      ifNoneMatch = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
headObject_ifUnmodifiedSince :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.UTCTime)
headObject_ifUnmodifiedSince = Lens.lens (\HeadObject' {ifUnmodifiedSince} -> ifUnmodifiedSince) (\s@HeadObject' {} a -> s {ifUnmodifiedSince = a} :: HeadObject) Prelude.. Lens.mapping Core._Time

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>.
--
-- Amazon S3 doesn\'t support retrieving multiple ranges of data per @GET@
-- request.
headObject_range :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_range = Lens.lens (\HeadObject' {range} -> range) (\s@HeadObject' {} a -> s {range = a} :: HeadObject)

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
headObject_ifModifiedSince :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.UTCTime)
headObject_ifModifiedSince = Lens.lens (\HeadObject' {ifModifiedSince} -> ifModifiedSince) (\s@HeadObject' {} a -> s {ifModifiedSince = a} :: HeadObject) Prelude.. Lens.mapping Core._Time

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
headObject_expectedBucketOwner :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_expectedBucketOwner = Lens.lens (\HeadObject' {expectedBucketOwner} -> expectedBucketOwner) (\s@HeadObject' {} a -> s {expectedBucketOwner = a} :: HeadObject)

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
headObject_sSECustomerKeyMD5 :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_sSECustomerKeyMD5 = Lens.lens (\HeadObject' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@HeadObject' {} a -> s {sSECustomerKeyMD5 = a} :: HeadObject)

-- | VersionId used to reference a specific version of the object.
headObject_versionId :: Lens.Lens' HeadObject (Prelude.Maybe ObjectVersionId)
headObject_versionId = Lens.lens (\HeadObject' {versionId} -> versionId) (\s@HeadObject' {} a -> s {versionId = a} :: HeadObject)

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
headObject_ifMatch :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_ifMatch = Lens.lens (\HeadObject' {ifMatch} -> ifMatch) (\s@HeadObject' {} a -> s {ifMatch = a} :: HeadObject)

-- | Part number of the object being read. This is a positive integer between
-- 1 and 10,000. Effectively performs a \'ranged\' HEAD request for the
-- part specified. Useful querying about the size of the part and the
-- number of parts in this object.
headObject_partNumber :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Int)
headObject_partNumber = Lens.lens (\HeadObject' {partNumber} -> partNumber) (\s@HeadObject' {} a -> s {partNumber = a} :: HeadObject)

-- | Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
headObject_ifNoneMatch :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_ifNoneMatch = Lens.lens (\HeadObject' {ifNoneMatch} -> ifNoneMatch) (\s@HeadObject' {} a -> s {ifNoneMatch = a} :: HeadObject)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
headObject_sSECustomerAlgorithm :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_sSECustomerAlgorithm = Lens.lens (\HeadObject' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@HeadObject' {} a -> s {sSECustomerAlgorithm = a} :: HeadObject)

-- | Undocumented member.
headObject_requestPayer :: Lens.Lens' HeadObject (Prelude.Maybe RequestPayer)
headObject_requestPayer = Lens.lens (\HeadObject' {requestPayer} -> requestPayer) (\s@HeadObject' {} a -> s {requestPayer = a} :: HeadObject)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
headObject_sSECustomerKey :: Lens.Lens' HeadObject (Prelude.Maybe Prelude.Text)
headObject_sSECustomerKey = Lens.lens (\HeadObject' {sSECustomerKey} -> sSECustomerKey) (\s@HeadObject' {} a -> s {sSECustomerKey = a} :: HeadObject) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the bucket containing the object.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
headObject_bucket :: Lens.Lens' HeadObject BucketName
headObject_bucket = Lens.lens (\HeadObject' {bucket} -> bucket) (\s@HeadObject' {} a -> s {bucket = a} :: HeadObject)

-- | The object key.
headObject_key :: Lens.Lens' HeadObject ObjectKey
headObject_key = Lens.lens (\HeadObject' {key} -> key) (\s@HeadObject' {} a -> s {key = a} :: HeadObject)

instance Core.AWSRequest HeadObject where
  type AWSResponse HeadObject = HeadObjectResponse
  request = Request.head' defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          HeadObjectResponse'
            Prelude.<$> (h Core..#? "ETag")
            Prelude.<*> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (h Core..#? "x-amz-mp-parts-count")
            Prelude.<*> (h Core..#? "x-amz-website-redirect-location")
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (h Core..#? "Content-Disposition")
            Prelude.<*> (h Core..#? "x-amz-archive-status")
            Prelude.<*> (h Core..#? "x-amz-delete-marker")
            Prelude.<*> (h Core..#? "x-amz-expiration")
            Prelude.<*> (h Core..#? "Content-Language")
            Prelude.<*> (h Core..#? "x-amz-replication-status")
            Prelude.<*> (Core.parseHeadersMap "x-amz-meta-" h)
            Prelude.<*> (h Core..#? "Content-Length")
            Prelude.<*> (h Core..#? "Content-Encoding")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> (h Core..#? "x-amz-storage-class")
            Prelude.<*> (h Core..#? "x-amz-version-id")
            Prelude.<*> (h Core..#? "accept-ranges")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Core..#? "x-amz-server-side-encryption")
            Prelude.<*> (h Core..#? "x-amz-missing-meta")
            Prelude.<*> (h Core..#? "x-amz-object-lock-legal-hold")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> (h Core..#? "Last-Modified")
            Prelude.<*> (h Core..#? "Cache-Control")
            Prelude.<*> (h Core..#? "Expires")
            Prelude.<*> (h Core..#? "x-amz-restore")
            Prelude.<*> (h Core..#? "x-amz-object-lock-mode")
            Prelude.<*> (h Core..#? "x-amz-object-lock-retain-until-date")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable HeadObject

instance Prelude.NFData HeadObject

instance Core.ToHeaders HeadObject where
  toHeaders HeadObject' {..} =
    Prelude.mconcat
      [ "If-Unmodified-Since" Core.=# ifUnmodifiedSince,
        "Range" Core.=# range,
        "If-Modified-Since" Core.=# ifModifiedSince,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "If-Match" Core.=# ifMatch,
        "If-None-Match" Core.=# ifNoneMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey
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
  { -- | An ETag is an opaque identifier assigned by a web server to a specific
    -- version of a resource found at a URL.
    eTag :: Prelude.Maybe ETag,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | The count of parts this object has.
    partsCount :: Prelude.Maybe Prelude.Int,
    -- | If the bucket is configured as a website, redirects requests for this
    -- object to another object in the same bucket or to an external URL.
    -- Amazon S3 stores the value of this header in the object metadata.
    websiteRedirectLocation :: Prelude.Maybe Prelude.Text,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | Specifies presentational information for the object.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The archive state of the head object.
    archiveStatus :: Prelude.Maybe ArchiveStatus,
    -- | Specifies whether the object retrieved was (true) or was not (false) a
    -- Delete Marker. If false, this response header does not appear in the
    -- response.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | If the object expiration is configured (see PUT Bucket lifecycle), the
    -- response includes this header. It includes the expiry-date and rule-id
    -- key-value pairs providing object expiration information. The value of
    -- the rule-id is URL encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | The language the content is in.
    contentLanguage :: Prelude.Maybe Prelude.Text,
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
    -- | A map of metadata to store with the object in S3.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Size of the body in bytes.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | Specifies what content encodings have been applied to the object and
    -- thus what decoding mechanisms must be applied to obtain the media-type
    -- referenced by the Content-Type header field.
    contentEncoding :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
    -- symmetric customer managed customer master key (CMK) that was used for
    -- the object.
    sSEKMSKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | Provides storage class information of the object. Amazon S3 returns this
    -- header for all objects except for S3 Standard storage class objects.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
    storageClass :: Prelude.Maybe StorageClass,
    -- | Version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Indicates that a range of bytes was specified.
    acceptRanges :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the object uses an S3 Bucket Key for server-side
    -- encryption with AWS KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If the object is stored using server-side encryption either with an AWS
    -- KMS customer master key (CMK) or an Amazon S3-managed encryption key,
    -- the response includes this header with the value of the server-side
    -- encryption algorithm used when storing this object in Amazon S3 (for
    -- example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | This is set to the number of metadata entries not returned in
    -- @x-amz-meta@ headers. This can happen if you create metadata using an
    -- API like SOAP that supports more flexible metadata than the REST API.
    -- For example, using SOAP, you can create metadata whose values are not
    -- legal HTTP headers.
    missingMeta :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether a legal hold is in effect for this object. This header
    -- is only returned if the requester has the @s3:GetObjectLegalHold@
    -- permission. This header is not returned if the specified version of this
    -- object has never had a legal hold applied. For more information about S3
    -- Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
    objectLockLegalHoldStatus :: Prelude.Maybe ObjectLockLegalHoldStatus,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Creation date of the object.
    lastModified :: Prelude.Maybe Core.ISO8601,
    -- | Specifies caching behavior along the request\/reply chain.
    cacheControl :: Prelude.Maybe Prelude.Text,
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
    -- @x-amz-restore: ongoing-request=\"false\", expiry-date=\"Fri, 23 Dec 2012 00:00:00 GMT\"@
    --
    -- If the object restoration is in progress, the header returns the value
    -- @ongoing-request=\"true\"@.
    --
    -- For more information about archiving objects, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations>.
    restore :: Prelude.Maybe Prelude.Text,
    -- | The Object Lock mode, if any, that\'s in effect for this object. This
    -- header is only returned if the requester has the @s3:GetObjectRetention@
    -- permission. For more information about S3 Object Lock, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
    objectLockMode :: Prelude.Maybe ObjectLockMode,
    -- | The date and time when the Object Lock retention period expires. This
    -- header is only returned if the requester has the @s3:GetObjectRetention@
    -- permission.
    objectLockRetainUntilDate :: Prelude.Maybe Core.ISO8601,
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
-- 'eTag', 'headObjectResponse_eTag' - An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
--
-- 'requestCharged', 'headObjectResponse_requestCharged' - Undocumented member.
--
-- 'partsCount', 'headObjectResponse_partsCount' - The count of parts this object has.
--
-- 'websiteRedirectLocation', 'headObjectResponse_websiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
--
-- 'contentType', 'headObjectResponse_contentType' - A standard MIME type describing the format of the object data.
--
-- 'contentDisposition', 'headObjectResponse_contentDisposition' - Specifies presentational information for the object.
--
-- 'archiveStatus', 'headObjectResponse_archiveStatus' - The archive state of the head object.
--
-- 'deleteMarker', 'headObjectResponse_deleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
--
-- 'expiration', 'headObjectResponse_expiration' - If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key-value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
--
-- 'contentLanguage', 'headObjectResponse_contentLanguage' - The language the content is in.
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
-- 'metadata', 'headObjectResponse_metadata' - A map of metadata to store with the object in S3.
--
-- 'contentLength', 'headObjectResponse_contentLength' - Size of the body in bytes.
--
-- 'contentEncoding', 'headObjectResponse_contentEncoding' - Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
--
-- 'sSEKMSKeyId', 'headObjectResponse_sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
--
-- 'sSECustomerKeyMD5', 'headObjectResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'storageClass', 'headObjectResponse_storageClass' - Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
--
-- 'versionId', 'headObjectResponse_versionId' - Version of the object.
--
-- 'acceptRanges', 'headObjectResponse_acceptRanges' - Indicates that a range of bytes was specified.
--
-- 'bucketKeyEnabled', 'headObjectResponse_bucketKeyEnabled' - Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with AWS KMS (SSE-KMS).
--
-- 'serverSideEncryption', 'headObjectResponse_serverSideEncryption' - If the object is stored using server-side encryption either with an AWS
-- KMS customer master key (CMK) or an Amazon S3-managed encryption key,
-- the response includes this header with the value of the server-side
-- encryption algorithm used when storing this object in Amazon S3 (for
-- example, AES256, aws:kms).
--
-- 'missingMeta', 'headObjectResponse_missingMeta' - This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
--
-- 'objectLockLegalHoldStatus', 'headObjectResponse_objectLockLegalHoldStatus' - Specifies whether a legal hold is in effect for this object. This header
-- is only returned if the requester has the @s3:GetObjectLegalHold@
-- permission. This header is not returned if the specified version of this
-- object has never had a legal hold applied. For more information about S3
-- Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
--
-- 'sSECustomerAlgorithm', 'headObjectResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'lastModified', 'headObjectResponse_lastModified' - Creation date of the object.
--
-- 'cacheControl', 'headObjectResponse_cacheControl' - Specifies caching behavior along the request\/reply chain.
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
-- @x-amz-restore: ongoing-request=\"false\", expiry-date=\"Fri, 23 Dec 2012 00:00:00 GMT\"@
--
-- If the object restoration is in progress, the header returns the value
-- @ongoing-request=\"true\"@.
--
-- For more information about archiving objects, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations>.
--
-- 'objectLockMode', 'headObjectResponse_objectLockMode' - The Object Lock mode, if any, that\'s in effect for this object. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission. For more information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
--
-- 'objectLockRetainUntilDate', 'headObjectResponse_objectLockRetainUntilDate' - The date and time when the Object Lock retention period expires. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission.
--
-- 'httpStatus', 'headObjectResponse_httpStatus' - The response's http status code.
newHeadObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  HeadObjectResponse
newHeadObjectResponse pHttpStatus_ =
  HeadObjectResponse'
    { eTag = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      partsCount = Prelude.Nothing,
      websiteRedirectLocation = Prelude.Nothing,
      contentType = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      archiveStatus = Prelude.Nothing,
      deleteMarker = Prelude.Nothing,
      expiration = Prelude.Nothing,
      contentLanguage = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      metadata = Prelude.mempty,
      contentLength = Prelude.Nothing,
      contentEncoding = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      versionId = Prelude.Nothing,
      acceptRanges = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      missingMeta = Prelude.Nothing,
      objectLockLegalHoldStatus = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      expires = Prelude.Nothing,
      restore = Prelude.Nothing,
      objectLockMode = Prelude.Nothing,
      objectLockRetainUntilDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
headObjectResponse_eTag :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ETag)
headObjectResponse_eTag = Lens.lens (\HeadObjectResponse' {eTag} -> eTag) (\s@HeadObjectResponse' {} a -> s {eTag = a} :: HeadObjectResponse)

-- | Undocumented member.
headObjectResponse_requestCharged :: Lens.Lens' HeadObjectResponse (Prelude.Maybe RequestCharged)
headObjectResponse_requestCharged = Lens.lens (\HeadObjectResponse' {requestCharged} -> requestCharged) (\s@HeadObjectResponse' {} a -> s {requestCharged = a} :: HeadObjectResponse)

-- | The count of parts this object has.
headObjectResponse_partsCount :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Int)
headObjectResponse_partsCount = Lens.lens (\HeadObjectResponse' {partsCount} -> partsCount) (\s@HeadObjectResponse' {} a -> s {partsCount = a} :: HeadObjectResponse)

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
headObjectResponse_websiteRedirectLocation :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_websiteRedirectLocation = Lens.lens (\HeadObjectResponse' {websiteRedirectLocation} -> websiteRedirectLocation) (\s@HeadObjectResponse' {} a -> s {websiteRedirectLocation = a} :: HeadObjectResponse)

-- | A standard MIME type describing the format of the object data.
headObjectResponse_contentType :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentType = Lens.lens (\HeadObjectResponse' {contentType} -> contentType) (\s@HeadObjectResponse' {} a -> s {contentType = a} :: HeadObjectResponse)

-- | Specifies presentational information for the object.
headObjectResponse_contentDisposition :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentDisposition = Lens.lens (\HeadObjectResponse' {contentDisposition} -> contentDisposition) (\s@HeadObjectResponse' {} a -> s {contentDisposition = a} :: HeadObjectResponse)

-- | The archive state of the head object.
headObjectResponse_archiveStatus :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ArchiveStatus)
headObjectResponse_archiveStatus = Lens.lens (\HeadObjectResponse' {archiveStatus} -> archiveStatus) (\s@HeadObjectResponse' {} a -> s {archiveStatus = a} :: HeadObjectResponse)

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
headObjectResponse_deleteMarker :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Bool)
headObjectResponse_deleteMarker = Lens.lens (\HeadObjectResponse' {deleteMarker} -> deleteMarker) (\s@HeadObjectResponse' {} a -> s {deleteMarker = a} :: HeadObjectResponse)

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key-value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
headObjectResponse_expiration :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_expiration = Lens.lens (\HeadObjectResponse' {expiration} -> expiration) (\s@HeadObjectResponse' {} a -> s {expiration = a} :: HeadObjectResponse)

-- | The language the content is in.
headObjectResponse_contentLanguage :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentLanguage = Lens.lens (\HeadObjectResponse' {contentLanguage} -> contentLanguage) (\s@HeadObjectResponse' {} a -> s {contentLanguage = a} :: HeadObjectResponse)

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

-- | A map of metadata to store with the object in S3.
headObjectResponse_metadata :: Lens.Lens' HeadObjectResponse (Prelude.HashMap Prelude.Text Prelude.Text)
headObjectResponse_metadata = Lens.lens (\HeadObjectResponse' {metadata} -> metadata) (\s@HeadObjectResponse' {} a -> s {metadata = a} :: HeadObjectResponse) Prelude.. Lens._Coerce

-- | Size of the body in bytes.
headObjectResponse_contentLength :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Integer)
headObjectResponse_contentLength = Lens.lens (\HeadObjectResponse' {contentLength} -> contentLength) (\s@HeadObjectResponse' {} a -> s {contentLength = a} :: HeadObjectResponse)

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
headObjectResponse_contentEncoding :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_contentEncoding = Lens.lens (\HeadObjectResponse' {contentEncoding} -> contentEncoding) (\s@HeadObjectResponse' {} a -> s {contentEncoding = a} :: HeadObjectResponse)

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
headObjectResponse_sSEKMSKeyId :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_sSEKMSKeyId = Lens.lens (\HeadObjectResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@HeadObjectResponse' {} a -> s {sSEKMSKeyId = a} :: HeadObjectResponse) Prelude.. Lens.mapping Core._Sensitive

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
headObjectResponse_sSECustomerKeyMD5 :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_sSECustomerKeyMD5 = Lens.lens (\HeadObjectResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@HeadObjectResponse' {} a -> s {sSECustomerKeyMD5 = a} :: HeadObjectResponse)

-- | Provides storage class information of the object. Amazon S3 returns this
-- header for all objects except for S3 Standard storage class objects.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
headObjectResponse_storageClass :: Lens.Lens' HeadObjectResponse (Prelude.Maybe StorageClass)
headObjectResponse_storageClass = Lens.lens (\HeadObjectResponse' {storageClass} -> storageClass) (\s@HeadObjectResponse' {} a -> s {storageClass = a} :: HeadObjectResponse)

-- | Version of the object.
headObjectResponse_versionId :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ObjectVersionId)
headObjectResponse_versionId = Lens.lens (\HeadObjectResponse' {versionId} -> versionId) (\s@HeadObjectResponse' {} a -> s {versionId = a} :: HeadObjectResponse)

-- | Indicates that a range of bytes was specified.
headObjectResponse_acceptRanges :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_acceptRanges = Lens.lens (\HeadObjectResponse' {acceptRanges} -> acceptRanges) (\s@HeadObjectResponse' {} a -> s {acceptRanges = a} :: HeadObjectResponse)

-- | Indicates whether the object uses an S3 Bucket Key for server-side
-- encryption with AWS KMS (SSE-KMS).
headObjectResponse_bucketKeyEnabled :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Bool)
headObjectResponse_bucketKeyEnabled = Lens.lens (\HeadObjectResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@HeadObjectResponse' {} a -> s {bucketKeyEnabled = a} :: HeadObjectResponse)

-- | If the object is stored using server-side encryption either with an AWS
-- KMS customer master key (CMK) or an Amazon S3-managed encryption key,
-- the response includes this header with the value of the server-side
-- encryption algorithm used when storing this object in Amazon S3 (for
-- example, AES256, aws:kms).
headObjectResponse_serverSideEncryption :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ServerSideEncryption)
headObjectResponse_serverSideEncryption = Lens.lens (\HeadObjectResponse' {serverSideEncryption} -> serverSideEncryption) (\s@HeadObjectResponse' {} a -> s {serverSideEncryption = a} :: HeadObjectResponse)

-- | This is set to the number of metadata entries not returned in
-- @x-amz-meta@ headers. This can happen if you create metadata using an
-- API like SOAP that supports more flexible metadata than the REST API.
-- For example, using SOAP, you can create metadata whose values are not
-- legal HTTP headers.
headObjectResponse_missingMeta :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Int)
headObjectResponse_missingMeta = Lens.lens (\HeadObjectResponse' {missingMeta} -> missingMeta) (\s@HeadObjectResponse' {} a -> s {missingMeta = a} :: HeadObjectResponse)

-- | Specifies whether a legal hold is in effect for this object. This header
-- is only returned if the requester has the @s3:GetObjectLegalHold@
-- permission. This header is not returned if the specified version of this
-- object has never had a legal hold applied. For more information about S3
-- Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
headObjectResponse_objectLockLegalHoldStatus :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ObjectLockLegalHoldStatus)
headObjectResponse_objectLockLegalHoldStatus = Lens.lens (\HeadObjectResponse' {objectLockLegalHoldStatus} -> objectLockLegalHoldStatus) (\s@HeadObjectResponse' {} a -> s {objectLockLegalHoldStatus = a} :: HeadObjectResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
headObjectResponse_sSECustomerAlgorithm :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_sSECustomerAlgorithm = Lens.lens (\HeadObjectResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@HeadObjectResponse' {} a -> s {sSECustomerAlgorithm = a} :: HeadObjectResponse)

-- | Creation date of the object.
headObjectResponse_lastModified :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.UTCTime)
headObjectResponse_lastModified = Lens.lens (\HeadObjectResponse' {lastModified} -> lastModified) (\s@HeadObjectResponse' {} a -> s {lastModified = a} :: HeadObjectResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies caching behavior along the request\/reply chain.
headObjectResponse_cacheControl :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_cacheControl = Lens.lens (\HeadObjectResponse' {cacheControl} -> cacheControl) (\s@HeadObjectResponse' {} a -> s {cacheControl = a} :: HeadObjectResponse)

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
-- @x-amz-restore: ongoing-request=\"false\", expiry-date=\"Fri, 23 Dec 2012 00:00:00 GMT\"@
--
-- If the object restoration is in progress, the header returns the value
-- @ongoing-request=\"true\"@.
--
-- For more information about archiving objects, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations>.
headObjectResponse_restore :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.Text)
headObjectResponse_restore = Lens.lens (\HeadObjectResponse' {restore} -> restore) (\s@HeadObjectResponse' {} a -> s {restore = a} :: HeadObjectResponse)

-- | The Object Lock mode, if any, that\'s in effect for this object. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission. For more information about S3 Object Lock, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock>.
headObjectResponse_objectLockMode :: Lens.Lens' HeadObjectResponse (Prelude.Maybe ObjectLockMode)
headObjectResponse_objectLockMode = Lens.lens (\HeadObjectResponse' {objectLockMode} -> objectLockMode) (\s@HeadObjectResponse' {} a -> s {objectLockMode = a} :: HeadObjectResponse)

-- | The date and time when the Object Lock retention period expires. This
-- header is only returned if the requester has the @s3:GetObjectRetention@
-- permission.
headObjectResponse_objectLockRetainUntilDate :: Lens.Lens' HeadObjectResponse (Prelude.Maybe Prelude.UTCTime)
headObjectResponse_objectLockRetainUntilDate = Lens.lens (\HeadObjectResponse' {objectLockRetainUntilDate} -> objectLockRetainUntilDate) (\s@HeadObjectResponse' {} a -> s {objectLockRetainUntilDate = a} :: HeadObjectResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
headObjectResponse_httpStatus :: Lens.Lens' HeadObjectResponse Prelude.Int
headObjectResponse_httpStatus = Lens.lens (\HeadObjectResponse' {httpStatus} -> httpStatus) (\s@HeadObjectResponse' {} a -> s {httpStatus = a} :: HeadObjectResponse)

instance Prelude.NFData HeadObjectResponse

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
-- Module      : Amazonka.S3.GetObjectAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the metadata from an object without returning the object
-- itself. This action is useful if you\'re interested only in an object\'s
-- metadata. To use @GetObjectAttributes@, you must have READ access to the
-- object.
--
-- @GetObjectAttributes@ combines the functionality of @GetObjectAcl@,
-- @GetObjectLegalHold@, @GetObjectLockConfiguration@,
-- @GetObjectRetention@, @GetObjectTagging@, @HeadObject@, and @ListParts@.
-- All of the data returned with each of those individual calls can be
-- returned with a single call to @GetObjectAttributes@.
--
-- If you encrypt an object by using server-side encryption with
-- customer-provided encryption keys (SSE-C) when you store the object in
-- Amazon S3, then when you retrieve the metadata from the object, you must
-- use the following headers:
--
-- -   @x-amz-server-side-encryption-customer-algorithm@
--
-- -   @x-amz-server-side-encryption-customer-key@
--
-- -   @x-amz-server-side-encryption-customer-key-MD5@
--
-- For more information about SSE-C, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys)>
-- in the /Amazon S3 User Guide/.
--
-- -   Encryption request headers, such as @x-amz-server-side-encryption@,
--     should not be sent for GET requests if your object uses server-side
--     encryption with Amazon Web Services KMS keys stored in Amazon Web
--     Services Key Management Service (SSE-KMS) or server-side encryption
--     with Amazon S3 managed encryption keys (SSE-S3). If your object does
--     use these types of keys, you\'ll get an HTTP @400 Bad Request@
--     error.
--
-- -   The last modified property in this case is the creation date of the
--     object.
--
-- Consider the following when using request headers:
--
-- -   If both of the @If-Match@ and @If-Unmodified-Since@ headers are
--     present in the request as follows, then Amazon S3 returns the HTTP
--     status code @200 OK@ and the data requested:
--
--     -   @If-Match@ condition evaluates to @true@.
--
--     -   @If-Unmodified-Since@ condition evaluates to @false@.
--
-- -   If both of the @If-None-Match@ and @If-Modified-Since@ headers are
--     present in the request as follows, then Amazon S3 returns the HTTP
--     status code @304 Not Modified@:
--
--     -   @If-None-Match@ condition evaluates to @false@.
--
--     -   @If-Modified-Since@ condition evaluates to @true@.
--
-- For more information about conditional requests, see
-- <https://tools.ietf.org/html/rfc7232 RFC 7232>.
--
-- __Permissions__
--
-- The permissions that you need to use this operation depend on whether
-- the bucket is versioned. If the bucket is versioned, you need both the
-- @s3:GetObjectVersion@ and @s3:GetObjectVersionAttributes@ permissions
-- for this operation. If the bucket is not versioned, you need the
-- @s3:GetObject@ and @s3:GetObjectAttributes@ permissions. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>
-- in the /Amazon S3 User Guide/. If the object that you request does not
-- exist, the error Amazon S3 returns depends on whether you also have the
-- @s3:ListBucket@ permission.
--
-- -   If you have the @s3:ListBucket@ permission on the bucket, Amazon S3
--     returns an HTTP status code @404 Not Found@ (\"no such key\") error.
--
-- -   If you don\'t have the @s3:ListBucket@ permission, Amazon S3 returns
--     an HTTP status code @403 Forbidden@ (\"access denied\") error.
--
-- The following actions are related to @GetObjectAttributes@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectLegalHold.html GetObjectLegalHold>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectLockConfiguration.html GetObjectLockConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectRetention.html GetObjectRetention>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_HeadObject.html HeadObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
module Amazonka.S3.GetObjectAttributes
  ( -- * Creating a Request
    GetObjectAttributes (..),
    newGetObjectAttributes,

    -- * Request Lenses
    getObjectAttributes_expectedBucketOwner,
    getObjectAttributes_requestPayer,
    getObjectAttributes_sSECustomerAlgorithm,
    getObjectAttributes_maxParts,
    getObjectAttributes_sSECustomerKeyMD5,
    getObjectAttributes_partNumberMarker,
    getObjectAttributes_versionId,
    getObjectAttributes_sSECustomerKey,
    getObjectAttributes_bucket,
    getObjectAttributes_key,
    getObjectAttributes_objectAttributes,

    -- * Destructuring the Response
    GetObjectAttributesResponse (..),
    newGetObjectAttributesResponse,

    -- * Response Lenses
    getObjectAttributesResponse_requestCharged,
    getObjectAttributesResponse_checksum,
    getObjectAttributesResponse_lastModified,
    getObjectAttributesResponse_objectParts,
    getObjectAttributesResponse_storageClass,
    getObjectAttributesResponse_eTag,
    getObjectAttributesResponse_objectSize,
    getObjectAttributesResponse_deleteMarker,
    getObjectAttributesResponse_versionId,
    getObjectAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObjectAttributes' smart constructor.
data GetObjectAttributes = GetObjectAttributes'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Specifies the algorithm to use when encrypting the object (for example,
    -- AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Sets the maximum number of parts to return.
    maxParts :: Prelude.Maybe Prelude.Int,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | Specifies the part after which listing should begin. Only parts with
    -- higher part numbers will be listed.
    partNumberMarker :: Prelude.Maybe Prelude.Int,
    -- | The version ID used to reference a specific version of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm@ header.
    sSECustomerKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the bucket that contains the object.
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
    -- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | The object key.
    key :: ObjectKey,
    -- | An XML header that specifies the fields at the root level that you want
    -- returned in the response. Fields that you do not specify are not
    -- returned.
    objectAttributes :: [ObjectAttributes]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectAttributes_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'getObjectAttributes_requestPayer' - Undocumented member.
--
-- 'sSECustomerAlgorithm', 'getObjectAttributes_sSECustomerAlgorithm' - Specifies the algorithm to use when encrypting the object (for example,
-- AES256).
--
-- 'maxParts', 'getObjectAttributes_maxParts' - Sets the maximum number of parts to return.
--
-- 'sSECustomerKeyMD5', 'getObjectAttributes_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'partNumberMarker', 'getObjectAttributes_partNumberMarker' - Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
--
-- 'versionId', 'getObjectAttributes_versionId' - The version ID used to reference a specific version of the object.
--
-- 'sSECustomerKey', 'getObjectAttributes_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- 'bucket', 'getObjectAttributes_bucket' - The name of the bucket that contains the object.
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
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'key', 'getObjectAttributes_key' - The object key.
--
-- 'objectAttributes', 'getObjectAttributes_objectAttributes' - An XML header that specifies the fields at the root level that you want
-- returned in the response. Fields that you do not specify are not
-- returned.
newGetObjectAttributes ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectAttributes
newGetObjectAttributes pBucket_ pKey_ =
  GetObjectAttributes'
    { expectedBucketOwner =
        Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      maxParts = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      partNumberMarker = Prelude.Nothing,
      versionId = Prelude.Nothing,
      sSECustomerKey = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      objectAttributes = Prelude.mempty
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getObjectAttributes_expectedBucketOwner :: Lens.Lens' GetObjectAttributes (Prelude.Maybe Prelude.Text)
getObjectAttributes_expectedBucketOwner = Lens.lens (\GetObjectAttributes' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectAttributes' {} a -> s {expectedBucketOwner = a} :: GetObjectAttributes)

-- | Undocumented member.
getObjectAttributes_requestPayer :: Lens.Lens' GetObjectAttributes (Prelude.Maybe RequestPayer)
getObjectAttributes_requestPayer = Lens.lens (\GetObjectAttributes' {requestPayer} -> requestPayer) (\s@GetObjectAttributes' {} a -> s {requestPayer = a} :: GetObjectAttributes)

-- | Specifies the algorithm to use when encrypting the object (for example,
-- AES256).
getObjectAttributes_sSECustomerAlgorithm :: Lens.Lens' GetObjectAttributes (Prelude.Maybe Prelude.Text)
getObjectAttributes_sSECustomerAlgorithm = Lens.lens (\GetObjectAttributes' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@GetObjectAttributes' {} a -> s {sSECustomerAlgorithm = a} :: GetObjectAttributes)

-- | Sets the maximum number of parts to return.
getObjectAttributes_maxParts :: Lens.Lens' GetObjectAttributes (Prelude.Maybe Prelude.Int)
getObjectAttributes_maxParts = Lens.lens (\GetObjectAttributes' {maxParts} -> maxParts) (\s@GetObjectAttributes' {} a -> s {maxParts = a} :: GetObjectAttributes)

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
getObjectAttributes_sSECustomerKeyMD5 :: Lens.Lens' GetObjectAttributes (Prelude.Maybe Prelude.Text)
getObjectAttributes_sSECustomerKeyMD5 = Lens.lens (\GetObjectAttributes' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@GetObjectAttributes' {} a -> s {sSECustomerKeyMD5 = a} :: GetObjectAttributes)

-- | Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
getObjectAttributes_partNumberMarker :: Lens.Lens' GetObjectAttributes (Prelude.Maybe Prelude.Int)
getObjectAttributes_partNumberMarker = Lens.lens (\GetObjectAttributes' {partNumberMarker} -> partNumberMarker) (\s@GetObjectAttributes' {} a -> s {partNumberMarker = a} :: GetObjectAttributes)

-- | The version ID used to reference a specific version of the object.
getObjectAttributes_versionId :: Lens.Lens' GetObjectAttributes (Prelude.Maybe ObjectVersionId)
getObjectAttributes_versionId = Lens.lens (\GetObjectAttributes' {versionId} -> versionId) (\s@GetObjectAttributes' {} a -> s {versionId = a} :: GetObjectAttributes)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm@ header.
getObjectAttributes_sSECustomerKey :: Lens.Lens' GetObjectAttributes (Prelude.Maybe Prelude.Text)
getObjectAttributes_sSECustomerKey = Lens.lens (\GetObjectAttributes' {sSECustomerKey} -> sSECustomerKey) (\s@GetObjectAttributes' {} a -> s {sSECustomerKey = a} :: GetObjectAttributes) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the bucket that contains the object.
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
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
getObjectAttributes_bucket :: Lens.Lens' GetObjectAttributes BucketName
getObjectAttributes_bucket = Lens.lens (\GetObjectAttributes' {bucket} -> bucket) (\s@GetObjectAttributes' {} a -> s {bucket = a} :: GetObjectAttributes)

-- | The object key.
getObjectAttributes_key :: Lens.Lens' GetObjectAttributes ObjectKey
getObjectAttributes_key = Lens.lens (\GetObjectAttributes' {key} -> key) (\s@GetObjectAttributes' {} a -> s {key = a} :: GetObjectAttributes)

-- | An XML header that specifies the fields at the root level that you want
-- returned in the response. Fields that you do not specify are not
-- returned.
getObjectAttributes_objectAttributes :: Lens.Lens' GetObjectAttributes [ObjectAttributes]
getObjectAttributes_objectAttributes = Lens.lens (\GetObjectAttributes' {objectAttributes} -> objectAttributes) (\s@GetObjectAttributes' {} a -> s {objectAttributes = a} :: GetObjectAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest GetObjectAttributes where
  type
    AWSResponse GetObjectAttributes =
      GetObjectAttributesResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectAttributesResponse'
            Prelude.<$> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (x Data..@? "Checksum")
            Prelude.<*> (h Data..#? "Last-Modified")
            Prelude.<*> (x Data..@? "ObjectParts")
            Prelude.<*> (x Data..@? "StorageClass")
            Prelude.<*> (x Data..@? "ETag")
            Prelude.<*> (x Data..@? "ObjectSize")
            Prelude.<*> (h Data..#? "x-amz-delete-marker")
            Prelude.<*> (h Data..#? "x-amz-version-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectAttributes where
  hashWithSalt _salt GetObjectAttributes' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` sSECustomerAlgorithm
      `Prelude.hashWithSalt` maxParts
      `Prelude.hashWithSalt` sSECustomerKeyMD5
      `Prelude.hashWithSalt` partNumberMarker
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` sSECustomerKey
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` objectAttributes

instance Prelude.NFData GetObjectAttributes where
  rnf GetObjectAttributes' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf maxParts
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf partNumberMarker
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf sSECustomerKey
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf objectAttributes

instance Data.ToHeaders GetObjectAttributes where
  toHeaders GetObjectAttributes' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer,
        "x-amz-server-side-encryption-customer-algorithm"
          Data.=# sSECustomerAlgorithm,
        "x-amz-max-parts" Data.=# maxParts,
        "x-amz-server-side-encryption-customer-key-MD5"
          Data.=# sSECustomerKeyMD5,
        "x-amz-part-number-marker" Data.=# partNumberMarker,
        "x-amz-server-side-encryption-customer-key"
          Data.=# sSECustomerKey,
        "x-amz-object-attributes" Data.=# objectAttributes
      ]

instance Data.ToPath GetObjectAttributes where
  toPath GetObjectAttributes' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery GetObjectAttributes where
  toQuery GetObjectAttributes' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "attributes"]

-- | /See:/ 'newGetObjectAttributesResponse' smart constructor.
data GetObjectAttributesResponse = GetObjectAttributesResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The checksum or digest of the object.
    checksum :: Prelude.Maybe Checksum,
    -- | The creation date of the object.
    lastModified :: Prelude.Maybe Data.ISO8601,
    -- | A collection of parts associated with a multipart upload.
    objectParts :: Prelude.Maybe GetObjectAttributesParts,
    -- | Provides the storage class information of the object. Amazon S3 returns
    -- this header for all objects except for S3 Standard storage class
    -- objects.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
    storageClass :: Prelude.Maybe StorageClass,
    -- | An ETag is an opaque identifier assigned by a web server to a specific
    -- version of a resource found at a URL.
    eTag :: Prelude.Maybe ETag,
    -- | The size of the object in bytes.
    objectSize :: Prelude.Maybe Prelude.Integer,
    -- | Specifies whether the object retrieved was (@true@) or was not (@false@)
    -- a delete marker. If @false@, this response header does not appear in the
    -- response.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | The version ID of the object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'getObjectAttributesResponse_requestCharged' - Undocumented member.
--
-- 'checksum', 'getObjectAttributesResponse_checksum' - The checksum or digest of the object.
--
-- 'lastModified', 'getObjectAttributesResponse_lastModified' - The creation date of the object.
--
-- 'objectParts', 'getObjectAttributesResponse_objectParts' - A collection of parts associated with a multipart upload.
--
-- 'storageClass', 'getObjectAttributesResponse_storageClass' - Provides the storage class information of the object. Amazon S3 returns
-- this header for all objects except for S3 Standard storage class
-- objects.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
--
-- 'eTag', 'getObjectAttributesResponse_eTag' - An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
--
-- 'objectSize', 'getObjectAttributesResponse_objectSize' - The size of the object in bytes.
--
-- 'deleteMarker', 'getObjectAttributesResponse_deleteMarker' - Specifies whether the object retrieved was (@true@) or was not (@false@)
-- a delete marker. If @false@, this response header does not appear in the
-- response.
--
-- 'versionId', 'getObjectAttributesResponse_versionId' - The version ID of the object.
--
-- 'httpStatus', 'getObjectAttributesResponse_httpStatus' - The response's http status code.
newGetObjectAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetObjectAttributesResponse
newGetObjectAttributesResponse pHttpStatus_ =
  GetObjectAttributesResponse'
    { requestCharged =
        Prelude.Nothing,
      checksum = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      objectParts = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      eTag = Prelude.Nothing,
      objectSize = Prelude.Nothing,
      deleteMarker = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getObjectAttributesResponse_requestCharged :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe RequestCharged)
getObjectAttributesResponse_requestCharged = Lens.lens (\GetObjectAttributesResponse' {requestCharged} -> requestCharged) (\s@GetObjectAttributesResponse' {} a -> s {requestCharged = a} :: GetObjectAttributesResponse)

-- | The checksum or digest of the object.
getObjectAttributesResponse_checksum :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe Checksum)
getObjectAttributesResponse_checksum = Lens.lens (\GetObjectAttributesResponse' {checksum} -> checksum) (\s@GetObjectAttributesResponse' {} a -> s {checksum = a} :: GetObjectAttributesResponse)

-- | The creation date of the object.
getObjectAttributesResponse_lastModified :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe Prelude.UTCTime)
getObjectAttributesResponse_lastModified = Lens.lens (\GetObjectAttributesResponse' {lastModified} -> lastModified) (\s@GetObjectAttributesResponse' {} a -> s {lastModified = a} :: GetObjectAttributesResponse) Prelude.. Lens.mapping Data._Time

-- | A collection of parts associated with a multipart upload.
getObjectAttributesResponse_objectParts :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe GetObjectAttributesParts)
getObjectAttributesResponse_objectParts = Lens.lens (\GetObjectAttributesResponse' {objectParts} -> objectParts) (\s@GetObjectAttributesResponse' {} a -> s {objectParts = a} :: GetObjectAttributesResponse)

-- | Provides the storage class information of the object. Amazon S3 returns
-- this header for all objects except for S3 Standard storage class
-- objects.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes>.
getObjectAttributesResponse_storageClass :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe StorageClass)
getObjectAttributesResponse_storageClass = Lens.lens (\GetObjectAttributesResponse' {storageClass} -> storageClass) (\s@GetObjectAttributesResponse' {} a -> s {storageClass = a} :: GetObjectAttributesResponse)

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
getObjectAttributesResponse_eTag :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe ETag)
getObjectAttributesResponse_eTag = Lens.lens (\GetObjectAttributesResponse' {eTag} -> eTag) (\s@GetObjectAttributesResponse' {} a -> s {eTag = a} :: GetObjectAttributesResponse)

-- | The size of the object in bytes.
getObjectAttributesResponse_objectSize :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe Prelude.Integer)
getObjectAttributesResponse_objectSize = Lens.lens (\GetObjectAttributesResponse' {objectSize} -> objectSize) (\s@GetObjectAttributesResponse' {} a -> s {objectSize = a} :: GetObjectAttributesResponse)

-- | Specifies whether the object retrieved was (@true@) or was not (@false@)
-- a delete marker. If @false@, this response header does not appear in the
-- response.
getObjectAttributesResponse_deleteMarker :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe Prelude.Bool)
getObjectAttributesResponse_deleteMarker = Lens.lens (\GetObjectAttributesResponse' {deleteMarker} -> deleteMarker) (\s@GetObjectAttributesResponse' {} a -> s {deleteMarker = a} :: GetObjectAttributesResponse)

-- | The version ID of the object.
getObjectAttributesResponse_versionId :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe ObjectVersionId)
getObjectAttributesResponse_versionId = Lens.lens (\GetObjectAttributesResponse' {versionId} -> versionId) (\s@GetObjectAttributesResponse' {} a -> s {versionId = a} :: GetObjectAttributesResponse)

-- | The response's http status code.
getObjectAttributesResponse_httpStatus :: Lens.Lens' GetObjectAttributesResponse Prelude.Int
getObjectAttributesResponse_httpStatus = Lens.lens (\GetObjectAttributesResponse' {httpStatus} -> httpStatus) (\s@GetObjectAttributesResponse' {} a -> s {httpStatus = a} :: GetObjectAttributesResponse)

instance Prelude.NFData GetObjectAttributesResponse where
  rnf GetObjectAttributesResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf objectParts
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf objectSize
      `Prelude.seq` Prelude.rnf deleteMarker
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus

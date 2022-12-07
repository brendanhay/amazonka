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
-- Module      : Amazonka.S3.UploadPart
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part in a multipart upload.
--
-- In this operation, you provide part data in your request. However, you
-- have an option to specify your existing Amazon S3 object as a data
-- source for the part you are uploading. To upload a part from an existing
-- object, you use the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPartCopy.html UploadPartCopy>
-- operation.
--
-- You must initiate a multipart upload (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>)
-- before you can upload any part. In response to your initiate request,
-- Amazon S3 returns an upload ID, a unique identifier, that you must
-- include in your upload part request.
--
-- Part numbers can be any number from 1 to 10,000, inclusive. A part
-- number uniquely identifies a part and also defines its position within
-- the object being created. If you upload a new part using the same part
-- number that was used with a previous part, the previously uploaded part
-- is overwritten.
--
-- For information about maximum and minimum part sizes and other multipart
-- upload specifications, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/qfacts.html Multipart upload limits>
-- in the /Amazon S3 User Guide/.
--
-- To ensure that data is not corrupted when traversing the network,
-- specify the @Content-MD5@ header in the upload part request. Amazon S3
-- checks the part data against the provided MD5 value. If they do not
-- match, Amazon S3 returns an error.
--
-- If the upload request is signed with Signature Version 4, then Amazon
-- Web Services S3 uses the @x-amz-content-sha256@ header as a checksum
-- instead of @Content-MD5@. For more information see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html Authenticating Requests: Using the Authorization Header (Amazon Web Services Signature Version 4)>.
--
-- __Note:__ After you initiate multipart upload and upload one or more
-- parts, you must either complete or abort multipart upload in order to
-- stop getting charged for storage of the uploaded parts. Only after you
-- either complete or abort multipart upload, Amazon S3 frees up the parts
-- storage and stops charging you for the parts storage.
--
-- For more information on multipart uploads, go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart Upload Overview>
-- in the /Amazon S3 User Guide/ .
--
-- For information on the permissions required to use the multipart upload
-- API, go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload and Permissions>
-- in the /Amazon S3 User Guide/.
--
-- You can optionally request server-side encryption where Amazon S3
-- encrypts your data as it writes it to disks in its data centers and
-- decrypts it for you when you access it. You have the option of providing
-- your own encryption key, or you can use the Amazon Web Services managed
-- encryption keys. If you choose to provide your own encryption key, the
-- request headers you provide in the request must match the headers you
-- used in the request to initiate the upload by using
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>.
-- For more information, go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption>
-- in the /Amazon S3 User Guide/.
--
-- Server-side encryption is supported by the S3 Multipart Upload actions.
-- Unless you are using a customer-provided encryption key, you don\'t need
-- to specify the encryption parameters in each UploadPart request.
-- Instead, you only need to specify the server-side encryption parameters
-- in the initial Initiate Multipart request. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>.
--
-- If you requested server-side encryption using a customer-provided
-- encryption key in your initiate multipart upload request, you must
-- provide identical encryption information in each part upload using the
-- following headers.
--
-- -   x-amz-server-side-encryption-customer-algorithm
--
-- -   x-amz-server-side-encryption-customer-key
--
-- -   x-amz-server-side-encryption-customer-key-MD5
--
-- __Special Errors__
--
-- -   -   /Code: NoSuchUpload/
--
--     -   /Cause: The specified multipart upload does not exist. The
--         upload ID might be invalid, or the multipart upload might have
--         been aborted or completed./
--
--     -   /HTTP Status Code: 404 Not Found/
--
--     -   /SOAP Fault Code Prefix: Client/
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Amazonka.S3.UploadPart
  ( -- * Creating a Request
    UploadPart (..),
    newUploadPart,

    -- * Request Lenses
    uploadPart_checksumAlgorithm,
    uploadPart_checksumCRC32C,
    uploadPart_checksumSHA1,
    uploadPart_contentMD5,
    uploadPart_expectedBucketOwner,
    uploadPart_checksumCRC32,
    uploadPart_requestPayer,
    uploadPart_checksumSHA256,
    uploadPart_contentLength,
    uploadPart_sSECustomerAlgorithm,
    uploadPart_sSECustomerKeyMD5,
    uploadPart_sSECustomerKey,
    uploadPart_bucket,
    uploadPart_key,
    uploadPart_partNumber,
    uploadPart_uploadId,
    uploadPart_body,

    -- * Destructuring the Response
    UploadPartResponse (..),
    newUploadPartResponse,

    -- * Response Lenses
    uploadPartResponse_serverSideEncryption,
    uploadPartResponse_checksumCRC32C,
    uploadPartResponse_bucketKeyEnabled,
    uploadPartResponse_requestCharged,
    uploadPartResponse_checksumSHA1,
    uploadPartResponse_checksumCRC32,
    uploadPartResponse_sSEKMSKeyId,
    uploadPartResponse_checksumSHA256,
    uploadPartResponse_sSECustomerAlgorithm,
    uploadPartResponse_sSECustomerKeyMD5,
    uploadPartResponse_eTag,
    uploadPartResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newUploadPart' smart constructor.
data UploadPart = UploadPart'
  { -- | Indicates the algorithm used to create the checksum for the object when
    -- using the SDK. This header will not provide any additional functionality
    -- if not using the SDK. When sending this header, there must be a
    -- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
    -- Otherwise, Amazon S3 fails the request with the HTTP status code
    -- @400 Bad Request@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    --
    -- If you provide an individual checksum, Amazon S3 ignores any provided
    -- @ChecksumAlgorithm@ parameter.
    --
    -- This checksum algorithm must be the same for all parts and it match the
    -- checksum value supplied in the @CreateMultipartUpload@ request.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 32-bit CRC32C checksum of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32C :: Prelude.Maybe Prelude.Text,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 160-bit SHA-1 digest of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA1 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the part data. This parameter
    -- is auto-populated when using the command from the CLI. This parameter is
    -- required if object lock parameters are specified.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32 :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | This header can be used as a data integrity check to verify that the
    -- data received is the same data that was originally sent. This header
    -- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text,
    -- | Size of the body in bytes. This parameter is useful when the size of the
    -- body cannot be determined automatically.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm header@. This must be
    -- the same encryption key specified in the initiate multipart upload
    -- request.
    sSECustomerKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the bucket to which the multipart upload was initiated.
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
    -- | Object key for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | Part number of part being uploaded. This is a positive integer between 1
    -- and 10,000.
    partNumber :: Prelude.Int,
    -- | Upload ID identifying the multipart upload whose part is being uploaded.
    uploadId :: Prelude.Text,
    -- | Object data.
    body :: Data.RequestBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'uploadPart_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
--
-- This checksum algorithm must be the same for all parts and it match the
-- checksum value supplied in the @CreateMultipartUpload@ request.
--
-- 'checksumCRC32C', 'uploadPart_checksumCRC32C' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32C checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumSHA1', 'uploadPart_checksumSHA1' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 160-bit SHA-1 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'contentMD5', 'uploadPart_contentMD5' - The base64-encoded 128-bit MD5 digest of the part data. This parameter
-- is auto-populated when using the command from the CLI. This parameter is
-- required if object lock parameters are specified.
--
-- 'expectedBucketOwner', 'uploadPart_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'checksumCRC32', 'uploadPart_checksumCRC32' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'requestPayer', 'uploadPart_requestPayer' - Undocumented member.
--
-- 'checksumSHA256', 'uploadPart_checksumSHA256' - This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'contentLength', 'uploadPart_contentLength' - Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
--
-- 'sSECustomerAlgorithm', 'uploadPart_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKeyMD5', 'uploadPart_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'sSECustomerKey', 'uploadPart_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm header@. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
--
-- 'bucket', 'uploadPart_bucket' - The name of the bucket to which the multipart upload was initiated.
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
-- 'key', 'uploadPart_key' - Object key for which the multipart upload was initiated.
--
-- 'partNumber', 'uploadPart_partNumber' - Part number of part being uploaded. This is a positive integer between 1
-- and 10,000.
--
-- 'uploadId', 'uploadPart_uploadId' - Upload ID identifying the multipart upload whose part is being uploaded.
--
-- 'body', 'uploadPart_body' - Object data.
newUploadPart ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'partNumber'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'body'
  Data.RequestBody ->
  UploadPart
newUploadPart
  pBucket_
  pKey_
  pPartNumber_
  pUploadId_
  pBody_ =
    UploadPart'
      { checksumAlgorithm = Prelude.Nothing,
        checksumCRC32C = Prelude.Nothing,
        checksumSHA1 = Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        checksumCRC32 = Prelude.Nothing,
        requestPayer = Prelude.Nothing,
        checksumSHA256 = Prelude.Nothing,
        contentLength = Prelude.Nothing,
        sSECustomerAlgorithm = Prelude.Nothing,
        sSECustomerKeyMD5 = Prelude.Nothing,
        sSECustomerKey = Prelude.Nothing,
        bucket = pBucket_,
        key = pKey_,
        partNumber = pPartNumber_,
        uploadId = pUploadId_,
        body = pBody_
      }

-- | Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
--
-- This checksum algorithm must be the same for all parts and it match the
-- checksum value supplied in the @CreateMultipartUpload@ request.
uploadPart_checksumAlgorithm :: Lens.Lens' UploadPart (Prelude.Maybe ChecksumAlgorithm)
uploadPart_checksumAlgorithm = Lens.lens (\UploadPart' {checksumAlgorithm} -> checksumAlgorithm) (\s@UploadPart' {} a -> s {checksumAlgorithm = a} :: UploadPart)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32C checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPart_checksumCRC32C :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_checksumCRC32C = Lens.lens (\UploadPart' {checksumCRC32C} -> checksumCRC32C) (\s@UploadPart' {} a -> s {checksumCRC32C = a} :: UploadPart)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 160-bit SHA-1 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPart_checksumSHA1 :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_checksumSHA1 = Lens.lens (\UploadPart' {checksumSHA1} -> checksumSHA1) (\s@UploadPart' {} a -> s {checksumSHA1 = a} :: UploadPart)

-- | The base64-encoded 128-bit MD5 digest of the part data. This parameter
-- is auto-populated when using the command from the CLI. This parameter is
-- required if object lock parameters are specified.
uploadPart_contentMD5 :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_contentMD5 = Lens.lens (\UploadPart' {contentMD5} -> contentMD5) (\s@UploadPart' {} a -> s {contentMD5 = a} :: UploadPart)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
uploadPart_expectedBucketOwner :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_expectedBucketOwner = Lens.lens (\UploadPart' {expectedBucketOwner} -> expectedBucketOwner) (\s@UploadPart' {} a -> s {expectedBucketOwner = a} :: UploadPart)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 32-bit CRC32 checksum of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPart_checksumCRC32 :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_checksumCRC32 = Lens.lens (\UploadPart' {checksumCRC32} -> checksumCRC32) (\s@UploadPart' {} a -> s {checksumCRC32 = a} :: UploadPart)

-- | Undocumented member.
uploadPart_requestPayer :: Lens.Lens' UploadPart (Prelude.Maybe RequestPayer)
uploadPart_requestPayer = Lens.lens (\UploadPart' {requestPayer} -> requestPayer) (\s@UploadPart' {} a -> s {requestPayer = a} :: UploadPart)

-- | This header can be used as a data integrity check to verify that the
-- data received is the same data that was originally sent. This header
-- specifies the base64-encoded, 256-bit SHA-256 digest of the object. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPart_checksumSHA256 :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_checksumSHA256 = Lens.lens (\UploadPart' {checksumSHA256} -> checksumSHA256) (\s@UploadPart' {} a -> s {checksumSHA256 = a} :: UploadPart)

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
uploadPart_contentLength :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Integer)
uploadPart_contentLength = Lens.lens (\UploadPart' {contentLength} -> contentLength) (\s@UploadPart' {} a -> s {contentLength = a} :: UploadPart)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
uploadPart_sSECustomerAlgorithm :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_sSECustomerAlgorithm = Lens.lens (\UploadPart' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPart' {} a -> s {sSECustomerAlgorithm = a} :: UploadPart)

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
uploadPart_sSECustomerKeyMD5 :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_sSECustomerKeyMD5 = Lens.lens (\UploadPart' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPart' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPart)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm header@. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
uploadPart_sSECustomerKey :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_sSECustomerKey = Lens.lens (\UploadPart' {sSECustomerKey} -> sSECustomerKey) (\s@UploadPart' {} a -> s {sSECustomerKey = a} :: UploadPart) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the bucket to which the multipart upload was initiated.
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
uploadPart_bucket :: Lens.Lens' UploadPart BucketName
uploadPart_bucket = Lens.lens (\UploadPart' {bucket} -> bucket) (\s@UploadPart' {} a -> s {bucket = a} :: UploadPart)

-- | Object key for which the multipart upload was initiated.
uploadPart_key :: Lens.Lens' UploadPart ObjectKey
uploadPart_key = Lens.lens (\UploadPart' {key} -> key) (\s@UploadPart' {} a -> s {key = a} :: UploadPart)

-- | Part number of part being uploaded. This is a positive integer between 1
-- and 10,000.
uploadPart_partNumber :: Lens.Lens' UploadPart Prelude.Int
uploadPart_partNumber = Lens.lens (\UploadPart' {partNumber} -> partNumber) (\s@UploadPart' {} a -> s {partNumber = a} :: UploadPart)

-- | Upload ID identifying the multipart upload whose part is being uploaded.
uploadPart_uploadId :: Lens.Lens' UploadPart Prelude.Text
uploadPart_uploadId = Lens.lens (\UploadPart' {uploadId} -> uploadId) (\s@UploadPart' {} a -> s {uploadId = a} :: UploadPart)

-- | Object data.
uploadPart_body :: Lens.Lens' UploadPart Data.RequestBody
uploadPart_body = Lens.lens (\UploadPart' {body} -> body) (\s@UploadPart' {} a -> s {body = a} :: UploadPart)

instance Core.AWSRequest UploadPart where
  type AWSResponse UploadPart = UploadPartResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putBody (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UploadPartResponse'
            Prelude.<$> (h Data..#? "x-amz-server-side-encryption")
            Prelude.<*> (h Data..#? "x-amz-checksum-crc32c")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (h Data..#? "x-amz-checksum-sha1")
            Prelude.<*> (h Data..#? "x-amz-checksum-crc32")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Data..#? "x-amz-checksum-sha256")
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> ( h
                            Data..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Data.ToBody UploadPart where
  toBody UploadPart' {..} = Data.toBody body

instance Data.ToHeaders UploadPart where
  toHeaders UploadPart' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "x-amz-checksum-crc32c" Data.=# checksumCRC32C,
        "x-amz-checksum-sha1" Data.=# checksumSHA1,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-checksum-crc32" Data.=# checksumCRC32,
        "x-amz-request-payer" Data.=# requestPayer,
        "x-amz-checksum-sha256" Data.=# checksumSHA256,
        "Content-Length" Data.=# contentLength,
        "x-amz-server-side-encryption-customer-algorithm"
          Data.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key-MD5"
          Data.=# sSECustomerKeyMD5,
        "x-amz-server-side-encryption-customer-key"
          Data.=# sSECustomerKey
      ]

instance Data.ToPath UploadPart where
  toPath UploadPart' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery UploadPart where
  toQuery UploadPart' {..} =
    Prelude.mconcat
      [ "partNumber" Data.=: partNumber,
        "uploadId" Data.=: uploadId
      ]

-- | /See:/ 'newUploadPartResponse' smart constructor.
data UploadPartResponse = UploadPartResponse'
  { -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32C :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    requestCharged :: Prelude.Maybe RequestCharged,
    -- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA1 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumCRC32 :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the ID of the Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) symmetric customer managed key was
    -- used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
    -- be present if it was uploaded with the object. With multipart uploads,
    -- this may not be a checksum value of the object. For more information
    -- about how checksums are calculated with multipart uploads, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    checksumSHA256 :: Prelude.Maybe Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | Entity tag for the uploaded object.
    eTag :: Prelude.Maybe ETag,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadPartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverSideEncryption', 'uploadPartResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'checksumCRC32C', 'uploadPartResponse_checksumCRC32C' - The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'bucketKeyEnabled', 'uploadPartResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'requestCharged', 'uploadPartResponse_requestCharged' - Undocumented member.
--
-- 'checksumSHA1', 'uploadPartResponse_checksumSHA1' - The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'checksumCRC32', 'uploadPartResponse_checksumCRC32' - The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'sSEKMSKeyId', 'uploadPartResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key was
-- used for the object.
--
-- 'checksumSHA256', 'uploadPartResponse_checksumSHA256' - The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- 'sSECustomerAlgorithm', 'uploadPartResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'sSECustomerKeyMD5', 'uploadPartResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'eTag', 'uploadPartResponse_eTag' - Entity tag for the uploaded object.
--
-- 'httpStatus', 'uploadPartResponse_httpStatus' - The response's http status code.
newUploadPartResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UploadPartResponse
newUploadPartResponse pHttpStatus_ =
  UploadPartResponse'
    { serverSideEncryption =
        Prelude.Nothing,
      checksumCRC32C = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      requestCharged = Prelude.Nothing,
      checksumSHA1 = Prelude.Nothing,
      checksumCRC32 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      checksumSHA256 = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
uploadPartResponse_serverSideEncryption :: Lens.Lens' UploadPartResponse (Prelude.Maybe ServerSideEncryption)
uploadPartResponse_serverSideEncryption = Lens.lens (\UploadPartResponse' {serverSideEncryption} -> serverSideEncryption) (\s@UploadPartResponse' {} a -> s {serverSideEncryption = a} :: UploadPartResponse)

-- | The base64-encoded, 32-bit CRC32C checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPartResponse_checksumCRC32C :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_checksumCRC32C = Lens.lens (\UploadPartResponse' {checksumCRC32C} -> checksumCRC32C) (\s@UploadPartResponse' {} a -> s {checksumCRC32C = a} :: UploadPartResponse)

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
uploadPartResponse_bucketKeyEnabled :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Bool)
uploadPartResponse_bucketKeyEnabled = Lens.lens (\UploadPartResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@UploadPartResponse' {} a -> s {bucketKeyEnabled = a} :: UploadPartResponse)

-- | Undocumented member.
uploadPartResponse_requestCharged :: Lens.Lens' UploadPartResponse (Prelude.Maybe RequestCharged)
uploadPartResponse_requestCharged = Lens.lens (\UploadPartResponse' {requestCharged} -> requestCharged) (\s@UploadPartResponse' {} a -> s {requestCharged = a} :: UploadPartResponse)

-- | The base64-encoded, 160-bit SHA-1 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPartResponse_checksumSHA1 :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_checksumSHA1 = Lens.lens (\UploadPartResponse' {checksumSHA1} -> checksumSHA1) (\s@UploadPartResponse' {} a -> s {checksumSHA1 = a} :: UploadPartResponse)

-- | The base64-encoded, 32-bit CRC32 checksum of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPartResponse_checksumCRC32 :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_checksumCRC32 = Lens.lens (\UploadPartResponse' {checksumCRC32} -> checksumCRC32) (\s@UploadPartResponse' {} a -> s {checksumCRC32 = a} :: UploadPartResponse)

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key was
-- used for the object.
uploadPartResponse_sSEKMSKeyId :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_sSEKMSKeyId = Lens.lens (\UploadPartResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@UploadPartResponse' {} a -> s {sSEKMSKeyId = a} :: UploadPartResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The base64-encoded, 256-bit SHA-256 digest of the object. This will only
-- be present if it was uploaded with the object. With multipart uploads,
-- this may not be a checksum value of the object. For more information
-- about how checksums are calculated with multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums Checking object integrity>
-- in the /Amazon S3 User Guide/.
uploadPartResponse_checksumSHA256 :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_checksumSHA256 = Lens.lens (\UploadPartResponse' {checksumSHA256} -> checksumSHA256) (\s@UploadPartResponse' {} a -> s {checksumSHA256 = a} :: UploadPartResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
uploadPartResponse_sSECustomerAlgorithm :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_sSECustomerAlgorithm = Lens.lens (\UploadPartResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPartResponse' {} a -> s {sSECustomerAlgorithm = a} :: UploadPartResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
uploadPartResponse_sSECustomerKeyMD5 :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_sSECustomerKeyMD5 = Lens.lens (\UploadPartResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPartResponse' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPartResponse)

-- | Entity tag for the uploaded object.
uploadPartResponse_eTag :: Lens.Lens' UploadPartResponse (Prelude.Maybe ETag)
uploadPartResponse_eTag = Lens.lens (\UploadPartResponse' {eTag} -> eTag) (\s@UploadPartResponse' {} a -> s {eTag = a} :: UploadPartResponse)

-- | The response's http status code.
uploadPartResponse_httpStatus :: Lens.Lens' UploadPartResponse Prelude.Int
uploadPartResponse_httpStatus = Lens.lens (\UploadPartResponse' {httpStatus} -> httpStatus) (\s@UploadPartResponse' {} a -> s {httpStatus = a} :: UploadPartResponse)

instance Prelude.NFData UploadPartResponse where
  rnf UploadPartResponse' {..} =
    Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf checksumCRC32C
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf checksumSHA1
      `Prelude.seq` Prelude.rnf checksumCRC32
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf checksumSHA256
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus

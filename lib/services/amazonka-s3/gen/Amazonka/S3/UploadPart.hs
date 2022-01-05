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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- is overwritten. Each part must be at least 5 MB in size, except the last
-- part. There is no size limit on the last part of your multipart upload.
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
    uploadPart_contentLength,
    uploadPart_sSECustomerAlgorithm,
    uploadPart_sSECustomerKey,
    uploadPart_requestPayer,
    uploadPart_sSECustomerKeyMD5,
    uploadPart_contentMD5,
    uploadPart_expectedBucketOwner,
    uploadPart_bucket,
    uploadPart_key,
    uploadPart_partNumber,
    uploadPart_uploadId,
    uploadPart_body,

    -- * Destructuring the Response
    UploadPartResponse (..),
    newUploadPartResponse,

    -- * Response Lenses
    uploadPartResponse_requestCharged,
    uploadPartResponse_eTag,
    uploadPartResponse_sSECustomerAlgorithm,
    uploadPartResponse_bucketKeyEnabled,
    uploadPartResponse_sSECustomerKeyMD5,
    uploadPartResponse_sSEKMSKeyId,
    uploadPartResponse_serverSideEncryption,
    uploadPartResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newUploadPart' smart constructor.
data UploadPart = UploadPart'
  { -- | Size of the body in bytes. This parameter is useful when the size of the
    -- body cannot be determined automatically.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the algorithm to use to when encrypting the object (for
    -- example, AES256).
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Specifies the customer-provided encryption key for Amazon S3 to use in
    -- encrypting data. This value is used to store the object and then it is
    -- discarded; Amazon S3 does not store the encryption key. The key must be
    -- appropriate for use with the algorithm specified in the
    -- @x-amz-server-side-encryption-customer-algorithm header@. This must be
    -- the same encryption key specified in the initiate multipart upload
    -- request.
    sSECustomerKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
    -- 1321. Amazon S3 uses this header for a message integrity check to ensure
    -- that the encryption key was transmitted without error.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the part data. This parameter
    -- is auto-populated when using the command from the CLI. This parameter is
    -- required if object lock parameters are specified.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
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
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this action using S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
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
    body :: Core.RequestBody
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
-- 'contentLength', 'uploadPart_contentLength' - Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
--
-- 'sSECustomerAlgorithm', 'uploadPart_sSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
--
-- 'sSECustomerKey', 'uploadPart_sSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm header@. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
--
-- 'requestPayer', 'uploadPart_requestPayer' - Undocumented member.
--
-- 'sSECustomerKeyMD5', 'uploadPart_sSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
--
-- 'contentMD5', 'uploadPart_contentMD5' - The base64-encoded 128-bit MD5 digest of the part data. This parameter
-- is auto-populated when using the command from the CLI. This parameter is
-- required if object lock parameters are specified.
--
-- 'expectedBucketOwner', 'uploadPart_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
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
  Core.RequestBody ->
  UploadPart
newUploadPart
  pBucket_
  pKey_
  pPartNumber_
  pUploadId_
  pBody_ =
    UploadPart'
      { contentLength = Prelude.Nothing,
        sSECustomerAlgorithm = Prelude.Nothing,
        sSECustomerKey = Prelude.Nothing,
        requestPayer = Prelude.Nothing,
        sSECustomerKeyMD5 = Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        bucket = pBucket_,
        key = pKey_,
        partNumber = pPartNumber_,
        uploadId = pUploadId_,
        body = pBody_
      }

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
uploadPart_contentLength :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Integer)
uploadPart_contentLength = Lens.lens (\UploadPart' {contentLength} -> contentLength) (\s@UploadPart' {} a -> s {contentLength = a} :: UploadPart)

-- | Specifies the algorithm to use to when encrypting the object (for
-- example, AES256).
uploadPart_sSECustomerAlgorithm :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_sSECustomerAlgorithm = Lens.lens (\UploadPart' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPart' {} a -> s {sSECustomerAlgorithm = a} :: UploadPart)

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon S3 does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- @x-amz-server-side-encryption-customer-algorithm header@. This must be
-- the same encryption key specified in the initiate multipart upload
-- request.
uploadPart_sSECustomerKey :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_sSECustomerKey = Lens.lens (\UploadPart' {sSECustomerKey} -> sSECustomerKey) (\s@UploadPart' {} a -> s {sSECustomerKey = a} :: UploadPart) Prelude.. Lens.mapping Core._Sensitive

-- | Undocumented member.
uploadPart_requestPayer :: Lens.Lens' UploadPart (Prelude.Maybe RequestPayer)
uploadPart_requestPayer = Lens.lens (\UploadPart' {requestPayer} -> requestPayer) (\s@UploadPart' {} a -> s {requestPayer = a} :: UploadPart)

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- that the encryption key was transmitted without error.
uploadPart_sSECustomerKeyMD5 :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_sSECustomerKeyMD5 = Lens.lens (\UploadPart' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPart' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPart)

-- | The base64-encoded 128-bit MD5 digest of the part data. This parameter
-- is auto-populated when using the command from the CLI. This parameter is
-- required if object lock parameters are specified.
uploadPart_contentMD5 :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_contentMD5 = Lens.lens (\UploadPart' {contentMD5} -> contentMD5) (\s@UploadPart' {} a -> s {contentMD5 = a} :: UploadPart)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
uploadPart_expectedBucketOwner :: Lens.Lens' UploadPart (Prelude.Maybe Prelude.Text)
uploadPart_expectedBucketOwner = Lens.lens (\UploadPart' {expectedBucketOwner} -> expectedBucketOwner) (\s@UploadPart' {} a -> s {expectedBucketOwner = a} :: UploadPart)

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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
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
uploadPart_body :: Lens.Lens' UploadPart Core.RequestBody
uploadPart_body = Lens.lens (\UploadPart' {body} -> body) (\s@UploadPart' {} a -> s {body = a} :: UploadPart)

instance Core.AWSRequest UploadPart where
  type AWSResponse UploadPart = UploadPartResponse
  request =
    Request.s3vhost
      Prelude.. Request.putBody defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UploadPartResponse'
            Prelude.<$> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-algorithm"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-customer-key-MD5"
                        )
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Core..#? "x-amz-server-side-encryption")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Core.ToBody UploadPart where
  toBody UploadPart' {..} = Core.toBody body

instance Core.ToHeaders UploadPart where
  toHeaders UploadPart' {..} =
    Prelude.mconcat
      [ "Content-Length" Core.=# contentLength,
        "x-amz-server-side-encryption-customer-algorithm"
          Core.=# sSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key"
          Core.=# sSECustomerKey,
        "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-server-side-encryption-customer-key-MD5"
          Core.=# sSECustomerKeyMD5,
        "Content-MD5" Core.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath UploadPart where
  toPath UploadPart' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery UploadPart where
  toQuery UploadPart' {..} =
    Prelude.mconcat
      [ "partNumber" Core.=: partNumber,
        "uploadId" Core.=: uploadId
      ]

-- | /See:/ 'newUploadPartResponse' smart constructor.
data UploadPartResponse = UploadPartResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | Entity tag for the uploaded object.
    eTag :: Prelude.Maybe ETag,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header confirming the
    -- encryption algorithm used.
    sSECustomerAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If server-side encryption with a customer-provided encryption key was
    -- requested, the response will include this header to provide round-trip
    -- message integrity verification of the customer-provided encryption key.
    sSECustomerKeyMD5 :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the ID of the Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) symmetric customer managed key was
    -- used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The server-side encryption algorithm used when storing this object in
    -- Amazon S3 (for example, AES256, aws:kms).
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
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
-- 'requestCharged', 'uploadPartResponse_requestCharged' - Undocumented member.
--
-- 'eTag', 'uploadPartResponse_eTag' - Entity tag for the uploaded object.
--
-- 'sSECustomerAlgorithm', 'uploadPartResponse_sSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
--
-- 'bucketKeyEnabled', 'uploadPartResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'sSECustomerKeyMD5', 'uploadPartResponse_sSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
--
-- 'sSEKMSKeyId', 'uploadPartResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key was
-- used for the object.
--
-- 'serverSideEncryption', 'uploadPartResponse_serverSideEncryption' - The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
--
-- 'httpStatus', 'uploadPartResponse_httpStatus' - The response's http status code.
newUploadPartResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UploadPartResponse
newUploadPartResponse pHttpStatus_ =
  UploadPartResponse'
    { requestCharged =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      sSECustomerAlgorithm = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      sSECustomerKeyMD5 = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
uploadPartResponse_requestCharged :: Lens.Lens' UploadPartResponse (Prelude.Maybe RequestCharged)
uploadPartResponse_requestCharged = Lens.lens (\UploadPartResponse' {requestCharged} -> requestCharged) (\s@UploadPartResponse' {} a -> s {requestCharged = a} :: UploadPartResponse)

-- | Entity tag for the uploaded object.
uploadPartResponse_eTag :: Lens.Lens' UploadPartResponse (Prelude.Maybe ETag)
uploadPartResponse_eTag = Lens.lens (\UploadPartResponse' {eTag} -> eTag) (\s@UploadPartResponse' {} a -> s {eTag = a} :: UploadPartResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
uploadPartResponse_sSECustomerAlgorithm :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_sSECustomerAlgorithm = Lens.lens (\UploadPartResponse' {sSECustomerAlgorithm} -> sSECustomerAlgorithm) (\s@UploadPartResponse' {} a -> s {sSECustomerAlgorithm = a} :: UploadPartResponse)

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
uploadPartResponse_bucketKeyEnabled :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Bool)
uploadPartResponse_bucketKeyEnabled = Lens.lens (\UploadPartResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@UploadPartResponse' {} a -> s {bucketKeyEnabled = a} :: UploadPartResponse)

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round-trip
-- message integrity verification of the customer-provided encryption key.
uploadPartResponse_sSECustomerKeyMD5 :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_sSECustomerKeyMD5 = Lens.lens (\UploadPartResponse' {sSECustomerKeyMD5} -> sSECustomerKeyMD5) (\s@UploadPartResponse' {} a -> s {sSECustomerKeyMD5 = a} :: UploadPartResponse)

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key was
-- used for the object.
uploadPartResponse_sSEKMSKeyId :: Lens.Lens' UploadPartResponse (Prelude.Maybe Prelude.Text)
uploadPartResponse_sSEKMSKeyId = Lens.lens (\UploadPartResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@UploadPartResponse' {} a -> s {sSEKMSKeyId = a} :: UploadPartResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The server-side encryption algorithm used when storing this object in
-- Amazon S3 (for example, AES256, aws:kms).
uploadPartResponse_serverSideEncryption :: Lens.Lens' UploadPartResponse (Prelude.Maybe ServerSideEncryption)
uploadPartResponse_serverSideEncryption = Lens.lens (\UploadPartResponse' {serverSideEncryption} -> serverSideEncryption) (\s@UploadPartResponse' {} a -> s {serverSideEncryption = a} :: UploadPartResponse)

-- | The response's http status code.
uploadPartResponse_httpStatus :: Lens.Lens' UploadPartResponse Prelude.Int
uploadPartResponse_httpStatus = Lens.lens (\UploadPartResponse' {httpStatus} -> httpStatus) (\s@UploadPartResponse' {} a -> s {httpStatus = a} :: UploadPartResponse)

instance Prelude.NFData UploadPartResponse where
  rnf UploadPartResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf sSECustomerAlgorithm
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf sSECustomerKeyMD5
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf httpStatus

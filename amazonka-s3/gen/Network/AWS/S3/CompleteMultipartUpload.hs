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
-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes a multipart upload by assembling previously uploaded parts.
--
-- You first initiate the multipart upload and then upload all parts using
-- the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
-- operation. After successfully uploading all relevant parts of an upload,
-- you call this operation to complete the upload. Upon receiving this
-- request, Amazon S3 concatenates all the parts in ascending order by part
-- number to create a new object. In the Complete Multipart Upload request,
-- you must provide the parts list. You must ensure that the parts list is
-- complete. This operation concatenates the parts that you provide in the
-- list. For each part in the list, you must provide the part number and
-- the @ETag@ value, returned after that part was uploaded.
--
-- Processing of a Complete Multipart Upload request could take several
-- minutes to complete. After Amazon S3 begins processing the request, it
-- sends an HTTP response header that specifies a 200 OK response. While
-- processing is in progress, Amazon S3 periodically sends white space
-- characters to keep the connection from timing out. Because a request
-- could fail after the initial 200 OK response has been sent, it is
-- important that you check the response body to determine whether the
-- request succeeded.
--
-- Note that if @CompleteMultipartUpload@ fails, applications should be
-- prepared to retry the failed requests. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ErrorBestPractices.html Amazon S3 Error Best Practices>.
--
-- For more information about multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload>.
--
-- For information about permissions required to use the multipart upload
-- API, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions>.
--
-- @CompleteMultipartUpload@ has the following special errors:
--
-- -   Error code: @EntityTooSmall@
--
--     -   Description: Your proposed upload is smaller than the minimum
--         allowed object size. Each part must be at least 5 MB in size,
--         except the last part.
--
--     -   400 Bad Request
--
-- -   Error code: @InvalidPart@
--
--     -   Description: One or more of the specified parts could not be
--         found. The part might not have been uploaded, or the specified
--         entity tag might not have matched the part\'s entity tag.
--
--     -   400 Bad Request
--
-- -   Error code: @InvalidPartOrder@
--
--     -   Description: The list of parts was not in ascending order. The
--         parts list must be specified in order by part number.
--
--     -   400 Bad Request
--
-- -   Error code: @NoSuchUpload@
--
--     -   Description: The specified multipart upload does not exist. The
--         upload ID might be invalid, or the multipart upload might have
--         been aborted or completed.
--
--     -   404 Not Found
--
-- The following operations are related to @CompleteMultipartUpload@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.CompleteMultipartUpload
  ( -- * Creating a Request
    CompleteMultipartUpload (..),
    newCompleteMultipartUpload,

    -- * Request Lenses
    completeMultipartUpload_expectedBucketOwner,
    completeMultipartUpload_requestPayer,
    completeMultipartUpload_multipartUpload,
    completeMultipartUpload_bucket,
    completeMultipartUpload_key,
    completeMultipartUpload_uploadId,

    -- * Destructuring the Response
    CompleteMultipartUploadResponse (..),
    newCompleteMultipartUploadResponse,

    -- * Response Lenses
    completeMultipartUploadResponse_eTag,
    completeMultipartUploadResponse_requestCharged,
    completeMultipartUploadResponse_key,
    completeMultipartUploadResponse_expiration,
    completeMultipartUploadResponse_sSEKMSKeyId,
    completeMultipartUploadResponse_versionId,
    completeMultipartUploadResponse_bucketKeyEnabled,
    completeMultipartUploadResponse_serverSideEncryption,
    completeMultipartUploadResponse_bucket,
    completeMultipartUploadResponse_location,
    completeMultipartUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newCompleteMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    requestPayer :: Core.Maybe RequestPayer,
    -- | The container for the multipart upload request information.
    multipartUpload :: Core.Maybe CompletedMultipartUpload,
    -- | Name of the bucket to which the multipart upload was initiated.
    bucket :: BucketName,
    -- | Object key for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | ID for the initiated multipart upload.
    uploadId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CompleteMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'completeMultipartUpload_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'requestPayer', 'completeMultipartUpload_requestPayer' - Undocumented member.
--
-- 'multipartUpload', 'completeMultipartUpload_multipartUpload' - The container for the multipart upload request information.
--
-- 'bucket', 'completeMultipartUpload_bucket' - Name of the bucket to which the multipart upload was initiated.
--
-- 'key', 'completeMultipartUpload_key' - Object key for which the multipart upload was initiated.
--
-- 'uploadId', 'completeMultipartUpload_uploadId' - ID for the initiated multipart upload.
newCompleteMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'uploadId'
  Core.Text ->
  CompleteMultipartUpload
newCompleteMultipartUpload pBucket_ pKey_ pUploadId_ =
  CompleteMultipartUpload'
    { expectedBucketOwner =
        Core.Nothing,
      requestPayer = Core.Nothing,
      multipartUpload = Core.Nothing,
      bucket = pBucket_,
      key = pKey_,
      uploadId = pUploadId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
completeMultipartUpload_expectedBucketOwner :: Lens.Lens' CompleteMultipartUpload (Core.Maybe Core.Text)
completeMultipartUpload_expectedBucketOwner = Lens.lens (\CompleteMultipartUpload' {expectedBucketOwner} -> expectedBucketOwner) (\s@CompleteMultipartUpload' {} a -> s {expectedBucketOwner = a} :: CompleteMultipartUpload)

-- | Undocumented member.
completeMultipartUpload_requestPayer :: Lens.Lens' CompleteMultipartUpload (Core.Maybe RequestPayer)
completeMultipartUpload_requestPayer = Lens.lens (\CompleteMultipartUpload' {requestPayer} -> requestPayer) (\s@CompleteMultipartUpload' {} a -> s {requestPayer = a} :: CompleteMultipartUpload)

-- | The container for the multipart upload request information.
completeMultipartUpload_multipartUpload :: Lens.Lens' CompleteMultipartUpload (Core.Maybe CompletedMultipartUpload)
completeMultipartUpload_multipartUpload = Lens.lens (\CompleteMultipartUpload' {multipartUpload} -> multipartUpload) (\s@CompleteMultipartUpload' {} a -> s {multipartUpload = a} :: CompleteMultipartUpload)

-- | Name of the bucket to which the multipart upload was initiated.
completeMultipartUpload_bucket :: Lens.Lens' CompleteMultipartUpload BucketName
completeMultipartUpload_bucket = Lens.lens (\CompleteMultipartUpload' {bucket} -> bucket) (\s@CompleteMultipartUpload' {} a -> s {bucket = a} :: CompleteMultipartUpload)

-- | Object key for which the multipart upload was initiated.
completeMultipartUpload_key :: Lens.Lens' CompleteMultipartUpload ObjectKey
completeMultipartUpload_key = Lens.lens (\CompleteMultipartUpload' {key} -> key) (\s@CompleteMultipartUpload' {} a -> s {key = a} :: CompleteMultipartUpload)

-- | ID for the initiated multipart upload.
completeMultipartUpload_uploadId :: Lens.Lens' CompleteMultipartUpload Core.Text
completeMultipartUpload_uploadId = Lens.lens (\CompleteMultipartUpload' {uploadId} -> uploadId) (\s@CompleteMultipartUpload' {} a -> s {uploadId = a} :: CompleteMultipartUpload)

instance Core.AWSRequest CompleteMultipartUpload where
  type
    AWSResponse CompleteMultipartUpload =
      CompleteMultipartUploadResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CompleteMultipartUploadResponse'
            Core.<$> (x Core..@? "ETag")
            Core.<*> (h Core..#? "x-amz-request-charged")
            Core.<*> (x Core..@? "Key")
            Core.<*> (h Core..#? "x-amz-expiration")
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                     )
            Core.<*> (h Core..#? "x-amz-version-id")
            Core.<*> ( h
                         Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                     )
            Core.<*> (h Core..#? "x-amz-server-side-encryption")
            Core.<*> (x Core..@? "Bucket")
            Core.<*> (x Core..@? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CompleteMultipartUpload

instance Core.NFData CompleteMultipartUpload

instance Core.ToElement CompleteMultipartUpload where
  toElement CompleteMultipartUpload' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CompleteMultipartUpload"
      multipartUpload

instance Core.ToHeaders CompleteMultipartUpload where
  toHeaders CompleteMultipartUpload' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath CompleteMultipartUpload where
  toPath CompleteMultipartUpload' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery CompleteMultipartUpload where
  toQuery CompleteMultipartUpload' {..} =
    Core.mconcat ["uploadId" Core.=: uploadId]

-- | /See:/ 'newCompleteMultipartUploadResponse' smart constructor.
data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse'
  { -- | Entity tag that identifies the newly created object\'s data. Objects
    -- with different object data will have different entity tags. The entity
    -- tag is an opaque string. The entity tag may or may not be an MD5 digest
    -- of the object data. If the entity tag is not an MD5 digest of the object
    -- data, it will contain one or more nonhexadecimal characters and\/or will
    -- consist of less than 32 or more than 32 hexadecimal digits.
    eTag :: Core.Maybe ETag,
    requestCharged :: Core.Maybe RequestCharged,
    -- | The object key of the newly created object.
    key :: Core.Maybe ObjectKey,
    -- | If the object expiration is configured, this will contain the expiration
    -- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
    -- encoded.
    expiration :: Core.Maybe Core.Text,
    -- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
    -- symmetric customer managed customer master key (CMK) that was used for
    -- the object.
    sSEKMSKeyId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Version ID of the newly created object, in case the bucket has
    -- versioning turned on.
    versionId :: Core.Maybe ObjectVersionId,
    -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with AWS KMS (SSE-KMS).
    bucketKeyEnabled :: Core.Maybe Core.Bool,
    -- | If you specified server-side encryption either with an Amazon S3-managed
    -- encryption key or an AWS KMS customer master key (CMK) in your initiate
    -- multipart upload request, the response includes this header. It confirms
    -- the encryption algorithm that Amazon S3 used to encrypt the object.
    serverSideEncryption :: Core.Maybe ServerSideEncryption,
    -- | The name of the bucket that contains the newly created object.
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
    bucket :: Core.Maybe BucketName,
    -- | The URI that identifies the newly created object.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CompleteMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'completeMultipartUploadResponse_eTag' - Entity tag that identifies the newly created object\'s data. Objects
-- with different object data will have different entity tags. The entity
-- tag is an opaque string. The entity tag may or may not be an MD5 digest
-- of the object data. If the entity tag is not an MD5 digest of the object
-- data, it will contain one or more nonhexadecimal characters and\/or will
-- consist of less than 32 or more than 32 hexadecimal digits.
--
-- 'requestCharged', 'completeMultipartUploadResponse_requestCharged' - Undocumented member.
--
-- 'key', 'completeMultipartUploadResponse_key' - The object key of the newly created object.
--
-- 'expiration', 'completeMultipartUploadResponse_expiration' - If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
--
-- 'sSEKMSKeyId', 'completeMultipartUploadResponse_sSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
--
-- 'versionId', 'completeMultipartUploadResponse_versionId' - Version ID of the newly created object, in case the bucket has
-- versioning turned on.
--
-- 'bucketKeyEnabled', 'completeMultipartUploadResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
--
-- 'serverSideEncryption', 'completeMultipartUploadResponse_serverSideEncryption' - If you specified server-side encryption either with an Amazon S3-managed
-- encryption key or an AWS KMS customer master key (CMK) in your initiate
-- multipart upload request, the response includes this header. It confirms
-- the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- 'bucket', 'completeMultipartUploadResponse_bucket' - The name of the bucket that contains the newly created object.
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
-- 'location', 'completeMultipartUploadResponse_location' - The URI that identifies the newly created object.
--
-- 'httpStatus', 'completeMultipartUploadResponse_httpStatus' - The response's http status code.
newCompleteMultipartUploadResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CompleteMultipartUploadResponse
newCompleteMultipartUploadResponse pHttpStatus_ =
  CompleteMultipartUploadResponse'
    { eTag =
        Core.Nothing,
      requestCharged = Core.Nothing,
      key = Core.Nothing,
      expiration = Core.Nothing,
      sSEKMSKeyId = Core.Nothing,
      versionId = Core.Nothing,
      bucketKeyEnabled = Core.Nothing,
      serverSideEncryption = Core.Nothing,
      bucket = Core.Nothing,
      location = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Entity tag that identifies the newly created object\'s data. Objects
-- with different object data will have different entity tags. The entity
-- tag is an opaque string. The entity tag may or may not be an MD5 digest
-- of the object data. If the entity tag is not an MD5 digest of the object
-- data, it will contain one or more nonhexadecimal characters and\/or will
-- consist of less than 32 or more than 32 hexadecimal digits.
completeMultipartUploadResponse_eTag :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe ETag)
completeMultipartUploadResponse_eTag = Lens.lens (\CompleteMultipartUploadResponse' {eTag} -> eTag) (\s@CompleteMultipartUploadResponse' {} a -> s {eTag = a} :: CompleteMultipartUploadResponse)

-- | Undocumented member.
completeMultipartUploadResponse_requestCharged :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe RequestCharged)
completeMultipartUploadResponse_requestCharged = Lens.lens (\CompleteMultipartUploadResponse' {requestCharged} -> requestCharged) (\s@CompleteMultipartUploadResponse' {} a -> s {requestCharged = a} :: CompleteMultipartUploadResponse)

-- | The object key of the newly created object.
completeMultipartUploadResponse_key :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe ObjectKey)
completeMultipartUploadResponse_key = Lens.lens (\CompleteMultipartUploadResponse' {key} -> key) (\s@CompleteMultipartUploadResponse' {} a -> s {key = a} :: CompleteMultipartUploadResponse)

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
completeMultipartUploadResponse_expiration :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Core.Text)
completeMultipartUploadResponse_expiration = Lens.lens (\CompleteMultipartUploadResponse' {expiration} -> expiration) (\s@CompleteMultipartUploadResponse' {} a -> s {expiration = a} :: CompleteMultipartUploadResponse)

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS)
-- symmetric customer managed customer master key (CMK) that was used for
-- the object.
completeMultipartUploadResponse_sSEKMSKeyId :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Core.Text)
completeMultipartUploadResponse_sSEKMSKeyId = Lens.lens (\CompleteMultipartUploadResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CompleteMultipartUploadResponse' {} a -> s {sSEKMSKeyId = a} :: CompleteMultipartUploadResponse) Core.. Lens.mapping Core._Sensitive

-- | Version ID of the newly created object, in case the bucket has
-- versioning turned on.
completeMultipartUploadResponse_versionId :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe ObjectVersionId)
completeMultipartUploadResponse_versionId = Lens.lens (\CompleteMultipartUploadResponse' {versionId} -> versionId) (\s@CompleteMultipartUploadResponse' {} a -> s {versionId = a} :: CompleteMultipartUploadResponse)

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with AWS KMS (SSE-KMS).
completeMultipartUploadResponse_bucketKeyEnabled :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Core.Bool)
completeMultipartUploadResponse_bucketKeyEnabled = Lens.lens (\CompleteMultipartUploadResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CompleteMultipartUploadResponse' {} a -> s {bucketKeyEnabled = a} :: CompleteMultipartUploadResponse)

-- | If you specified server-side encryption either with an Amazon S3-managed
-- encryption key or an AWS KMS customer master key (CMK) in your initiate
-- multipart upload request, the response includes this header. It confirms
-- the encryption algorithm that Amazon S3 used to encrypt the object.
completeMultipartUploadResponse_serverSideEncryption :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe ServerSideEncryption)
completeMultipartUploadResponse_serverSideEncryption = Lens.lens (\CompleteMultipartUploadResponse' {serverSideEncryption} -> serverSideEncryption) (\s@CompleteMultipartUploadResponse' {} a -> s {serverSideEncryption = a} :: CompleteMultipartUploadResponse)

-- | The name of the bucket that contains the newly created object.
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
completeMultipartUploadResponse_bucket :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe BucketName)
completeMultipartUploadResponse_bucket = Lens.lens (\CompleteMultipartUploadResponse' {bucket} -> bucket) (\s@CompleteMultipartUploadResponse' {} a -> s {bucket = a} :: CompleteMultipartUploadResponse)

-- | The URI that identifies the newly created object.
completeMultipartUploadResponse_location :: Lens.Lens' CompleteMultipartUploadResponse (Core.Maybe Core.Text)
completeMultipartUploadResponse_location = Lens.lens (\CompleteMultipartUploadResponse' {location} -> location) (\s@CompleteMultipartUploadResponse' {} a -> s {location = a} :: CompleteMultipartUploadResponse)

-- | The response's http status code.
completeMultipartUploadResponse_httpStatus :: Lens.Lens' CompleteMultipartUploadResponse Core.Int
completeMultipartUploadResponse_httpStatus = Lens.lens (\CompleteMultipartUploadResponse' {httpStatus} -> httpStatus) (\s@CompleteMultipartUploadResponse' {} a -> s {httpStatus = a} :: CompleteMultipartUploadResponse)

instance Core.NFData CompleteMultipartUploadResponse

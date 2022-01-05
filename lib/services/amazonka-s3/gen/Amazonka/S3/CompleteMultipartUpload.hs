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
-- Module      : Amazonka.S3.CompleteMultipartUpload
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
-- you call this action to complete the upload. Upon receiving this
-- request, Amazon S3 concatenates all the parts in ascending order by part
-- number to create a new object. In the Complete Multipart Upload request,
-- you must provide the parts list. You must ensure that the parts list is
-- complete. This action concatenates the parts that you provide in the
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
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload and Permissions>.
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
module Amazonka.S3.CompleteMultipartUpload
  ( -- * Creating a Request
    CompleteMultipartUpload (..),
    newCompleteMultipartUpload,

    -- * Request Lenses
    completeMultipartUpload_requestPayer,
    completeMultipartUpload_multipartUpload,
    completeMultipartUpload_expectedBucketOwner,
    completeMultipartUpload_bucket,
    completeMultipartUpload_key,
    completeMultipartUpload_uploadId,

    -- * Destructuring the Response
    CompleteMultipartUploadResponse (..),
    newCompleteMultipartUploadResponse,

    -- * Response Lenses
    completeMultipartUploadResponse_requestCharged,
    completeMultipartUploadResponse_eTag,
    completeMultipartUploadResponse_versionId,
    completeMultipartUploadResponse_location,
    completeMultipartUploadResponse_expiration,
    completeMultipartUploadResponse_bucket,
    completeMultipartUploadResponse_bucketKeyEnabled,
    completeMultipartUploadResponse_key,
    completeMultipartUploadResponse_sSEKMSKeyId,
    completeMultipartUploadResponse_serverSideEncryption,
    completeMultipartUploadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newCompleteMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { requestPayer :: Prelude.Maybe RequestPayer,
    -- | The container for the multipart upload request information.
    multipartUpload :: Prelude.Maybe CompletedMultipartUpload,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Name of the bucket to which the multipart upload was initiated.
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
    -- | ID for the initiated multipart upload.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestPayer', 'completeMultipartUpload_requestPayer' - Undocumented member.
--
-- 'multipartUpload', 'completeMultipartUpload_multipartUpload' - The container for the multipart upload request information.
--
-- 'expectedBucketOwner', 'completeMultipartUpload_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'completeMultipartUpload_bucket' - Name of the bucket to which the multipart upload was initiated.
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
-- 'key', 'completeMultipartUpload_key' - Object key for which the multipart upload was initiated.
--
-- 'uploadId', 'completeMultipartUpload_uploadId' - ID for the initiated multipart upload.
newCompleteMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'uploadId'
  Prelude.Text ->
  CompleteMultipartUpload
newCompleteMultipartUpload pBucket_ pKey_ pUploadId_ =
  CompleteMultipartUpload'
    { requestPayer =
        Prelude.Nothing,
      multipartUpload = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      uploadId = pUploadId_
    }

-- | Undocumented member.
completeMultipartUpload_requestPayer :: Lens.Lens' CompleteMultipartUpload (Prelude.Maybe RequestPayer)
completeMultipartUpload_requestPayer = Lens.lens (\CompleteMultipartUpload' {requestPayer} -> requestPayer) (\s@CompleteMultipartUpload' {} a -> s {requestPayer = a} :: CompleteMultipartUpload)

-- | The container for the multipart upload request information.
completeMultipartUpload_multipartUpload :: Lens.Lens' CompleteMultipartUpload (Prelude.Maybe CompletedMultipartUpload)
completeMultipartUpload_multipartUpload = Lens.lens (\CompleteMultipartUpload' {multipartUpload} -> multipartUpload) (\s@CompleteMultipartUpload' {} a -> s {multipartUpload = a} :: CompleteMultipartUpload)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
completeMultipartUpload_expectedBucketOwner :: Lens.Lens' CompleteMultipartUpload (Prelude.Maybe Prelude.Text)
completeMultipartUpload_expectedBucketOwner = Lens.lens (\CompleteMultipartUpload' {expectedBucketOwner} -> expectedBucketOwner) (\s@CompleteMultipartUpload' {} a -> s {expectedBucketOwner = a} :: CompleteMultipartUpload)

-- | Name of the bucket to which the multipart upload was initiated.
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
completeMultipartUpload_bucket :: Lens.Lens' CompleteMultipartUpload BucketName
completeMultipartUpload_bucket = Lens.lens (\CompleteMultipartUpload' {bucket} -> bucket) (\s@CompleteMultipartUpload' {} a -> s {bucket = a} :: CompleteMultipartUpload)

-- | Object key for which the multipart upload was initiated.
completeMultipartUpload_key :: Lens.Lens' CompleteMultipartUpload ObjectKey
completeMultipartUpload_key = Lens.lens (\CompleteMultipartUpload' {key} -> key) (\s@CompleteMultipartUpload' {} a -> s {key = a} :: CompleteMultipartUpload)

-- | ID for the initiated multipart upload.
completeMultipartUpload_uploadId :: Lens.Lens' CompleteMultipartUpload Prelude.Text
completeMultipartUpload_uploadId = Lens.lens (\CompleteMultipartUpload' {uploadId} -> uploadId) (\s@CompleteMultipartUpload' {} a -> s {uploadId = a} :: CompleteMultipartUpload)

instance Core.AWSRequest CompleteMultipartUpload where
  type
    AWSResponse CompleteMultipartUpload =
      CompleteMultipartUploadResponse
  request =
    Request.s3vhost
      Prelude.. Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CompleteMultipartUploadResponse'
            Prelude.<$> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (x Core..@? "ETag")
            Prelude.<*> (h Core..#? "x-amz-version-id")
            Prelude.<*> (x Core..@? "Location")
            Prelude.<*> (h Core..#? "x-amz-expiration")
            Prelude.<*> (x Core..@? "Bucket")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-bucket-key-enabled"
                        )
            Prelude.<*> (x Core..@? "Key")
            Prelude.<*> ( h
                            Core..#? "x-amz-server-side-encryption-aws-kms-key-id"
                        )
            Prelude.<*> (h Core..#? "x-amz-server-side-encryption")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CompleteMultipartUpload where
  hashWithSalt _salt CompleteMultipartUpload' {..} =
    _salt `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` multipartUpload
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` uploadId

instance Prelude.NFData CompleteMultipartUpload where
  rnf CompleteMultipartUpload' {..} =
    Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf multipartUpload
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf uploadId

instance Core.ToElement CompleteMultipartUpload where
  toElement CompleteMultipartUpload' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CompleteMultipartUpload"
      multipartUpload

instance Core.ToHeaders CompleteMultipartUpload where
  toHeaders CompleteMultipartUpload' {..} =
    Prelude.mconcat
      [ "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath CompleteMultipartUpload where
  toPath CompleteMultipartUpload' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery CompleteMultipartUpload where
  toQuery CompleteMultipartUpload' {..} =
    Prelude.mconcat ["uploadId" Core.=: uploadId]

-- | /See:/ 'newCompleteMultipartUploadResponse' smart constructor.
data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | Entity tag that identifies the newly created object\'s data. Objects
    -- with different object data will have different entity tags. The entity
    -- tag is an opaque string. The entity tag may or may not be an MD5 digest
    -- of the object data. If the entity tag is not an MD5 digest of the object
    -- data, it will contain one or more nonhexadecimal characters and\/or will
    -- consist of less than 32 or more than 32 hexadecimal digits.
    eTag :: Prelude.Maybe ETag,
    -- | Version ID of the newly created object, in case the bucket has
    -- versioning turned on.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The URI that identifies the newly created object.
    location :: Prelude.Maybe Prelude.Text,
    -- | If the object expiration is configured, this will contain the expiration
    -- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
    -- encoded.
    expiration :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket that contains the newly created object. Does not
    -- return the access point ARN or access point alias if used.
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
    bucket :: Prelude.Maybe BucketName,
    -- | Indicates whether the multipart upload uses an S3 Bucket Key for
    -- server-side encryption with Amazon Web Services KMS (SSE-KMS).
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The object key of the newly created object.
    key :: Prelude.Maybe ObjectKey,
    -- | If present, specifies the ID of the Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) symmetric customer managed key that
    -- was used for the object.
    sSEKMSKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | If you specified server-side encryption either with an Amazon S3-managed
    -- encryption key or an Amazon Web Services KMS key in your initiate
    -- multipart upload request, the response includes this header. It confirms
    -- the encryption algorithm that Amazon S3 used to encrypt the object.
    serverSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'completeMultipartUploadResponse_requestCharged' - Undocumented member.
--
-- 'eTag', 'completeMultipartUploadResponse_eTag' - Entity tag that identifies the newly created object\'s data. Objects
-- with different object data will have different entity tags. The entity
-- tag is an opaque string. The entity tag may or may not be an MD5 digest
-- of the object data. If the entity tag is not an MD5 digest of the object
-- data, it will contain one or more nonhexadecimal characters and\/or will
-- consist of less than 32 or more than 32 hexadecimal digits.
--
-- 'versionId', 'completeMultipartUploadResponse_versionId' - Version ID of the newly created object, in case the bucket has
-- versioning turned on.
--
-- 'location', 'completeMultipartUploadResponse_location' - The URI that identifies the newly created object.
--
-- 'expiration', 'completeMultipartUploadResponse_expiration' - If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
--
-- 'bucket', 'completeMultipartUploadResponse_bucket' - The name of the bucket that contains the newly created object. Does not
-- return the access point ARN or access point alias if used.
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
-- 'bucketKeyEnabled', 'completeMultipartUploadResponse_bucketKeyEnabled' - Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
--
-- 'key', 'completeMultipartUploadResponse_key' - The object key of the newly created object.
--
-- 'sSEKMSKeyId', 'completeMultipartUploadResponse_sSEKMSKeyId' - If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
--
-- 'serverSideEncryption', 'completeMultipartUploadResponse_serverSideEncryption' - If you specified server-side encryption either with an Amazon S3-managed
-- encryption key or an Amazon Web Services KMS key in your initiate
-- multipart upload request, the response includes this header. It confirms
-- the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- 'httpStatus', 'completeMultipartUploadResponse_httpStatus' - The response's http status code.
newCompleteMultipartUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CompleteMultipartUploadResponse
newCompleteMultipartUploadResponse pHttpStatus_ =
  CompleteMultipartUploadResponse'
    { requestCharged =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      versionId = Prelude.Nothing,
      location = Prelude.Nothing,
      expiration = Prelude.Nothing,
      bucket = Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing,
      key = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
completeMultipartUploadResponse_requestCharged :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe RequestCharged)
completeMultipartUploadResponse_requestCharged = Lens.lens (\CompleteMultipartUploadResponse' {requestCharged} -> requestCharged) (\s@CompleteMultipartUploadResponse' {} a -> s {requestCharged = a} :: CompleteMultipartUploadResponse)

-- | Entity tag that identifies the newly created object\'s data. Objects
-- with different object data will have different entity tags. The entity
-- tag is an opaque string. The entity tag may or may not be an MD5 digest
-- of the object data. If the entity tag is not an MD5 digest of the object
-- data, it will contain one or more nonhexadecimal characters and\/or will
-- consist of less than 32 or more than 32 hexadecimal digits.
completeMultipartUploadResponse_eTag :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe ETag)
completeMultipartUploadResponse_eTag = Lens.lens (\CompleteMultipartUploadResponse' {eTag} -> eTag) (\s@CompleteMultipartUploadResponse' {} a -> s {eTag = a} :: CompleteMultipartUploadResponse)

-- | Version ID of the newly created object, in case the bucket has
-- versioning turned on.
completeMultipartUploadResponse_versionId :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe ObjectVersionId)
completeMultipartUploadResponse_versionId = Lens.lens (\CompleteMultipartUploadResponse' {versionId} -> versionId) (\s@CompleteMultipartUploadResponse' {} a -> s {versionId = a} :: CompleteMultipartUploadResponse)

-- | The URI that identifies the newly created object.
completeMultipartUploadResponse_location :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe Prelude.Text)
completeMultipartUploadResponse_location = Lens.lens (\CompleteMultipartUploadResponse' {location} -> location) (\s@CompleteMultipartUploadResponse' {} a -> s {location = a} :: CompleteMultipartUploadResponse)

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
completeMultipartUploadResponse_expiration :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe Prelude.Text)
completeMultipartUploadResponse_expiration = Lens.lens (\CompleteMultipartUploadResponse' {expiration} -> expiration) (\s@CompleteMultipartUploadResponse' {} a -> s {expiration = a} :: CompleteMultipartUploadResponse)

-- | The name of the bucket that contains the newly created object. Does not
-- return the access point ARN or access point alias if used.
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
completeMultipartUploadResponse_bucket :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe BucketName)
completeMultipartUploadResponse_bucket = Lens.lens (\CompleteMultipartUploadResponse' {bucket} -> bucket) (\s@CompleteMultipartUploadResponse' {} a -> s {bucket = a} :: CompleteMultipartUploadResponse)

-- | Indicates whether the multipart upload uses an S3 Bucket Key for
-- server-side encryption with Amazon Web Services KMS (SSE-KMS).
completeMultipartUploadResponse_bucketKeyEnabled :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe Prelude.Bool)
completeMultipartUploadResponse_bucketKeyEnabled = Lens.lens (\CompleteMultipartUploadResponse' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@CompleteMultipartUploadResponse' {} a -> s {bucketKeyEnabled = a} :: CompleteMultipartUploadResponse)

-- | The object key of the newly created object.
completeMultipartUploadResponse_key :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe ObjectKey)
completeMultipartUploadResponse_key = Lens.lens (\CompleteMultipartUploadResponse' {key} -> key) (\s@CompleteMultipartUploadResponse' {} a -> s {key = a} :: CompleteMultipartUploadResponse)

-- | If present, specifies the ID of the Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) symmetric customer managed key that
-- was used for the object.
completeMultipartUploadResponse_sSEKMSKeyId :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe Prelude.Text)
completeMultipartUploadResponse_sSEKMSKeyId = Lens.lens (\CompleteMultipartUploadResponse' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@CompleteMultipartUploadResponse' {} a -> s {sSEKMSKeyId = a} :: CompleteMultipartUploadResponse) Prelude.. Lens.mapping Core._Sensitive

-- | If you specified server-side encryption either with an Amazon S3-managed
-- encryption key or an Amazon Web Services KMS key in your initiate
-- multipart upload request, the response includes this header. It confirms
-- the encryption algorithm that Amazon S3 used to encrypt the object.
completeMultipartUploadResponse_serverSideEncryption :: Lens.Lens' CompleteMultipartUploadResponse (Prelude.Maybe ServerSideEncryption)
completeMultipartUploadResponse_serverSideEncryption = Lens.lens (\CompleteMultipartUploadResponse' {serverSideEncryption} -> serverSideEncryption) (\s@CompleteMultipartUploadResponse' {} a -> s {serverSideEncryption = a} :: CompleteMultipartUploadResponse)

-- | The response's http status code.
completeMultipartUploadResponse_httpStatus :: Lens.Lens' CompleteMultipartUploadResponse Prelude.Int
completeMultipartUploadResponse_httpStatus = Lens.lens (\CompleteMultipartUploadResponse' {httpStatus} -> httpStatus) (\s@CompleteMultipartUploadResponse' {} a -> s {httpStatus = a} :: CompleteMultipartUploadResponse)

instance
  Prelude.NFData
    CompleteMultipartUploadResponse
  where
  rnf CompleteMultipartUploadResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf bucketKeyEnabled
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.S3.AbortMultipartUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts a multipart upload. After a multipart upload is
-- aborted, no additional parts can be uploaded using that upload ID. The
-- storage consumed by any previously uploaded parts will be freed.
-- However, if any part uploads are currently in progress, those part
-- uploads might or might not succeed. As a result, it might be necessary
-- to abort a given multipart upload multiple times in order to completely
-- free all storage consumed by all parts.
--
-- To verify that all parts have been removed, so you don\'t get charged
-- for the part storage, you should call the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
-- operation and ensure that the parts list is empty.
--
-- For information about permissions required to use the multipart upload
-- API, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions>.
--
-- The following operations are related to @AbortMultipartUpload@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.AbortMultipartUpload
  ( -- * Creating a Request
    AbortMultipartUpload (..),
    newAbortMultipartUpload,

    -- * Request Lenses
    abortMultipartUpload_expectedBucketOwner,
    abortMultipartUpload_requestPayer,
    abortMultipartUpload_bucket,
    abortMultipartUpload_key,
    abortMultipartUpload_uploadId,

    -- * Destructuring the Response
    AbortMultipartUploadResponse (..),
    newAbortMultipartUploadResponse,

    -- * Response Lenses
    abortMultipartUploadResponse_requestCharged,
    abortMultipartUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newAbortMultipartUpload' smart constructor.
data AbortMultipartUpload = AbortMultipartUpload'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The bucket name to which the upload was taking place.
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
    -- | Key of the object for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | Upload ID that identifies the multipart upload.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'abortMultipartUpload_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'requestPayer', 'abortMultipartUpload_requestPayer' - Undocumented member.
--
-- 'bucket', 'abortMultipartUpload_bucket' - The bucket name to which the upload was taking place.
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
-- 'key', 'abortMultipartUpload_key' - Key of the object for which the multipart upload was initiated.
--
-- 'uploadId', 'abortMultipartUpload_uploadId' - Upload ID that identifies the multipart upload.
newAbortMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'uploadId'
  Prelude.Text ->
  AbortMultipartUpload
newAbortMultipartUpload pBucket_ pKey_ pUploadId_ =
  AbortMultipartUpload'
    { expectedBucketOwner =
        Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      uploadId = pUploadId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
abortMultipartUpload_expectedBucketOwner :: Lens.Lens' AbortMultipartUpload (Prelude.Maybe Prelude.Text)
abortMultipartUpload_expectedBucketOwner = Lens.lens (\AbortMultipartUpload' {expectedBucketOwner} -> expectedBucketOwner) (\s@AbortMultipartUpload' {} a -> s {expectedBucketOwner = a} :: AbortMultipartUpload)

-- | Undocumented member.
abortMultipartUpload_requestPayer :: Lens.Lens' AbortMultipartUpload (Prelude.Maybe RequestPayer)
abortMultipartUpload_requestPayer = Lens.lens (\AbortMultipartUpload' {requestPayer} -> requestPayer) (\s@AbortMultipartUpload' {} a -> s {requestPayer = a} :: AbortMultipartUpload)

-- | The bucket name to which the upload was taking place.
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
abortMultipartUpload_bucket :: Lens.Lens' AbortMultipartUpload BucketName
abortMultipartUpload_bucket = Lens.lens (\AbortMultipartUpload' {bucket} -> bucket) (\s@AbortMultipartUpload' {} a -> s {bucket = a} :: AbortMultipartUpload)

-- | Key of the object for which the multipart upload was initiated.
abortMultipartUpload_key :: Lens.Lens' AbortMultipartUpload ObjectKey
abortMultipartUpload_key = Lens.lens (\AbortMultipartUpload' {key} -> key) (\s@AbortMultipartUpload' {} a -> s {key = a} :: AbortMultipartUpload)

-- | Upload ID that identifies the multipart upload.
abortMultipartUpload_uploadId :: Lens.Lens' AbortMultipartUpload Prelude.Text
abortMultipartUpload_uploadId = Lens.lens (\AbortMultipartUpload' {uploadId} -> uploadId) (\s@AbortMultipartUpload' {} a -> s {uploadId = a} :: AbortMultipartUpload)

instance Core.AWSRequest AbortMultipartUpload where
  type
    AWSResponse AbortMultipartUpload =
      AbortMultipartUploadResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AbortMultipartUploadResponse'
            Prelude.<$> (h Core..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AbortMultipartUpload

instance Prelude.NFData AbortMultipartUpload

instance Core.ToHeaders AbortMultipartUpload where
  toHeaders AbortMultipartUpload' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath AbortMultipartUpload where
  toPath AbortMultipartUpload' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery AbortMultipartUpload where
  toQuery AbortMultipartUpload' {..} =
    Prelude.mconcat ["uploadId" Core.=: uploadId]

-- | /See:/ 'newAbortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'abortMultipartUploadResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'abortMultipartUploadResponse_httpStatus' - The response's http status code.
newAbortMultipartUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AbortMultipartUploadResponse
newAbortMultipartUploadResponse pHttpStatus_ =
  AbortMultipartUploadResponse'
    { requestCharged =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
abortMultipartUploadResponse_requestCharged :: Lens.Lens' AbortMultipartUploadResponse (Prelude.Maybe RequestCharged)
abortMultipartUploadResponse_requestCharged = Lens.lens (\AbortMultipartUploadResponse' {requestCharged} -> requestCharged) (\s@AbortMultipartUploadResponse' {} a -> s {requestCharged = a} :: AbortMultipartUploadResponse)

-- | The response's http status code.
abortMultipartUploadResponse_httpStatus :: Lens.Lens' AbortMultipartUploadResponse Prelude.Int
abortMultipartUploadResponse_httpStatus = Lens.lens (\AbortMultipartUploadResponse' {httpStatus} -> httpStatus) (\s@AbortMultipartUploadResponse' {} a -> s {httpStatus = a} :: AbortMultipartUploadResponse)

instance Prelude.NFData AbortMultipartUploadResponse

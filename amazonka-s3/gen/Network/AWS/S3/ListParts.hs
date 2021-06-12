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
-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the parts that have been uploaded for a specific multipart upload.
-- This operation must include the upload ID, which you obtain by sending
-- the initiate multipart upload request (see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>).
-- This request returns a maximum of 1,000 uploaded parts. The default
-- number of parts returned is 1,000 parts. You can restrict the number of
-- parts returned by specifying the @max-parts@ request parameter. If your
-- multipart upload consists of more than 1,000 parts, the response returns
-- an @IsTruncated@ field with the value of true, and a
-- @NextPartNumberMarker@ element. In subsequent @ListParts@ requests you
-- can include the part-number-marker query string parameter and set its
-- value to the @NextPartNumberMarker@ field value from the previous
-- response.
--
-- For more information on multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload>.
--
-- For information on permissions required to use the multipart upload API,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions>.
--
-- The following operations are related to @ListParts@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
--
-- This operation returns paginated results.
module Network.AWS.S3.ListParts
  ( -- * Creating a Request
    ListParts (..),
    newListParts,

    -- * Request Lenses
    listParts_expectedBucketOwner,
    listParts_partNumberMarker,
    listParts_maxParts,
    listParts_requestPayer,
    listParts_bucket,
    listParts_key,
    listParts_uploadId,

    -- * Destructuring the Response
    ListPartsResponse (..),
    newListPartsResponse,

    -- * Response Lenses
    listPartsResponse_requestCharged,
    listPartsResponse_key,
    listPartsResponse_nextPartNumberMarker,
    listPartsResponse_uploadId,
    listPartsResponse_abortDate,
    listPartsResponse_partNumberMarker,
    listPartsResponse_maxParts,
    listPartsResponse_isTruncated,
    listPartsResponse_storageClass,
    listPartsResponse_parts,
    listPartsResponse_abortRuleId,
    listPartsResponse_owner,
    listPartsResponse_bucket,
    listPartsResponse_initiator,
    listPartsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListParts' smart constructor.
data ListParts = ListParts'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | Specifies the part after which listing should begin. Only parts with
    -- higher part numbers will be listed.
    partNumberMarker :: Core.Maybe Core.Int,
    -- | Sets the maximum number of parts to return.
    maxParts :: Core.Maybe Core.Int,
    requestPayer :: Core.Maybe RequestPayer,
    -- | The name of the bucket to which the parts are being uploaded.
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
    -- | Object key for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | Upload ID identifying the multipart upload whose parts are being listed.
    uploadId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListParts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'listParts_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'partNumberMarker', 'listParts_partNumberMarker' - Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
--
-- 'maxParts', 'listParts_maxParts' - Sets the maximum number of parts to return.
--
-- 'requestPayer', 'listParts_requestPayer' - Undocumented member.
--
-- 'bucket', 'listParts_bucket' - The name of the bucket to which the parts are being uploaded.
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
-- 'key', 'listParts_key' - Object key for which the multipart upload was initiated.
--
-- 'uploadId', 'listParts_uploadId' - Upload ID identifying the multipart upload whose parts are being listed.
newListParts ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'uploadId'
  Core.Text ->
  ListParts
newListParts pBucket_ pKey_ pUploadId_ =
  ListParts'
    { expectedBucketOwner = Core.Nothing,
      partNumberMarker = Core.Nothing,
      maxParts = Core.Nothing,
      requestPayer = Core.Nothing,
      bucket = pBucket_,
      key = pKey_,
      uploadId = pUploadId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listParts_expectedBucketOwner :: Lens.Lens' ListParts (Core.Maybe Core.Text)
listParts_expectedBucketOwner = Lens.lens (\ListParts' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListParts' {} a -> s {expectedBucketOwner = a} :: ListParts)

-- | Specifies the part after which listing should begin. Only parts with
-- higher part numbers will be listed.
listParts_partNumberMarker :: Lens.Lens' ListParts (Core.Maybe Core.Int)
listParts_partNumberMarker = Lens.lens (\ListParts' {partNumberMarker} -> partNumberMarker) (\s@ListParts' {} a -> s {partNumberMarker = a} :: ListParts)

-- | Sets the maximum number of parts to return.
listParts_maxParts :: Lens.Lens' ListParts (Core.Maybe Core.Int)
listParts_maxParts = Lens.lens (\ListParts' {maxParts} -> maxParts) (\s@ListParts' {} a -> s {maxParts = a} :: ListParts)

-- | Undocumented member.
listParts_requestPayer :: Lens.Lens' ListParts (Core.Maybe RequestPayer)
listParts_requestPayer = Lens.lens (\ListParts' {requestPayer} -> requestPayer) (\s@ListParts' {} a -> s {requestPayer = a} :: ListParts)

-- | The name of the bucket to which the parts are being uploaded.
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
listParts_bucket :: Lens.Lens' ListParts BucketName
listParts_bucket = Lens.lens (\ListParts' {bucket} -> bucket) (\s@ListParts' {} a -> s {bucket = a} :: ListParts)

-- | Object key for which the multipart upload was initiated.
listParts_key :: Lens.Lens' ListParts ObjectKey
listParts_key = Lens.lens (\ListParts' {key} -> key) (\s@ListParts' {} a -> s {key = a} :: ListParts)

-- | Upload ID identifying the multipart upload whose parts are being listed.
listParts_uploadId :: Lens.Lens' ListParts Core.Text
listParts_uploadId = Lens.lens (\ListParts' {uploadId} -> uploadId) (\s@ListParts' {} a -> s {uploadId = a} :: ListParts)

instance Core.AWSPager ListParts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPartsResponse_isTruncated Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listPartsResponse_nextPartNumberMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listParts_partNumberMarker
          Lens..~ rs
          Lens.^? listPartsResponse_nextPartNumberMarker
            Core.. Lens._Just

instance Core.AWSRequest ListParts where
  type AWSResponse ListParts = ListPartsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListPartsResponse'
            Core.<$> (h Core..#? "x-amz-request-charged")
            Core.<*> (x Core..@? "Key")
            Core.<*> (x Core..@? "NextPartNumberMarker")
            Core.<*> (x Core..@? "UploadId")
            Core.<*> (h Core..#? "x-amz-abort-date")
            Core.<*> (x Core..@? "PartNumberMarker")
            Core.<*> (x Core..@? "MaxParts")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "StorageClass")
            Core.<*> (Core.may (Core.parseXMLList "Part") x)
            Core.<*> (h Core..#? "x-amz-abort-rule-id")
            Core.<*> (x Core..@? "Owner")
            Core.<*> (x Core..@? "Bucket")
            Core.<*> (x Core..@? "Initiator")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListParts

instance Core.NFData ListParts

instance Core.ToHeaders ListParts where
  toHeaders ListParts' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath ListParts where
  toPath ListParts' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery ListParts where
  toQuery ListParts' {..} =
    Core.mconcat
      [ "part-number-marker" Core.=: partNumberMarker,
        "max-parts" Core.=: maxParts,
        "uploadId" Core.=: uploadId
      ]

-- | /See:/ 'newListPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { requestCharged :: Core.Maybe RequestCharged,
    -- | Object key for which the multipart upload was initiated.
    key :: Core.Maybe ObjectKey,
    -- | When a list is truncated, this element specifies the last part in the
    -- list, as well as the value to use for the part-number-marker request
    -- parameter in a subsequent request.
    nextPartNumberMarker :: Core.Maybe Core.Int,
    -- | Upload ID identifying the multipart upload whose parts are being listed.
    uploadId :: Core.Maybe Core.Text,
    -- | If the bucket has a lifecycle rule configured with an action to abort
    -- incomplete multipart uploads and the prefix in the lifecycle rule
    -- matches the object name in the request, then the response includes this
    -- header indicating when the initiated multipart upload will become
    -- eligible for abort operation. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
    --
    -- The response will also include the @x-amz-abort-rule-id@ header that
    -- will provide the ID of the lifecycle configuration rule that defines
    -- this action.
    abortDate :: Core.Maybe Core.ISO8601,
    -- | When a list is truncated, this element specifies the last part in the
    -- list, as well as the value to use for the part-number-marker request
    -- parameter in a subsequent request.
    partNumberMarker :: Core.Maybe Core.Int,
    -- | Maximum number of parts that were allowed in the response.
    maxParts :: Core.Maybe Core.Int,
    -- | Indicates whether the returned list of parts is truncated. A true value
    -- indicates that the list was truncated. A list can be truncated if the
    -- number of parts exceeds the limit returned in the MaxParts element.
    isTruncated :: Core.Maybe Core.Bool,
    -- | Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the
    -- uploaded object.
    storageClass :: Core.Maybe StorageClass,
    -- | Container for elements related to a particular part. A response can
    -- contain zero or more @Part@ elements.
    parts :: Core.Maybe [Part],
    -- | This header is returned along with the @x-amz-abort-date@ header. It
    -- identifies applicable lifecycle configuration rule that defines the
    -- action to abort incomplete multipart uploads.
    abortRuleId :: Core.Maybe Core.Text,
    -- | Container element that identifies the object owner, after the object is
    -- created. If multipart upload is initiated by an IAM user, this element
    -- provides the parent account ID and display name.
    owner :: Core.Maybe Owner,
    -- | The name of the bucket to which the multipart upload was initiated.
    bucket :: Core.Maybe BucketName,
    -- | Container element that identifies who initiated the multipart upload. If
    -- the initiator is an AWS account, this element provides the same
    -- information as the @Owner@ element. If the initiator is an IAM User,
    -- this element provides the user ARN and display name.
    initiator :: Core.Maybe Initiator,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPartsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'listPartsResponse_requestCharged' - Undocumented member.
--
-- 'key', 'listPartsResponse_key' - Object key for which the multipart upload was initiated.
--
-- 'nextPartNumberMarker', 'listPartsResponse_nextPartNumberMarker' - When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
--
-- 'uploadId', 'listPartsResponse_uploadId' - Upload ID identifying the multipart upload whose parts are being listed.
--
-- 'abortDate', 'listPartsResponse_abortDate' - If the bucket has a lifecycle rule configured with an action to abort
-- incomplete multipart uploads and the prefix in the lifecycle rule
-- matches the object name in the request, then the response includes this
-- header indicating when the initiated multipart upload will become
-- eligible for abort operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
--
-- The response will also include the @x-amz-abort-rule-id@ header that
-- will provide the ID of the lifecycle configuration rule that defines
-- this action.
--
-- 'partNumberMarker', 'listPartsResponse_partNumberMarker' - When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
--
-- 'maxParts', 'listPartsResponse_maxParts' - Maximum number of parts that were allowed in the response.
--
-- 'isTruncated', 'listPartsResponse_isTruncated' - Indicates whether the returned list of parts is truncated. A true value
-- indicates that the list was truncated. A list can be truncated if the
-- number of parts exceeds the limit returned in the MaxParts element.
--
-- 'storageClass', 'listPartsResponse_storageClass' - Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the
-- uploaded object.
--
-- 'parts', 'listPartsResponse_parts' - Container for elements related to a particular part. A response can
-- contain zero or more @Part@ elements.
--
-- 'abortRuleId', 'listPartsResponse_abortRuleId' - This header is returned along with the @x-amz-abort-date@ header. It
-- identifies applicable lifecycle configuration rule that defines the
-- action to abort incomplete multipart uploads.
--
-- 'owner', 'listPartsResponse_owner' - Container element that identifies the object owner, after the object is
-- created. If multipart upload is initiated by an IAM user, this element
-- provides the parent account ID and display name.
--
-- 'bucket', 'listPartsResponse_bucket' - The name of the bucket to which the multipart upload was initiated.
--
-- 'initiator', 'listPartsResponse_initiator' - Container element that identifies who initiated the multipart upload. If
-- the initiator is an AWS account, this element provides the same
-- information as the @Owner@ element. If the initiator is an IAM User,
-- this element provides the user ARN and display name.
--
-- 'httpStatus', 'listPartsResponse_httpStatus' - The response's http status code.
newListPartsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPartsResponse
newListPartsResponse pHttpStatus_ =
  ListPartsResponse'
    { requestCharged = Core.Nothing,
      key = Core.Nothing,
      nextPartNumberMarker = Core.Nothing,
      uploadId = Core.Nothing,
      abortDate = Core.Nothing,
      partNumberMarker = Core.Nothing,
      maxParts = Core.Nothing,
      isTruncated = Core.Nothing,
      storageClass = Core.Nothing,
      parts = Core.Nothing,
      abortRuleId = Core.Nothing,
      owner = Core.Nothing,
      bucket = Core.Nothing,
      initiator = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listPartsResponse_requestCharged :: Lens.Lens' ListPartsResponse (Core.Maybe RequestCharged)
listPartsResponse_requestCharged = Lens.lens (\ListPartsResponse' {requestCharged} -> requestCharged) (\s@ListPartsResponse' {} a -> s {requestCharged = a} :: ListPartsResponse)

-- | Object key for which the multipart upload was initiated.
listPartsResponse_key :: Lens.Lens' ListPartsResponse (Core.Maybe ObjectKey)
listPartsResponse_key = Lens.lens (\ListPartsResponse' {key} -> key) (\s@ListPartsResponse' {} a -> s {key = a} :: ListPartsResponse)

-- | When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
listPartsResponse_nextPartNumberMarker :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Int)
listPartsResponse_nextPartNumberMarker = Lens.lens (\ListPartsResponse' {nextPartNumberMarker} -> nextPartNumberMarker) (\s@ListPartsResponse' {} a -> s {nextPartNumberMarker = a} :: ListPartsResponse)

-- | Upload ID identifying the multipart upload whose parts are being listed.
listPartsResponse_uploadId :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Text)
listPartsResponse_uploadId = Lens.lens (\ListPartsResponse' {uploadId} -> uploadId) (\s@ListPartsResponse' {} a -> s {uploadId = a} :: ListPartsResponse)

-- | If the bucket has a lifecycle rule configured with an action to abort
-- incomplete multipart uploads and the prefix in the lifecycle rule
-- matches the object name in the request, then the response includes this
-- header indicating when the initiated multipart upload will become
-- eligible for abort operation. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>.
--
-- The response will also include the @x-amz-abort-rule-id@ header that
-- will provide the ID of the lifecycle configuration rule that defines
-- this action.
listPartsResponse_abortDate :: Lens.Lens' ListPartsResponse (Core.Maybe Core.UTCTime)
listPartsResponse_abortDate = Lens.lens (\ListPartsResponse' {abortDate} -> abortDate) (\s@ListPartsResponse' {} a -> s {abortDate = a} :: ListPartsResponse) Core.. Lens.mapping Core._Time

-- | When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the part-number-marker request
-- parameter in a subsequent request.
listPartsResponse_partNumberMarker :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Int)
listPartsResponse_partNumberMarker = Lens.lens (\ListPartsResponse' {partNumberMarker} -> partNumberMarker) (\s@ListPartsResponse' {} a -> s {partNumberMarker = a} :: ListPartsResponse)

-- | Maximum number of parts that were allowed in the response.
listPartsResponse_maxParts :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Int)
listPartsResponse_maxParts = Lens.lens (\ListPartsResponse' {maxParts} -> maxParts) (\s@ListPartsResponse' {} a -> s {maxParts = a} :: ListPartsResponse)

-- | Indicates whether the returned list of parts is truncated. A true value
-- indicates that the list was truncated. A list can be truncated if the
-- number of parts exceeds the limit returned in the MaxParts element.
listPartsResponse_isTruncated :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Bool)
listPartsResponse_isTruncated = Lens.lens (\ListPartsResponse' {isTruncated} -> isTruncated) (\s@ListPartsResponse' {} a -> s {isTruncated = a} :: ListPartsResponse)

-- | Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the
-- uploaded object.
listPartsResponse_storageClass :: Lens.Lens' ListPartsResponse (Core.Maybe StorageClass)
listPartsResponse_storageClass = Lens.lens (\ListPartsResponse' {storageClass} -> storageClass) (\s@ListPartsResponse' {} a -> s {storageClass = a} :: ListPartsResponse)

-- | Container for elements related to a particular part. A response can
-- contain zero or more @Part@ elements.
listPartsResponse_parts :: Lens.Lens' ListPartsResponse (Core.Maybe [Part])
listPartsResponse_parts = Lens.lens (\ListPartsResponse' {parts} -> parts) (\s@ListPartsResponse' {} a -> s {parts = a} :: ListPartsResponse) Core.. Lens.mapping Lens._Coerce

-- | This header is returned along with the @x-amz-abort-date@ header. It
-- identifies applicable lifecycle configuration rule that defines the
-- action to abort incomplete multipart uploads.
listPartsResponse_abortRuleId :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Text)
listPartsResponse_abortRuleId = Lens.lens (\ListPartsResponse' {abortRuleId} -> abortRuleId) (\s@ListPartsResponse' {} a -> s {abortRuleId = a} :: ListPartsResponse)

-- | Container element that identifies the object owner, after the object is
-- created. If multipart upload is initiated by an IAM user, this element
-- provides the parent account ID and display name.
listPartsResponse_owner :: Lens.Lens' ListPartsResponse (Core.Maybe Owner)
listPartsResponse_owner = Lens.lens (\ListPartsResponse' {owner} -> owner) (\s@ListPartsResponse' {} a -> s {owner = a} :: ListPartsResponse)

-- | The name of the bucket to which the multipart upload was initiated.
listPartsResponse_bucket :: Lens.Lens' ListPartsResponse (Core.Maybe BucketName)
listPartsResponse_bucket = Lens.lens (\ListPartsResponse' {bucket} -> bucket) (\s@ListPartsResponse' {} a -> s {bucket = a} :: ListPartsResponse)

-- | Container element that identifies who initiated the multipart upload. If
-- the initiator is an AWS account, this element provides the same
-- information as the @Owner@ element. If the initiator is an IAM User,
-- this element provides the user ARN and display name.
listPartsResponse_initiator :: Lens.Lens' ListPartsResponse (Core.Maybe Initiator)
listPartsResponse_initiator = Lens.lens (\ListPartsResponse' {initiator} -> initiator) (\s@ListPartsResponse' {} a -> s {initiator = a} :: ListPartsResponse)

-- | The response's http status code.
listPartsResponse_httpStatus :: Lens.Lens' ListPartsResponse Core.Int
listPartsResponse_httpStatus = Lens.lens (\ListPartsResponse' {httpStatus} -> httpStatus) (\s@ListPartsResponse' {} a -> s {httpStatus = a} :: ListPartsResponse)

instance Core.NFData ListPartsResponse

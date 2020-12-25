{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.AbortMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts a multipart upload. After a multipart upload is aborted, no additional parts can be uploaded using that upload ID. The storage consumed by any previously uploaded parts will be freed. However, if any part uploads are currently in progress, those part uploads might or might not succeed. As a result, it might be necessary to abort a given multipart upload multiple times in order to completely free all storage consumed by all parts.
--
-- To verify that all parts have been removed, so you don't get charged for the part storage, you should call the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts> operation and ensure that the parts list is empty.
-- For information about permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
-- The following operations are related to @AbortMultipartUpload@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
module Network.AWS.S3.AbortMultipartUpload
  ( -- * Creating a request
    AbortMultipartUpload (..),
    mkAbortMultipartUpload,

    -- ** Request lenses
    amuBucket,
    amuKey,
    amuUploadId,
    amuExpectedBucketOwner,
    amuRequestPayer,

    -- * Destructuring the response
    AbortMultipartUploadResponse (..),
    mkAbortMultipartUploadResponse,

    -- ** Response lenses
    amurrsRequestCharged,
    amurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkAbortMultipartUpload' smart constructor.
data AbortMultipartUpload = AbortMultipartUpload'
  { -- | The bucket name to which the upload was taking place.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | Key of the object for which the multipart upload was initiated.
    key :: Types.Key,
    -- | Upload ID that identifies the multipart upload.
    uploadId :: Types.MultipartUploadId,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    requestPayer :: Core.Maybe Types.RequestPayer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortMultipartUpload' value with any optional fields omitted.
mkAbortMultipartUpload ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  -- | 'uploadId'
  Types.MultipartUploadId ->
  AbortMultipartUpload
mkAbortMultipartUpload bucket key uploadId =
  AbortMultipartUpload'
    { bucket,
      key,
      uploadId,
      expectedBucketOwner = Core.Nothing,
      requestPayer = Core.Nothing
    }

-- | The bucket name to which the upload was taking place.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuBucket :: Lens.Lens' AbortMultipartUpload Types.BucketName
amuBucket = Lens.field @"bucket"
{-# DEPRECATED amuBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Key of the object for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuKey :: Lens.Lens' AbortMultipartUpload Types.Key
amuKey = Lens.field @"key"
{-# DEPRECATED amuKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Upload ID that identifies the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuUploadId :: Lens.Lens' AbortMultipartUpload Types.MultipartUploadId
amuUploadId = Lens.field @"uploadId"
{-# DEPRECATED amuUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuExpectedBucketOwner :: Lens.Lens' AbortMultipartUpload (Core.Maybe Types.ExpectedBucketOwner)
amuExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED amuExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuRequestPayer :: Lens.Lens' AbortMultipartUpload (Core.Maybe Types.RequestPayer)
amuRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED amuRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

instance Core.AWSRequest AbortMultipartUpload where
  type Rs AbortMultipartUpload = AbortMultipartUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery = Core.toQueryValue "uploadId" uploadId,
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AbortMultipartUploadResponse'
            Core.<$> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAbortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  { requestCharged :: Core.Maybe Types.RequestCharged,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortMultipartUploadResponse' value with any optional fields omitted.
mkAbortMultipartUploadResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AbortMultipartUploadResponse
mkAbortMultipartUploadResponse responseStatus =
  AbortMultipartUploadResponse'
    { requestCharged = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amurrsRequestCharged :: Lens.Lens' AbortMultipartUploadResponse (Core.Maybe Types.RequestCharged)
amurrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED amurrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amurrsResponseStatus :: Lens.Lens' AbortMultipartUploadResponse Core.Int
amurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED amurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

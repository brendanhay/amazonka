{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the parts that have been uploaded for a specific multipart upload. This operation must include the upload ID, which you obtain by sending the initiate multipart upload request (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> ). This request returns a maximum of 1,000 uploaded parts. The default number of parts returned is 1,000 parts. You can restrict the number of parts returned by specifying the @max-parts@ request parameter. If your multipart upload consists of more than 1,000 parts, the response returns an @IsTruncated@ field with the value of true, and a @NextPartNumberMarker@ element. In subsequent @ListParts@ requests you can include the part-number-marker query string parameter and set its value to the @NextPartNumberMarker@ field value from the previous response.
--
-- For more information on multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> .
-- For information on permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
-- The following operations are related to @ListParts@ :
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
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListMultipartUploads.html ListMultipartUploads>
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListParts
  ( -- * Creating a request
    ListParts (..),
    mkListParts,

    -- ** Request lenses
    lpBucket,
    lpKey,
    lpUploadId,
    lpExpectedBucketOwner,
    lpMaxParts,
    lpPartNumberMarker,
    lpRequestPayer,

    -- * Destructuring the response
    ListPartsResponse (..),
    mkListPartsResponse,

    -- ** Response lenses
    lprrsAbortDate,
    lprrsAbortRuleId,
    lprrsBucket,
    lprrsInitiator,
    lprrsIsTruncated,
    lprrsKey,
    lprrsMaxParts,
    lprrsNextPartNumberMarker,
    lprrsOwner,
    lprrsPartNumberMarker,
    lprrsParts,
    lprrsRequestCharged,
    lprrsStorageClass,
    lprrsUploadId,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListParts' smart constructor.
data ListParts = ListParts'
  { -- | The name of the bucket to which the parts are being uploaded.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | Object key for which the multipart upload was initiated.
    key :: Types.Key,
    -- | Upload ID identifying the multipart upload whose parts are being listed.
    uploadId :: Types.MultipartUploadId,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId,
    -- | Sets the maximum number of parts to return.
    maxParts :: Core.Maybe Core.Int,
    -- | Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
    partNumberMarker :: Core.Maybe Core.Int,
    requestPayer :: Core.Maybe Types.RequestPayer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListParts' value with any optional fields omitted.
mkListParts ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  -- | 'uploadId'
  Types.MultipartUploadId ->
  ListParts
mkListParts bucket key uploadId =
  ListParts'
    { bucket,
      key,
      uploadId,
      expectedBucketOwner = Core.Nothing,
      maxParts = Core.Nothing,
      partNumberMarker = Core.Nothing,
      requestPayer = Core.Nothing
    }

-- | The name of the bucket to which the parts are being uploaded.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpBucket :: Lens.Lens' ListParts Types.BucketName
lpBucket = Lens.field @"bucket"
{-# DEPRECATED lpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpKey :: Lens.Lens' ListParts Types.Key
lpKey = Lens.field @"key"
{-# DEPRECATED lpKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUploadId :: Lens.Lens' ListParts Types.MultipartUploadId
lpUploadId = Lens.field @"uploadId"
{-# DEPRECATED lpUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpExpectedBucketOwner :: Lens.Lens' ListParts (Core.Maybe Types.AccountId)
lpExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED lpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Sets the maximum number of parts to return.
--
-- /Note:/ Consider using 'maxParts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxParts :: Lens.Lens' ListParts (Core.Maybe Core.Int)
lpMaxParts = Lens.field @"maxParts"
{-# DEPRECATED lpMaxParts "Use generic-lens or generic-optics with 'maxParts' instead." #-}

-- | Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
--
-- /Note:/ Consider using 'partNumberMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPartNumberMarker :: Lens.Lens' ListParts (Core.Maybe Core.Int)
lpPartNumberMarker = Lens.field @"partNumberMarker"
{-# DEPRECATED lpPartNumberMarker "Use generic-lens or generic-optics with 'partNumberMarker' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpRequestPayer :: Lens.Lens' ListParts (Core.Maybe Types.RequestPayer)
lpRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED lpRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

instance Core.AWSRequest ListParts where
  type Rs ListParts = ListPartsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery =
          Core.toQueryValue "uploadId" uploadId
            Core.<> (Core.toQueryValue "max-parts" Core.<$> maxParts)
            Core.<> (Core.toQueryValue "part-number-marker" Core.<$> partNumberMarker),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListPartsResponse'
            Core.<$> (Core.parseHeaderMaybe "x-amz-abort-date" h)
            Core.<*> (Core.parseHeaderMaybe "x-amz-abort-rule-id" h)
            Core.<*> (x Core..@? "Bucket")
            Core.<*> (x Core..@? "Initiator")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Key")
            Core.<*> (x Core..@? "MaxParts")
            Core.<*> (x Core..@? "NextPartNumberMarker")
            Core.<*> (x Core..@? "Owner")
            Core.<*> (x Core..@? "PartNumberMarker")
            Core.<*> (x Core..@? "Part")
            Core.<*> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> (x Core..@? "StorageClass")
            Core.<*> (x Core..@? "UploadId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListParts where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"nextPartNumberMarker") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"partNumberMarker"
            Lens..~ rs Lens.^. Lens.field @"nextPartNumberMarker"
        )

-- | /See:/ 'mkListPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { -- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, then the response includes this header indicating when the initiated multipart upload will become eligible for abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
    --
    -- The response will also include the @x-amz-abort-rule-id@ header that will provide the ID of the lifecycle configuration rule that defines this action.
    abortDate :: Core.Maybe Core.UTCTime,
    -- | This header is returned along with the @x-amz-abort-date@ header. It identifies applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
    abortRuleId :: Core.Maybe Types.AbortRuleId,
    -- | The name of the bucket to which the multipart upload was initiated.
    bucket :: Core.Maybe Types.BucketName,
    -- | Container element that identifies who initiated the multipart upload. If the initiator is an AWS account, this element provides the same information as the @Owner@ element. If the initiator is an IAM User, this element provides the user ARN and display name.
    initiator :: Core.Maybe Types.Initiator,
    -- | Indicates whether the returned list of parts is truncated. A true value indicates that the list was truncated. A list can be truncated if the number of parts exceeds the limit returned in the MaxParts element.
    isTruncated :: Core.Maybe Core.Bool,
    -- | Object key for which the multipart upload was initiated.
    key :: Core.Maybe Types.ObjectKey,
    -- | Maximum number of parts that were allowed in the response.
    maxParts :: Core.Maybe Core.Int,
    -- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
    nextPartNumberMarker :: Core.Maybe Core.Int,
    -- | Container element that identifies the object owner, after the object is created. If multipart upload is initiated by an IAM user, this element provides the parent account ID and display name.
    owner :: Core.Maybe Types.Owner,
    -- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
    partNumberMarker :: Core.Maybe Core.Int,
    -- | Container for elements related to a particular part. A response can contain zero or more @Part@ elements.
    parts :: Core.Maybe [Types.Part],
    requestCharged :: Core.Maybe Types.RequestCharged,
    -- | Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the uploaded object.
    storageClass :: Core.Maybe Types.StorageClass,
    -- | Upload ID identifying the multipart upload whose parts are being listed.
    uploadId :: Core.Maybe Types.MultipartUploadId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPartsResponse' value with any optional fields omitted.
mkListPartsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPartsResponse
mkListPartsResponse responseStatus =
  ListPartsResponse'
    { abortDate = Core.Nothing,
      abortRuleId = Core.Nothing,
      bucket = Core.Nothing,
      initiator = Core.Nothing,
      isTruncated = Core.Nothing,
      key = Core.Nothing,
      maxParts = Core.Nothing,
      nextPartNumberMarker = Core.Nothing,
      owner = Core.Nothing,
      partNumberMarker = Core.Nothing,
      parts = Core.Nothing,
      requestCharged = Core.Nothing,
      storageClass = Core.Nothing,
      uploadId = Core.Nothing,
      responseStatus
    }

-- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, then the response includes this header indicating when the initiated multipart upload will become eligible for abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
--
-- The response will also include the @x-amz-abort-rule-id@ header that will provide the ID of the lifecycle configuration rule that defines this action.
--
-- /Note:/ Consider using 'abortDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsAbortDate :: Lens.Lens' ListPartsResponse (Core.Maybe Core.UTCTime)
lprrsAbortDate = Lens.field @"abortDate"
{-# DEPRECATED lprrsAbortDate "Use generic-lens or generic-optics with 'abortDate' instead." #-}

-- | This header is returned along with the @x-amz-abort-date@ header. It identifies applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
--
-- /Note:/ Consider using 'abortRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsAbortRuleId :: Lens.Lens' ListPartsResponse (Core.Maybe Types.AbortRuleId)
lprrsAbortRuleId = Lens.field @"abortRuleId"
{-# DEPRECATED lprrsAbortRuleId "Use generic-lens or generic-optics with 'abortRuleId' instead." #-}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsBucket :: Lens.Lens' ListPartsResponse (Core.Maybe Types.BucketName)
lprrsBucket = Lens.field @"bucket"
{-# DEPRECATED lprrsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container element that identifies who initiated the multipart upload. If the initiator is an AWS account, this element provides the same information as the @Owner@ element. If the initiator is an IAM User, this element provides the user ARN and display name.
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsInitiator :: Lens.Lens' ListPartsResponse (Core.Maybe Types.Initiator)
lprrsInitiator = Lens.field @"initiator"
{-# DEPRECATED lprrsInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

-- | Indicates whether the returned list of parts is truncated. A true value indicates that the list was truncated. A list can be truncated if the number of parts exceeds the limit returned in the MaxParts element.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsIsTruncated :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Bool)
lprrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lprrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsKey :: Lens.Lens' ListPartsResponse (Core.Maybe Types.ObjectKey)
lprrsKey = Lens.field @"key"
{-# DEPRECATED lprrsKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Maximum number of parts that were allowed in the response.
--
-- /Note:/ Consider using 'maxParts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsMaxParts :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Int)
lprrsMaxParts = Lens.field @"maxParts"
{-# DEPRECATED lprrsMaxParts "Use generic-lens or generic-optics with 'maxParts' instead." #-}

-- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextPartNumberMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextPartNumberMarker :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Int)
lprrsNextPartNumberMarker = Lens.field @"nextPartNumberMarker"
{-# DEPRECATED lprrsNextPartNumberMarker "Use generic-lens or generic-optics with 'nextPartNumberMarker' instead." #-}

-- | Container element that identifies the object owner, after the object is created. If multipart upload is initiated by an IAM user, this element provides the parent account ID and display name.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsOwner :: Lens.Lens' ListPartsResponse (Core.Maybe Types.Owner)
lprrsOwner = Lens.field @"owner"
{-# DEPRECATED lprrsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'partNumberMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPartNumberMarker :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Int)
lprrsPartNumberMarker = Lens.field @"partNumberMarker"
{-# DEPRECATED lprrsPartNumberMarker "Use generic-lens or generic-optics with 'partNumberMarker' instead." #-}

-- | Container for elements related to a particular part. A response can contain zero or more @Part@ elements.
--
-- /Note:/ Consider using 'parts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsParts :: Lens.Lens' ListPartsResponse (Core.Maybe [Types.Part])
lprrsParts = Lens.field @"parts"
{-# DEPRECATED lprrsParts "Use generic-lens or generic-optics with 'parts' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsRequestCharged :: Lens.Lens' ListPartsResponse (Core.Maybe Types.RequestCharged)
lprrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED lprrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the uploaded object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsStorageClass :: Lens.Lens' ListPartsResponse (Core.Maybe Types.StorageClass)
lprrsStorageClass = Lens.field @"storageClass"
{-# DEPRECATED lprrsStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsUploadId :: Lens.Lens' ListPartsResponse (Core.Maybe Types.MultipartUploadId)
lprrsUploadId = Lens.field @"uploadId"
{-# DEPRECATED lprrsUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPartsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

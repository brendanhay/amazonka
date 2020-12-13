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
    lpMaxParts,
    lpBucket,
    lpRequestPayer,
    lpKey,
    lpPartNumberMarker,
    lpUploadId,
    lpExpectedBucketOwner,

    -- * Destructuring the response
    ListPartsResponse (..),
    mkListPartsResponse,

    -- ** Response lenses
    lprsParts,
    lprsRequestCharged,
    lprsMaxParts,
    lprsInitiator,
    lprsBucket,
    lprsAbortDate,
    lprsNextPartNumberMarker,
    lprsAbortRuleId,
    lprsOwner,
    lprsKey,
    lprsStorageClass,
    lprsIsTruncated,
    lprsPartNumberMarker,
    lprsUploadId,
    lprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListParts' smart constructor.
data ListParts = ListParts'
  { -- | Sets the maximum number of parts to return.
    maxParts :: Lude.Maybe Lude.Int,
    -- | The name of the bucket to which the parts are being uploaded.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    requestPayer :: Lude.Maybe RequestPayer,
    -- | Object key for which the multipart upload was initiated.
    key :: ObjectKey,
    -- | Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
    partNumberMarker :: Lude.Maybe Lude.Int,
    -- | Upload ID identifying the multipart upload whose parts are being listed.
    uploadId :: Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListParts' with the minimum fields required to make a request.
--
-- * 'maxParts' - Sets the maximum number of parts to return.
-- * 'bucket' - The name of the bucket to which the parts are being uploaded.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'requestPayer' -
-- * 'key' - Object key for which the multipart upload was initiated.
-- * 'partNumberMarker' - Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
-- * 'uploadId' - Upload ID identifying the multipart upload whose parts are being listed.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkListParts ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'uploadId'
  Lude.Text ->
  ListParts
mkListParts pBucket_ pKey_ pUploadId_ =
  ListParts'
    { maxParts = Lude.Nothing,
      bucket = pBucket_,
      requestPayer = Lude.Nothing,
      key = pKey_,
      partNumberMarker = Lude.Nothing,
      uploadId = pUploadId_,
      expectedBucketOwner = Lude.Nothing
    }

-- | Sets the maximum number of parts to return.
--
-- /Note:/ Consider using 'maxParts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxParts :: Lens.Lens' ListParts (Lude.Maybe Lude.Int)
lpMaxParts = Lens.lens (maxParts :: ListParts -> Lude.Maybe Lude.Int) (\s a -> s {maxParts = a} :: ListParts)
{-# DEPRECATED lpMaxParts "Use generic-lens or generic-optics with 'maxParts' instead." #-}

-- | The name of the bucket to which the parts are being uploaded.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpBucket :: Lens.Lens' ListParts BucketName
lpBucket = Lens.lens (bucket :: ListParts -> BucketName) (\s a -> s {bucket = a} :: ListParts)
{-# DEPRECATED lpBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpRequestPayer :: Lens.Lens' ListParts (Lude.Maybe RequestPayer)
lpRequestPayer = Lens.lens (requestPayer :: ListParts -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: ListParts)
{-# DEPRECATED lpRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpKey :: Lens.Lens' ListParts ObjectKey
lpKey = Lens.lens (key :: ListParts -> ObjectKey) (\s a -> s {key = a} :: ListParts)
{-# DEPRECATED lpKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.
--
-- /Note:/ Consider using 'partNumberMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPartNumberMarker :: Lens.Lens' ListParts (Lude.Maybe Lude.Int)
lpPartNumberMarker = Lens.lens (partNumberMarker :: ListParts -> Lude.Maybe Lude.Int) (\s a -> s {partNumberMarker = a} :: ListParts)
{-# DEPRECATED lpPartNumberMarker "Use generic-lens or generic-optics with 'partNumberMarker' instead." #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUploadId :: Lens.Lens' ListParts Lude.Text
lpUploadId = Lens.lens (uploadId :: ListParts -> Lude.Text) (\s a -> s {uploadId = a} :: ListParts)
{-# DEPRECATED lpUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpExpectedBucketOwner :: Lens.Lens' ListParts (Lude.Maybe Lude.Text)
lpExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListParts -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListParts)
{-# DEPRECATED lpExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Page.AWSPager ListParts where
  page rq rs
    | Page.stop (rs Lens.^. lprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lprsNextPartNumberMarker) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpPartNumberMarker Lens..~ rs Lens.^. lprsNextPartNumberMarker

instance Lude.AWSRequest ListParts where
  type Rs ListParts = ListPartsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListPartsResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "Part") x)
            Lude.<*> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (x Lude..@? "MaxParts")
            Lude.<*> (x Lude..@? "Initiator")
            Lude.<*> (x Lude..@? "Bucket")
            Lude.<*> (h Lude..#? "x-amz-abort-date")
            Lude.<*> (x Lude..@? "NextPartNumberMarker")
            Lude.<*> (h Lude..#? "x-amz-abort-rule-id")
            Lude.<*> (x Lude..@? "Owner")
            Lude.<*> (x Lude..@? "Key")
            Lude.<*> (x Lude..@? "StorageClass")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (x Lude..@? "PartNumberMarker")
            Lude.<*> (x Lude..@? "UploadId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListParts where
  toHeaders ListParts' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath ListParts where
  toPath ListParts' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery ListParts where
  toQuery ListParts' {..} =
    Lude.mconcat
      [ "max-parts" Lude.=: maxParts,
        "part-number-marker" Lude.=: partNumberMarker,
        "uploadId" Lude.=: uploadId
      ]

-- | /See:/ 'mkListPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { -- | Container for elements related to a particular part. A response can contain zero or more @Part@ elements.
    parts :: Lude.Maybe [Part],
    requestCharged :: Lude.Maybe RequestCharged,
    -- | Maximum number of parts that were allowed in the response.
    maxParts :: Lude.Maybe Lude.Int,
    -- | Container element that identifies who initiated the multipart upload. If the initiator is an AWS account, this element provides the same information as the @Owner@ element. If the initiator is an IAM User, this element provides the user ARN and display name.
    initiator :: Lude.Maybe Initiator,
    -- | The name of the bucket to which the multipart upload was initiated.
    bucket :: Lude.Maybe BucketName,
    -- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, then the response includes this header indicating when the initiated multipart upload will become eligible for abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
    --
    -- The response will also include the @x-amz-abort-rule-id@ header that will provide the ID of the lifecycle configuration rule that defines this action.
    abortDate :: Lude.Maybe Lude.DateTime,
    -- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
    nextPartNumberMarker :: Lude.Maybe Lude.Int,
    -- | This header is returned along with the @x-amz-abort-date@ header. It identifies applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
    abortRuleId :: Lude.Maybe Lude.Text,
    -- | Container element that identifies the object owner, after the object is created. If multipart upload is initiated by an IAM user, this element provides the parent account ID and display name.
    owner :: Lude.Maybe Owner,
    -- | Object key for which the multipart upload was initiated.
    key :: Lude.Maybe ObjectKey,
    -- | Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the uploaded object.
    storageClass :: Lude.Maybe StorageClass,
    -- | Indicates whether the returned list of parts is truncated. A true value indicates that the list was truncated. A list can be truncated if the number of parts exceeds the limit returned in the MaxParts element.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
    partNumberMarker :: Lude.Maybe Lude.Int,
    -- | Upload ID identifying the multipart upload whose parts are being listed.
    uploadId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPartsResponse' with the minimum fields required to make a request.
--
-- * 'parts' - Container for elements related to a particular part. A response can contain zero or more @Part@ elements.
-- * 'requestCharged' -
-- * 'maxParts' - Maximum number of parts that were allowed in the response.
-- * 'initiator' - Container element that identifies who initiated the multipart upload. If the initiator is an AWS account, this element provides the same information as the @Owner@ element. If the initiator is an IAM User, this element provides the user ARN and display name.
-- * 'bucket' - The name of the bucket to which the multipart upload was initiated.
-- * 'abortDate' - If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, then the response includes this header indicating when the initiated multipart upload will become eligible for abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
--
-- The response will also include the @x-amz-abort-rule-id@ header that will provide the ID of the lifecycle configuration rule that defines this action.
-- * 'nextPartNumberMarker' - When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
-- * 'abortRuleId' - This header is returned along with the @x-amz-abort-date@ header. It identifies applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
-- * 'owner' - Container element that identifies the object owner, after the object is created. If multipart upload is initiated by an IAM user, this element provides the parent account ID and display name.
-- * 'key' - Object key for which the multipart upload was initiated.
-- * 'storageClass' - Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the uploaded object.
-- * 'isTruncated' - Indicates whether the returned list of parts is truncated. A true value indicates that the list was truncated. A list can be truncated if the number of parts exceeds the limit returned in the MaxParts element.
-- * 'partNumberMarker' - When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
-- * 'uploadId' - Upload ID identifying the multipart upload whose parts are being listed.
-- * 'responseStatus' - The response status code.
mkListPartsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPartsResponse
mkListPartsResponse pResponseStatus_ =
  ListPartsResponse'
    { parts = Lude.Nothing,
      requestCharged = Lude.Nothing,
      maxParts = Lude.Nothing,
      initiator = Lude.Nothing,
      bucket = Lude.Nothing,
      abortDate = Lude.Nothing,
      nextPartNumberMarker = Lude.Nothing,
      abortRuleId = Lude.Nothing,
      owner = Lude.Nothing,
      key = Lude.Nothing,
      storageClass = Lude.Nothing,
      isTruncated = Lude.Nothing,
      partNumberMarker = Lude.Nothing,
      uploadId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Container for elements related to a particular part. A response can contain zero or more @Part@ elements.
--
-- /Note:/ Consider using 'parts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsParts :: Lens.Lens' ListPartsResponse (Lude.Maybe [Part])
lprsParts = Lens.lens (parts :: ListPartsResponse -> Lude.Maybe [Part]) (\s a -> s {parts = a} :: ListPartsResponse)
{-# DEPRECATED lprsParts "Use generic-lens or generic-optics with 'parts' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsRequestCharged :: Lens.Lens' ListPartsResponse (Lude.Maybe RequestCharged)
lprsRequestCharged = Lens.lens (requestCharged :: ListPartsResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: ListPartsResponse)
{-# DEPRECATED lprsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | Maximum number of parts that were allowed in the response.
--
-- /Note:/ Consider using 'maxParts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsMaxParts :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Int)
lprsMaxParts = Lens.lens (maxParts :: ListPartsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxParts = a} :: ListPartsResponse)
{-# DEPRECATED lprsMaxParts "Use generic-lens or generic-optics with 'maxParts' instead." #-}

-- | Container element that identifies who initiated the multipart upload. If the initiator is an AWS account, this element provides the same information as the @Owner@ element. If the initiator is an IAM User, this element provides the user ARN and display name.
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsInitiator :: Lens.Lens' ListPartsResponse (Lude.Maybe Initiator)
lprsInitiator = Lens.lens (initiator :: ListPartsResponse -> Lude.Maybe Initiator) (\s a -> s {initiator = a} :: ListPartsResponse)
{-# DEPRECATED lprsInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsBucket :: Lens.Lens' ListPartsResponse (Lude.Maybe BucketName)
lprsBucket = Lens.lens (bucket :: ListPartsResponse -> Lude.Maybe BucketName) (\s a -> s {bucket = a} :: ListPartsResponse)
{-# DEPRECATED lprsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | If the bucket has a lifecycle rule configured with an action to abort incomplete multipart uploads and the prefix in the lifecycle rule matches the object name in the request, then the response includes this header indicating when the initiated multipart upload will become eligible for abort operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy> .
--
-- The response will also include the @x-amz-abort-rule-id@ header that will provide the ID of the lifecycle configuration rule that defines this action.
--
-- /Note:/ Consider using 'abortDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsAbortDate :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.DateTime)
lprsAbortDate = Lens.lens (abortDate :: ListPartsResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {abortDate = a} :: ListPartsResponse)
{-# DEPRECATED lprsAbortDate "Use generic-lens or generic-optics with 'abortDate' instead." #-}

-- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextPartNumberMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextPartNumberMarker :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Int)
lprsNextPartNumberMarker = Lens.lens (nextPartNumberMarker :: ListPartsResponse -> Lude.Maybe Lude.Int) (\s a -> s {nextPartNumberMarker = a} :: ListPartsResponse)
{-# DEPRECATED lprsNextPartNumberMarker "Use generic-lens or generic-optics with 'nextPartNumberMarker' instead." #-}

-- | This header is returned along with the @x-amz-abort-date@ header. It identifies applicable lifecycle configuration rule that defines the action to abort incomplete multipart uploads.
--
-- /Note:/ Consider using 'abortRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsAbortRuleId :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Text)
lprsAbortRuleId = Lens.lens (abortRuleId :: ListPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {abortRuleId = a} :: ListPartsResponse)
{-# DEPRECATED lprsAbortRuleId "Use generic-lens or generic-optics with 'abortRuleId' instead." #-}

-- | Container element that identifies the object owner, after the object is created. If multipart upload is initiated by an IAM user, this element provides the parent account ID and display name.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsOwner :: Lens.Lens' ListPartsResponse (Lude.Maybe Owner)
lprsOwner = Lens.lens (owner :: ListPartsResponse -> Lude.Maybe Owner) (\s a -> s {owner = a} :: ListPartsResponse)
{-# DEPRECATED lprsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | Object key for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsKey :: Lens.Lens' ListPartsResponse (Lude.Maybe ObjectKey)
lprsKey = Lens.lens (key :: ListPartsResponse -> Lude.Maybe ObjectKey) (\s a -> s {key = a} :: ListPartsResponse)
{-# DEPRECATED lprsKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Class of storage (STANDARD or REDUCED_REDUNDANCY) used to store the uploaded object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsStorageClass :: Lens.Lens' ListPartsResponse (Lude.Maybe StorageClass)
lprsStorageClass = Lens.lens (storageClass :: ListPartsResponse -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: ListPartsResponse)
{-# DEPRECATED lprsStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Indicates whether the returned list of parts is truncated. A true value indicates that the list was truncated. A list can be truncated if the number of parts exceeds the limit returned in the MaxParts element.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsIsTruncated :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Bool)
lprsIsTruncated = Lens.lens (isTruncated :: ListPartsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListPartsResponse)
{-# DEPRECATED lprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When a list is truncated, this element specifies the last part in the list, as well as the value to use for the part-number-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'partNumberMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPartNumberMarker :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Int)
lprsPartNumberMarker = Lens.lens (partNumberMarker :: ListPartsResponse -> Lude.Maybe Lude.Int) (\s a -> s {partNumberMarker = a} :: ListPartsResponse)
{-# DEPRECATED lprsPartNumberMarker "Use generic-lens or generic-optics with 'partNumberMarker' instead." #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsUploadId :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Text)
lprsUploadId = Lens.lens (uploadId :: ListPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {uploadId = a} :: ListPartsResponse)
{-# DEPRECATED lprsUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPartsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPartsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPartsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

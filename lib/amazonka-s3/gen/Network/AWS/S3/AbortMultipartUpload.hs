{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    amuRequestPayer,
    amuExpectedBucketOwner,
    amuBucket,
    amuKey,
    amuUploadId,

    -- * Destructuring the response
    AbortMultipartUploadResponse (..),
    mkAbortMultipartUploadResponse,

    -- ** Response lenses
    amursRequestCharged,
    amursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkAbortMultipartUpload' smart constructor.
data AbortMultipartUpload = AbortMultipartUpload'
  { requestPayer ::
      Lude.Maybe RequestPayer,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    key :: ObjectKey,
    uploadId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortMultipartUpload' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name to which the upload was taking place.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'key' - Key of the object for which the multipart upload was initiated.
-- * 'requestPayer' - Undocumented field.
-- * 'uploadId' - Upload ID that identifies the multipart upload.
mkAbortMultipartUpload ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'uploadId'
  Lude.Text ->
  AbortMultipartUpload
mkAbortMultipartUpload pBucket_ pKey_ pUploadId_ =
  AbortMultipartUpload'
    { requestPayer = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      uploadId = pUploadId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuRequestPayer :: Lens.Lens' AbortMultipartUpload (Lude.Maybe RequestPayer)
amuRequestPayer = Lens.lens (requestPayer :: AbortMultipartUpload -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: AbortMultipartUpload)
{-# DEPRECATED amuRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuExpectedBucketOwner :: Lens.Lens' AbortMultipartUpload (Lude.Maybe Lude.Text)
amuExpectedBucketOwner = Lens.lens (expectedBucketOwner :: AbortMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: AbortMultipartUpload)
{-# DEPRECATED amuExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name to which the upload was taking place.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuBucket :: Lens.Lens' AbortMultipartUpload BucketName
amuBucket = Lens.lens (bucket :: AbortMultipartUpload -> BucketName) (\s a -> s {bucket = a} :: AbortMultipartUpload)
{-# DEPRECATED amuBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Key of the object for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuKey :: Lens.Lens' AbortMultipartUpload ObjectKey
amuKey = Lens.lens (key :: AbortMultipartUpload -> ObjectKey) (\s a -> s {key = a} :: AbortMultipartUpload)
{-# DEPRECATED amuKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Upload ID that identifies the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuUploadId :: Lens.Lens' AbortMultipartUpload Lude.Text
amuUploadId = Lens.lens (uploadId :: AbortMultipartUpload -> Lude.Text) (\s a -> s {uploadId = a} :: AbortMultipartUpload)
{-# DEPRECATED amuUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Lude.AWSRequest AbortMultipartUpload where
  type Rs AbortMultipartUpload = AbortMultipartUploadResponse
  request = Req.delete s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          AbortMultipartUploadResponse'
            Lude.<$> (h Lude..#? "x-amz-request-charged")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AbortMultipartUpload where
  toHeaders AbortMultipartUpload' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath AbortMultipartUpload where
  toPath AbortMultipartUpload' {..} =
    Lude.mconcat ["/", Lude.toBS bucket, "/", Lude.toBS key]

instance Lude.ToQuery AbortMultipartUpload where
  toQuery AbortMultipartUpload' {..} =
    Lude.mconcat ["uploadId" Lude.=: uploadId]

-- | /See:/ 'mkAbortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  { requestCharged ::
      Lude.Maybe RequestCharged,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortMultipartUploadResponse' with the minimum fields required to make a request.
--
-- * 'requestCharged' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkAbortMultipartUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AbortMultipartUploadResponse
mkAbortMultipartUploadResponse pResponseStatus_ =
  AbortMultipartUploadResponse'
    { requestCharged = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amursRequestCharged :: Lens.Lens' AbortMultipartUploadResponse (Lude.Maybe RequestCharged)
amursRequestCharged = Lens.lens (requestCharged :: AbortMultipartUploadResponse -> Lude.Maybe RequestCharged) (\s a -> s {requestCharged = a} :: AbortMultipartUploadResponse)
{-# DEPRECATED amursRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amursResponseStatus :: Lens.Lens' AbortMultipartUploadResponse Lude.Int
amursResponseStatus = Lens.lens (responseStatus :: AbortMultipartUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AbortMultipartUploadResponse)
{-# DEPRECATED amursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

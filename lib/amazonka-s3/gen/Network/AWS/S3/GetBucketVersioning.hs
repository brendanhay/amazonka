{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the versioning state of a bucket.
--
-- To retrieve the versioning state of a bucket, you must be the bucket owner.
-- This implementation also returns the MFA Delete status of the versioning state. If the MFA Delete status is @enabled@ , the bucket owner must use an authentication device to change the versioning state of the bucket.
-- The following operations are related to @GetBucketVersioning@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.GetBucketVersioning
  ( -- * Creating a request
    GetBucketVersioning (..),
    mkGetBucketVersioning,

    -- ** Request lenses
    gbvBucket,
    gbvExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketVersioningResponse (..),
    mkGetBucketVersioningResponse,

    -- ** Response lenses
    gbvrsStatus,
    gbvrsMFADelete,
    gbvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketVersioning' smart constructor.
data GetBucketVersioning = GetBucketVersioning'
  { -- | The name of the bucket for which to get the versioning information.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketVersioning' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to get the versioning information.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketVersioning ::
  -- | 'bucket'
  BucketName ->
  GetBucketVersioning
mkGetBucketVersioning pBucket_ =
  GetBucketVersioning'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket for which to get the versioning information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvBucket :: Lens.Lens' GetBucketVersioning BucketName
gbvBucket = Lens.lens (bucket :: GetBucketVersioning -> BucketName) (\s a -> s {bucket = a} :: GetBucketVersioning)
{-# DEPRECATED gbvBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvExpectedBucketOwner :: Lens.Lens' GetBucketVersioning (Lude.Maybe Lude.Text)
gbvExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketVersioning -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketVersioning)
{-# DEPRECATED gbvExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketVersioning where
  type Rs GetBucketVersioning = GetBucketVersioningResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketVersioningResponse'
            Lude.<$> (x Lude..@? "Status")
            Lude.<*> (x Lude..@? "MfaDelete")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketVersioning where
  toHeaders GetBucketVersioning' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketVersioning where
  toPath GetBucketVersioning' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketVersioning where
  toQuery = Lude.const (Lude.mconcat ["versioning"])

-- | /See:/ 'mkGetBucketVersioningResponse' smart constructor.
data GetBucketVersioningResponse = GetBucketVersioningResponse'
  { -- | The versioning state of the bucket.
    status :: Lude.Maybe BucketVersioningStatus,
    -- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
    mfaDelete :: Lude.Maybe MFADeleteStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketVersioningResponse' with the minimum fields required to make a request.
--
-- * 'status' - The versioning state of the bucket.
-- * 'mfaDelete' - Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
-- * 'responseStatus' - The response status code.
mkGetBucketVersioningResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketVersioningResponse
mkGetBucketVersioningResponse pResponseStatus_ =
  GetBucketVersioningResponse'
    { status = Lude.Nothing,
      mfaDelete = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The versioning state of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrsStatus :: Lens.Lens' GetBucketVersioningResponse (Lude.Maybe BucketVersioningStatus)
gbvrsStatus = Lens.lens (status :: GetBucketVersioningResponse -> Lude.Maybe BucketVersioningStatus) (\s a -> s {status = a} :: GetBucketVersioningResponse)
{-# DEPRECATED gbvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
--
-- /Note:/ Consider using 'mfaDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrsMFADelete :: Lens.Lens' GetBucketVersioningResponse (Lude.Maybe MFADeleteStatus)
gbvrsMFADelete = Lens.lens (mfaDelete :: GetBucketVersioningResponse -> Lude.Maybe MFADeleteStatus) (\s a -> s {mfaDelete = a} :: GetBucketVersioningResponse)
{-# DEPRECATED gbvrsMFADelete "Use generic-lens or generic-optics with 'mfaDelete' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrsResponseStatus :: Lens.Lens' GetBucketVersioningResponse Lude.Int
gbvrsResponseStatus = Lens.lens (responseStatus :: GetBucketVersioningResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketVersioningResponse)
{-# DEPRECATED gbvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

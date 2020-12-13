{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET operation uses the @accelerate@ subresource to return the Transfer Acceleration state of a bucket, which is either @Enabled@ or @Suspended@ . Amazon S3 Transfer Acceleration is a bucket-level feature that enables you to perform faster data transfers to and from Amazon S3.
--
-- To use this operation, you must have permission to perform the @s3:GetAccelerateConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ .
-- You set the Transfer Acceleration state of an existing bucket to @Enabled@ or @Suspended@ by using the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration> operation.
-- A GET @accelerate@ request does not return a state value for a bucket that has no transfer acceleration state. A bucket has no Transfer Acceleration state if a state has never been set on the bucket.
-- For more information about transfer acceleration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> in the Amazon Simple Storage Service Developer Guide.
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration>
module Network.AWS.S3.GetBucketAccelerateConfiguration
  ( -- * Creating a request
    GetBucketAccelerateConfiguration (..),
    mkGetBucketAccelerateConfiguration,

    -- ** Request lenses
    gbacBucket,
    gbacExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketAccelerateConfigurationResponse (..),
    mkGetBucketAccelerateConfigurationResponse,

    -- ** Response lenses
    gbacrsStatus,
    gbacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketAccelerateConfiguration' smart constructor.
data GetBucketAccelerateConfiguration = GetBucketAccelerateConfiguration'
  { -- | The name of the bucket for which the accelerate configuration is retrieved.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketAccelerateConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which the accelerate configuration is retrieved.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketAccelerateConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetBucketAccelerateConfiguration
mkGetBucketAccelerateConfiguration pBucket_ =
  GetBucketAccelerateConfiguration'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket for which the accelerate configuration is retrieved.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacBucket :: Lens.Lens' GetBucketAccelerateConfiguration BucketName
gbacBucket = Lens.lens (bucket :: GetBucketAccelerateConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetBucketAccelerateConfiguration)
{-# DEPRECATED gbacBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacExpectedBucketOwner :: Lens.Lens' GetBucketAccelerateConfiguration (Lude.Maybe Lude.Text)
gbacExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketAccelerateConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketAccelerateConfiguration)
{-# DEPRECATED gbacExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketAccelerateConfiguration where
  type
    Rs GetBucketAccelerateConfiguration =
      GetBucketAccelerateConfigurationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketAccelerateConfigurationResponse'
            Lude.<$> (x Lude..@? "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketAccelerateConfiguration where
  toHeaders GetBucketAccelerateConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketAccelerateConfiguration where
  toPath GetBucketAccelerateConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketAccelerateConfiguration where
  toQuery = Lude.const (Lude.mconcat ["accelerate"])

-- | /See:/ 'mkGetBucketAccelerateConfigurationResponse' smart constructor.
data GetBucketAccelerateConfigurationResponse = GetBucketAccelerateConfigurationResponse'
  { -- | The accelerate configuration of the bucket.
    status :: Lude.Maybe BucketAccelerateStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketAccelerateConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'status' - The accelerate configuration of the bucket.
-- * 'responseStatus' - The response status code.
mkGetBucketAccelerateConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketAccelerateConfigurationResponse
mkGetBucketAccelerateConfigurationResponse pResponseStatus_ =
  GetBucketAccelerateConfigurationResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The accelerate configuration of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrsStatus :: Lens.Lens' GetBucketAccelerateConfigurationResponse (Lude.Maybe BucketAccelerateStatus)
gbacrsStatus = Lens.lens (status :: GetBucketAccelerateConfigurationResponse -> Lude.Maybe BucketAccelerateStatus) (\s a -> s {status = a} :: GetBucketAccelerateConfigurationResponse)
{-# DEPRECATED gbacrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrsResponseStatus :: Lens.Lens' GetBucketAccelerateConfigurationResponse Lude.Int
gbacrsResponseStatus = Lens.lens (responseStatus :: GetBucketAccelerateConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketAccelerateConfigurationResponse)
{-# DEPRECATED gbacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

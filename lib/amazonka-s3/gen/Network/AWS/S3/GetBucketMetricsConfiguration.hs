{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketMetricsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a metrics configuration (specified by the metrics configuration ID) from the bucket. Note that this doesn't include the daily storage metrics.
--
-- To use this operation, you must have permissions to perform the @s3:GetMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about CloudWatch request metrics for Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> .
-- The following operations are related to @GetBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>
module Network.AWS.S3.GetBucketMetricsConfiguration
  ( -- * Creating a request
    GetBucketMetricsConfiguration (..),
    mkGetBucketMetricsConfiguration,

    -- ** Request lenses
    gbmcExpectedBucketOwner,
    gbmcBucket,
    gbmcId,

    -- * Destructuring the response
    GetBucketMetricsConfigurationResponse (..),
    mkGetBucketMetricsConfigurationResponse,

    -- ** Response lenses
    gbmcrsMetricsConfiguration,
    gbmcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketMetricsConfiguration' smart constructor.
data GetBucketMetricsConfiguration = GetBucketMetricsConfiguration'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket containing the metrics configuration to retrieve.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'id' - The ID used to identify the metrics configuration.
mkGetBucketMetricsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  GetBucketMetricsConfiguration
mkGetBucketMetricsConfiguration pBucket_ pId_ =
  GetBucketMetricsConfiguration'
    { expectedBucketOwner =
        Lude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcExpectedBucketOwner :: Lens.Lens' GetBucketMetricsConfiguration (Lude.Maybe Lude.Text)
gbmcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketMetricsConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketMetricsConfiguration)
{-# DEPRECATED gbmcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket containing the metrics configuration to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcBucket :: Lens.Lens' GetBucketMetricsConfiguration BucketName
gbmcBucket = Lens.lens (bucket :: GetBucketMetricsConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetBucketMetricsConfiguration)
{-# DEPRECATED gbmcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcId :: Lens.Lens' GetBucketMetricsConfiguration Lude.Text
gbmcId = Lens.lens (id :: GetBucketMetricsConfiguration -> Lude.Text) (\s a -> s {id = a} :: GetBucketMetricsConfiguration)
{-# DEPRECATED gbmcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetBucketMetricsConfiguration where
  type
    Rs GetBucketMetricsConfiguration =
      GetBucketMetricsConfigurationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketMetricsConfigurationResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketMetricsConfiguration where
  toHeaders GetBucketMetricsConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketMetricsConfiguration where
  toPath GetBucketMetricsConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketMetricsConfiguration where
  toQuery GetBucketMetricsConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "metrics"]

-- | /See:/ 'mkGetBucketMetricsConfigurationResponse' smart constructor.
data GetBucketMetricsConfigurationResponse = GetBucketMetricsConfigurationResponse'
  { metricsConfiguration ::
      Lude.Maybe
        MetricsConfiguration,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'metricsConfiguration' - Specifies the metrics configuration.
-- * 'responseStatus' - The response status code.
mkGetBucketMetricsConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketMetricsConfigurationResponse
mkGetBucketMetricsConfigurationResponse pResponseStatus_ =
  GetBucketMetricsConfigurationResponse'
    { metricsConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies the metrics configuration.
--
-- /Note:/ Consider using 'metricsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcrsMetricsConfiguration :: Lens.Lens' GetBucketMetricsConfigurationResponse (Lude.Maybe MetricsConfiguration)
gbmcrsMetricsConfiguration = Lens.lens (metricsConfiguration :: GetBucketMetricsConfigurationResponse -> Lude.Maybe MetricsConfiguration) (\s a -> s {metricsConfiguration = a} :: GetBucketMetricsConfigurationResponse)
{-# DEPRECATED gbmcrsMetricsConfiguration "Use generic-lens or generic-optics with 'metricsConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcrsResponseStatus :: Lens.Lens' GetBucketMetricsConfigurationResponse Lude.Int
gbmcrsResponseStatus = Lens.lens (responseStatus :: GetBucketMetricsConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketMetricsConfigurationResponse)
{-# DEPRECATED gbmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

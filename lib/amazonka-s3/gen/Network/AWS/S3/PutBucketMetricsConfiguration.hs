{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketMetricsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a metrics configuration (specified by the metrics configuration ID) for the bucket. You can have up to 1,000 metrics configurations per bucket. If you're updating an existing metrics configuration, note that this is a full replacement of the existing metrics configuration. If you don't include the elements you want to keep, they are erased.
--
-- To use this operation, you must have permissions to perform the @s3:PutMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about CloudWatch request metrics for Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> .
-- The following operations are related to @PutBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
--
-- @GetBucketLifecycle@ has the following special error:
--
--     * Error code: @TooManyConfigurations@
--
--     * Description: You are attempting to create a new configuration but have already reached the 1,000-configuration limit.
--
--
--     * HTTP Status Code: HTTP 400 Bad Request
module Network.AWS.S3.PutBucketMetricsConfiguration
  ( -- * Creating a request
    PutBucketMetricsConfiguration (..),
    mkPutBucketMetricsConfiguration,

    -- ** Request lenses
    pbmcExpectedBucketOwner,
    pbmcBucket,
    pbmcId,
    pbmcMetricsConfiguration,

    -- * Destructuring the response
    PutBucketMetricsConfigurationResponse (..),
    mkPutBucketMetricsConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketMetricsConfiguration' smart constructor.
data PutBucketMetricsConfiguration = PutBucketMetricsConfiguration'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName,
    id :: Lude.Text,
    metricsConfiguration ::
      MetricsConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which the metrics configuration is set.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'id' - The ID used to identify the metrics configuration.
-- * 'metricsConfiguration' - Specifies the metrics configuration.
mkPutBucketMetricsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  -- | 'metricsConfiguration'
  MetricsConfiguration ->
  PutBucketMetricsConfiguration
mkPutBucketMetricsConfiguration
  pBucket_
  pId_
  pMetricsConfiguration_ =
    PutBucketMetricsConfiguration'
      { expectedBucketOwner =
          Lude.Nothing,
        bucket = pBucket_,
        id = pId_,
        metricsConfiguration = pMetricsConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcExpectedBucketOwner :: Lens.Lens' PutBucketMetricsConfiguration (Lude.Maybe Lude.Text)
pbmcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketMetricsConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketMetricsConfiguration)
{-# DEPRECATED pbmcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket for which the metrics configuration is set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcBucket :: Lens.Lens' PutBucketMetricsConfiguration BucketName
pbmcBucket = Lens.lens (bucket :: PutBucketMetricsConfiguration -> BucketName) (\s a -> s {bucket = a} :: PutBucketMetricsConfiguration)
{-# DEPRECATED pbmcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcId :: Lens.Lens' PutBucketMetricsConfiguration Lude.Text
pbmcId = Lens.lens (id :: PutBucketMetricsConfiguration -> Lude.Text) (\s a -> s {id = a} :: PutBucketMetricsConfiguration)
{-# DEPRECATED pbmcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies the metrics configuration.
--
-- /Note:/ Consider using 'metricsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcMetricsConfiguration :: Lens.Lens' PutBucketMetricsConfiguration MetricsConfiguration
pbmcMetricsConfiguration = Lens.lens (metricsConfiguration :: PutBucketMetricsConfiguration -> MetricsConfiguration) (\s a -> s {metricsConfiguration = a} :: PutBucketMetricsConfiguration)
{-# DEPRECATED pbmcMetricsConfiguration "Use generic-lens or generic-optics with 'metricsConfiguration' instead." #-}

instance Lude.AWSRequest PutBucketMetricsConfiguration where
  type
    Rs PutBucketMetricsConfiguration =
      PutBucketMetricsConfigurationResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketMetricsConfigurationResponse'

instance Lude.ToElement PutBucketMetricsConfiguration where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}MetricsConfiguration"
      Lude.. metricsConfiguration

instance Lude.ToHeaders PutBucketMetricsConfiguration where
  toHeaders PutBucketMetricsConfiguration' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath PutBucketMetricsConfiguration where
  toPath PutBucketMetricsConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketMetricsConfiguration where
  toQuery PutBucketMetricsConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "metrics"]

-- | /See:/ 'mkPutBucketMetricsConfigurationResponse' smart constructor.
data PutBucketMetricsConfigurationResponse = PutBucketMetricsConfigurationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
mkPutBucketMetricsConfigurationResponse ::
  PutBucketMetricsConfigurationResponse
mkPutBucketMetricsConfigurationResponse =
  PutBucketMetricsConfigurationResponse'

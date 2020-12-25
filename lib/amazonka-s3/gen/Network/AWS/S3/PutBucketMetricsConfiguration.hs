{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    pbmcBucket,
    pbmcId,
    pbmcMetricsConfiguration,
    pbmcExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketMetricsConfigurationResponse (..),
    mkPutBucketMetricsConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketMetricsConfiguration' smart constructor.
data PutBucketMetricsConfiguration = PutBucketMetricsConfiguration'
  { -- | The name of the bucket for which the metrics configuration is set.
    bucket :: Types.BucketName,
    -- | The ID used to identify the metrics configuration.
    id :: Types.MetricsId,
    -- | Specifies the metrics configuration.
    metricsConfiguration :: Types.MetricsConfiguration,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketMetricsConfiguration' value with any optional fields omitted.
mkPutBucketMetricsConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'id'
  Types.MetricsId ->
  -- | 'metricsConfiguration'
  Types.MetricsConfiguration ->
  PutBucketMetricsConfiguration
mkPutBucketMetricsConfiguration bucket id metricsConfiguration =
  PutBucketMetricsConfiguration'
    { bucket,
      id,
      metricsConfiguration,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket for which the metrics configuration is set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcBucket :: Lens.Lens' PutBucketMetricsConfiguration Types.BucketName
pbmcBucket = Lens.field @"bucket"
{-# DEPRECATED pbmcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcId :: Lens.Lens' PutBucketMetricsConfiguration Types.MetricsId
pbmcId = Lens.field @"id"
{-# DEPRECATED pbmcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies the metrics configuration.
--
-- /Note:/ Consider using 'metricsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcMetricsConfiguration :: Lens.Lens' PutBucketMetricsConfiguration Types.MetricsConfiguration
pbmcMetricsConfiguration = Lens.field @"metricsConfiguration"
{-# DEPRECATED pbmcMetricsConfiguration "Use generic-lens or generic-optics with 'metricsConfiguration' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbmcExpectedBucketOwner :: Lens.Lens' PutBucketMetricsConfiguration (Core.Maybe Types.AccountId)
pbmcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED pbmcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest PutBucketMetricsConfiguration where
  type
    Rs PutBucketMetricsConfiguration =
      PutBucketMetricsConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "id" id Core.<> (Core.pure ("metrics", "")),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveNull PutBucketMetricsConfigurationResponse'

-- | /See:/ 'mkPutBucketMetricsConfigurationResponse' smart constructor.
data PutBucketMetricsConfigurationResponse = PutBucketMetricsConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketMetricsConfigurationResponse' value with any optional fields omitted.
mkPutBucketMetricsConfigurationResponse ::
  PutBucketMetricsConfigurationResponse
mkPutBucketMetricsConfigurationResponse =
  PutBucketMetricsConfigurationResponse'

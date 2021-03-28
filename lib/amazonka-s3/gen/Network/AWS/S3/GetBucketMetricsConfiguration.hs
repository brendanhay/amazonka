{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.S3.GetBucketMetricsConfiguration
    (
    -- * Creating a request
      GetBucketMetricsConfiguration (..)
    , mkGetBucketMetricsConfiguration
    -- ** Request lenses
    , gbmcBucket
    , gbmcId
    , gbmcExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketMetricsConfigurationResponse (..)
    , mkGetBucketMetricsConfigurationResponse
    -- ** Response lenses
    , gbmcrrsMetricsConfiguration
    , gbmcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketMetricsConfiguration' smart constructor.
data GetBucketMetricsConfiguration = GetBucketMetricsConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket containing the metrics configuration to retrieve.
  , id :: Types.MetricsId
    -- ^ The ID used to identify the metrics configuration.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketMetricsConfiguration' value with any optional fields omitted.
mkGetBucketMetricsConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.MetricsId -- ^ 'id'
    -> GetBucketMetricsConfiguration
mkGetBucketMetricsConfiguration bucket id
  = GetBucketMetricsConfiguration'{bucket, id,
                                   expectedBucketOwner = Core.Nothing}

-- | The name of the bucket containing the metrics configuration to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcBucket :: Lens.Lens' GetBucketMetricsConfiguration Types.BucketName
gbmcBucket = Lens.field @"bucket"
{-# INLINEABLE gbmcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcId :: Lens.Lens' GetBucketMetricsConfiguration Types.MetricsId
gbmcId = Lens.field @"id"
{-# INLINEABLE gbmcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcExpectedBucketOwner :: Lens.Lens' GetBucketMetricsConfiguration (Core.Maybe Types.AccountId)
gbmcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbmcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketMetricsConfiguration where
        toQuery GetBucketMetricsConfiguration{..}
          = Core.toQueryPair "id" id Core.<>
              Core.toQueryPair "metrics" ("" :: Core.Text)

instance Core.ToHeaders GetBucketMetricsConfiguration where
        toHeaders GetBucketMetricsConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketMetricsConfiguration where
        type Rs GetBucketMetricsConfiguration =
             GetBucketMetricsConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetBucketMetricsConfigurationResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketMetricsConfigurationResponse' smart constructor.
data GetBucketMetricsConfigurationResponse = GetBucketMetricsConfigurationResponse'
  { metricsConfiguration :: Core.Maybe Types.MetricsConfiguration
    -- ^ Specifies the metrics configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketMetricsConfigurationResponse' value with any optional fields omitted.
mkGetBucketMetricsConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketMetricsConfigurationResponse
mkGetBucketMetricsConfigurationResponse responseStatus
  = GetBucketMetricsConfigurationResponse'{metricsConfiguration =
                                             Core.Nothing,
                                           responseStatus}

-- | Specifies the metrics configuration.
--
-- /Note:/ Consider using 'metricsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcrrsMetricsConfiguration :: Lens.Lens' GetBucketMetricsConfigurationResponse (Core.Maybe Types.MetricsConfiguration)
gbmcrrsMetricsConfiguration = Lens.field @"metricsConfiguration"
{-# INLINEABLE gbmcrrsMetricsConfiguration #-}
{-# DEPRECATED metricsConfiguration "Use generic-lens or generic-optics with 'metricsConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmcrrsResponseStatus :: Lens.Lens' GetBucketMetricsConfigurationResponse Core.Int
gbmcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbmcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketMetricsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metrics configuration for the Amazon CloudWatch request metrics (specified by the metrics configuration ID) from the bucket. Note that this doesn't include the daily storage metrics.
--
-- To use this operation, you must have permissions to perform the @s3:PutMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For information about CloudWatch request metrics for Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> . 
-- The following operations are related to @DeleteBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> 
--
--
module Network.AWS.S3.DeleteBucketMetricsConfiguration
    (
    -- * Creating a request
      DeleteBucketMetricsConfiguration (..)
    , mkDeleteBucketMetricsConfiguration
    -- ** Request lenses
    , dbmcBucket
    , dbmcId
    , dbmcExpectedBucketOwner

    -- * Destructuring the response
    , DeleteBucketMetricsConfigurationResponse (..)
    , mkDeleteBucketMetricsConfigurationResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteBucketMetricsConfiguration' smart constructor.
data DeleteBucketMetricsConfiguration = DeleteBucketMetricsConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket containing the metrics configuration to delete.
  , id :: Types.MetricsId
    -- ^ The ID used to identify the metrics configuration.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketMetricsConfiguration' value with any optional fields omitted.
mkDeleteBucketMetricsConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.MetricsId -- ^ 'id'
    -> DeleteBucketMetricsConfiguration
mkDeleteBucketMetricsConfiguration bucket id
  = DeleteBucketMetricsConfiguration'{bucket, id,
                                      expectedBucketOwner = Core.Nothing}

-- | The name of the bucket containing the metrics configuration to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmcBucket :: Lens.Lens' DeleteBucketMetricsConfiguration Types.BucketName
dbmcBucket = Lens.field @"bucket"
{-# INLINEABLE dbmcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmcId :: Lens.Lens' DeleteBucketMetricsConfiguration Types.MetricsId
dbmcId = Lens.field @"id"
{-# INLINEABLE dbmcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmcExpectedBucketOwner :: Lens.Lens' DeleteBucketMetricsConfiguration (Core.Maybe Types.AccountId)
dbmcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dbmcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery DeleteBucketMetricsConfiguration where
        toQuery DeleteBucketMetricsConfiguration{..}
          = Core.toQueryPair "id" id Core.<>
              Core.toQueryPair "metrics" ("" :: Core.Text)

instance Core.ToHeaders DeleteBucketMetricsConfiguration where
        toHeaders DeleteBucketMetricsConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest DeleteBucketMetricsConfiguration where
        type Rs DeleteBucketMetricsConfiguration =
             DeleteBucketMetricsConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteBucketMetricsConfigurationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBucketMetricsConfigurationResponse' smart constructor.
data DeleteBucketMetricsConfigurationResponse = DeleteBucketMetricsConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBucketMetricsConfigurationResponse' value with any optional fields omitted.
mkDeleteBucketMetricsConfigurationResponse
    :: DeleteBucketMetricsConfigurationResponse
mkDeleteBucketMetricsConfigurationResponse
  = DeleteBucketMetricsConfigurationResponse'

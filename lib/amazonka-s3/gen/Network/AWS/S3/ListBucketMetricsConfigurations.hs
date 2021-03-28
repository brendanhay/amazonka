{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBucketMetricsConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the metrics configurations for the bucket. The metrics configurations are only for the request metrics of the bucket and do not provide information on daily storage metrics. You can have up to 1,000 configurations per bucket.
--
-- This operation supports list pagination and does not return more than 100 configurations at a time. Always check the @IsTruncated@ element in the response. If there are no more configurations to list, @IsTruncated@ is set to false. If there are more configurations to list, @IsTruncated@ is set to true, and there is a value in @NextContinuationToken@ . You use the @NextContinuationToken@ value to continue the pagination of the list by passing the value in @continuation-token@ in the request to @GET@ the next page.
-- To use this operation, you must have permissions to perform the @s3:GetMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- For more information about metrics configurations and CloudWatch request metrics, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> .
-- The following operations are related to @ListBucketMetricsConfigurations@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration> 
--
--
module Network.AWS.S3.ListBucketMetricsConfigurations
    (
    -- * Creating a request
      ListBucketMetricsConfigurations (..)
    , mkListBucketMetricsConfigurations
    -- ** Request lenses
    , lbmcBucket
    , lbmcContinuationToken
    , lbmcExpectedBucketOwner

    -- * Destructuring the response
    , ListBucketMetricsConfigurationsResponse (..)
    , mkListBucketMetricsConfigurationsResponse
    -- ** Response lenses
    , lbmcrrsContinuationToken
    , lbmcrrsIsTruncated
    , lbmcrrsMetricsConfigurationList
    , lbmcrrsNextContinuationToken
    , lbmcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListBucketMetricsConfigurations' smart constructor.
data ListBucketMetricsConfigurations = ListBucketMetricsConfigurations'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket containing the metrics configurations to retrieve.
  , continuationToken :: Core.Maybe Types.Token
    -- ^ The marker that is used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBucketMetricsConfigurations' value with any optional fields omitted.
mkListBucketMetricsConfigurations
    :: Types.BucketName -- ^ 'bucket'
    -> ListBucketMetricsConfigurations
mkListBucketMetricsConfigurations bucket
  = ListBucketMetricsConfigurations'{bucket,
                                     continuationToken = Core.Nothing,
                                     expectedBucketOwner = Core.Nothing}

-- | The name of the bucket containing the metrics configurations to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcBucket :: Lens.Lens' ListBucketMetricsConfigurations Types.BucketName
lbmcBucket = Lens.field @"bucket"
{-# INLINEABLE lbmcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The marker that is used to continue a metrics configuration listing that has been truncated. Use the NextContinuationToken from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcContinuationToken :: Lens.Lens' ListBucketMetricsConfigurations (Core.Maybe Types.Token)
lbmcContinuationToken = Lens.field @"continuationToken"
{-# INLINEABLE lbmcContinuationToken #-}
{-# DEPRECATED continuationToken "Use generic-lens or generic-optics with 'continuationToken' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcExpectedBucketOwner :: Lens.Lens' ListBucketMetricsConfigurations (Core.Maybe Types.ExpectedBucketOwner)
lbmcExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE lbmcExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery ListBucketMetricsConfigurations where
        toQuery ListBucketMetricsConfigurations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "continuation-token")
              continuationToken
              Core.<> Core.toQueryPair "metrics" ("" :: Core.Text)

instance Core.ToHeaders ListBucketMetricsConfigurations where
        toHeaders ListBucketMetricsConfigurations{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest ListBucketMetricsConfigurations where
        type Rs ListBucketMetricsConfigurations =
             ListBucketMetricsConfigurationsResponse
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
                 ListBucketMetricsConfigurationsResponse' Core.<$>
                   (x Core..@? "ContinuationToken") Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "MetricsConfiguration"
                     Core.<*> x Core..@? "NextContinuationToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListBucketMetricsConfigurationsResponse' smart constructor.
data ListBucketMetricsConfigurationsResponse = ListBucketMetricsConfigurationsResponse'
  { continuationToken :: Core.Maybe Types.ContinuationToken
    -- ^ The marker that is used as a starting point for this metrics configuration list response. This value is present if it was sent in the request.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ Indicates whether the returned list of metrics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
  , metricsConfigurationList :: Core.Maybe [Types.MetricsConfiguration]
    -- ^ The list of metrics configurations for a bucket.
  , nextContinuationToken :: Core.Maybe Types.NextContinuationToken
    -- ^ The marker used to continue a metrics configuration listing that has been truncated. Use the @NextContinuationToken@ from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBucketMetricsConfigurationsResponse' value with any optional fields omitted.
mkListBucketMetricsConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBucketMetricsConfigurationsResponse
mkListBucketMetricsConfigurationsResponse responseStatus
  = ListBucketMetricsConfigurationsResponse'{continuationToken =
                                               Core.Nothing,
                                             isTruncated = Core.Nothing,
                                             metricsConfigurationList = Core.Nothing,
                                             nextContinuationToken = Core.Nothing, responseStatus}

-- | The marker that is used as a starting point for this metrics configuration list response. This value is present if it was sent in the request.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrrsContinuationToken :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Core.Maybe Types.ContinuationToken)
lbmcrrsContinuationToken = Lens.field @"continuationToken"
{-# INLINEABLE lbmcrrsContinuationToken #-}
{-# DEPRECATED continuationToken "Use generic-lens or generic-optics with 'continuationToken' instead"  #-}

-- | Indicates whether the returned list of metrics configurations is complete. A value of true indicates that the list is not complete and the NextContinuationToken will be provided for a subsequent request.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrrsIsTruncated :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Core.Maybe Core.Bool)
lbmcrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lbmcrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | The list of metrics configurations for a bucket.
--
-- /Note:/ Consider using 'metricsConfigurationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrrsMetricsConfigurationList :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Core.Maybe [Types.MetricsConfiguration])
lbmcrrsMetricsConfigurationList = Lens.field @"metricsConfigurationList"
{-# INLINEABLE lbmcrrsMetricsConfigurationList #-}
{-# DEPRECATED metricsConfigurationList "Use generic-lens or generic-optics with 'metricsConfigurationList' instead"  #-}

-- | The marker used to continue a metrics configuration listing that has been truncated. Use the @NextContinuationToken@ from a previously truncated list response to continue the listing. The continuation token is an opaque value that Amazon S3 understands.
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrrsNextContinuationToken :: Lens.Lens' ListBucketMetricsConfigurationsResponse (Core.Maybe Types.NextContinuationToken)
lbmcrrsNextContinuationToken = Lens.field @"nextContinuationToken"
{-# INLINEABLE lbmcrrsNextContinuationToken #-}
{-# DEPRECATED nextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmcrrsResponseStatus :: Lens.Lens' ListBucketMetricsConfigurationsResponse Core.Int
lbmcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbmcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

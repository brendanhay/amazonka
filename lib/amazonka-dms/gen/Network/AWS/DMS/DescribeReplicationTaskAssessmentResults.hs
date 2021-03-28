{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the task assessment results from Amazon S3. This action always returns the latest results.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
    (
    -- * Creating a request
      DescribeReplicationTaskAssessmentResults (..)
    , mkDescribeReplicationTaskAssessmentResults
    -- ** Request lenses
    , dMarker
    , dMaxRecords
    , dReplicationTaskArn

    -- * Destructuring the response
    , DescribeReplicationTaskAssessmentResultsResponse (..)
    , mkDescribeReplicationTaskAssessmentResultsResponse
    -- ** Response lenses
    , drtarrgrsBucketName
    , drtarrgrsMarker
    , drtarrgrsReplicationTaskAssessmentResults
    , drtarrgrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeReplicationTaskAssessmentResults' smart constructor.
data DescribeReplicationTaskAssessmentResults = DescribeReplicationTaskAssessmentResults'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , replicationTaskArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) string that uniquely identifies the task. When this input parameter is specified, the API returns only one result and ignore the values of the @MaxRecords@ and @Marker@ parameters. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationTaskAssessmentResults' value with any optional fields omitted.
mkDescribeReplicationTaskAssessmentResults
    :: DescribeReplicationTaskAssessmentResults
mkDescribeReplicationTaskAssessmentResults
  = DescribeReplicationTaskAssessmentResults'{marker = Core.Nothing,
                                              maxRecords = Core.Nothing,
                                              replicationTaskArn = Core.Nothing}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Core.Maybe Core.Text)
dMarker = Lens.field @"marker"
{-# INLINEABLE dMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Core.Maybe Core.Int)
dMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the task. When this input parameter is specified, the API returns only one result and ignore the values of the @MaxRecords@ and @Marker@ parameters. 
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReplicationTaskArn :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Core.Maybe Core.Text)
dReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE dReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

instance Core.ToQuery DescribeReplicationTaskAssessmentResults
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeReplicationTaskAssessmentResults
         where
        toHeaders DescribeReplicationTaskAssessmentResults{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.DescribeReplicationTaskAssessmentResults")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeReplicationTaskAssessmentResults
         where
        toJSON DescribeReplicationTaskAssessmentResults{..}
          = Core.object
              (Core.catMaybes
                 [("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords,
                  ("ReplicationTaskArn" Core..=) Core.<$> replicationTaskArn])

instance Core.AWSRequest DescribeReplicationTaskAssessmentResults
         where
        type Rs DescribeReplicationTaskAssessmentResults =
             DescribeReplicationTaskAssessmentResultsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReplicationTaskAssessmentResultsResponse' Core.<$>
                   (x Core..:? "BucketName") Core.<*> x Core..:? "Marker" Core.<*>
                     x Core..:? "ReplicationTaskAssessmentResults"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeReplicationTaskAssessmentResults
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"replicationTaskAssessmentResults" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeReplicationTaskAssessmentResultsResponse' smart constructor.
data DescribeReplicationTaskAssessmentResultsResponse = DescribeReplicationTaskAssessmentResultsResponse'
  { bucketName :: Core.Maybe Core.Text
    -- ^ - The Amazon S3 bucket where the task assessment report is located. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , replicationTaskAssessmentResults :: Core.Maybe [Types.ReplicationTaskAssessmentResult]
    -- ^ The task assessment report. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReplicationTaskAssessmentResultsResponse' value with any optional fields omitted.
mkDescribeReplicationTaskAssessmentResultsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReplicationTaskAssessmentResultsResponse
mkDescribeReplicationTaskAssessmentResultsResponse responseStatus
  = DescribeReplicationTaskAssessmentResultsResponse'{bucketName =
                                                        Core.Nothing,
                                                      marker = Core.Nothing,
                                                      replicationTaskAssessmentResults =
                                                        Core.Nothing,
                                                      responseStatus}

-- | - The Amazon S3 bucket where the task assessment report is located. 
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrgrsBucketName :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Core.Maybe Core.Text)
drtarrgrsBucketName = Lens.field @"bucketName"
{-# INLINEABLE drtarrgrsBucketName #-}
{-# DEPRECATED bucketName "Use generic-lens or generic-optics with 'bucketName' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrgrsMarker :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Core.Maybe Core.Text)
drtarrgrsMarker = Lens.field @"marker"
{-# INLINEABLE drtarrgrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The task assessment report. 
--
-- /Note:/ Consider using 'replicationTaskAssessmentResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrgrsReplicationTaskAssessmentResults :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Core.Maybe [Types.ReplicationTaskAssessmentResult])
drtarrgrsReplicationTaskAssessmentResults = Lens.field @"replicationTaskAssessmentResults"
{-# INLINEABLE drtarrgrsReplicationTaskAssessmentResults #-}
{-# DEPRECATED replicationTaskAssessmentResults "Use generic-lens or generic-optics with 'replicationTaskAssessmentResults' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrgrsResponseStatus :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse Core.Int
drtarrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drtarrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

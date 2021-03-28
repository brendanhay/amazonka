{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of premigration assessment runs based on filter settings.
--
-- These filter settings can specify a combination of premigration assessment runs, migration tasks, replication instances, and assessment run status values.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
    (
    -- * Creating a request
      DescribeReplicationTaskAssessmentRuns (..)
    , mkDescribeReplicationTaskAssessmentRuns
    -- ** Request lenses
    , drtarFilters
    , drtarMarker
    , drtarMaxRecords

    -- * Destructuring the response
    , DescribeReplicationTaskAssessmentRunsResponse (..)
    , mkDescribeReplicationTaskAssessmentRunsResponse
    -- ** Response lenses
    , drtarrfrsMarker
    , drtarrfrsReplicationTaskAssessmentRuns
    , drtarrfrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeReplicationTaskAssessmentRuns' smart constructor.
data DescribeReplicationTaskAssessmentRuns = DescribeReplicationTaskAssessmentRuns'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to the premigration assessment runs described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @replication-instance-arn@ , @status@ 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationTaskAssessmentRuns' value with any optional fields omitted.
mkDescribeReplicationTaskAssessmentRuns
    :: DescribeReplicationTaskAssessmentRuns
mkDescribeReplicationTaskAssessmentRuns
  = DescribeReplicationTaskAssessmentRuns'{filters = Core.Nothing,
                                           marker = Core.Nothing, maxRecords = Core.Nothing}

-- | Filters applied to the premigration assessment runs described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @replication-instance-arn@ , @status@ 
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarFilters :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Core.Maybe [Types.Filter])
drtarFilters = Lens.field @"filters"
{-# INLINEABLE drtarFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarMarker :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Core.Maybe Core.Text)
drtarMarker = Lens.field @"marker"
{-# INLINEABLE drtarMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarMaxRecords :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Core.Maybe Core.Int)
drtarMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE drtarMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeReplicationTaskAssessmentRuns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeReplicationTaskAssessmentRuns where
        toHeaders DescribeReplicationTaskAssessmentRuns{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.DescribeReplicationTaskAssessmentRuns")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeReplicationTaskAssessmentRuns where
        toJSON DescribeReplicationTaskAssessmentRuns{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeReplicationTaskAssessmentRuns
         where
        type Rs DescribeReplicationTaskAssessmentRuns =
             DescribeReplicationTaskAssessmentRunsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReplicationTaskAssessmentRunsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*>
                     x Core..:? "ReplicationTaskAssessmentRuns"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDescribeReplicationTaskAssessmentRunsResponse' smart constructor.
data DescribeReplicationTaskAssessmentRunsResponse = DescribeReplicationTaskAssessmentRunsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
  , replicationTaskAssessmentRuns :: Core.Maybe [Types.ReplicationTaskAssessmentRun]
    -- ^ One or more premigration assessment runs as specified by @Filters@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReplicationTaskAssessmentRunsResponse' value with any optional fields omitted.
mkDescribeReplicationTaskAssessmentRunsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReplicationTaskAssessmentRunsResponse
mkDescribeReplicationTaskAssessmentRunsResponse responseStatus
  = DescribeReplicationTaskAssessmentRunsResponse'{marker =
                                                     Core.Nothing,
                                                   replicationTaskAssessmentRuns = Core.Nothing,
                                                   responseStatus}

-- | A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrfrsMarker :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Core.Maybe Core.Text)
drtarrfrsMarker = Lens.field @"marker"
{-# INLINEABLE drtarrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | One or more premigration assessment runs as specified by @Filters@ .
--
-- /Note:/ Consider using 'replicationTaskAssessmentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrfrsReplicationTaskAssessmentRuns :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Core.Maybe [Types.ReplicationTaskAssessmentRun])
drtarrfrsReplicationTaskAssessmentRuns = Lens.field @"replicationTaskAssessmentRuns"
{-# INLINEABLE drtarrfrsReplicationTaskAssessmentRuns #-}
{-# DEPRECATED replicationTaskAssessmentRuns "Use generic-lens or generic-optics with 'replicationTaskAssessmentRuns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrfrsResponseStatus :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse Core.Int
drtarrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drtarrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

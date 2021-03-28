{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of individual assessments based on filter settings.
--
-- These filter settings can specify a combination of premigration assessment runs, migration tasks, and assessment status values.
module Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
    (
    -- * Creating a request
      DescribeReplicationTaskIndividualAssessments (..)
    , mkDescribeReplicationTaskIndividualAssessments
    -- ** Request lenses
    , drtiaFilters
    , drtiaMarker
    , drtiaMaxRecords

    -- * Destructuring the response
    , DescribeReplicationTaskIndividualAssessmentsResponse (..)
    , mkDescribeReplicationTaskIndividualAssessmentsResponse
    -- ** Response lenses
    , drtiarrsMarker
    , drtiarrsReplicationTaskIndividualAssessments
    , drtiarrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeReplicationTaskIndividualAssessments' smart constructor.
data DescribeReplicationTaskIndividualAssessments = DescribeReplicationTaskIndividualAssessments'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to the individual assessments described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @status@ 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationTaskIndividualAssessments' value with any optional fields omitted.
mkDescribeReplicationTaskIndividualAssessments
    :: DescribeReplicationTaskIndividualAssessments
mkDescribeReplicationTaskIndividualAssessments
  = DescribeReplicationTaskIndividualAssessments'{filters =
                                                    Core.Nothing,
                                                  marker = Core.Nothing, maxRecords = Core.Nothing}

-- | Filters applied to the individual assessments described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @status@ 
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiaFilters :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Core.Maybe [Types.Filter])
drtiaFilters = Lens.field @"filters"
{-# INLINEABLE drtiaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiaMarker :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Core.Maybe Core.Text)
drtiaMarker = Lens.field @"marker"
{-# INLINEABLE drtiaMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiaMaxRecords :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Core.Maybe Core.Int)
drtiaMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE drtiaMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeReplicationTaskIndividualAssessments
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           DescribeReplicationTaskIndividualAssessments
         where
        toHeaders DescribeReplicationTaskIndividualAssessments{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.DescribeReplicationTaskIndividualAssessments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeReplicationTaskIndividualAssessments
         where
        toJSON DescribeReplicationTaskIndividualAssessments{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest
           DescribeReplicationTaskIndividualAssessments
         where
        type Rs DescribeReplicationTaskIndividualAssessments =
             DescribeReplicationTaskIndividualAssessmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReplicationTaskIndividualAssessmentsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*>
                     x Core..:? "ReplicationTaskIndividualAssessments"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDescribeReplicationTaskIndividualAssessmentsResponse' smart constructor.
data DescribeReplicationTaskIndividualAssessmentsResponse = DescribeReplicationTaskIndividualAssessmentsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
  , replicationTaskIndividualAssessments :: Core.Maybe [Types.ReplicationTaskIndividualAssessment]
    -- ^ One or more individual assessments as specified by @Filters@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReplicationTaskIndividualAssessmentsResponse' value with any optional fields omitted.
mkDescribeReplicationTaskIndividualAssessmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReplicationTaskIndividualAssessmentsResponse
mkDescribeReplicationTaskIndividualAssessmentsResponse
  responseStatus
  = DescribeReplicationTaskIndividualAssessmentsResponse'{marker =
                                                            Core.Nothing,
                                                          replicationTaskIndividualAssessments =
                                                            Core.Nothing,
                                                          responseStatus}

-- | A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiarrsMarker :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Core.Maybe Core.Text)
drtiarrsMarker = Lens.field @"marker"
{-# INLINEABLE drtiarrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | One or more individual assessments as specified by @Filters@ .
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiarrsReplicationTaskIndividualAssessments :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Core.Maybe [Types.ReplicationTaskIndividualAssessment])
drtiarrsReplicationTaskIndividualAssessments = Lens.field @"replicationTaskIndividualAssessments"
{-# INLINEABLE drtiarrsReplicationTaskIndividualAssessments #-}
{-# DEPRECATED replicationTaskIndividualAssessments "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessments' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiarrsResponseStatus :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse Core.Int
drtiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drtiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

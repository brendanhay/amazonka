{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists events for a given source identifier and source type. You can also specify a start and end time. For more information on AWS DMS events, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications> in the /AWS Database Migration User Guide./ 
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEvents
    (
    -- * Creating a request
      DescribeEvents (..)
    , mkDescribeEvents
    -- ** Request lenses
    , deDuration
    , deEndTime
    , deEventCategories
    , deFilters
    , deMarker
    , deMaxRecords
    , deSourceIdentifier
    , deSourceType
    , deStartTime

    -- * Destructuring the response
    , DescribeEventsResponse (..)
    , mkDescribeEventsResponse
    -- ** Response lenses
    , dergrsEvents
    , dergrsMarker
    , dergrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { duration :: Core.Maybe Core.Int
    -- ^ The duration of the events to be listed.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end time for the events to be listed.
  , eventCategories :: Core.Maybe [Core.Text]
    -- ^ A list of event categories for the source type that you've chosen.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to events.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , sourceIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of an event source.
  , sourceType :: Core.Maybe Types.SourceType
    -- ^ The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start time for the events to be listed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEvents' value with any optional fields omitted.
mkDescribeEvents
    :: DescribeEvents
mkDescribeEvents
  = DescribeEvents'{duration = Core.Nothing, endTime = Core.Nothing,
                    eventCategories = Core.Nothing, filters = Core.Nothing,
                    marker = Core.Nothing, maxRecords = Core.Nothing,
                    sourceIdentifier = Core.Nothing, sourceType = Core.Nothing,
                    startTime = Core.Nothing}

-- | The duration of the events to be listed.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDuration :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
deDuration = Lens.field @"duration"
{-# INLINEABLE deDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The end time for the events to be listed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.NominalDiffTime)
deEndTime = Lens.field @"endTime"
{-# INLINEABLE deEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | A list of event categories for the source type that you've chosen.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEventCategories :: Lens.Lens' DescribeEvents (Core.Maybe [Core.Text])
deEventCategories = Lens.field @"eventCategories"
{-# INLINEABLE deEventCategories #-}
{-# DEPRECATED eventCategories "Use generic-lens or generic-optics with 'eventCategories' instead"  #-}

-- | Filters applied to events.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deFilters :: Lens.Lens' DescribeEvents (Core.Maybe [Types.Filter])
deFilters = Lens.field @"filters"
{-# INLINEABLE deFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMarker :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
deMarker = Lens.field @"marker"
{-# INLINEABLE deMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
deMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE deMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The identifier of an event source.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceIdentifier :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
deSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE deSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceType :: Lens.Lens' DescribeEvents (Core.Maybe Types.SourceType)
deSourceType = Lens.field @"sourceType"
{-# INLINEABLE deSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | The start time for the events to be listed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.NominalDiffTime)
deStartTime = Lens.field @"startTime"
{-# INLINEABLE deStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery DescribeEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEvents where
        toHeaders DescribeEvents{..}
          = Core.pure ("X-Amz-Target", "AmazonDMSv20160101.DescribeEvents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEvents where
        toJSON DescribeEvents{..}
          = Core.object
              (Core.catMaybes
                 [("Duration" Core..=) Core.<$> duration,
                  ("EndTime" Core..=) Core.<$> endTime,
                  ("EventCategories" Core..=) Core.<$> eventCategories,
                  ("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords,
                  ("SourceIdentifier" Core..=) Core.<$> sourceIdentifier,
                  ("SourceType" Core..=) Core.<$> sourceType,
                  ("StartTime" Core..=) Core.<$> startTime])

instance Core.AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventsResponse' Core.<$>
                   (x Core..:? "Events") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEvents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"events" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { events :: Core.Maybe [Types.Event]
    -- ^ The events described.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventsResponse' value with any optional fields omitted.
mkDescribeEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventsResponse
mkDescribeEventsResponse responseStatus
  = DescribeEventsResponse'{events = Core.Nothing,
                            marker = Core.Nothing, responseStatus}

-- | The events described.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsEvents :: Lens.Lens' DescribeEventsResponse (Core.Maybe [Types.Event])
dergrsEvents = Lens.field @"events"
{-# INLINEABLE dergrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsMarker :: Lens.Lens' DescribeEventsResponse (Core.Maybe Core.Text)
dergrsMarker = Lens.field @"marker"
{-# INLINEABLE dergrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsResponseStatus :: Lens.Lens' DescribeEventsResponse Core.Int
dergrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dergrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

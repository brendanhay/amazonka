{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to clusters, cache security groups, and cache parameter groups. You can obtain events specific to a particular cluster, cache security group, or cache parameter group by providing the name as a parameter.
--
-- By default, only the events occurring within the last hour are returned; however, you can retrieve up to 14 days' worth of events if necessary.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeEvents
    (
    -- * Creating a request
      DescribeEvents (..)
    , mkDescribeEvents
    -- ** Request lenses
    , deDuration
    , deEndTime
    , deMarker
    , deMaxRecords
    , deSourceIdentifier
    , deSourceType
    , deStartTime

    -- * Destructuring the response
    , DescribeEventsResponse (..)
    , mkDescribeEventsResponse
    -- ** Response lenses
    , derrsEvents
    , derrsMarker
    , derrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeEvents@ operation.
--
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { duration :: Core.Maybe Core.Int
    -- ^ The number of minutes worth of events to retrieve.
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The end of the time interval for which to retrieve events, specified in ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
  , sourceIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the event source for which events are returned. If not specified, all sources are included in the response.
  , sourceType :: Core.Maybe Types.SourceType
    -- ^ The event source to retrieve events for. If no value is specified, all events are returned.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The beginning of the time interval to retrieve events for, specified in ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEvents' value with any optional fields omitted.
mkDescribeEvents
    :: DescribeEvents
mkDescribeEvents
  = DescribeEvents'{duration = Core.Nothing, endTime = Core.Nothing,
                    marker = Core.Nothing, maxRecords = Core.Nothing,
                    sourceIdentifier = Core.Nothing, sourceType = Core.Nothing,
                    startTime = Core.Nothing}

-- | The number of minutes worth of events to retrieve.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDuration :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
deDuration = Lens.field @"duration"
{-# INLINEABLE deDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
deEndTime = Lens.field @"endTime"
{-# INLINEABLE deEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMarker :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
deMarker = Lens.field @"marker"
{-# INLINEABLE deMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
deMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE deMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The identifier of the event source for which events are returned. If not specified, all sources are included in the response.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceIdentifier :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
deSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE deSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

-- | The event source to retrieve events for. If no value is specified, all events are returned.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceType :: Lens.Lens' DescribeEvents (Core.Maybe Types.SourceType)
deSourceType = Lens.field @"sourceType"
{-# INLINEABLE deSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
deStartTime = Lens.field @"startTime"
{-# INLINEABLE deStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery DescribeEvents where
        toQuery DescribeEvents{..}
          = Core.toQueryPair "Action" ("DescribeEvents" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Duration") duration
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "EndTime") endTime
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceIdentifier")
                sourceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceType") sourceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StartTime") startTime

instance Core.ToHeaders DescribeEvents where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeEventsResult"
              (\ s h x ->
                 DescribeEventsResponse' Core.<$>
                   (x Core..@? "Events" Core..<@> Core.parseXMLList "Event") Core.<*>
                     x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
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

-- | Represents the output of a @DescribeEvents@ operation.
--
-- /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { events :: Core.Maybe [Types.Event]
    -- ^ A list of events. Each element in the list contains detailed information about one event.
  , marker :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
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

-- | A list of events. Each element in the list contains detailed information about one event.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEvents :: Lens.Lens' DescribeEventsResponse (Core.Maybe [Types.Event])
derrsEvents = Lens.field @"events"
{-# INLINEABLE derrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsMarker :: Lens.Lens' DescribeEventsResponse (Core.Maybe Core.Text)
derrsMarker = Lens.field @"marker"
{-# INLINEABLE derrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeEventsResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

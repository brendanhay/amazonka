{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to clusters, security groups, snapshots, and parameter groups for the past 14 days. Events specific to a particular cluster, security group, snapshot or parameter group can be obtained by providing the name as a parameter. By default, the past hour of events are returned.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeEvents
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { duration :: Core.Maybe Core.Int
    -- ^ The number of minutes prior to the time of the request for which to retrieve events. For example, if the request is sent at 18:00 and you specify a duration of 60, then only events which have occurred after 17:00 will be returned.
--
-- Default: @60@ 
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> 
--
-- Example: @2009-07-08T18:00Z@ 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeEvents' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
  , sourceIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the event source for which events will be returned. If this parameter is not specified, then all sources are included in the response.
--
-- Constraints:
-- If /SourceIdentifier/ is supplied, /SourceType/ must also be provided.
--
--     * Specify a cluster identifier when /SourceType/ is @cluster@ .
--
--
--     * Specify a cluster security group name when /SourceType/ is @cluster-security-group@ .
--
--
--     * Specify a cluster parameter group name when /SourceType/ is @cluster-parameter-group@ .
--
--
--     * Specify a cluster snapshot identifier when /SourceType/ is @cluster-snapshot@ .
--
--
  , sourceType :: Core.Maybe Types.SourceType
    -- ^ The event source to retrieve events for. If no value is specified, all events are returned.
--
-- Constraints:
-- If /SourceType/ is supplied, /SourceIdentifier/ must also be provided.
--
--     * Specify @cluster@ when /SourceIdentifier/ is a cluster identifier.
--
--
--     * Specify @cluster-security-group@ when /SourceIdentifier/ is a cluster security group name.
--
--
--     * Specify @cluster-parameter-group@ when /SourceIdentifier/ is a cluster parameter group name.
--
--
--     * Specify @cluster-snapshot@ when /SourceIdentifier/ is a cluster snapshot identifier.
--
--
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> 
--
-- Example: @2009-07-08T18:00Z@ 
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

-- | The number of minutes prior to the time of the request for which to retrieve events. For example, if the request is sent at 18:00 and you specify a duration of 60, then only events which have occurred after 17:00 will be returned.
--
-- Default: @60@ 
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDuration :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
deDuration = Lens.field @"duration"
{-# INLINEABLE deDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> 
--
-- Example: @2009-07-08T18:00Z@ 
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
deEndTime = Lens.field @"endTime"
{-# INLINEABLE deEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeEvents' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMarker :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
deMarker = Lens.field @"marker"
{-# INLINEABLE deMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
deMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE deMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The identifier of the event source for which events will be returned. If this parameter is not specified, then all sources are included in the response.
--
-- Constraints:
-- If /SourceIdentifier/ is supplied, /SourceType/ must also be provided.
--
--     * Specify a cluster identifier when /SourceType/ is @cluster@ .
--
--
--     * Specify a cluster security group name when /SourceType/ is @cluster-security-group@ .
--
--
--     * Specify a cluster parameter group name when /SourceType/ is @cluster-parameter-group@ .
--
--
--     * Specify a cluster snapshot identifier when /SourceType/ is @cluster-snapshot@ .
--
--
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceIdentifier :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
deSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE deSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

-- | The event source to retrieve events for. If no value is specified, all events are returned.
--
-- Constraints:
-- If /SourceType/ is supplied, /SourceIdentifier/ must also be provided.
--
--     * Specify @cluster@ when /SourceIdentifier/ is a cluster identifier.
--
--
--     * Specify @cluster-security-group@ when /SourceIdentifier/ is a cluster security group name.
--
--
--     * Specify @cluster-parameter-group@ when /SourceIdentifier/ is a cluster parameter group name.
--
--
--     * Specify @cluster-snapshot@ when /SourceIdentifier/ is a cluster snapshot identifier.
--
--
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceType :: Lens.Lens' DescribeEvents (Core.Maybe Types.SourceType)
deSourceType = Lens.field @"sourceType"
{-# INLINEABLE deSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> 
--
-- Example: @2009-07-08T18:00Z@ 
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
deStartTime = Lens.field @"startTime"
{-# INLINEABLE deStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery DescribeEvents where
        toQuery DescribeEvents{..}
          = Core.toQueryPair "Action" ("DescribeEvents" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
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

-- | 
--
-- /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { events :: Core.Maybe [Types.Event]
    -- ^ A list of @Event@ instances. 
  , marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
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

-- | A list of @Event@ instances. 
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEvents :: Lens.Lens' DescribeEventsResponse (Core.Maybe [Types.Event])
derrsEvents = Lens.field @"events"
{-# INLINEABLE derrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
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

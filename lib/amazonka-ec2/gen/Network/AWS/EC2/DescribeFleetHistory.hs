{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFleetHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified EC2 Fleet during the specified time.
--
-- EC2 Fleet events are delayed by up to 30 seconds before they can be described. This ensures that you can query by the last evaluated time and not miss a recorded event. EC2 Fleet events are available for 48 hours.
module Network.AWS.EC2.DescribeFleetHistory
    (
    -- * Creating a request
      DescribeFleetHistory (..)
    , mkDescribeFleetHistory
    -- ** Request lenses
    , dfhFleetId
    , dfhStartTime
    , dfhDryRun
    , dfhEventType
    , dfhMaxResults
    , dfhNextToken

    -- * Destructuring the response
    , DescribeFleetHistoryResponse (..)
    , mkDescribeFleetHistoryResponse
    -- ** Response lenses
    , dfhrrsFleetId
    , dfhrrsHistoryRecords
    , dfhrrsLastEvaluatedTime
    , dfhrrsNextToken
    , dfhrrsStartTime
    , dfhrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeFleetHistory' smart constructor.
data DescribeFleetHistory = DescribeFleetHistory'
  { fleetId :: Types.FleetId
    -- ^ The ID of the EC2 Fleet.
  , startTime :: Core.UTCTime
    -- ^ The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , eventType :: Core.Maybe Types.FleetEventType
    -- ^ The type of events to describe. By default, all events are described.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeFleetHistory' value with any optional fields omitted.
mkDescribeFleetHistory
    :: Types.FleetId -- ^ 'fleetId'
    -> Core.UTCTime -- ^ 'startTime'
    -> DescribeFleetHistory
mkDescribeFleetHistory fleetId startTime
  = DescribeFleetHistory'{fleetId, startTime, dryRun = Core.Nothing,
                          eventType = Core.Nothing, maxResults = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhFleetId :: Lens.Lens' DescribeFleetHistory Types.FleetId
dfhFleetId = Lens.field @"fleetId"
{-# INLINEABLE dfhFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhStartTime :: Lens.Lens' DescribeFleetHistory Core.UTCTime
dfhStartTime = Lens.field @"startTime"
{-# INLINEABLE dfhStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhDryRun :: Lens.Lens' DescribeFleetHistory (Core.Maybe Core.Bool)
dfhDryRun = Lens.field @"dryRun"
{-# INLINEABLE dfhDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The type of events to describe. By default, all events are described.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhEventType :: Lens.Lens' DescribeFleetHistory (Core.Maybe Types.FleetEventType)
dfhEventType = Lens.field @"eventType"
{-# INLINEABLE dfhEventType #-}
{-# DEPRECATED eventType "Use generic-lens or generic-optics with 'eventType' instead"  #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhMaxResults :: Lens.Lens' DescribeFleetHistory (Core.Maybe Core.Int)
dfhMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dfhMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhNextToken :: Lens.Lens' DescribeFleetHistory (Core.Maybe Core.Text)
dfhNextToken = Lens.field @"nextToken"
{-# INLINEABLE dfhNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeFleetHistory where
        toQuery DescribeFleetHistory{..}
          = Core.toQueryPair "Action" ("DescribeFleetHistory" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "FleetId" fleetId
              Core.<> Core.toQueryPair "StartTime" startTime
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EventType") eventType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeFleetHistory where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeFleetHistory where
        type Rs DescribeFleetHistory = DescribeFleetHistoryResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DescribeFleetHistoryResponse' Core.<$>
                   (x Core..@? "fleetId") Core.<*>
                     x Core..@? "historyRecordSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "lastEvaluatedTime"
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> x Core..@? "startTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeFleetHistoryResponse' smart constructor.
data DescribeFleetHistoryResponse = DescribeFleetHistoryResponse'
  { fleetId :: Core.Maybe Types.FleetId
    -- ^ The ID of the EC Fleet.
  , historyRecords :: Core.Maybe [Types.HistoryRecordEntry]
    -- ^ Information about the events in the history of the EC2 Fleet.
  , lastEvaluatedTime :: Core.Maybe Core.UTCTime
    -- ^ The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not present.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeFleetHistoryResponse' value with any optional fields omitted.
mkDescribeFleetHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeFleetHistoryResponse
mkDescribeFleetHistoryResponse responseStatus
  = DescribeFleetHistoryResponse'{fleetId = Core.Nothing,
                                  historyRecords = Core.Nothing, lastEvaluatedTime = Core.Nothing,
                                  nextToken = Core.Nothing, startTime = Core.Nothing,
                                  responseStatus}

-- | The ID of the EC Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrrsFleetId :: Lens.Lens' DescribeFleetHistoryResponse (Core.Maybe Types.FleetId)
dfhrrsFleetId = Lens.field @"fleetId"
{-# INLINEABLE dfhrrsFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | Information about the events in the history of the EC2 Fleet.
--
-- /Note:/ Consider using 'historyRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrrsHistoryRecords :: Lens.Lens' DescribeFleetHistoryResponse (Core.Maybe [Types.HistoryRecordEntry])
dfhrrsHistoryRecords = Lens.field @"historyRecords"
{-# INLINEABLE dfhrrsHistoryRecords #-}
{-# DEPRECATED historyRecords "Use generic-lens or generic-optics with 'historyRecords' instead"  #-}

-- | The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not present.
--
-- /Note:/ Consider using 'lastEvaluatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrrsLastEvaluatedTime :: Lens.Lens' DescribeFleetHistoryResponse (Core.Maybe Core.UTCTime)
dfhrrsLastEvaluatedTime = Lens.field @"lastEvaluatedTime"
{-# INLINEABLE dfhrrsLastEvaluatedTime #-}
{-# DEPRECATED lastEvaluatedTime "Use generic-lens or generic-optics with 'lastEvaluatedTime' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrrsNextToken :: Lens.Lens' DescribeFleetHistoryResponse (Core.Maybe Core.Text)
dfhrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dfhrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrrsStartTime :: Lens.Lens' DescribeFleetHistoryResponse (Core.Maybe Core.UTCTime)
dfhrrsStartTime = Lens.field @"startTime"
{-# INLINEABLE dfhrrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrrsResponseStatus :: Lens.Lens' DescribeFleetHistoryResponse Core.Int
dfhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

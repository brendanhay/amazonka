{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequestHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified Spot Fleet request during the specified time.
--
-- Spot Fleet events are delayed by up to 30 seconds before they can be described. This ensures that you can query by the last evaluated time and not miss a recorded event. Spot Fleet events are available for 48 hours.
module Network.AWS.EC2.DescribeSpotFleetRequestHistory
  ( -- * Creating a request
    DescribeSpotFleetRequestHistory (..),
    mkDescribeSpotFleetRequestHistory,

    -- ** Request lenses
    dsfrhSpotFleetRequestId,
    dsfrhStartTime,
    dsfrhDryRun,
    dsfrhEventType,
    dsfrhMaxResults,
    dsfrhNextToken,

    -- * Destructuring the response
    DescribeSpotFleetRequestHistoryResponse (..),
    mkDescribeSpotFleetRequestHistoryResponse,

    -- ** Response lenses
    dsfrhrrsHistoryRecords,
    dsfrhrrsLastEvaluatedTime,
    dsfrhrrsNextToken,
    dsfrhrrsSpotFleetRequestId,
    dsfrhrrsStartTime,
    dsfrhrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotFleetRequestHistory.
--
-- /See:/ 'mkDescribeSpotFleetRequestHistory' smart constructor.
data DescribeSpotFleetRequestHistory = DescribeSpotFleetRequestHistory'
  { -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Types.SpotFleetRequestId,
    -- | The starting date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    startTime :: Core.UTCTime,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The type of events to describe. By default, all events are described.
    eventType :: Core.Maybe Types.EventType,
    -- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSpotFleetRequestHistory' value with any optional fields omitted.
mkDescribeSpotFleetRequestHistory ::
  -- | 'spotFleetRequestId'
  Types.SpotFleetRequestId ->
  -- | 'startTime'
  Core.UTCTime ->
  DescribeSpotFleetRequestHistory
mkDescribeSpotFleetRequestHistory spotFleetRequestId startTime =
  DescribeSpotFleetRequestHistory'
    { spotFleetRequestId,
      startTime,
      dryRun = Core.Nothing,
      eventType = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetRequestHistory Types.SpotFleetRequestId
dsfrhSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED dsfrhSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The starting date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhStartTime :: Lens.Lens' DescribeSpotFleetRequestHistory Core.UTCTime
dsfrhStartTime = Lens.field @"startTime"
{-# DEPRECATED dsfrhStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhDryRun :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe Core.Bool)
dsfrhDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsfrhDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The type of events to describe. By default, all events are described.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhEventType :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe Types.EventType)
dsfrhEventType = Lens.field @"eventType"
{-# DEPRECATED dsfrhEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhMaxResults :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe Core.Natural)
dsfrhMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsfrhMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhNextToken :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe Types.String)
dsfrhNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfrhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeSpotFleetRequestHistory where
  type
    Rs DescribeSpotFleetRequestHistory =
      DescribeSpotFleetRequestHistoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeSpotFleetRequestHistory")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "SpotFleetRequestId" spotFleetRequestId)
                Core.<> (Core.toQueryValue "StartTime" startTime)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "EventType" Core.<$> eventType)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotFleetRequestHistoryResponse'
            Core.<$> (x Core..@? "historyRecordSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "lastEvaluatedTime")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "spotFleetRequestId")
            Core.<*> (x Core..@? "startTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DescribeSpotFleetRequestHistory.
--
-- /See:/ 'mkDescribeSpotFleetRequestHistoryResponse' smart constructor.
data DescribeSpotFleetRequestHistoryResponse = DescribeSpotFleetRequestHistoryResponse'
  { -- | Information about the events in the history of the Spot Fleet request.
    historyRecords :: Core.Maybe [Types.HistoryRecord],
    -- | The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved.
    --
    -- If @nextToken@ indicates that there are more results, this value is not present.
    lastEvaluatedTime :: Core.Maybe Core.UTCTime,
    -- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Types.String,
    -- | The starting date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    startTime :: Core.Maybe Core.UTCTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSpotFleetRequestHistoryResponse' value with any optional fields omitted.
mkDescribeSpotFleetRequestHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSpotFleetRequestHistoryResponse
mkDescribeSpotFleetRequestHistoryResponse responseStatus =
  DescribeSpotFleetRequestHistoryResponse'
    { historyRecords =
        Core.Nothing,
      lastEvaluatedTime = Core.Nothing,
      nextToken = Core.Nothing,
      spotFleetRequestId = Core.Nothing,
      startTime = Core.Nothing,
      responseStatus
    }

-- | Information about the events in the history of the Spot Fleet request.
--
-- /Note:/ Consider using 'historyRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhrrsHistoryRecords :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe [Types.HistoryRecord])
dsfrhrrsHistoryRecords = Lens.field @"historyRecords"
{-# DEPRECATED dsfrhrrsHistoryRecords "Use generic-lens or generic-optics with 'historyRecords' instead." #-}

-- | The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not present.
--
-- /Note:/ Consider using 'lastEvaluatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhrrsLastEvaluatedTime :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Core.UTCTime)
dsfrhrrsLastEvaluatedTime = Lens.field @"lastEvaluatedTime"
{-# DEPRECATED dsfrhrrsLastEvaluatedTime "Use generic-lens or generic-optics with 'lastEvaluatedTime' instead." #-}

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhrrsNextToken :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Types.String)
dsfrhrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfrhrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhrrsSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Types.String)
dsfrhrrsSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED dsfrhrrsSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The starting date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhrrsStartTime :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Core.UTCTime)
dsfrhrrsStartTime = Lens.field @"startTime"
{-# DEPRECATED dsfrhrrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrhrrsResponseStatus :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse Core.Int
dsfrhrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsfrhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

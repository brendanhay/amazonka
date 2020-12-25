{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves entries from the specified fleet's event log. You can specify a time range to limit the result set. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a collection of event log entries matching the request are returned.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- __Related operations__
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'DescribeFleetPortSettings'
--
--
--     * 'DescribeFleetUtilization'
--
--
--     * 'DescribeRuntimeConfiguration'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * 'DescribeFleetEvents'
--
--
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetEvents
  ( -- * Creating a request
    DescribeFleetEvents (..),
    mkDescribeFleetEvents,

    -- ** Request lenses
    dfeFleetId,
    dfeEndTime,
    dfeLimit,
    dfeNextToken,
    dfeStartTime,

    -- * Destructuring the response
    DescribeFleetEventsResponse (..),
    mkDescribeFleetEventsResponse,

    -- ** Response lenses
    dferrsEvents,
    dferrsNextToken,
    dferrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeFleetEvents' smart constructor.
data DescribeFleetEvents = DescribeFleetEvents'
  { -- | A unique identifier for a fleet to get event logs for. You can use either the fleet ID or ARN value.
    fleetId :: Types.FleetIdOrArn,
    -- | Most recent date to retrieve event logs for. If no end time is specified, this call returns entries from the specified start time up to the present. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Earliest date to retrieve event logs for. If no start time is specified, this call returns entries starting from when the fleet was created to the specified end time. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeFleetEvents' value with any optional fields omitted.
mkDescribeFleetEvents ::
  -- | 'fleetId'
  Types.FleetIdOrArn ->
  DescribeFleetEvents
mkDescribeFleetEvents fleetId =
  DescribeFleetEvents'
    { fleetId,
      endTime = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing,
      startTime = Core.Nothing
    }

-- | A unique identifier for a fleet to get event logs for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeFleetId :: Lens.Lens' DescribeFleetEvents Types.FleetIdOrArn
dfeFleetId = Lens.field @"fleetId"
{-# DEPRECATED dfeFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Most recent date to retrieve event logs for. If no end time is specified, this call returns entries from the specified start time up to the present. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeEndTime :: Lens.Lens' DescribeFleetEvents (Core.Maybe Core.NominalDiffTime)
dfeEndTime = Lens.field @"endTime"
{-# DEPRECATED dfeEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeLimit :: Lens.Lens' DescribeFleetEvents (Core.Maybe Core.Natural)
dfeLimit = Lens.field @"limit"
{-# DEPRECATED dfeLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeNextToken :: Lens.Lens' DescribeFleetEvents (Core.Maybe Types.NonZeroAndMaxString)
dfeNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Earliest date to retrieve event logs for. If no start time is specified, this call returns entries starting from when the fleet was created to the specified end time. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeStartTime :: Lens.Lens' DescribeFleetEvents (Core.Maybe Core.NominalDiffTime)
dfeStartTime = Lens.field @"startTime"
{-# DEPRECATED dfeStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON DescribeFleetEvents where
  toJSON DescribeFleetEvents {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            ("EndTime" Core..=) Core.<$> endTime,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StartTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest DescribeFleetEvents where
  type Rs DescribeFleetEvents = DescribeFleetEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeFleetEvents")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetEventsResponse'
            Core.<$> (x Core..:? "Events")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeFleetEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"events" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetEventsResponse' smart constructor.
data DescribeFleetEventsResponse = DescribeFleetEventsResponse'
  { -- | A collection of objects containing event log entries for the specified fleet.
    events :: Core.Maybe [Types.Event],
    -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeFleetEventsResponse' value with any optional fields omitted.
mkDescribeFleetEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeFleetEventsResponse
mkDescribeFleetEventsResponse responseStatus =
  DescribeFleetEventsResponse'
    { events = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A collection of objects containing event log entries for the specified fleet.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dferrsEvents :: Lens.Lens' DescribeFleetEventsResponse (Core.Maybe [Types.Event])
dferrsEvents = Lens.field @"events"
{-# DEPRECATED dferrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dferrsNextToken :: Lens.Lens' DescribeFleetEventsResponse (Core.Maybe Types.NonZeroAndMaxString)
dferrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dferrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dferrsResponseStatus :: Lens.Lens' DescribeFleetEventsResponse Core.Int
dferrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dferrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current capacity statistics for one or more fleets. These statistics present a snapshot of the fleet's instances and provide insight on current or imminent scaling activity. To get statistics on game hosting activity in the fleet, see 'DescribeFleetUtilization' .
--
-- You can request capacity for all fleets or specify a list of one or more fleet identifiers. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetCapacity' object is returned for each requested fleet ID. When a list of fleet IDs is provided, attribute objects are returned only for fleets that currently exist.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html#gamelift-metrics-fleet GameLift Metrics for Fleets>
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
module Network.AWS.GameLift.DescribeFleetCapacity
  ( -- * Creating a request
    DescribeFleetCapacity (..),
    mkDescribeFleetCapacity,

    -- ** Request lenses
    dfcFleetIds,
    dfcLimit,
    dfcNextToken,

    -- * Destructuring the response
    DescribeFleetCapacityResponse (..),
    mkDescribeFleetCapacityResponse,

    -- ** Response lenses
    dfcrrsFleetCapacity,
    dfcrrsNextToken,
    dfcrrsResponseStatus,
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
-- /See:/ 'mkDescribeFleetCapacity' smart constructor.
data DescribeFleetCapacity = DescribeFleetCapacity'
  { -- | A unique identifier for a fleet(s) to retrieve capacity information for. You can use either the fleet ID or ARN value.
    fleetIds :: Core.Maybe (Core.NonEmpty Types.FleetIdOrArn),
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
    limit :: Core.Maybe Core.Natural,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleetCapacity' value with any optional fields omitted.
mkDescribeFleetCapacity ::
  DescribeFleetCapacity
mkDescribeFleetCapacity =
  DescribeFleetCapacity'
    { fleetIds = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A unique identifier for a fleet(s) to retrieve capacity information for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcFleetIds :: Lens.Lens' DescribeFleetCapacity (Core.Maybe (Core.NonEmpty Types.FleetIdOrArn))
dfcFleetIds = Lens.field @"fleetIds"
{-# DEPRECATED dfcFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcLimit :: Lens.Lens' DescribeFleetCapacity (Core.Maybe Core.Natural)
dfcLimit = Lens.field @"limit"
{-# DEPRECATED dfcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcNextToken :: Lens.Lens' DescribeFleetCapacity (Core.Maybe Types.NonZeroAndMaxString)
dfcNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeFleetCapacity where
  toJSON DescribeFleetCapacity {..} =
    Core.object
      ( Core.catMaybes
          [ ("FleetIds" Core..=) Core.<$> fleetIds,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeFleetCapacity where
  type Rs DescribeFleetCapacity = DescribeFleetCapacityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeFleetCapacity")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetCapacityResponse'
            Core.<$> (x Core..:? "FleetCapacity")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeFleetCapacity where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"fleetCapacity" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetCapacityResponse' smart constructor.
data DescribeFleetCapacityResponse = DescribeFleetCapacityResponse'
  { -- | A collection of objects containing capacity information for each requested fleet ID. Leave this parameter empty to retrieve capacity information for all fleets.
    fleetCapacity :: Core.Maybe [Types.FleetCapacity],
    -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleetCapacityResponse' value with any optional fields omitted.
mkDescribeFleetCapacityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeFleetCapacityResponse
mkDescribeFleetCapacityResponse responseStatus =
  DescribeFleetCapacityResponse'
    { fleetCapacity = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A collection of objects containing capacity information for each requested fleet ID. Leave this parameter empty to retrieve capacity information for all fleets.
--
-- /Note:/ Consider using 'fleetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrrsFleetCapacity :: Lens.Lens' DescribeFleetCapacityResponse (Core.Maybe [Types.FleetCapacity])
dfcrrsFleetCapacity = Lens.field @"fleetCapacity"
{-# DEPRECATED dfcrrsFleetCapacity "Use generic-lens or generic-optics with 'fleetCapacity' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrrsNextToken :: Lens.Lens' DescribeFleetCapacityResponse (Core.Maybe Types.NonZeroAndMaxString)
dfcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcrrsResponseStatus :: Lens.Lens' DescribeFleetCapacityResponse Core.Int
dfcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

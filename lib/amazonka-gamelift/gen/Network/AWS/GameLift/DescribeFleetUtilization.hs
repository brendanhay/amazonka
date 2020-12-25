{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves utilization statistics for one or more fleets. These statistics provide insight into how available hosting resources are currently being used. To get statistics on available hosting resources, see 'DescribeFleetCapacity' .
--
-- You can request utilization data for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetUtilization' object is returned for each requested fleet ID, unless the fleet identifier is not found.
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
module Network.AWS.GameLift.DescribeFleetUtilization
  ( -- * Creating a request
    DescribeFleetUtilization (..),
    mkDescribeFleetUtilization,

    -- ** Request lenses
    dfuFleetIds,
    dfuLimit,
    dfuNextToken,

    -- * Destructuring the response
    DescribeFleetUtilizationResponse (..),
    mkDescribeFleetUtilizationResponse,

    -- ** Response lenses
    dfurrsFleetUtilization,
    dfurrsNextToken,
    dfurrsResponseStatus,
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
-- /See:/ 'mkDescribeFleetUtilization' smart constructor.
data DescribeFleetUtilization = DescribeFleetUtilization'
  { -- | A unique identifier for a fleet(s) to retrieve utilization data for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
    fleetIds :: Core.Maybe (Core.NonEmpty Types.FleetIdOrArn),
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
    limit :: Core.Maybe Core.Natural,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleetUtilization' value with any optional fields omitted.
mkDescribeFleetUtilization ::
  DescribeFleetUtilization
mkDescribeFleetUtilization =
  DescribeFleetUtilization'
    { fleetIds = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A unique identifier for a fleet(s) to retrieve utilization data for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfuFleetIds :: Lens.Lens' DescribeFleetUtilization (Core.Maybe (Core.NonEmpty Types.FleetIdOrArn))
dfuFleetIds = Lens.field @"fleetIds"
{-# DEPRECATED dfuFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfuLimit :: Lens.Lens' DescribeFleetUtilization (Core.Maybe Core.Natural)
dfuLimit = Lens.field @"limit"
{-# DEPRECATED dfuLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfuNextToken :: Lens.Lens' DescribeFleetUtilization (Core.Maybe Types.NextToken)
dfuNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeFleetUtilization where
  toJSON DescribeFleetUtilization {..} =
    Core.object
      ( Core.catMaybes
          [ ("FleetIds" Core..=) Core.<$> fleetIds,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeFleetUtilization where
  type Rs DescribeFleetUtilization = DescribeFleetUtilizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeFleetUtilization")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetUtilizationResponse'
            Core.<$> (x Core..:? "FleetUtilization")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeFleetUtilization where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"fleetUtilization" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetUtilizationResponse' smart constructor.
data DescribeFleetUtilizationResponse = DescribeFleetUtilizationResponse'
  { -- | A collection of objects containing utilization information for each requested fleet ID.
    fleetUtilization :: Core.Maybe [Types.FleetUtilization],
    -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleetUtilizationResponse' value with any optional fields omitted.
mkDescribeFleetUtilizationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeFleetUtilizationResponse
mkDescribeFleetUtilizationResponse responseStatus =
  DescribeFleetUtilizationResponse'
    { fleetUtilization =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A collection of objects containing utilization information for each requested fleet ID.
--
-- /Note:/ Consider using 'fleetUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfurrsFleetUtilization :: Lens.Lens' DescribeFleetUtilizationResponse (Core.Maybe [Types.FleetUtilization])
dfurrsFleetUtilization = Lens.field @"fleetUtilization"
{-# DEPRECATED dfurrsFleetUtilization "Use generic-lens or generic-optics with 'fleetUtilization' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfurrsNextToken :: Lens.Lens' DescribeFleetUtilizationResponse (Core.Maybe Types.NonZeroAndMaxString)
dfurrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dfurrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfurrsResponseStatus :: Lens.Lens' DescribeFleetUtilizationResponse Core.Int
dfurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

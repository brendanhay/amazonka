{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeScalingPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all scaling policies applied to a fleet.
--
-- To get a fleet's scaling policies, specify the fleet ID. You can filter this request by policy status, such as to retrieve only active scaling policies. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, set of 'ScalingPolicy' objects is returned for the fleet.
-- A fleet may have all of its scaling policies suspended ('StopFleetActions' ). This operation does not affect the status of the scaling policies, which remains ACTIVE. To see whether a fleet's scaling policies are in force or suspended, call 'DescribeFleetAttributes' and check the stopped actions.
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--
--     * 'StopFleetActions'
--
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeScalingPolicies
  ( -- * Creating a request
    DescribeScalingPolicies (..),
    mkDescribeScalingPolicies,

    -- ** Request lenses
    dFleetId,
    dLimit,
    dNextToken,
    dStatusFilter,

    -- * Destructuring the response
    DescribeScalingPoliciesResponse (..),
    mkDescribeScalingPoliciesResponse,

    -- ** Response lenses
    dsprrsNextToken,
    dsprrsScalingPolicies,
    dsprrsResponseStatus,
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
-- /See:/ 'mkDescribeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
  { -- | A unique identifier for a fleet to retrieve scaling policies for. You can use either the fleet ID or ARN value.
    fleetId :: Types.FleetIdOrArn,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Scaling policy status to filter results on. A scaling policy is only in force when in an @ACTIVE@ status.
    --
    --
    --     * __ACTIVE__ -- The scaling policy is currently in force.
    --
    --
    --     * __UPDATEREQUESTED__ -- A request to update the scaling policy has been received.
    --
    --
    --     * __UPDATING__ -- A change is being made to the scaling policy.
    --
    --
    --     * __DELETEREQUESTED__ -- A request to delete the scaling policy has been received.
    --
    --
    --     * __DELETING__ -- The scaling policy is being deleted.
    --
    --
    --     * __DELETED__ -- The scaling policy has been deleted.
    --
    --
    --     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
    statusFilter :: Core.Maybe Types.ScalingStatusType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingPolicies' value with any optional fields omitted.
mkDescribeScalingPolicies ::
  -- | 'fleetId'
  Types.FleetIdOrArn ->
  DescribeScalingPolicies
mkDescribeScalingPolicies fleetId =
  DescribeScalingPolicies'
    { fleetId,
      limit = Core.Nothing,
      nextToken = Core.Nothing,
      statusFilter = Core.Nothing
    }

-- | A unique identifier for a fleet to retrieve scaling policies for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetId :: Lens.Lens' DescribeScalingPolicies Types.FleetIdOrArn
dFleetId = Lens.field @"fleetId"
{-# DEPRECATED dFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLimit :: Lens.Lens' DescribeScalingPolicies (Core.Maybe Core.Natural)
dLimit = Lens.field @"limit"
{-# DEPRECATED dLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeScalingPolicies (Core.Maybe Types.NonZeroAndMaxString)
dNextToken = Lens.field @"nextToken"
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Scaling policy status to filter results on. A scaling policy is only in force when in an @ACTIVE@ status.
--
--
--     * __ACTIVE__ -- The scaling policy is currently in force.
--
--
--     * __UPDATEREQUESTED__ -- A request to update the scaling policy has been received.
--
--
--     * __UPDATING__ -- A change is being made to the scaling policy.
--
--
--     * __DELETEREQUESTED__ -- A request to delete the scaling policy has been received.
--
--
--     * __DELETING__ -- The scaling policy is being deleted.
--
--
--     * __DELETED__ -- The scaling policy has been deleted.
--
--
--     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
--
--
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatusFilter :: Lens.Lens' DescribeScalingPolicies (Core.Maybe Types.ScalingStatusType)
dStatusFilter = Lens.field @"statusFilter"
{-# DEPRECATED dStatusFilter "Use generic-lens or generic-optics with 'statusFilter' instead." #-}

instance Core.FromJSON DescribeScalingPolicies where
  toJSON DescribeScalingPolicies {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StatusFilter" Core..=) Core.<$> statusFilter
          ]
      )

instance Core.AWSRequest DescribeScalingPolicies where
  type Rs DescribeScalingPolicies = DescribeScalingPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeScalingPolicies")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScalingPoliciesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ScalingPolicies")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeScalingPolicies where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"scalingPolicies" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
  { -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | A collection of objects containing the scaling policies matching the request.
    scalingPolicies :: Core.Maybe [Types.ScalingPolicy],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingPoliciesResponse' value with any optional fields omitted.
mkDescribeScalingPoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScalingPoliciesResponse
mkDescribeScalingPoliciesResponse responseStatus =
  DescribeScalingPoliciesResponse'
    { nextToken = Core.Nothing,
      scalingPolicies = Core.Nothing,
      responseStatus
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsNextToken :: Lens.Lens' DescribeScalingPoliciesResponse (Core.Maybe Types.NonZeroAndMaxString)
dsprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing the scaling policies matching the request.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsScalingPolicies :: Lens.Lens' DescribeScalingPoliciesResponse (Core.Maybe [Types.ScalingPolicy])
dsprrsScalingPolicies = Lens.field @"scalingPolicies"
{-# DEPRECATED dsprrsScalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsResponseStatus :: Lens.Lens' DescribeScalingPoliciesResponse Core.Int
dsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

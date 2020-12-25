{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateFleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates capacity settings for a fleet. Use this operation to specify the number of EC2 instances (hosts) that you want this fleet to contain. Before calling this operation, you may want to call 'DescribeEC2InstanceLimits' to get the maximum capacity based on the fleet's EC2 instance type.
--
-- Specify minimum and maximum number of instances. Amazon GameLift will not change fleet capacity to values fall outside of this range. This is particularly important when using auto-scaling (see 'PutScalingPolicy' ) to allow capacity to adjust based on player demand while imposing limits on automatic adjustments.
-- To update fleet capacity, specify the fleet ID and the number of instances you want the fleet to host. If successful, Amazon GameLift starts or terminates instances so that the fleet's active instance count matches the desired instance count. You can view a fleet's current capacity information by calling 'DescribeFleetCapacity' . If the desired instance count is higher than the instance type's limit, the "Limit Exceeded" exception occurs.
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
--     * 'DescribeFleetAttributes'
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'UpdateFleetPortSettings'
--
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.UpdateFleetCapacity
  ( -- * Creating a request
    UpdateFleetCapacity (..),
    mkUpdateFleetCapacity,

    -- ** Request lenses
    ufcFleetId,
    ufcDesiredInstances,
    ufcMaxSize,
    ufcMinSize,

    -- * Destructuring the response
    UpdateFleetCapacityResponse (..),
    mkUpdateFleetCapacityResponse,

    -- ** Response lenses
    ufcrrsFleetId,
    ufcrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateFleetCapacity' smart constructor.
data UpdateFleetCapacity = UpdateFleetCapacity'
  { -- | A unique identifier for a fleet to update capacity for. You can use either the fleet ID or ARN value.
    fleetId :: Types.FleetIdOrArn,
    -- | Number of EC2 instances you want this fleet to host.
    desiredInstances :: Core.Maybe Core.Natural,
    -- | The maximum value allowed for the fleet's instance count. Default if not set is 1.
    maxSize :: Core.Maybe Core.Natural,
    -- | The minimum value allowed for the fleet's instance count. Default if not set is 0.
    minSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFleetCapacity' value with any optional fields omitted.
mkUpdateFleetCapacity ::
  -- | 'fleetId'
  Types.FleetIdOrArn ->
  UpdateFleetCapacity
mkUpdateFleetCapacity fleetId =
  UpdateFleetCapacity'
    { fleetId,
      desiredInstances = Core.Nothing,
      maxSize = Core.Nothing,
      minSize = Core.Nothing
    }

-- | A unique identifier for a fleet to update capacity for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcFleetId :: Lens.Lens' UpdateFleetCapacity Types.FleetIdOrArn
ufcFleetId = Lens.field @"fleetId"
{-# DEPRECATED ufcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Number of EC2 instances you want this fleet to host.
--
-- /Note:/ Consider using 'desiredInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcDesiredInstances :: Lens.Lens' UpdateFleetCapacity (Core.Maybe Core.Natural)
ufcDesiredInstances = Lens.field @"desiredInstances"
{-# DEPRECATED ufcDesiredInstances "Use generic-lens or generic-optics with 'desiredInstances' instead." #-}

-- | The maximum value allowed for the fleet's instance count. Default if not set is 1.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcMaxSize :: Lens.Lens' UpdateFleetCapacity (Core.Maybe Core.Natural)
ufcMaxSize = Lens.field @"maxSize"
{-# DEPRECATED ufcMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The minimum value allowed for the fleet's instance count. Default if not set is 0.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcMinSize :: Lens.Lens' UpdateFleetCapacity (Core.Maybe Core.Natural)
ufcMinSize = Lens.field @"minSize"
{-# DEPRECATED ufcMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

instance Core.FromJSON UpdateFleetCapacity where
  toJSON UpdateFleetCapacity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            ("DesiredInstances" Core..=) Core.<$> desiredInstances,
            ("MaxSize" Core..=) Core.<$> maxSize,
            ("MinSize" Core..=) Core.<$> minSize
          ]
      )

instance Core.AWSRequest UpdateFleetCapacity where
  type Rs UpdateFleetCapacity = UpdateFleetCapacityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.UpdateFleetCapacity")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetCapacityResponse'
            Core.<$> (x Core..:? "FleetId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateFleetCapacityResponse' smart constructor.
data UpdateFleetCapacityResponse = UpdateFleetCapacityResponse'
  { -- | A unique identifier for a fleet that was updated.
    fleetId :: Core.Maybe Types.FleetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFleetCapacityResponse' value with any optional fields omitted.
mkUpdateFleetCapacityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateFleetCapacityResponse
mkUpdateFleetCapacityResponse responseStatus =
  UpdateFleetCapacityResponse'
    { fleetId = Core.Nothing,
      responseStatus
    }

-- | A unique identifier for a fleet that was updated.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcrrsFleetId :: Lens.Lens' UpdateFleetCapacityResponse (Core.Maybe Types.FleetId)
ufcrrsFleetId = Lens.field @"fleetId"
{-# DEPRECATED ufcrrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufcrrsResponseStatus :: Lens.Lens' UpdateFleetCapacityResponse Core.Int
ufcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ufcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

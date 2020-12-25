{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateFleetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates fleet properties, including name and description, for a fleet. To update metadata, specify the fleet ID and the property values that you want to change. If successful, the fleet ID for the updated fleet is returned.
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
module Network.AWS.GameLift.UpdateFleetAttributes
  ( -- * Creating a request
    UpdateFleetAttributes (..),
    mkUpdateFleetAttributes,

    -- ** Request lenses
    ufaFleetId,
    ufaDescription,
    ufaMetricGroups,
    ufaName,
    ufaNewGameSessionProtectionPolicy,
    ufaResourceCreationLimitPolicy,

    -- * Destructuring the response
    UpdateFleetAttributesResponse (..),
    mkUpdateFleetAttributesResponse,

    -- ** Response lenses
    ufarrsFleetId,
    ufarrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateFleetAttributes' smart constructor.
data UpdateFleetAttributes = UpdateFleetAttributes'
  { -- | A unique identifier for a fleet to update attribute metadata for. You can use either the fleet ID or ARN value.
    fleetId :: Types.FleetIdOrArn,
    -- | Human-readable description of a fleet.
    description :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Names of metric groups to include this fleet in. Amazon CloudWatch uses a fleet metric group is to aggregate metrics from multiple fleets. Use an existing metric group name to add this fleet to the group. Or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
    metricGroups :: Core.Maybe [Types.MetricGroup],
    -- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
    name :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Game session protection policy to apply to all new instances created in this fleet. Instances that already exist are not affected. You can set protection for individual instances using 'UpdateGameSession' .
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy :: Core.Maybe Types.ProtectionPolicy,
    -- | Policy that limits the number of game sessions an individual player can create over a span of time.
    resourceCreationLimitPolicy :: Core.Maybe Types.ResourceCreationLimitPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFleetAttributes' value with any optional fields omitted.
mkUpdateFleetAttributes ::
  -- | 'fleetId'
  Types.FleetIdOrArn ->
  UpdateFleetAttributes
mkUpdateFleetAttributes fleetId =
  UpdateFleetAttributes'
    { fleetId,
      description = Core.Nothing,
      metricGroups = Core.Nothing,
      name = Core.Nothing,
      newGameSessionProtectionPolicy = Core.Nothing,
      resourceCreationLimitPolicy = Core.Nothing
    }

-- | A unique identifier for a fleet to update attribute metadata for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaFleetId :: Lens.Lens' UpdateFleetAttributes Types.FleetIdOrArn
ufaFleetId = Lens.field @"fleetId"
{-# DEPRECATED ufaFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Human-readable description of a fleet.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaDescription :: Lens.Lens' UpdateFleetAttributes (Core.Maybe Types.NonZeroAndMaxString)
ufaDescription = Lens.field @"description"
{-# DEPRECATED ufaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Names of metric groups to include this fleet in. Amazon CloudWatch uses a fleet metric group is to aggregate metrics from multiple fleets. Use an existing metric group name to add this fleet to the group. Or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
--
-- /Note:/ Consider using 'metricGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaMetricGroups :: Lens.Lens' UpdateFleetAttributes (Core.Maybe [Types.MetricGroup])
ufaMetricGroups = Lens.field @"metricGroups"
{-# DEPRECATED ufaMetricGroups "Use generic-lens or generic-optics with 'metricGroups' instead." #-}

-- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaName :: Lens.Lens' UpdateFleetAttributes (Core.Maybe Types.NonZeroAndMaxString)
ufaName = Lens.field @"name"
{-# DEPRECATED ufaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Game session protection policy to apply to all new instances created in this fleet. Instances that already exist are not affected. You can set protection for individual instances using 'UpdateGameSession' .
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
--
-- /Note:/ Consider using 'newGameSessionProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaNewGameSessionProtectionPolicy :: Lens.Lens' UpdateFleetAttributes (Core.Maybe Types.ProtectionPolicy)
ufaNewGameSessionProtectionPolicy = Lens.field @"newGameSessionProtectionPolicy"
{-# DEPRECATED ufaNewGameSessionProtectionPolicy "Use generic-lens or generic-optics with 'newGameSessionProtectionPolicy' instead." #-}

-- | Policy that limits the number of game sessions an individual player can create over a span of time.
--
-- /Note:/ Consider using 'resourceCreationLimitPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufaResourceCreationLimitPolicy :: Lens.Lens' UpdateFleetAttributes (Core.Maybe Types.ResourceCreationLimitPolicy)
ufaResourceCreationLimitPolicy = Lens.field @"resourceCreationLimitPolicy"
{-# DEPRECATED ufaResourceCreationLimitPolicy "Use generic-lens or generic-optics with 'resourceCreationLimitPolicy' instead." #-}

instance Core.FromJSON UpdateFleetAttributes where
  toJSON UpdateFleetAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            ("Description" Core..=) Core.<$> description,
            ("MetricGroups" Core..=) Core.<$> metricGroups,
            ("Name" Core..=) Core.<$> name,
            ("NewGameSessionProtectionPolicy" Core..=)
              Core.<$> newGameSessionProtectionPolicy,
            ("ResourceCreationLimitPolicy" Core..=)
              Core.<$> resourceCreationLimitPolicy
          ]
      )

instance Core.AWSRequest UpdateFleetAttributes where
  type Rs UpdateFleetAttributes = UpdateFleetAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.UpdateFleetAttributes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetAttributesResponse'
            Core.<$> (x Core..:? "FleetId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateFleetAttributesResponse' smart constructor.
data UpdateFleetAttributesResponse = UpdateFleetAttributesResponse'
  { -- | A unique identifier for a fleet that was updated. Use either the fleet ID or ARN value.
    fleetId :: Core.Maybe Types.FleetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFleetAttributesResponse' value with any optional fields omitted.
mkUpdateFleetAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateFleetAttributesResponse
mkUpdateFleetAttributesResponse responseStatus =
  UpdateFleetAttributesResponse'
    { fleetId = Core.Nothing,
      responseStatus
    }

-- | A unique identifier for a fleet that was updated. Use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufarrsFleetId :: Lens.Lens' UpdateFleetAttributesResponse (Core.Maybe Types.FleetId)
ufarrsFleetId = Lens.field @"fleetId"
{-# DEPRECATED ufarrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufarrsResponseStatus :: Lens.Lens' UpdateFleetAttributesResponse Core.Int
ufarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ufarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

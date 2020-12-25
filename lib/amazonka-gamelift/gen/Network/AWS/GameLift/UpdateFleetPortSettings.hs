{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateFleetPortSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates port settings for a fleet. To update settings, specify the fleet ID to be updated and list the permissions you want to update. List the permissions you want to add in @InboundPermissionAuthorizations@ , and permissions you want to remove in @InboundPermissionRevocations@ . Permissions to be removed must match existing fleet permissions. If successful, the fleet ID for the updated fleet is returned.
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
module Network.AWS.GameLift.UpdateFleetPortSettings
  ( -- * Creating a request
    UpdateFleetPortSettings (..),
    mkUpdateFleetPortSettings,

    -- ** Request lenses
    ufpsFleetId,
    ufpsInboundPermissionAuthorizations,
    ufpsInboundPermissionRevocations,

    -- * Destructuring the response
    UpdateFleetPortSettingsResponse (..),
    mkUpdateFleetPortSettingsResponse,

    -- ** Response lenses
    ufpsrrsFleetId,
    ufpsrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateFleetPortSettings' smart constructor.
data UpdateFleetPortSettings = UpdateFleetPortSettings'
  { -- | A unique identifier for a fleet to update port settings for. You can use either the fleet ID or ARN value.
    fleetId :: Types.FleetIdOrArn,
    -- | A collection of port settings to be added to the fleet resource.
    inboundPermissionAuthorizations :: Core.Maybe [Types.IpPermission],
    -- | A collection of port settings to be removed from the fleet resource.
    inboundPermissionRevocations :: Core.Maybe [Types.IpPermission]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFleetPortSettings' value with any optional fields omitted.
mkUpdateFleetPortSettings ::
  -- | 'fleetId'
  Types.FleetIdOrArn ->
  UpdateFleetPortSettings
mkUpdateFleetPortSettings fleetId =
  UpdateFleetPortSettings'
    { fleetId,
      inboundPermissionAuthorizations = Core.Nothing,
      inboundPermissionRevocations = Core.Nothing
    }

-- | A unique identifier for a fleet to update port settings for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsFleetId :: Lens.Lens' UpdateFleetPortSettings Types.FleetIdOrArn
ufpsFleetId = Lens.field @"fleetId"
{-# DEPRECATED ufpsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | A collection of port settings to be added to the fleet resource.
--
-- /Note:/ Consider using 'inboundPermissionAuthorizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsInboundPermissionAuthorizations :: Lens.Lens' UpdateFleetPortSettings (Core.Maybe [Types.IpPermission])
ufpsInboundPermissionAuthorizations = Lens.field @"inboundPermissionAuthorizations"
{-# DEPRECATED ufpsInboundPermissionAuthorizations "Use generic-lens or generic-optics with 'inboundPermissionAuthorizations' instead." #-}

-- | A collection of port settings to be removed from the fleet resource.
--
-- /Note:/ Consider using 'inboundPermissionRevocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsInboundPermissionRevocations :: Lens.Lens' UpdateFleetPortSettings (Core.Maybe [Types.IpPermission])
ufpsInboundPermissionRevocations = Lens.field @"inboundPermissionRevocations"
{-# DEPRECATED ufpsInboundPermissionRevocations "Use generic-lens or generic-optics with 'inboundPermissionRevocations' instead." #-}

instance Core.FromJSON UpdateFleetPortSettings where
  toJSON UpdateFleetPortSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            ("InboundPermissionAuthorizations" Core..=)
              Core.<$> inboundPermissionAuthorizations,
            ("InboundPermissionRevocations" Core..=)
              Core.<$> inboundPermissionRevocations
          ]
      )

instance Core.AWSRequest UpdateFleetPortSettings where
  type Rs UpdateFleetPortSettings = UpdateFleetPortSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.UpdateFleetPortSettings")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetPortSettingsResponse'
            Core.<$> (x Core..:? "FleetId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateFleetPortSettingsResponse' smart constructor.
data UpdateFleetPortSettingsResponse = UpdateFleetPortSettingsResponse'
  { -- | A unique identifier for a fleet that was updated.
    fleetId :: Core.Maybe Types.FleetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFleetPortSettingsResponse' value with any optional fields omitted.
mkUpdateFleetPortSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateFleetPortSettingsResponse
mkUpdateFleetPortSettingsResponse responseStatus =
  UpdateFleetPortSettingsResponse'
    { fleetId = Core.Nothing,
      responseStatus
    }

-- | A unique identifier for a fleet that was updated.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsrrsFleetId :: Lens.Lens' UpdateFleetPortSettingsResponse (Core.Maybe Types.FleetId)
ufpsrrsFleetId = Lens.field @"fleetId"
{-# DEPRECATED ufpsrrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufpsrrsResponseStatus :: Lens.Lens' UpdateFleetPortSettingsResponse Core.Int
ufpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ufpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

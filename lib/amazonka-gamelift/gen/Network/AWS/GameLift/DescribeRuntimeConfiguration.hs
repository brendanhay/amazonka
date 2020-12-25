{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fleet's runtime configuration settings. The runtime configuration tells Amazon GameLift which server processes to run (and how) on each instance in the fleet.
--
-- To get a runtime configuration, specify the fleet's unique identifier. If successful, a 'RuntimeConfiguration' object is returned for the requested fleet. If the requested fleet has been deleted, the result set is empty.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-multiprocess.html Running Multiple Processes on a Fleet>
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
module Network.AWS.GameLift.DescribeRuntimeConfiguration
  ( -- * Creating a request
    DescribeRuntimeConfiguration (..),
    mkDescribeRuntimeConfiguration,

    -- ** Request lenses
    drcFleetId,

    -- * Destructuring the response
    DescribeRuntimeConfigurationResponse (..),
    mkDescribeRuntimeConfigurationResponse,

    -- ** Response lenses
    drcrrsRuntimeConfiguration,
    drcrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeRuntimeConfiguration' smart constructor.
newtype DescribeRuntimeConfiguration = DescribeRuntimeConfiguration'
  { -- | A unique identifier for a fleet to get the runtime configuration for. You can use either the fleet ID or ARN value.
    fleetId :: Types.FleetIdOrArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRuntimeConfiguration' value with any optional fields omitted.
mkDescribeRuntimeConfiguration ::
  -- | 'fleetId'
  Types.FleetIdOrArn ->
  DescribeRuntimeConfiguration
mkDescribeRuntimeConfiguration fleetId =
  DescribeRuntimeConfiguration' {fleetId}

-- | A unique identifier for a fleet to get the runtime configuration for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcFleetId :: Lens.Lens' DescribeRuntimeConfiguration Types.FleetIdOrArn
drcFleetId = Lens.field @"fleetId"
{-# DEPRECATED drcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Core.FromJSON DescribeRuntimeConfiguration where
  toJSON DescribeRuntimeConfiguration {..} =
    Core.object
      (Core.catMaybes [Core.Just ("FleetId" Core..= fleetId)])

instance Core.AWSRequest DescribeRuntimeConfiguration where
  type
    Rs DescribeRuntimeConfiguration =
      DescribeRuntimeConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeRuntimeConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuntimeConfigurationResponse'
            Core.<$> (x Core..:? "RuntimeConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeRuntimeConfigurationResponse' smart constructor.
data DescribeRuntimeConfigurationResponse = DescribeRuntimeConfigurationResponse'
  { -- | Instructions describing how server processes should be launched and maintained on each instance in the fleet.
    runtimeConfiguration :: Core.Maybe Types.RuntimeConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRuntimeConfigurationResponse' value with any optional fields omitted.
mkDescribeRuntimeConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRuntimeConfigurationResponse
mkDescribeRuntimeConfigurationResponse responseStatus =
  DescribeRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | Instructions describing how server processes should be launched and maintained on each instance in the fleet.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsRuntimeConfiguration :: Lens.Lens' DescribeRuntimeConfigurationResponse (Core.Maybe Types.RuntimeConfiguration)
drcrrsRuntimeConfiguration = Lens.field @"runtimeConfiguration"
{-# DEPRECATED drcrrsRuntimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsResponseStatus :: Lens.Lens' DescribeRuntimeConfigurationResponse Core.Int
drcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

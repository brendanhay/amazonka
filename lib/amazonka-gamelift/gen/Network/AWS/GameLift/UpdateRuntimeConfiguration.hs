{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the current runtime configuration for the specified fleet, which tells Amazon GameLift how to launch server processes on instances in the fleet. You can update a fleet's runtime configuration at any time after the fleet is created; it does not need to be in an @ACTIVE@ status.
--
-- To update runtime configuration, specify the fleet ID and provide a @RuntimeConfiguration@ object with an updated set of server process configurations.
-- Each instance in a Amazon GameLift fleet checks regularly for an updated runtime configuration and changes how it launches server processes to comply with the latest version. Existing server processes are not affected by the update; runtime configuration changes are applied gradually as existing processes shut down and new processes are launched during Amazon GameLift's normal process recycling activity.
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
--
--
module Network.AWS.GameLift.UpdateRuntimeConfiguration
    (
    -- * Creating a request
      UpdateRuntimeConfiguration (..)
    , mkUpdateRuntimeConfiguration
    -- ** Request lenses
    , urcFleetId
    , urcRuntimeConfiguration

    -- * Destructuring the response
    , UpdateRuntimeConfigurationResponse (..)
    , mkUpdateRuntimeConfigurationResponse
    -- ** Response lenses
    , urcrrsRuntimeConfiguration
    , urcrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateRuntimeConfiguration' smart constructor.
data UpdateRuntimeConfiguration = UpdateRuntimeConfiguration'
  { fleetId :: Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to update runtime configuration for. You can use either the fleet ID or ARN value.
  , runtimeConfiguration :: Types.RuntimeConfiguration
    -- ^ Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime Servers script. The runtime configuration lists the types of server processes to run on an instance and includes the following configuration settings: the server executable or launch script file, launch parameters, and the number of processes to run concurrently on each instance. A CreateFleet request must include a runtime configuration with at least one server process configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRuntimeConfiguration' value with any optional fields omitted.
mkUpdateRuntimeConfiguration
    :: Types.FleetIdOrArn -- ^ 'fleetId'
    -> Types.RuntimeConfiguration -- ^ 'runtimeConfiguration'
    -> UpdateRuntimeConfiguration
mkUpdateRuntimeConfiguration fleetId runtimeConfiguration
  = UpdateRuntimeConfiguration'{fleetId, runtimeConfiguration}

-- | A unique identifier for a fleet to update runtime configuration for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcFleetId :: Lens.Lens' UpdateRuntimeConfiguration Types.FleetIdOrArn
urcFleetId = Lens.field @"fleetId"
{-# INLINEABLE urcFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime Servers script. The runtime configuration lists the types of server processes to run on an instance and includes the following configuration settings: the server executable or launch script file, launch parameters, and the number of processes to run concurrently on each instance. A CreateFleet request must include a runtime configuration with at least one server process configuration.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcRuntimeConfiguration :: Lens.Lens' UpdateRuntimeConfiguration Types.RuntimeConfiguration
urcRuntimeConfiguration = Lens.field @"runtimeConfiguration"
{-# INLINEABLE urcRuntimeConfiguration #-}
{-# DEPRECATED runtimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead"  #-}

instance Core.ToQuery UpdateRuntimeConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRuntimeConfiguration where
        toHeaders UpdateRuntimeConfiguration{..}
          = Core.pure ("X-Amz-Target", "GameLift.UpdateRuntimeConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRuntimeConfiguration where
        toJSON UpdateRuntimeConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetId" Core..= fleetId),
                  Core.Just ("RuntimeConfiguration" Core..= runtimeConfiguration)])

instance Core.AWSRequest UpdateRuntimeConfiguration where
        type Rs UpdateRuntimeConfiguration =
             UpdateRuntimeConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateRuntimeConfigurationResponse' Core.<$>
                   (x Core..:? "RuntimeConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateRuntimeConfigurationResponse' smart constructor.
data UpdateRuntimeConfigurationResponse = UpdateRuntimeConfigurationResponse'
  { runtimeConfiguration :: Core.Maybe Types.RuntimeConfiguration
    -- ^ The runtime configuration currently in force. If the update was successful, this object matches the one in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRuntimeConfigurationResponse' value with any optional fields omitted.
mkUpdateRuntimeConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRuntimeConfigurationResponse
mkUpdateRuntimeConfigurationResponse responseStatus
  = UpdateRuntimeConfigurationResponse'{runtimeConfiguration =
                                          Core.Nothing,
                                        responseStatus}

-- | The runtime configuration currently in force. If the update was successful, this object matches the one in the request.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrrsRuntimeConfiguration :: Lens.Lens' UpdateRuntimeConfigurationResponse (Core.Maybe Types.RuntimeConfiguration)
urcrrsRuntimeConfiguration = Lens.field @"runtimeConfiguration"
{-# INLINEABLE urcrrsRuntimeConfiguration #-}
{-# DEPRECATED runtimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrrsResponseStatus :: Lens.Lens' UpdateRuntimeConfigurationResponse Core.Int
urcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

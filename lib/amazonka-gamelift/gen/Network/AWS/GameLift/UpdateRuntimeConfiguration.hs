{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.GameLift.UpdateRuntimeConfiguration
  ( -- * Creating a request
    UpdateRuntimeConfiguration (..),
    mkUpdateRuntimeConfiguration,

    -- ** Request lenses
    urcRuntimeConfiguration,
    urcFleetId,

    -- * Destructuring the response
    UpdateRuntimeConfigurationResponse (..),
    mkUpdateRuntimeConfigurationResponse,

    -- ** Response lenses
    urcrsRuntimeConfiguration,
    urcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateRuntimeConfiguration' smart constructor.
data UpdateRuntimeConfiguration = UpdateRuntimeConfiguration'
  { -- | Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime Servers script. The runtime configuration lists the types of server processes to run on an instance and includes the following configuration settings: the server executable or launch script file, launch parameters, and the number of processes to run concurrently on each instance. A CreateFleet request must include a runtime configuration with at least one server process configuration.
    runtimeConfiguration :: RuntimeConfiguration,
    -- | A unique identifier for a fleet to update runtime configuration for. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRuntimeConfiguration' with the minimum fields required to make a request.
--
-- * 'runtimeConfiguration' - Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime Servers script. The runtime configuration lists the types of server processes to run on an instance and includes the following configuration settings: the server executable or launch script file, launch parameters, and the number of processes to run concurrently on each instance. A CreateFleet request must include a runtime configuration with at least one server process configuration.
-- * 'fleetId' - A unique identifier for a fleet to update runtime configuration for. You can use either the fleet ID or ARN value.
mkUpdateRuntimeConfiguration ::
  -- | 'runtimeConfiguration'
  RuntimeConfiguration ->
  -- | 'fleetId'
  Lude.Text ->
  UpdateRuntimeConfiguration
mkUpdateRuntimeConfiguration pRuntimeConfiguration_ pFleetId_ =
  UpdateRuntimeConfiguration'
    { runtimeConfiguration =
        pRuntimeConfiguration_,
      fleetId = pFleetId_
    }

-- | Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime Servers script. The runtime configuration lists the types of server processes to run on an instance and includes the following configuration settings: the server executable or launch script file, launch parameters, and the number of processes to run concurrently on each instance. A CreateFleet request must include a runtime configuration with at least one server process configuration.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcRuntimeConfiguration :: Lens.Lens' UpdateRuntimeConfiguration RuntimeConfiguration
urcRuntimeConfiguration = Lens.lens (runtimeConfiguration :: UpdateRuntimeConfiguration -> RuntimeConfiguration) (\s a -> s {runtimeConfiguration = a} :: UpdateRuntimeConfiguration)
{-# DEPRECATED urcRuntimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead." #-}

-- | A unique identifier for a fleet to update runtime configuration for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcFleetId :: Lens.Lens' UpdateRuntimeConfiguration Lude.Text
urcFleetId = Lens.lens (fleetId :: UpdateRuntimeConfiguration -> Lude.Text) (\s a -> s {fleetId = a} :: UpdateRuntimeConfiguration)
{-# DEPRECATED urcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest UpdateRuntimeConfiguration where
  type
    Rs UpdateRuntimeConfiguration =
      UpdateRuntimeConfigurationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRuntimeConfigurationResponse'
            Lude.<$> (x Lude..?> "RuntimeConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRuntimeConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateRuntimeConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRuntimeConfiguration where
  toJSON UpdateRuntimeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RuntimeConfiguration" Lude..= runtimeConfiguration),
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath UpdateRuntimeConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRuntimeConfiguration where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateRuntimeConfigurationResponse' smart constructor.
data UpdateRuntimeConfigurationResponse = UpdateRuntimeConfigurationResponse'
  { -- | The runtime configuration currently in force. If the update was successful, this object matches the one in the request.
    runtimeConfiguration :: Lude.Maybe RuntimeConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'runtimeConfiguration' - The runtime configuration currently in force. If the update was successful, this object matches the one in the request.
-- * 'responseStatus' - The response status code.
mkUpdateRuntimeConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRuntimeConfigurationResponse
mkUpdateRuntimeConfigurationResponse pResponseStatus_ =
  UpdateRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The runtime configuration currently in force. If the update was successful, this object matches the one in the request.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrsRuntimeConfiguration :: Lens.Lens' UpdateRuntimeConfigurationResponse (Lude.Maybe RuntimeConfiguration)
urcrsRuntimeConfiguration = Lens.lens (runtimeConfiguration :: UpdateRuntimeConfigurationResponse -> Lude.Maybe RuntimeConfiguration) (\s a -> s {runtimeConfiguration = a} :: UpdateRuntimeConfigurationResponse)
{-# DEPRECATED urcrsRuntimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrsResponseStatus :: Lens.Lens' UpdateRuntimeConfigurationResponse Lude.Int
urcrsResponseStatus = Lens.lens (responseStatus :: UpdateRuntimeConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRuntimeConfigurationResponse)
{-# DEPRECATED urcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

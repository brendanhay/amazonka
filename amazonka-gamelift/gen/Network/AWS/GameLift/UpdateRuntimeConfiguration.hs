{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateRuntimeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the current runtime configuration for the specified fleet, which
-- tells Amazon GameLift how to launch server processes on instances in the
-- fleet. You can update a fleet\'s runtime configuration at any time after
-- the fleet is created; it does not need to be in an @ACTIVE@ status.
--
-- To update runtime configuration, specify the fleet ID and provide a
-- @RuntimeConfiguration@ object with an updated set of server process
-- configurations.
--
-- Each instance in a Amazon GameLift fleet checks regularly for an updated
-- runtime configuration and changes how it launches server processes to
-- comply with the latest version. Existing server processes are not
-- affected by the update; runtime configuration changes are applied
-- gradually as existing processes shut down and new processes are launched
-- during Amazon GameLift\'s normal process recycling activity.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   Update fleets:
--
--     -   UpdateFleetAttributes
--
--     -   UpdateFleetCapacity
--
--     -   UpdateFleetPortSettings
--
--     -   UpdateRuntimeConfiguration
--
-- -   StartFleetActions or StopFleetActions
module Network.AWS.GameLift.UpdateRuntimeConfiguration
  ( -- * Creating a Request
    UpdateRuntimeConfiguration (..),
    newUpdateRuntimeConfiguration,

    -- * Request Lenses
    updateRuntimeConfiguration_fleetId,
    updateRuntimeConfiguration_runtimeConfiguration,

    -- * Destructuring the Response
    UpdateRuntimeConfigurationResponse (..),
    newUpdateRuntimeConfigurationResponse,

    -- * Response Lenses
    updateRuntimeConfigurationResponse_runtimeConfiguration,
    updateRuntimeConfigurationResponse_httpStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateRuntimeConfiguration' smart constructor.
data UpdateRuntimeConfiguration = UpdateRuntimeConfiguration'
  { -- | A unique identifier for a fleet to update runtime configuration for. You
    -- can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | Instructions for launching server processes on each instance in the
    -- fleet. Server processes run either a custom game build executable or a
    -- Realtime Servers script. The runtime configuration lists the types of
    -- server processes to run on an instance and includes the following
    -- configuration settings: the server executable or launch script file,
    -- launch parameters, and the number of processes to run concurrently on
    -- each instance. A CreateFleet request must include a runtime
    -- configuration with at least one server process configuration.
    runtimeConfiguration :: RuntimeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'updateRuntimeConfiguration_fleetId' - A unique identifier for a fleet to update runtime configuration for. You
-- can use either the fleet ID or ARN value.
--
-- 'runtimeConfiguration', 'updateRuntimeConfiguration_runtimeConfiguration' - Instructions for launching server processes on each instance in the
-- fleet. Server processes run either a custom game build executable or a
-- Realtime Servers script. The runtime configuration lists the types of
-- server processes to run on an instance and includes the following
-- configuration settings: the server executable or launch script file,
-- launch parameters, and the number of processes to run concurrently on
-- each instance. A CreateFleet request must include a runtime
-- configuration with at least one server process configuration.
newUpdateRuntimeConfiguration ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'runtimeConfiguration'
  RuntimeConfiguration ->
  UpdateRuntimeConfiguration
newUpdateRuntimeConfiguration
  pFleetId_
  pRuntimeConfiguration_ =
    UpdateRuntimeConfiguration'
      { fleetId = pFleetId_,
        runtimeConfiguration = pRuntimeConfiguration_
      }

-- | A unique identifier for a fleet to update runtime configuration for. You
-- can use either the fleet ID or ARN value.
updateRuntimeConfiguration_fleetId :: Lens.Lens' UpdateRuntimeConfiguration Prelude.Text
updateRuntimeConfiguration_fleetId = Lens.lens (\UpdateRuntimeConfiguration' {fleetId} -> fleetId) (\s@UpdateRuntimeConfiguration' {} a -> s {fleetId = a} :: UpdateRuntimeConfiguration)

-- | Instructions for launching server processes on each instance in the
-- fleet. Server processes run either a custom game build executable or a
-- Realtime Servers script. The runtime configuration lists the types of
-- server processes to run on an instance and includes the following
-- configuration settings: the server executable or launch script file,
-- launch parameters, and the number of processes to run concurrently on
-- each instance. A CreateFleet request must include a runtime
-- configuration with at least one server process configuration.
updateRuntimeConfiguration_runtimeConfiguration :: Lens.Lens' UpdateRuntimeConfiguration RuntimeConfiguration
updateRuntimeConfiguration_runtimeConfiguration = Lens.lens (\UpdateRuntimeConfiguration' {runtimeConfiguration} -> runtimeConfiguration) (\s@UpdateRuntimeConfiguration' {} a -> s {runtimeConfiguration = a} :: UpdateRuntimeConfiguration)

instance
  Prelude.AWSRequest
    UpdateRuntimeConfiguration
  where
  type
    Rs UpdateRuntimeConfiguration =
      UpdateRuntimeConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuntimeConfigurationResponse'
            Prelude.<$> (x Prelude..?> "RuntimeConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRuntimeConfiguration

instance Prelude.NFData UpdateRuntimeConfiguration

instance Prelude.ToHeaders UpdateRuntimeConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.UpdateRuntimeConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateRuntimeConfiguration where
  toJSON UpdateRuntimeConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Prelude..= fleetId),
            Prelude.Just
              ( "RuntimeConfiguration"
                  Prelude..= runtimeConfiguration
              )
          ]
      )

instance Prelude.ToPath UpdateRuntimeConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateRuntimeConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateRuntimeConfigurationResponse' smart constructor.
data UpdateRuntimeConfigurationResponse = UpdateRuntimeConfigurationResponse'
  { -- | The runtime configuration currently in force. If the update was
    -- successful, this object matches the one in the request.
    runtimeConfiguration :: Prelude.Maybe RuntimeConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuntimeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeConfiguration', 'updateRuntimeConfigurationResponse_runtimeConfiguration' - The runtime configuration currently in force. If the update was
-- successful, this object matches the one in the request.
--
-- 'httpStatus', 'updateRuntimeConfigurationResponse_httpStatus' - The response's http status code.
newUpdateRuntimeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuntimeConfigurationResponse
newUpdateRuntimeConfigurationResponse pHttpStatus_ =
  UpdateRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The runtime configuration currently in force. If the update was
-- successful, this object matches the one in the request.
updateRuntimeConfigurationResponse_runtimeConfiguration :: Lens.Lens' UpdateRuntimeConfigurationResponse (Prelude.Maybe RuntimeConfiguration)
updateRuntimeConfigurationResponse_runtimeConfiguration = Lens.lens (\UpdateRuntimeConfigurationResponse' {runtimeConfiguration} -> runtimeConfiguration) (\s@UpdateRuntimeConfigurationResponse' {} a -> s {runtimeConfiguration = a} :: UpdateRuntimeConfigurationResponse)

-- | The response's http status code.
updateRuntimeConfigurationResponse_httpStatus :: Lens.Lens' UpdateRuntimeConfigurationResponse Prelude.Int
updateRuntimeConfigurationResponse_httpStatus = Lens.lens (\UpdateRuntimeConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateRuntimeConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateRuntimeConfigurationResponse)

instance
  Prelude.NFData
    UpdateRuntimeConfigurationResponse

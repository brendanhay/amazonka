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
-- Module      : Amazonka.GameLift.UpdateRuntimeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the current runtime configuration for the specified fleet, which
-- tells GameLift how to launch server processes on all instances in the
-- fleet. You can update a fleet\'s runtime configuration at any time after
-- the fleet is created; it does not need to be in @ACTIVE@ status.
--
-- To update runtime configuration, specify the fleet ID and provide a
-- @RuntimeConfiguration@ with an updated set of server process
-- configurations.
--
-- If successful, the fleet\'s runtime configuration settings are updated.
-- Each instance in the fleet regularly checks for and retrieves updated
-- runtime configurations. Instances immediately begin complying with the
-- new configuration by launching new server processes or not replacing
-- existing processes when they shut down. Updating a fleet\'s runtime
-- configuration never affects existing server processes.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
module Amazonka.GameLift.UpdateRuntimeConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRuntimeConfiguration' smart constructor.
data UpdateRuntimeConfiguration = UpdateRuntimeConfiguration'
  { -- | A unique identifier for the fleet to update runtime configuration for.
    -- You can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | Instructions for alaunching server processes on each instance in the
    -- fleet. Server processes run either a custom game build executable or a
    -- Realtime Servers script. The runtime configuration lists the types of
    -- server processes to run on an instance, how to launch them, and the
    -- number of processes to run concurrently.
    runtimeConfiguration :: RuntimeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'updateRuntimeConfiguration_fleetId' - A unique identifier for the fleet to update runtime configuration for.
-- You can use either the fleet ID or ARN value.
--
-- 'runtimeConfiguration', 'updateRuntimeConfiguration_runtimeConfiguration' - Instructions for alaunching server processes on each instance in the
-- fleet. Server processes run either a custom game build executable or a
-- Realtime Servers script. The runtime configuration lists the types of
-- server processes to run on an instance, how to launch them, and the
-- number of processes to run concurrently.
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

-- | A unique identifier for the fleet to update runtime configuration for.
-- You can use either the fleet ID or ARN value.
updateRuntimeConfiguration_fleetId :: Lens.Lens' UpdateRuntimeConfiguration Prelude.Text
updateRuntimeConfiguration_fleetId = Lens.lens (\UpdateRuntimeConfiguration' {fleetId} -> fleetId) (\s@UpdateRuntimeConfiguration' {} a -> s {fleetId = a} :: UpdateRuntimeConfiguration)

-- | Instructions for alaunching server processes on each instance in the
-- fleet. Server processes run either a custom game build executable or a
-- Realtime Servers script. The runtime configuration lists the types of
-- server processes to run on an instance, how to launch them, and the
-- number of processes to run concurrently.
updateRuntimeConfiguration_runtimeConfiguration :: Lens.Lens' UpdateRuntimeConfiguration RuntimeConfiguration
updateRuntimeConfiguration_runtimeConfiguration = Lens.lens (\UpdateRuntimeConfiguration' {runtimeConfiguration} -> runtimeConfiguration) (\s@UpdateRuntimeConfiguration' {} a -> s {runtimeConfiguration = a} :: UpdateRuntimeConfiguration)

instance Core.AWSRequest UpdateRuntimeConfiguration where
  type
    AWSResponse UpdateRuntimeConfiguration =
      UpdateRuntimeConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuntimeConfigurationResponse'
            Prelude.<$> (x Data..?> "RuntimeConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRuntimeConfiguration where
  hashWithSalt _salt UpdateRuntimeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` runtimeConfiguration

instance Prelude.NFData UpdateRuntimeConfiguration where
  rnf UpdateRuntimeConfiguration' {..} =
    Prelude.rnf fleetId `Prelude.seq`
      Prelude.rnf runtimeConfiguration

instance Data.ToHeaders UpdateRuntimeConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.UpdateRuntimeConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRuntimeConfiguration where
  toJSON UpdateRuntimeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just
              ( "RuntimeConfiguration"
                  Data..= runtimeConfiguration
              )
          ]
      )

instance Data.ToPath UpdateRuntimeConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRuntimeConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuntimeConfigurationResponse' smart constructor.
data UpdateRuntimeConfigurationResponse = UpdateRuntimeConfigurationResponse'
  { -- | The runtime configuration currently in use by all instances in the
    -- fleet. If the update was successful, all property changes are shown.
    runtimeConfiguration :: Prelude.Maybe RuntimeConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuntimeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeConfiguration', 'updateRuntimeConfigurationResponse_runtimeConfiguration' - The runtime configuration currently in use by all instances in the
-- fleet. If the update was successful, all property changes are shown.
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

-- | The runtime configuration currently in use by all instances in the
-- fleet. If the update was successful, all property changes are shown.
updateRuntimeConfigurationResponse_runtimeConfiguration :: Lens.Lens' UpdateRuntimeConfigurationResponse (Prelude.Maybe RuntimeConfiguration)
updateRuntimeConfigurationResponse_runtimeConfiguration = Lens.lens (\UpdateRuntimeConfigurationResponse' {runtimeConfiguration} -> runtimeConfiguration) (\s@UpdateRuntimeConfigurationResponse' {} a -> s {runtimeConfiguration = a} :: UpdateRuntimeConfigurationResponse)

-- | The response's http status code.
updateRuntimeConfigurationResponse_httpStatus :: Lens.Lens' UpdateRuntimeConfigurationResponse Prelude.Int
updateRuntimeConfigurationResponse_httpStatus = Lens.lens (\UpdateRuntimeConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateRuntimeConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateRuntimeConfigurationResponse)

instance
  Prelude.NFData
    UpdateRuntimeConfigurationResponse
  where
  rnf UpdateRuntimeConfigurationResponse' {..} =
    Prelude.rnf runtimeConfiguration `Prelude.seq`
      Prelude.rnf httpStatus

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
-- Module      : Amazonka.GameLift.UpdateFleetPortSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates permissions that allow inbound traffic to connect to game
-- sessions that are being hosted on instances in the fleet.
--
-- To update settings, specify the fleet ID to be updated and specify the
-- changes to be made. List the permissions you want to add in
-- @InboundPermissionAuthorizations@, and permissions you want to remove in
-- @InboundPermissionRevocations@. Permissions to be removed must match
-- existing fleet permissions.
--
-- If successful, the fleet ID for the updated fleet is returned. For
-- fleets with remote locations, port setting updates can take time to
-- propagate across all locations. You can check the status of updates in
-- each location by calling @DescribeFleetPortSettings@ with a location
-- name.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- __Related actions__
--
-- CreateFleetLocations | UpdateFleetAttributes | UpdateFleetCapacity |
-- UpdateFleetPortSettings | UpdateRuntimeConfiguration | StopFleetActions
-- | StartFleetActions | PutScalingPolicy | DeleteFleet |
-- DeleteFleetLocations | DeleteScalingPolicy |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.UpdateFleetPortSettings
  ( -- * Creating a Request
    UpdateFleetPortSettings (..),
    newUpdateFleetPortSettings,

    -- * Request Lenses
    updateFleetPortSettings_inboundPermissionRevocations,
    updateFleetPortSettings_inboundPermissionAuthorizations,
    updateFleetPortSettings_fleetId,

    -- * Destructuring the Response
    UpdateFleetPortSettingsResponse (..),
    newUpdateFleetPortSettingsResponse,

    -- * Response Lenses
    updateFleetPortSettingsResponse_fleetId,
    updateFleetPortSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateFleetPortSettings' smart constructor.
data UpdateFleetPortSettings = UpdateFleetPortSettings'
  { -- | A collection of port settings to be removed from the fleet resource.
    inboundPermissionRevocations :: Prelude.Maybe [IpPermission],
    -- | A collection of port settings to be added to the fleet resource.
    inboundPermissionAuthorizations :: Prelude.Maybe [IpPermission],
    -- | A unique identifier for the fleet to update port settings for. You can
    -- use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetPortSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inboundPermissionRevocations', 'updateFleetPortSettings_inboundPermissionRevocations' - A collection of port settings to be removed from the fleet resource.
--
-- 'inboundPermissionAuthorizations', 'updateFleetPortSettings_inboundPermissionAuthorizations' - A collection of port settings to be added to the fleet resource.
--
-- 'fleetId', 'updateFleetPortSettings_fleetId' - A unique identifier for the fleet to update port settings for. You can
-- use either the fleet ID or ARN value.
newUpdateFleetPortSettings ::
  -- | 'fleetId'
  Prelude.Text ->
  UpdateFleetPortSettings
newUpdateFleetPortSettings pFleetId_ =
  UpdateFleetPortSettings'
    { inboundPermissionRevocations =
        Prelude.Nothing,
      inboundPermissionAuthorizations = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | A collection of port settings to be removed from the fleet resource.
updateFleetPortSettings_inboundPermissionRevocations :: Lens.Lens' UpdateFleetPortSettings (Prelude.Maybe [IpPermission])
updateFleetPortSettings_inboundPermissionRevocations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionRevocations} -> inboundPermissionRevocations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionRevocations = a} :: UpdateFleetPortSettings) Prelude.. Lens.mapping Lens.coerced

-- | A collection of port settings to be added to the fleet resource.
updateFleetPortSettings_inboundPermissionAuthorizations :: Lens.Lens' UpdateFleetPortSettings (Prelude.Maybe [IpPermission])
updateFleetPortSettings_inboundPermissionAuthorizations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionAuthorizations} -> inboundPermissionAuthorizations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionAuthorizations = a} :: UpdateFleetPortSettings) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the fleet to update port settings for. You can
-- use either the fleet ID or ARN value.
updateFleetPortSettings_fleetId :: Lens.Lens' UpdateFleetPortSettings Prelude.Text
updateFleetPortSettings_fleetId = Lens.lens (\UpdateFleetPortSettings' {fleetId} -> fleetId) (\s@UpdateFleetPortSettings' {} a -> s {fleetId = a} :: UpdateFleetPortSettings)

instance Core.AWSRequest UpdateFleetPortSettings where
  type
    AWSResponse UpdateFleetPortSettings =
      UpdateFleetPortSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetPortSettingsResponse'
            Prelude.<$> (x Core..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFleetPortSettings where
  hashWithSalt _salt UpdateFleetPortSettings' {..} =
    _salt
      `Prelude.hashWithSalt` inboundPermissionRevocations
      `Prelude.hashWithSalt` inboundPermissionAuthorizations
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData UpdateFleetPortSettings where
  rnf UpdateFleetPortSettings' {..} =
    Prelude.rnf inboundPermissionRevocations
      `Prelude.seq` Prelude.rnf inboundPermissionAuthorizations
      `Prelude.seq` Prelude.rnf fleetId

instance Core.ToHeaders UpdateFleetPortSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.UpdateFleetPortSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFleetPortSettings where
  toJSON UpdateFleetPortSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InboundPermissionRevocations" Core..=)
              Prelude.<$> inboundPermissionRevocations,
            ("InboundPermissionAuthorizations" Core..=)
              Prelude.<$> inboundPermissionAuthorizations,
            Prelude.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath UpdateFleetPortSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateFleetPortSettings where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateFleetPortSettingsResponse' smart constructor.
data UpdateFleetPortSettingsResponse = UpdateFleetPortSettingsResponse'
  { -- | A unique identifier for the fleet that was updated.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetPortSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'updateFleetPortSettingsResponse_fleetId' - A unique identifier for the fleet that was updated.
--
-- 'httpStatus', 'updateFleetPortSettingsResponse_httpStatus' - The response's http status code.
newUpdateFleetPortSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFleetPortSettingsResponse
newUpdateFleetPortSettingsResponse pHttpStatus_ =
  UpdateFleetPortSettingsResponse'
    { fleetId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the fleet that was updated.
updateFleetPortSettingsResponse_fleetId :: Lens.Lens' UpdateFleetPortSettingsResponse (Prelude.Maybe Prelude.Text)
updateFleetPortSettingsResponse_fleetId = Lens.lens (\UpdateFleetPortSettingsResponse' {fleetId} -> fleetId) (\s@UpdateFleetPortSettingsResponse' {} a -> s {fleetId = a} :: UpdateFleetPortSettingsResponse)

-- | The response's http status code.
updateFleetPortSettingsResponse_httpStatus :: Lens.Lens' UpdateFleetPortSettingsResponse Prelude.Int
updateFleetPortSettingsResponse_httpStatus = Lens.lens (\UpdateFleetPortSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetPortSettingsResponse' {} a -> s {httpStatus = a} :: UpdateFleetPortSettingsResponse)

instance
  Prelude.NFData
    UpdateFleetPortSettingsResponse
  where
  rnf UpdateFleetPortSettingsResponse' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf httpStatus

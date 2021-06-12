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
-- Module      : Network.AWS.GameLift.UpdateFleetPortSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates port settings for a fleet. To update settings, specify the fleet
-- ID to be updated and list the permissions you want to update. List the
-- permissions you want to add in @InboundPermissionAuthorizations@, and
-- permissions you want to remove in @InboundPermissionRevocations@.
-- Permissions to be removed must match existing fleet permissions. If
-- successful, the fleet ID for the updated fleet is returned.
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
module Network.AWS.GameLift.UpdateFleetPortSettings
  ( -- * Creating a Request
    UpdateFleetPortSettings (..),
    newUpdateFleetPortSettings,

    -- * Request Lenses
    updateFleetPortSettings_inboundPermissionAuthorizations,
    updateFleetPortSettings_inboundPermissionRevocations,
    updateFleetPortSettings_fleetId,

    -- * Destructuring the Response
    UpdateFleetPortSettingsResponse (..),
    newUpdateFleetPortSettingsResponse,

    -- * Response Lenses
    updateFleetPortSettingsResponse_fleetId,
    updateFleetPortSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateFleetPortSettings' smart constructor.
data UpdateFleetPortSettings = UpdateFleetPortSettings'
  { -- | A collection of port settings to be added to the fleet resource.
    inboundPermissionAuthorizations :: Core.Maybe [IpPermission],
    -- | A collection of port settings to be removed from the fleet resource.
    inboundPermissionRevocations :: Core.Maybe [IpPermission],
    -- | A unique identifier for a fleet to update port settings for. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleetPortSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inboundPermissionAuthorizations', 'updateFleetPortSettings_inboundPermissionAuthorizations' - A collection of port settings to be added to the fleet resource.
--
-- 'inboundPermissionRevocations', 'updateFleetPortSettings_inboundPermissionRevocations' - A collection of port settings to be removed from the fleet resource.
--
-- 'fleetId', 'updateFleetPortSettings_fleetId' - A unique identifier for a fleet to update port settings for. You can use
-- either the fleet ID or ARN value.
newUpdateFleetPortSettings ::
  -- | 'fleetId'
  Core.Text ->
  UpdateFleetPortSettings
newUpdateFleetPortSettings pFleetId_ =
  UpdateFleetPortSettings'
    { inboundPermissionAuthorizations =
        Core.Nothing,
      inboundPermissionRevocations = Core.Nothing,
      fleetId = pFleetId_
    }

-- | A collection of port settings to be added to the fleet resource.
updateFleetPortSettings_inboundPermissionAuthorizations :: Lens.Lens' UpdateFleetPortSettings (Core.Maybe [IpPermission])
updateFleetPortSettings_inboundPermissionAuthorizations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionAuthorizations} -> inboundPermissionAuthorizations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionAuthorizations = a} :: UpdateFleetPortSettings) Core.. Lens.mapping Lens._Coerce

-- | A collection of port settings to be removed from the fleet resource.
updateFleetPortSettings_inboundPermissionRevocations :: Lens.Lens' UpdateFleetPortSettings (Core.Maybe [IpPermission])
updateFleetPortSettings_inboundPermissionRevocations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionRevocations} -> inboundPermissionRevocations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionRevocations = a} :: UpdateFleetPortSettings) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for a fleet to update port settings for. You can use
-- either the fleet ID or ARN value.
updateFleetPortSettings_fleetId :: Lens.Lens' UpdateFleetPortSettings Core.Text
updateFleetPortSettings_fleetId = Lens.lens (\UpdateFleetPortSettings' {fleetId} -> fleetId) (\s@UpdateFleetPortSettings' {} a -> s {fleetId = a} :: UpdateFleetPortSettings)

instance Core.AWSRequest UpdateFleetPortSettings where
  type
    AWSResponse UpdateFleetPortSettings =
      UpdateFleetPortSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetPortSettingsResponse'
            Core.<$> (x Core..?> "FleetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateFleetPortSettings

instance Core.NFData UpdateFleetPortSettings

instance Core.ToHeaders UpdateFleetPortSettings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.UpdateFleetPortSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateFleetPortSettings where
  toJSON UpdateFleetPortSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InboundPermissionAuthorizations" Core..=)
              Core.<$> inboundPermissionAuthorizations,
            ("InboundPermissionRevocations" Core..=)
              Core.<$> inboundPermissionRevocations,
            Core.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath UpdateFleetPortSettings where
  toPath = Core.const "/"

instance Core.ToQuery UpdateFleetPortSettings where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateFleetPortSettingsResponse' smart constructor.
data UpdateFleetPortSettingsResponse = UpdateFleetPortSettingsResponse'
  { -- | A unique identifier for a fleet that was updated.
    fleetId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleetPortSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'updateFleetPortSettingsResponse_fleetId' - A unique identifier for a fleet that was updated.
--
-- 'httpStatus', 'updateFleetPortSettingsResponse_httpStatus' - The response's http status code.
newUpdateFleetPortSettingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateFleetPortSettingsResponse
newUpdateFleetPortSettingsResponse pHttpStatus_ =
  UpdateFleetPortSettingsResponse'
    { fleetId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for a fleet that was updated.
updateFleetPortSettingsResponse_fleetId :: Lens.Lens' UpdateFleetPortSettingsResponse (Core.Maybe Core.Text)
updateFleetPortSettingsResponse_fleetId = Lens.lens (\UpdateFleetPortSettingsResponse' {fleetId} -> fleetId) (\s@UpdateFleetPortSettingsResponse' {} a -> s {fleetId = a} :: UpdateFleetPortSettingsResponse)

-- | The response's http status code.
updateFleetPortSettingsResponse_httpStatus :: Lens.Lens' UpdateFleetPortSettingsResponse Core.Int
updateFleetPortSettingsResponse_httpStatus = Lens.lens (\UpdateFleetPortSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetPortSettingsResponse' {} a -> s {httpStatus = a} :: UpdateFleetPortSettingsResponse)

instance Core.NFData UpdateFleetPortSettingsResponse

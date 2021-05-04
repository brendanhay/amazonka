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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateFleetPortSettings' smart constructor.
data UpdateFleetPortSettings = UpdateFleetPortSettings'
  { -- | A collection of port settings to be added to the fleet resource.
    inboundPermissionAuthorizations :: Prelude.Maybe [IpPermission],
    -- | A collection of port settings to be removed from the fleet resource.
    inboundPermissionRevocations :: Prelude.Maybe [IpPermission],
    -- | A unique identifier for a fleet to update port settings for. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateFleetPortSettings
newUpdateFleetPortSettings pFleetId_ =
  UpdateFleetPortSettings'
    { inboundPermissionAuthorizations =
        Prelude.Nothing,
      inboundPermissionRevocations = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | A collection of port settings to be added to the fleet resource.
updateFleetPortSettings_inboundPermissionAuthorizations :: Lens.Lens' UpdateFleetPortSettings (Prelude.Maybe [IpPermission])
updateFleetPortSettings_inboundPermissionAuthorizations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionAuthorizations} -> inboundPermissionAuthorizations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionAuthorizations = a} :: UpdateFleetPortSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | A collection of port settings to be removed from the fleet resource.
updateFleetPortSettings_inboundPermissionRevocations :: Lens.Lens' UpdateFleetPortSettings (Prelude.Maybe [IpPermission])
updateFleetPortSettings_inboundPermissionRevocations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionRevocations} -> inboundPermissionRevocations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionRevocations = a} :: UpdateFleetPortSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | A unique identifier for a fleet to update port settings for. You can use
-- either the fleet ID or ARN value.
updateFleetPortSettings_fleetId :: Lens.Lens' UpdateFleetPortSettings Prelude.Text
updateFleetPortSettings_fleetId = Lens.lens (\UpdateFleetPortSettings' {fleetId} -> fleetId) (\s@UpdateFleetPortSettings' {} a -> s {fleetId = a} :: UpdateFleetPortSettings)

instance Prelude.AWSRequest UpdateFleetPortSettings where
  type
    Rs UpdateFleetPortSettings =
      UpdateFleetPortSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetPortSettingsResponse'
            Prelude.<$> (x Prelude..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFleetPortSettings

instance Prelude.NFData UpdateFleetPortSettings

instance Prelude.ToHeaders UpdateFleetPortSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.UpdateFleetPortSettings" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateFleetPortSettings where
  toJSON UpdateFleetPortSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InboundPermissionAuthorizations" Prelude..=)
              Prelude.<$> inboundPermissionAuthorizations,
            ("InboundPermissionRevocations" Prelude..=)
              Prelude.<$> inboundPermissionRevocations,
            Prelude.Just ("FleetId" Prelude..= fleetId)
          ]
      )

instance Prelude.ToPath UpdateFleetPortSettings where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateFleetPortSettings where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateFleetPortSettingsResponse' smart constructor.
data UpdateFleetPortSettingsResponse = UpdateFleetPortSettingsResponse'
  { -- | A unique identifier for a fleet that was updated.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateFleetPortSettingsResponse
newUpdateFleetPortSettingsResponse pHttpStatus_ =
  UpdateFleetPortSettingsResponse'
    { fleetId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for a fleet that was updated.
updateFleetPortSettingsResponse_fleetId :: Lens.Lens' UpdateFleetPortSettingsResponse (Prelude.Maybe Prelude.Text)
updateFleetPortSettingsResponse_fleetId = Lens.lens (\UpdateFleetPortSettingsResponse' {fleetId} -> fleetId) (\s@UpdateFleetPortSettingsResponse' {} a -> s {fleetId = a} :: UpdateFleetPortSettingsResponse)

-- | The response's http status code.
updateFleetPortSettingsResponse_httpStatus :: Lens.Lens' UpdateFleetPortSettingsResponse Prelude.Int
updateFleetPortSettingsResponse_httpStatus = Lens.lens (\UpdateFleetPortSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetPortSettingsResponse' {} a -> s {httpStatus = a} :: UpdateFleetPortSettingsResponse)

instance
  Prelude.NFData
    UpdateFleetPortSettingsResponse

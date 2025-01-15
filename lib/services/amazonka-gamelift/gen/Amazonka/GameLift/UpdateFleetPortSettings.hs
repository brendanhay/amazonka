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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.GameLift.UpdateFleetPortSettings
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
    updateFleetPortSettingsResponse_fleetArn,
    updateFleetPortSettingsResponse_fleetId,
    updateFleetPortSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFleetPortSettings' smart constructor.
data UpdateFleetPortSettings = UpdateFleetPortSettings'
  { -- | A collection of port settings to be added to the fleet resource.
    inboundPermissionAuthorizations :: Prelude.Maybe [IpPermission],
    -- | A collection of port settings to be removed from the fleet resource.
    inboundPermissionRevocations :: Prelude.Maybe [IpPermission],
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
-- 'inboundPermissionAuthorizations', 'updateFleetPortSettings_inboundPermissionAuthorizations' - A collection of port settings to be added to the fleet resource.
--
-- 'inboundPermissionRevocations', 'updateFleetPortSettings_inboundPermissionRevocations' - A collection of port settings to be removed from the fleet resource.
--
-- 'fleetId', 'updateFleetPortSettings_fleetId' - A unique identifier for the fleet to update port settings for. You can
-- use either the fleet ID or ARN value.
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
updateFleetPortSettings_inboundPermissionAuthorizations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionAuthorizations} -> inboundPermissionAuthorizations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionAuthorizations = a} :: UpdateFleetPortSettings) Prelude.. Lens.mapping Lens.coerced

-- | A collection of port settings to be removed from the fleet resource.
updateFleetPortSettings_inboundPermissionRevocations :: Lens.Lens' UpdateFleetPortSettings (Prelude.Maybe [IpPermission])
updateFleetPortSettings_inboundPermissionRevocations = Lens.lens (\UpdateFleetPortSettings' {inboundPermissionRevocations} -> inboundPermissionRevocations) (\s@UpdateFleetPortSettings' {} a -> s {inboundPermissionRevocations = a} :: UpdateFleetPortSettings) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Data..?> "FleetArn")
            Prelude.<*> (x Data..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFleetPortSettings where
  hashWithSalt _salt UpdateFleetPortSettings' {..} =
    _salt
      `Prelude.hashWithSalt` inboundPermissionAuthorizations
      `Prelude.hashWithSalt` inboundPermissionRevocations
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData UpdateFleetPortSettings where
  rnf UpdateFleetPortSettings' {..} =
    Prelude.rnf inboundPermissionAuthorizations `Prelude.seq`
      Prelude.rnf inboundPermissionRevocations `Prelude.seq`
        Prelude.rnf fleetId

instance Data.ToHeaders UpdateFleetPortSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.UpdateFleetPortSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFleetPortSettings where
  toJSON UpdateFleetPortSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InboundPermissionAuthorizations" Data..=)
              Prelude.<$> inboundPermissionAuthorizations,
            ("InboundPermissionRevocations" Data..=)
              Prelude.<$> inboundPermissionRevocations,
            Prelude.Just ("FleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath UpdateFleetPortSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFleetPortSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFleetPortSettingsResponse' smart constructor.
data UpdateFleetPortSettingsResponse = UpdateFleetPortSettingsResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that was updated.
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
-- 'fleetArn', 'updateFleetPortSettingsResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
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
    { fleetArn =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
updateFleetPortSettingsResponse_fleetArn :: Lens.Lens' UpdateFleetPortSettingsResponse (Prelude.Maybe Prelude.Text)
updateFleetPortSettingsResponse_fleetArn = Lens.lens (\UpdateFleetPortSettingsResponse' {fleetArn} -> fleetArn) (\s@UpdateFleetPortSettingsResponse' {} a -> s {fleetArn = a} :: UpdateFleetPortSettingsResponse)

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
    Prelude.rnf fleetArn `Prelude.seq`
      Prelude.rnf fleetId `Prelude.seq`
        Prelude.rnf httpStatus

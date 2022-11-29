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
-- Module      : Amazonka.GameLift.DescribeFleetPortSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fleet\'s inbound connection permissions. Connection
-- permissions specify the range of IP addresses and port settings that
-- incoming traffic can use to access server processes in the fleet. Game
-- sessions that are running on instances in the fleet must use connections
-- that fall in this range.
--
-- This operation can be used in the following ways:
--
-- -   To retrieve the inbound connection permissions for a fleet, identify
--     the fleet\'s unique identifier.
--
-- -   To check the status of recent updates to a fleet remote location,
--     specify the fleet ID and a location. Port setting updates can take
--     time to propagate across all locations.
--
-- If successful, a set of IpPermission objects is returned for the
-- requested fleet ID. When a location is specified, a pending status is
-- included. If the requested fleet has been deleted, the result set is
-- empty.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- __Related actions__
--
-- ListFleets | DescribeEC2InstanceLimits | DescribeFleetAttributes |
-- DescribeFleetCapacity | DescribeFleetEvents |
-- DescribeFleetLocationAttributes | DescribeFleetPortSettings |
-- DescribeFleetUtilization | DescribeRuntimeConfiguration |
-- DescribeScalingPolicies |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DescribeFleetPortSettings
  ( -- * Creating a Request
    DescribeFleetPortSettings (..),
    newDescribeFleetPortSettings,

    -- * Request Lenses
    describeFleetPortSettings_location,
    describeFleetPortSettings_fleetId,

    -- * Destructuring the Response
    DescribeFleetPortSettingsResponse (..),
    newDescribeFleetPortSettingsResponse,

    -- * Response Lenses
    describeFleetPortSettingsResponse_fleetId,
    describeFleetPortSettingsResponse_inboundPermissions,
    describeFleetPortSettingsResponse_updateStatus,
    describeFleetPortSettingsResponse_location,
    describeFleetPortSettingsResponse_fleetArn,
    describeFleetPortSettingsResponse_httpStatus,
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
-- /See:/ 'newDescribeFleetPortSettings' smart constructor.
data DescribeFleetPortSettings = DescribeFleetPortSettings'
  { -- | A remote location to check for status of port setting updates. Use the
    -- Amazon Web Services Region code format, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to retrieve port settings for. You can
    -- use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetPortSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'describeFleetPortSettings_location' - A remote location to check for status of port setting updates. Use the
-- Amazon Web Services Region code format, such as @us-west-2@.
--
-- 'fleetId', 'describeFleetPortSettings_fleetId' - A unique identifier for the fleet to retrieve port settings for. You can
-- use either the fleet ID or ARN value.
newDescribeFleetPortSettings ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeFleetPortSettings
newDescribeFleetPortSettings pFleetId_ =
  DescribeFleetPortSettings'
    { location =
        Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | A remote location to check for status of port setting updates. Use the
-- Amazon Web Services Region code format, such as @us-west-2@.
describeFleetPortSettings_location :: Lens.Lens' DescribeFleetPortSettings (Prelude.Maybe Prelude.Text)
describeFleetPortSettings_location = Lens.lens (\DescribeFleetPortSettings' {location} -> location) (\s@DescribeFleetPortSettings' {} a -> s {location = a} :: DescribeFleetPortSettings)

-- | A unique identifier for the fleet to retrieve port settings for. You can
-- use either the fleet ID or ARN value.
describeFleetPortSettings_fleetId :: Lens.Lens' DescribeFleetPortSettings Prelude.Text
describeFleetPortSettings_fleetId = Lens.lens (\DescribeFleetPortSettings' {fleetId} -> fleetId) (\s@DescribeFleetPortSettings' {} a -> s {fleetId = a} :: DescribeFleetPortSettings)

instance Core.AWSRequest DescribeFleetPortSettings where
  type
    AWSResponse DescribeFleetPortSettings =
      DescribeFleetPortSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetPortSettingsResponse'
            Prelude.<$> (x Core..?> "FleetId")
            Prelude.<*> ( x Core..?> "InboundPermissions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "UpdateStatus")
            Prelude.<*> (x Core..?> "Location")
            Prelude.<*> (x Core..?> "FleetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetPortSettings where
  hashWithSalt _salt DescribeFleetPortSettings' {..} =
    _salt `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DescribeFleetPortSettings where
  rnf DescribeFleetPortSettings' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf fleetId

instance Core.ToHeaders DescribeFleetPortSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeFleetPortSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFleetPortSettings where
  toJSON DescribeFleetPortSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Location" Core..=) Prelude.<$> location,
            Prelude.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath DescribeFleetPortSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFleetPortSettings where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetPortSettingsResponse' smart constructor.
data DescribeFleetPortSettingsResponse = DescribeFleetPortSettingsResponse'
  { -- | A unique identifier for the fleet that was requested.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The port settings for the requested fleet ID.
    inboundPermissions :: Prelude.Maybe [IpPermission],
    -- | The current status of updates to the fleet\'s port settings in the
    -- requested fleet location. A status of @PENDING_UPDATE@ indicates that an
    -- update was requested for the fleet but has not yet been completed for
    -- the location.
    updateStatus :: Prelude.Maybe LocationUpdateStatus,
    -- | The requested fleet location, expressed as an Amazon Web Services Region
    -- code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetPortSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeFleetPortSettingsResponse_fleetId' - A unique identifier for the fleet that was requested.
--
-- 'inboundPermissions', 'describeFleetPortSettingsResponse_inboundPermissions' - The port settings for the requested fleet ID.
--
-- 'updateStatus', 'describeFleetPortSettingsResponse_updateStatus' - The current status of updates to the fleet\'s port settings in the
-- requested fleet location. A status of @PENDING_UPDATE@ indicates that an
-- update was requested for the fleet but has not yet been completed for
-- the location.
--
-- 'location', 'describeFleetPortSettingsResponse_location' - The requested fleet location, expressed as an Amazon Web Services Region
-- code, such as @us-west-2@.
--
-- 'fleetArn', 'describeFleetPortSettingsResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'httpStatus', 'describeFleetPortSettingsResponse_httpStatus' - The response's http status code.
newDescribeFleetPortSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetPortSettingsResponse
newDescribeFleetPortSettingsResponse pHttpStatus_ =
  DescribeFleetPortSettingsResponse'
    { fleetId =
        Prelude.Nothing,
      inboundPermissions = Prelude.Nothing,
      updateStatus = Prelude.Nothing,
      location = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the fleet that was requested.
describeFleetPortSettingsResponse_fleetId :: Lens.Lens' DescribeFleetPortSettingsResponse (Prelude.Maybe Prelude.Text)
describeFleetPortSettingsResponse_fleetId = Lens.lens (\DescribeFleetPortSettingsResponse' {fleetId} -> fleetId) (\s@DescribeFleetPortSettingsResponse' {} a -> s {fleetId = a} :: DescribeFleetPortSettingsResponse)

-- | The port settings for the requested fleet ID.
describeFleetPortSettingsResponse_inboundPermissions :: Lens.Lens' DescribeFleetPortSettingsResponse (Prelude.Maybe [IpPermission])
describeFleetPortSettingsResponse_inboundPermissions = Lens.lens (\DescribeFleetPortSettingsResponse' {inboundPermissions} -> inboundPermissions) (\s@DescribeFleetPortSettingsResponse' {} a -> s {inboundPermissions = a} :: DescribeFleetPortSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current status of updates to the fleet\'s port settings in the
-- requested fleet location. A status of @PENDING_UPDATE@ indicates that an
-- update was requested for the fleet but has not yet been completed for
-- the location.
describeFleetPortSettingsResponse_updateStatus :: Lens.Lens' DescribeFleetPortSettingsResponse (Prelude.Maybe LocationUpdateStatus)
describeFleetPortSettingsResponse_updateStatus = Lens.lens (\DescribeFleetPortSettingsResponse' {updateStatus} -> updateStatus) (\s@DescribeFleetPortSettingsResponse' {} a -> s {updateStatus = a} :: DescribeFleetPortSettingsResponse)

-- | The requested fleet location, expressed as an Amazon Web Services Region
-- code, such as @us-west-2@.
describeFleetPortSettingsResponse_location :: Lens.Lens' DescribeFleetPortSettingsResponse (Prelude.Maybe Prelude.Text)
describeFleetPortSettingsResponse_location = Lens.lens (\DescribeFleetPortSettingsResponse' {location} -> location) (\s@DescribeFleetPortSettingsResponse' {} a -> s {location = a} :: DescribeFleetPortSettingsResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
describeFleetPortSettingsResponse_fleetArn :: Lens.Lens' DescribeFleetPortSettingsResponse (Prelude.Maybe Prelude.Text)
describeFleetPortSettingsResponse_fleetArn = Lens.lens (\DescribeFleetPortSettingsResponse' {fleetArn} -> fleetArn) (\s@DescribeFleetPortSettingsResponse' {} a -> s {fleetArn = a} :: DescribeFleetPortSettingsResponse)

-- | The response's http status code.
describeFleetPortSettingsResponse_httpStatus :: Lens.Lens' DescribeFleetPortSettingsResponse Prelude.Int
describeFleetPortSettingsResponse_httpStatus = Lens.lens (\DescribeFleetPortSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetPortSettingsResponse' {} a -> s {httpStatus = a} :: DescribeFleetPortSettingsResponse)

instance
  Prelude.NFData
    DescribeFleetPortSettingsResponse
  where
  rnf DescribeFleetPortSettingsResponse' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf inboundPermissions
      `Prelude.seq` Prelude.rnf updateStatus
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.GameLift.DescribeFleetPortSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fleet\'s inbound connection permissions. Connection
-- permissions specify the range of IP addresses and port settings that
-- incoming traffic can use to access server processes in the fleet. Game
-- sessions that are running on instances in the fleet use connections that
-- fall in this range.
--
-- To get a fleet\'s inbound connection permissions, specify the fleet\'s
-- unique identifier. If successful, a collection of IpPermission objects
-- is returned for the requested fleet ID. If the requested fleet has been
-- deleted, the result set is empty.
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
-- -   Describe fleets:
--
--     -   DescribeFleetAttributes
--
--     -   DescribeFleetCapacity
--
--     -   DescribeFleetPortSettings
--
--     -   DescribeFleetUtilization
--
--     -   DescribeRuntimeConfiguration
--
--     -   DescribeEC2InstanceLimits
--
--     -   DescribeFleetEvents
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
module Network.AWS.GameLift.DescribeFleetPortSettings
  ( -- * Creating a Request
    DescribeFleetPortSettings (..),
    newDescribeFleetPortSettings,

    -- * Request Lenses
    describeFleetPortSettings_fleetId,

    -- * Destructuring the Response
    DescribeFleetPortSettingsResponse (..),
    newDescribeFleetPortSettingsResponse,

    -- * Response Lenses
    describeFleetPortSettingsResponse_inboundPermissions,
    describeFleetPortSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeFleetPortSettings' smart constructor.
data DescribeFleetPortSettings = DescribeFleetPortSettings'
  { -- | A unique identifier for a fleet to retrieve port settings for. You can
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
-- 'fleetId', 'describeFleetPortSettings_fleetId' - A unique identifier for a fleet to retrieve port settings for. You can
-- use either the fleet ID or ARN value.
newDescribeFleetPortSettings ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeFleetPortSettings
newDescribeFleetPortSettings pFleetId_ =
  DescribeFleetPortSettings' {fleetId = pFleetId_}

-- | A unique identifier for a fleet to retrieve port settings for. You can
-- use either the fleet ID or ARN value.
describeFleetPortSettings_fleetId :: Lens.Lens' DescribeFleetPortSettings Prelude.Text
describeFleetPortSettings_fleetId = Lens.lens (\DescribeFleetPortSettings' {fleetId} -> fleetId) (\s@DescribeFleetPortSettings' {} a -> s {fleetId = a} :: DescribeFleetPortSettings)

instance Core.AWSRequest DescribeFleetPortSettings where
  type
    AWSResponse DescribeFleetPortSettings =
      DescribeFleetPortSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetPortSettingsResponse'
            Prelude.<$> ( x Core..?> "InboundPermissions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetPortSettings

instance Prelude.NFData DescribeFleetPortSettings

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
          [Prelude.Just ("FleetId" Core..= fleetId)]
      )

instance Core.ToPath DescribeFleetPortSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFleetPortSettings where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetPortSettingsResponse' smart constructor.
data DescribeFleetPortSettingsResponse = DescribeFleetPortSettingsResponse'
  { -- | The port settings for the requested fleet ID.
    inboundPermissions :: Prelude.Maybe [IpPermission],
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
-- 'inboundPermissions', 'describeFleetPortSettingsResponse_inboundPermissions' - The port settings for the requested fleet ID.
--
-- 'httpStatus', 'describeFleetPortSettingsResponse_httpStatus' - The response's http status code.
newDescribeFleetPortSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetPortSettingsResponse
newDescribeFleetPortSettingsResponse pHttpStatus_ =
  DescribeFleetPortSettingsResponse'
    { inboundPermissions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The port settings for the requested fleet ID.
describeFleetPortSettingsResponse_inboundPermissions :: Lens.Lens' DescribeFleetPortSettingsResponse (Prelude.Maybe [IpPermission])
describeFleetPortSettingsResponse_inboundPermissions = Lens.lens (\DescribeFleetPortSettingsResponse' {inboundPermissions} -> inboundPermissions) (\s@DescribeFleetPortSettingsResponse' {} a -> s {inboundPermissions = a} :: DescribeFleetPortSettingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetPortSettingsResponse_httpStatus :: Lens.Lens' DescribeFleetPortSettingsResponse Prelude.Int
describeFleetPortSettingsResponse_httpStatus = Lens.lens (\DescribeFleetPortSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetPortSettingsResponse' {} a -> s {httpStatus = a} :: DescribeFleetPortSettingsResponse)

instance
  Prelude.NFData
    DescribeFleetPortSettingsResponse

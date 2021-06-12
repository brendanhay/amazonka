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
-- Module      : Network.AWS.GameLift.DescribeRuntimeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fleet\'s runtime configuration settings. The runtime
-- configuration tells Amazon GameLift which server processes to run (and
-- how) on each instance in the fleet.
--
-- To get a runtime configuration, specify the fleet\'s unique identifier.
-- If successful, a RuntimeConfiguration object is returned for the
-- requested fleet. If the requested fleet has been deleted, the result set
-- is empty.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-multiprocess.html Running Multiple Processes on a Fleet>
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
module Network.AWS.GameLift.DescribeRuntimeConfiguration
  ( -- * Creating a Request
    DescribeRuntimeConfiguration (..),
    newDescribeRuntimeConfiguration,

    -- * Request Lenses
    describeRuntimeConfiguration_fleetId,

    -- * Destructuring the Response
    DescribeRuntimeConfigurationResponse (..),
    newDescribeRuntimeConfigurationResponse,

    -- * Response Lenses
    describeRuntimeConfigurationResponse_runtimeConfiguration,
    describeRuntimeConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeRuntimeConfiguration' smart constructor.
data DescribeRuntimeConfiguration = DescribeRuntimeConfiguration'
  { -- | A unique identifier for a fleet to get the runtime configuration for.
    -- You can use either the fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeRuntimeConfiguration_fleetId' - A unique identifier for a fleet to get the runtime configuration for.
-- You can use either the fleet ID or ARN value.
newDescribeRuntimeConfiguration ::
  -- | 'fleetId'
  Core.Text ->
  DescribeRuntimeConfiguration
newDescribeRuntimeConfiguration pFleetId_ =
  DescribeRuntimeConfiguration' {fleetId = pFleetId_}

-- | A unique identifier for a fleet to get the runtime configuration for.
-- You can use either the fleet ID or ARN value.
describeRuntimeConfiguration_fleetId :: Lens.Lens' DescribeRuntimeConfiguration Core.Text
describeRuntimeConfiguration_fleetId = Lens.lens (\DescribeRuntimeConfiguration' {fleetId} -> fleetId) (\s@DescribeRuntimeConfiguration' {} a -> s {fleetId = a} :: DescribeRuntimeConfiguration)

instance Core.AWSRequest DescribeRuntimeConfiguration where
  type
    AWSResponse DescribeRuntimeConfiguration =
      DescribeRuntimeConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuntimeConfigurationResponse'
            Core.<$> (x Core..?> "RuntimeConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRuntimeConfiguration

instance Core.NFData DescribeRuntimeConfiguration

instance Core.ToHeaders DescribeRuntimeConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeRuntimeConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeRuntimeConfiguration where
  toJSON DescribeRuntimeConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("FleetId" Core..= fleetId)]
      )

instance Core.ToPath DescribeRuntimeConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRuntimeConfiguration where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeRuntimeConfigurationResponse' smart constructor.
data DescribeRuntimeConfigurationResponse = DescribeRuntimeConfigurationResponse'
  { -- | Instructions describing how server processes should be launched and
    -- maintained on each instance in the fleet.
    runtimeConfiguration :: Core.Maybe RuntimeConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRuntimeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeConfiguration', 'describeRuntimeConfigurationResponse_runtimeConfiguration' - Instructions describing how server processes should be launched and
-- maintained on each instance in the fleet.
--
-- 'httpStatus', 'describeRuntimeConfigurationResponse_httpStatus' - The response's http status code.
newDescribeRuntimeConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeRuntimeConfigurationResponse
newDescribeRuntimeConfigurationResponse pHttpStatus_ =
  DescribeRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Instructions describing how server processes should be launched and
-- maintained on each instance in the fleet.
describeRuntimeConfigurationResponse_runtimeConfiguration :: Lens.Lens' DescribeRuntimeConfigurationResponse (Core.Maybe RuntimeConfiguration)
describeRuntimeConfigurationResponse_runtimeConfiguration = Lens.lens (\DescribeRuntimeConfigurationResponse' {runtimeConfiguration} -> runtimeConfiguration) (\s@DescribeRuntimeConfigurationResponse' {} a -> s {runtimeConfiguration = a} :: DescribeRuntimeConfigurationResponse)

-- | The response's http status code.
describeRuntimeConfigurationResponse_httpStatus :: Lens.Lens' DescribeRuntimeConfigurationResponse Core.Int
describeRuntimeConfigurationResponse_httpStatus = Lens.lens (\DescribeRuntimeConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeRuntimeConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeRuntimeConfigurationResponse)

instance
  Core.NFData
    DescribeRuntimeConfigurationResponse

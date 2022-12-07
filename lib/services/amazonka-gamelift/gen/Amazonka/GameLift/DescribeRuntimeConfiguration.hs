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
-- Module      : Amazonka.GameLift.DescribeRuntimeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fleet\'s runtime configuration settings. The runtime
-- configuration tells GameLift which server processes to run (and how) on
-- each instance in the fleet.
--
-- To get the runtime configuration that is currently in forces for a
-- fleet, provide the fleet ID.
--
-- If successful, a RuntimeConfiguration object is returned for the
-- requested fleet. If the requested fleet has been deleted, the result set
-- is empty.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-multiprocess.html Running multiple processes on a fleet>
--
-- __Related actions__
--
-- ListFleets | DescribeEC2InstanceLimits | DescribeFleetAttributes |
-- DescribeFleetCapacity | DescribeFleetEvents |
-- DescribeFleetLocationAttributes | DescribeFleetPortSettings |
-- DescribeFleetUtilization | DescribeRuntimeConfiguration |
-- DescribeScalingPolicies |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DescribeRuntimeConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeRuntimeConfiguration' smart constructor.
data DescribeRuntimeConfiguration = DescribeRuntimeConfiguration'
  { -- | A unique identifier for the fleet to get the runtime configuration for.
    -- You can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeRuntimeConfiguration_fleetId' - A unique identifier for the fleet to get the runtime configuration for.
-- You can use either the fleet ID or ARN value.
newDescribeRuntimeConfiguration ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeRuntimeConfiguration
newDescribeRuntimeConfiguration pFleetId_ =
  DescribeRuntimeConfiguration' {fleetId = pFleetId_}

-- | A unique identifier for the fleet to get the runtime configuration for.
-- You can use either the fleet ID or ARN value.
describeRuntimeConfiguration_fleetId :: Lens.Lens' DescribeRuntimeConfiguration Prelude.Text
describeRuntimeConfiguration_fleetId = Lens.lens (\DescribeRuntimeConfiguration' {fleetId} -> fleetId) (\s@DescribeRuntimeConfiguration' {} a -> s {fleetId = a} :: DescribeRuntimeConfiguration)

instance Core.AWSRequest DescribeRuntimeConfiguration where
  type
    AWSResponse DescribeRuntimeConfiguration =
      DescribeRuntimeConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuntimeConfigurationResponse'
            Prelude.<$> (x Data..?> "RuntimeConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRuntimeConfiguration
  where
  hashWithSalt _salt DescribeRuntimeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DescribeRuntimeConfiguration where
  rnf DescribeRuntimeConfiguration' {..} =
    Prelude.rnf fleetId

instance Data.ToHeaders DescribeRuntimeConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeRuntimeConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRuntimeConfiguration where
  toJSON DescribeRuntimeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetId" Data..= fleetId)]
      )

instance Data.ToPath DescribeRuntimeConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRuntimeConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeRuntimeConfigurationResponse' smart constructor.
data DescribeRuntimeConfigurationResponse = DescribeRuntimeConfigurationResponse'
  { -- | Instructions that describe how server processes should be launched and
    -- maintained on each instance in the fleet.
    runtimeConfiguration :: Prelude.Maybe RuntimeConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuntimeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeConfiguration', 'describeRuntimeConfigurationResponse_runtimeConfiguration' - Instructions that describe how server processes should be launched and
-- maintained on each instance in the fleet.
--
-- 'httpStatus', 'describeRuntimeConfigurationResponse_httpStatus' - The response's http status code.
newDescribeRuntimeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRuntimeConfigurationResponse
newDescribeRuntimeConfigurationResponse pHttpStatus_ =
  DescribeRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Instructions that describe how server processes should be launched and
-- maintained on each instance in the fleet.
describeRuntimeConfigurationResponse_runtimeConfiguration :: Lens.Lens' DescribeRuntimeConfigurationResponse (Prelude.Maybe RuntimeConfiguration)
describeRuntimeConfigurationResponse_runtimeConfiguration = Lens.lens (\DescribeRuntimeConfigurationResponse' {runtimeConfiguration} -> runtimeConfiguration) (\s@DescribeRuntimeConfigurationResponse' {} a -> s {runtimeConfiguration = a} :: DescribeRuntimeConfigurationResponse)

-- | The response's http status code.
describeRuntimeConfigurationResponse_httpStatus :: Lens.Lens' DescribeRuntimeConfigurationResponse Prelude.Int
describeRuntimeConfigurationResponse_httpStatus = Lens.lens (\DescribeRuntimeConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeRuntimeConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeRuntimeConfigurationResponse)

instance
  Prelude.NFData
    DescribeRuntimeConfigurationResponse
  where
  rnf DescribeRuntimeConfigurationResponse' {..} =
    Prelude.rnf runtimeConfiguration
      `Prelude.seq` Prelude.rnf httpStatus

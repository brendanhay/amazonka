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
-- Module      : Amazonka.GameLift.DescribeFleetLocationUtilization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves current usage data for a fleet location. Utilization data
-- provides a snapshot of current game hosting activity at the requested
-- location. Use this operation to retrieve utilization information for a
-- fleet\'s remote location or home Region (you can also retrieve home
-- Region utilization by calling @DescribeFleetUtilization@).
--
-- To retrieve utilization data, identify a fleet and location.
--
-- If successful, a @FleetUtilization@ object is returned for the requested
-- fleet location.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html#gamelift-metrics-fleet GameLift metrics for fleets>
module Amazonka.GameLift.DescribeFleetLocationUtilization
  ( -- * Creating a Request
    DescribeFleetLocationUtilization (..),
    newDescribeFleetLocationUtilization,

    -- * Request Lenses
    describeFleetLocationUtilization_fleetId,
    describeFleetLocationUtilization_location,

    -- * Destructuring the Response
    DescribeFleetLocationUtilizationResponse (..),
    newDescribeFleetLocationUtilizationResponse,

    -- * Response Lenses
    describeFleetLocationUtilizationResponse_fleetUtilization,
    describeFleetLocationUtilizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetLocationUtilization' smart constructor.
data DescribeFleetLocationUtilization = DescribeFleetLocationUtilization'
  { -- | A unique identifier for the fleet to request location utilization for.
    -- You can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | The fleet location to retrieve utilization information for. Specify a
    -- location in the form of an Amazon Web Services Region code, such as
    -- @us-west-2@.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetLocationUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeFleetLocationUtilization_fleetId' - A unique identifier for the fleet to request location utilization for.
-- You can use either the fleet ID or ARN value.
--
-- 'location', 'describeFleetLocationUtilization_location' - The fleet location to retrieve utilization information for. Specify a
-- location in the form of an Amazon Web Services Region code, such as
-- @us-west-2@.
newDescribeFleetLocationUtilization ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'location'
  Prelude.Text ->
  DescribeFleetLocationUtilization
newDescribeFleetLocationUtilization
  pFleetId_
  pLocation_ =
    DescribeFleetLocationUtilization'
      { fleetId =
          pFleetId_,
        location = pLocation_
      }

-- | A unique identifier for the fleet to request location utilization for.
-- You can use either the fleet ID or ARN value.
describeFleetLocationUtilization_fleetId :: Lens.Lens' DescribeFleetLocationUtilization Prelude.Text
describeFleetLocationUtilization_fleetId = Lens.lens (\DescribeFleetLocationUtilization' {fleetId} -> fleetId) (\s@DescribeFleetLocationUtilization' {} a -> s {fleetId = a} :: DescribeFleetLocationUtilization)

-- | The fleet location to retrieve utilization information for. Specify a
-- location in the form of an Amazon Web Services Region code, such as
-- @us-west-2@.
describeFleetLocationUtilization_location :: Lens.Lens' DescribeFleetLocationUtilization Prelude.Text
describeFleetLocationUtilization_location = Lens.lens (\DescribeFleetLocationUtilization' {location} -> location) (\s@DescribeFleetLocationUtilization' {} a -> s {location = a} :: DescribeFleetLocationUtilization)

instance
  Core.AWSRequest
    DescribeFleetLocationUtilization
  where
  type
    AWSResponse DescribeFleetLocationUtilization =
      DescribeFleetLocationUtilizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetLocationUtilizationResponse'
            Prelude.<$> (x Data..?> "FleetUtilization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFleetLocationUtilization
  where
  hashWithSalt
    _salt
    DescribeFleetLocationUtilization' {..} =
      _salt `Prelude.hashWithSalt` fleetId
        `Prelude.hashWithSalt` location

instance
  Prelude.NFData
    DescribeFleetLocationUtilization
  where
  rnf DescribeFleetLocationUtilization' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf location

instance
  Data.ToHeaders
    DescribeFleetLocationUtilization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeFleetLocationUtilization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFleetLocationUtilization where
  toJSON DescribeFleetLocationUtilization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("Location" Data..= location)
          ]
      )

instance Data.ToPath DescribeFleetLocationUtilization where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeFleetLocationUtilization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetLocationUtilizationResponse' smart constructor.
data DescribeFleetLocationUtilizationResponse = DescribeFleetLocationUtilizationResponse'
  { -- | Utilization information for the requested fleet location. Utilization
    -- objects are returned only for fleets and locations that currently exist.
    fleetUtilization :: Prelude.Maybe FleetUtilization,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetLocationUtilizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetUtilization', 'describeFleetLocationUtilizationResponse_fleetUtilization' - Utilization information for the requested fleet location. Utilization
-- objects are returned only for fleets and locations that currently exist.
--
-- 'httpStatus', 'describeFleetLocationUtilizationResponse_httpStatus' - The response's http status code.
newDescribeFleetLocationUtilizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetLocationUtilizationResponse
newDescribeFleetLocationUtilizationResponse
  pHttpStatus_ =
    DescribeFleetLocationUtilizationResponse'
      { fleetUtilization =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Utilization information for the requested fleet location. Utilization
-- objects are returned only for fleets and locations that currently exist.
describeFleetLocationUtilizationResponse_fleetUtilization :: Lens.Lens' DescribeFleetLocationUtilizationResponse (Prelude.Maybe FleetUtilization)
describeFleetLocationUtilizationResponse_fleetUtilization = Lens.lens (\DescribeFleetLocationUtilizationResponse' {fleetUtilization} -> fleetUtilization) (\s@DescribeFleetLocationUtilizationResponse' {} a -> s {fleetUtilization = a} :: DescribeFleetLocationUtilizationResponse)

-- | The response's http status code.
describeFleetLocationUtilizationResponse_httpStatus :: Lens.Lens' DescribeFleetLocationUtilizationResponse Prelude.Int
describeFleetLocationUtilizationResponse_httpStatus = Lens.lens (\DescribeFleetLocationUtilizationResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetLocationUtilizationResponse' {} a -> s {httpStatus = a} :: DescribeFleetLocationUtilizationResponse)

instance
  Prelude.NFData
    DescribeFleetLocationUtilizationResponse
  where
  rnf DescribeFleetLocationUtilizationResponse' {..} =
    Prelude.rnf fleetUtilization
      `Prelude.seq` Prelude.rnf httpStatus

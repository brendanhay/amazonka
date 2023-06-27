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
-- Module      : Amazonka.GameLift.DescribeFleetLocationCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource capacity settings for a fleet location. The data
-- returned includes the current capacity (number of EC2 instances) and
-- some scaling settings for the requested fleet location. Use this
-- operation to retrieve capacity information for a fleet\'s remote
-- location or home Region (you can also retrieve home Region capacity by
-- calling @DescribeFleetCapacity@).
--
-- To retrieve capacity data, identify a fleet and location.
--
-- If successful, a @FleetCapacity@ object is returned for the requested
-- fleet location.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up Amazon GameLift fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html#gamelift-metrics-fleet GameLift metrics for fleets>
module Amazonka.GameLift.DescribeFleetLocationCapacity
  ( -- * Creating a Request
    DescribeFleetLocationCapacity (..),
    newDescribeFleetLocationCapacity,

    -- * Request Lenses
    describeFleetLocationCapacity_fleetId,
    describeFleetLocationCapacity_location,

    -- * Destructuring the Response
    DescribeFleetLocationCapacityResponse (..),
    newDescribeFleetLocationCapacityResponse,

    -- * Response Lenses
    describeFleetLocationCapacityResponse_fleetCapacity,
    describeFleetLocationCapacityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetLocationCapacity' smart constructor.
data DescribeFleetLocationCapacity = DescribeFleetLocationCapacity'
  { -- | A unique identifier for the fleet to request location capacity for. You
    -- can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | The fleet location to retrieve capacity information for. Specify a
    -- location in the form of an Amazon Web Services Region code, such as
    -- @us-west-2@.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetLocationCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeFleetLocationCapacity_fleetId' - A unique identifier for the fleet to request location capacity for. You
-- can use either the fleet ID or ARN value.
--
-- 'location', 'describeFleetLocationCapacity_location' - The fleet location to retrieve capacity information for. Specify a
-- location in the form of an Amazon Web Services Region code, such as
-- @us-west-2@.
newDescribeFleetLocationCapacity ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'location'
  Prelude.Text ->
  DescribeFleetLocationCapacity
newDescribeFleetLocationCapacity pFleetId_ pLocation_ =
  DescribeFleetLocationCapacity'
    { fleetId = pFleetId_,
      location = pLocation_
    }

-- | A unique identifier for the fleet to request location capacity for. You
-- can use either the fleet ID or ARN value.
describeFleetLocationCapacity_fleetId :: Lens.Lens' DescribeFleetLocationCapacity Prelude.Text
describeFleetLocationCapacity_fleetId = Lens.lens (\DescribeFleetLocationCapacity' {fleetId} -> fleetId) (\s@DescribeFleetLocationCapacity' {} a -> s {fleetId = a} :: DescribeFleetLocationCapacity)

-- | The fleet location to retrieve capacity information for. Specify a
-- location in the form of an Amazon Web Services Region code, such as
-- @us-west-2@.
describeFleetLocationCapacity_location :: Lens.Lens' DescribeFleetLocationCapacity Prelude.Text
describeFleetLocationCapacity_location = Lens.lens (\DescribeFleetLocationCapacity' {location} -> location) (\s@DescribeFleetLocationCapacity' {} a -> s {location = a} :: DescribeFleetLocationCapacity)

instance
  Core.AWSRequest
    DescribeFleetLocationCapacity
  where
  type
    AWSResponse DescribeFleetLocationCapacity =
      DescribeFleetLocationCapacityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetLocationCapacityResponse'
            Prelude.<$> (x Data..?> "FleetCapacity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFleetLocationCapacity
  where
  hashWithSalt _salt DescribeFleetLocationCapacity' {..} =
    _salt
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` location

instance Prelude.NFData DescribeFleetLocationCapacity where
  rnf DescribeFleetLocationCapacity' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf location

instance Data.ToHeaders DescribeFleetLocationCapacity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeFleetLocationCapacity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFleetLocationCapacity where
  toJSON DescribeFleetLocationCapacity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("Location" Data..= location)
          ]
      )

instance Data.ToPath DescribeFleetLocationCapacity where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetLocationCapacity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetLocationCapacityResponse' smart constructor.
data DescribeFleetLocationCapacityResponse = DescribeFleetLocationCapacityResponse'
  { -- | Resource capacity information for the requested fleet location. Capacity
    -- objects are returned only for fleets and locations that currently exist.
    fleetCapacity :: Prelude.Maybe FleetCapacity,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetLocationCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetCapacity', 'describeFleetLocationCapacityResponse_fleetCapacity' - Resource capacity information for the requested fleet location. Capacity
-- objects are returned only for fleets and locations that currently exist.
--
-- 'httpStatus', 'describeFleetLocationCapacityResponse_httpStatus' - The response's http status code.
newDescribeFleetLocationCapacityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetLocationCapacityResponse
newDescribeFleetLocationCapacityResponse pHttpStatus_ =
  DescribeFleetLocationCapacityResponse'
    { fleetCapacity =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Resource capacity information for the requested fleet location. Capacity
-- objects are returned only for fleets and locations that currently exist.
describeFleetLocationCapacityResponse_fleetCapacity :: Lens.Lens' DescribeFleetLocationCapacityResponse (Prelude.Maybe FleetCapacity)
describeFleetLocationCapacityResponse_fleetCapacity = Lens.lens (\DescribeFleetLocationCapacityResponse' {fleetCapacity} -> fleetCapacity) (\s@DescribeFleetLocationCapacityResponse' {} a -> s {fleetCapacity = a} :: DescribeFleetLocationCapacityResponse)

-- | The response's http status code.
describeFleetLocationCapacityResponse_httpStatus :: Lens.Lens' DescribeFleetLocationCapacityResponse Prelude.Int
describeFleetLocationCapacityResponse_httpStatus = Lens.lens (\DescribeFleetLocationCapacityResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetLocationCapacityResponse' {} a -> s {httpStatus = a} :: DescribeFleetLocationCapacityResponse)

instance
  Prelude.NFData
    DescribeFleetLocationCapacityResponse
  where
  rnf DescribeFleetLocationCapacityResponse' {..} =
    Prelude.rnf fleetCapacity
      `Prelude.seq` Prelude.rnf httpStatus

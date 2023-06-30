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
-- Module      : Amazonka.GameLift.DescribeFleetCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource capacity settings for one or more fleets. The
-- data returned includes the current fleet capacity (number of EC2
-- instances), and settings that can control how capacity scaling. For
-- fleets with remote locations, this operation retrieves data for the
-- fleet\'s home Region only.
--
-- This operation can be used in the following ways:
--
-- -   To get capacity data for one or more specific fleets, provide a list
--     of fleet IDs or fleet ARNs.
--
-- -   To get capacity data for all fleets, do not provide a fleet
--     identifier.
--
-- When requesting multiple fleets, use the pagination parameters to
-- retrieve results as a set of sequential pages.
--
-- If successful, a @FleetCapacity@ object is returned for each requested
-- fleet ID. Each FleetCapacity object includes a @Location@ property,
-- which is set to the fleet\'s home Region. When a list of fleet IDs is
-- provided, attribute objects are returned only for fleets that currently
-- exist.
--
-- Some API operations may limit the number of fleet IDs that are allowed
-- in one request. If a request exceeds this limit, the request fails and
-- the error message includes the maximum allowed.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html#gamelift-metrics-fleet GameLift metrics for fleets>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeFleetCapacity
  ( -- * Creating a Request
    DescribeFleetCapacity (..),
    newDescribeFleetCapacity,

    -- * Request Lenses
    describeFleetCapacity_fleetIds,
    describeFleetCapacity_limit,
    describeFleetCapacity_nextToken,

    -- * Destructuring the Response
    DescribeFleetCapacityResponse (..),
    newDescribeFleetCapacityResponse,

    -- * Response Lenses
    describeFleetCapacityResponse_fleetCapacity,
    describeFleetCapacityResponse_nextToken,
    describeFleetCapacityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetCapacity' smart constructor.
data DescribeFleetCapacity = DescribeFleetCapacity'
  { -- | A unique identifier for the fleet to retrieve capacity information for.
    -- You can use either the fleet ID or ARN value. Leave this parameter empty
    -- to retrieve capacity information for all fleets.
    fleetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. This parameter
    -- is ignored when the request specifies one or a list of fleet IDs.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    -- This parameter is ignored when the request specifies one or a list of
    -- fleet IDs.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetIds', 'describeFleetCapacity_fleetIds' - A unique identifier for the fleet to retrieve capacity information for.
-- You can use either the fleet ID or ARN value. Leave this parameter empty
-- to retrieve capacity information for all fleets.
--
-- 'limit', 'describeFleetCapacity_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
--
-- 'nextToken', 'describeFleetCapacity_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
newDescribeFleetCapacity ::
  DescribeFleetCapacity
newDescribeFleetCapacity =
  DescribeFleetCapacity'
    { fleetIds = Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A unique identifier for the fleet to retrieve capacity information for.
-- You can use either the fleet ID or ARN value. Leave this parameter empty
-- to retrieve capacity information for all fleets.
describeFleetCapacity_fleetIds :: Lens.Lens' DescribeFleetCapacity (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeFleetCapacity_fleetIds = Lens.lens (\DescribeFleetCapacity' {fleetIds} -> fleetIds) (\s@DescribeFleetCapacity' {} a -> s {fleetIds = a} :: DescribeFleetCapacity) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
describeFleetCapacity_limit :: Lens.Lens' DescribeFleetCapacity (Prelude.Maybe Prelude.Natural)
describeFleetCapacity_limit = Lens.lens (\DescribeFleetCapacity' {limit} -> limit) (\s@DescribeFleetCapacity' {} a -> s {limit = a} :: DescribeFleetCapacity)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
describeFleetCapacity_nextToken :: Lens.Lens' DescribeFleetCapacity (Prelude.Maybe Prelude.Text)
describeFleetCapacity_nextToken = Lens.lens (\DescribeFleetCapacity' {nextToken} -> nextToken) (\s@DescribeFleetCapacity' {} a -> s {nextToken = a} :: DescribeFleetCapacity)

instance Core.AWSPager DescribeFleetCapacity where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetCapacityResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetCapacityResponse_fleetCapacity
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeFleetCapacity_nextToken
          Lens..~ rs
          Lens.^? describeFleetCapacityResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeFleetCapacity where
  type
    AWSResponse DescribeFleetCapacity =
      DescribeFleetCapacityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetCapacityResponse'
            Prelude.<$> (x Data..?> "FleetCapacity" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetCapacity where
  hashWithSalt _salt DescribeFleetCapacity' {..} =
    _salt
      `Prelude.hashWithSalt` fleetIds
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFleetCapacity where
  rnf DescribeFleetCapacity' {..} =
    Prelude.rnf fleetIds
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFleetCapacity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeFleetCapacity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFleetCapacity where
  toJSON DescribeFleetCapacity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FleetIds" Data..=) Prelude.<$> fleetIds,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeFleetCapacity where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetCapacity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetCapacityResponse' smart constructor.
data DescribeFleetCapacityResponse = DescribeFleetCapacityResponse'
  { -- | A collection of objects that contains capacity information for each
    -- requested fleet ID. Capacity objects are returned only for fleets that
    -- currently exist.
    fleetCapacity :: Prelude.Maybe [FleetCapacity],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetCapacity', 'describeFleetCapacityResponse_fleetCapacity' - A collection of objects that contains capacity information for each
-- requested fleet ID. Capacity objects are returned only for fleets that
-- currently exist.
--
-- 'nextToken', 'describeFleetCapacityResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'describeFleetCapacityResponse_httpStatus' - The response's http status code.
newDescribeFleetCapacityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetCapacityResponse
newDescribeFleetCapacityResponse pHttpStatus_ =
  DescribeFleetCapacityResponse'
    { fleetCapacity =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of objects that contains capacity information for each
-- requested fleet ID. Capacity objects are returned only for fleets that
-- currently exist.
describeFleetCapacityResponse_fleetCapacity :: Lens.Lens' DescribeFleetCapacityResponse (Prelude.Maybe [FleetCapacity])
describeFleetCapacityResponse_fleetCapacity = Lens.lens (\DescribeFleetCapacityResponse' {fleetCapacity} -> fleetCapacity) (\s@DescribeFleetCapacityResponse' {} a -> s {fleetCapacity = a} :: DescribeFleetCapacityResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeFleetCapacityResponse_nextToken :: Lens.Lens' DescribeFleetCapacityResponse (Prelude.Maybe Prelude.Text)
describeFleetCapacityResponse_nextToken = Lens.lens (\DescribeFleetCapacityResponse' {nextToken} -> nextToken) (\s@DescribeFleetCapacityResponse' {} a -> s {nextToken = a} :: DescribeFleetCapacityResponse)

-- | The response's http status code.
describeFleetCapacityResponse_httpStatus :: Lens.Lens' DescribeFleetCapacityResponse Prelude.Int
describeFleetCapacityResponse_httpStatus = Lens.lens (\DescribeFleetCapacityResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetCapacityResponse' {} a -> s {httpStatus = a} :: DescribeFleetCapacityResponse)

instance Prelude.NFData DescribeFleetCapacityResponse where
  rnf DescribeFleetCapacityResponse' {..} =
    Prelude.rnf fleetCapacity
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.GameLift.DescribeFleetUtilization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves utilization statistics for one or more fleets. Utilization
-- data provides a snapshot of how the fleet\'s hosting resources are
-- currently being used. For fleets with remote locations, this operation
-- retrieves data for the fleet\'s home Region only. See
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeFleetLocationUtilization.html DescribeFleetLocationUtilization>
-- to get utilization statistics for a fleet\'s remote locations.
--
-- This operation can be used in the following ways:
--
-- -   To get utilization data for one or more specific fleets, provide a
--     list of fleet IDs or fleet ARNs.
--
-- -   To get utilization data for all fleets, do not provide a fleet
--     identifier.
--
-- When requesting multiple fleets, use the pagination parameters to
-- retrieve results as a set of sequential pages.
--
-- If successful, a
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_FleetUtilization.html FleetUtilization>
-- object is returned for each requested fleet ID, unless the fleet
-- identifier is not found. Each fleet utilization object includes a
-- @Location@ property, which is set to the fleet\'s home Region.
--
-- Some API operations may limit the number of fleet IDs allowed in one
-- request. If a request exceeds this limit, the request fails and the
-- error message includes the maximum allowed.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html#gamelift-metrics-fleet GameLift Metrics for Fleets>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeFleetUtilization
  ( -- * Creating a Request
    DescribeFleetUtilization (..),
    newDescribeFleetUtilization,

    -- * Request Lenses
    describeFleetUtilization_fleetIds,
    describeFleetUtilization_limit,
    describeFleetUtilization_nextToken,

    -- * Destructuring the Response
    DescribeFleetUtilizationResponse (..),
    newDescribeFleetUtilizationResponse,

    -- * Response Lenses
    describeFleetUtilizationResponse_fleetUtilization,
    describeFleetUtilizationResponse_nextToken,
    describeFleetUtilizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetUtilization' smart constructor.
data DescribeFleetUtilization = DescribeFleetUtilization'
  { -- | A unique identifier for the fleet to retrieve utilization data for. You
    -- can use either the fleet ID or ARN value. To retrieve attributes for all
    -- current fleets, do not include this parameter.
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
-- Create a value of 'DescribeFleetUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetIds', 'describeFleetUtilization_fleetIds' - A unique identifier for the fleet to retrieve utilization data for. You
-- can use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter.
--
-- 'limit', 'describeFleetUtilization_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
--
-- 'nextToken', 'describeFleetUtilization_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
newDescribeFleetUtilization ::
  DescribeFleetUtilization
newDescribeFleetUtilization =
  DescribeFleetUtilization'
    { fleetIds =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A unique identifier for the fleet to retrieve utilization data for. You
-- can use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter.
describeFleetUtilization_fleetIds :: Lens.Lens' DescribeFleetUtilization (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeFleetUtilization_fleetIds = Lens.lens (\DescribeFleetUtilization' {fleetIds} -> fleetIds) (\s@DescribeFleetUtilization' {} a -> s {fleetIds = a} :: DescribeFleetUtilization) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
describeFleetUtilization_limit :: Lens.Lens' DescribeFleetUtilization (Prelude.Maybe Prelude.Natural)
describeFleetUtilization_limit = Lens.lens (\DescribeFleetUtilization' {limit} -> limit) (\s@DescribeFleetUtilization' {} a -> s {limit = a} :: DescribeFleetUtilization)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
describeFleetUtilization_nextToken :: Lens.Lens' DescribeFleetUtilization (Prelude.Maybe Prelude.Text)
describeFleetUtilization_nextToken = Lens.lens (\DescribeFleetUtilization' {nextToken} -> nextToken) (\s@DescribeFleetUtilization' {} a -> s {nextToken = a} :: DescribeFleetUtilization)

instance Core.AWSPager DescribeFleetUtilization where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetUtilizationResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetUtilizationResponse_fleetUtilization
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeFleetUtilization_nextToken
          Lens..~ rs
          Lens.^? describeFleetUtilizationResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeFleetUtilization where
  type
    AWSResponse DescribeFleetUtilization =
      DescribeFleetUtilizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetUtilizationResponse'
            Prelude.<$> ( x
                            Data..?> "FleetUtilization"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetUtilization where
  hashWithSalt _salt DescribeFleetUtilization' {..} =
    _salt
      `Prelude.hashWithSalt` fleetIds
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFleetUtilization where
  rnf DescribeFleetUtilization' {..} =
    Prelude.rnf fleetIds
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFleetUtilization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeFleetUtilization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFleetUtilization where
  toJSON DescribeFleetUtilization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FleetIds" Data..=) Prelude.<$> fleetIds,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeFleetUtilization where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetUtilization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetUtilizationResponse' smart constructor.
data DescribeFleetUtilizationResponse = DescribeFleetUtilizationResponse'
  { -- | A collection of objects containing utilization information for each
    -- requested fleet ID. Utilization objects are returned only for fleets
    -- that currently exist.
    fleetUtilization :: Prelude.Maybe [FleetUtilization],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetUtilizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetUtilization', 'describeFleetUtilizationResponse_fleetUtilization' - A collection of objects containing utilization information for each
-- requested fleet ID. Utilization objects are returned only for fleets
-- that currently exist.
--
-- 'nextToken', 'describeFleetUtilizationResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'describeFleetUtilizationResponse_httpStatus' - The response's http status code.
newDescribeFleetUtilizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetUtilizationResponse
newDescribeFleetUtilizationResponse pHttpStatus_ =
  DescribeFleetUtilizationResponse'
    { fleetUtilization =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of objects containing utilization information for each
-- requested fleet ID. Utilization objects are returned only for fleets
-- that currently exist.
describeFleetUtilizationResponse_fleetUtilization :: Lens.Lens' DescribeFleetUtilizationResponse (Prelude.Maybe [FleetUtilization])
describeFleetUtilizationResponse_fleetUtilization = Lens.lens (\DescribeFleetUtilizationResponse' {fleetUtilization} -> fleetUtilization) (\s@DescribeFleetUtilizationResponse' {} a -> s {fleetUtilization = a} :: DescribeFleetUtilizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeFleetUtilizationResponse_nextToken :: Lens.Lens' DescribeFleetUtilizationResponse (Prelude.Maybe Prelude.Text)
describeFleetUtilizationResponse_nextToken = Lens.lens (\DescribeFleetUtilizationResponse' {nextToken} -> nextToken) (\s@DescribeFleetUtilizationResponse' {} a -> s {nextToken = a} :: DescribeFleetUtilizationResponse)

-- | The response's http status code.
describeFleetUtilizationResponse_httpStatus :: Lens.Lens' DescribeFleetUtilizationResponse Prelude.Int
describeFleetUtilizationResponse_httpStatus = Lens.lens (\DescribeFleetUtilizationResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetUtilizationResponse' {} a -> s {httpStatus = a} :: DescribeFleetUtilizationResponse)

instance
  Prelude.NFData
    DescribeFleetUtilizationResponse
  where
  rnf DescribeFleetUtilizationResponse' {..} =
    Prelude.rnf fleetUtilization
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

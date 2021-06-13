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
-- Module      : Network.AWS.GameLift.DescribeFleetUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves utilization statistics for one or more fleets. These
-- statistics provide insight into how available hosting resources are
-- currently being used. To get statistics on available hosting resources,
-- see DescribeFleetCapacity.
--
-- You can request utilization data for all fleets, or specify a list of
-- one or more fleet IDs. When requesting multiple fleets, use the
-- pagination parameters to retrieve results as a set of sequential pages.
-- If successful, a FleetUtilization object is returned for each requested
-- fleet ID, unless the fleet identifier is not found.
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
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetUtilization
  ( -- * Creating a Request
    DescribeFleetUtilization (..),
    newDescribeFleetUtilization,

    -- * Request Lenses
    describeFleetUtilization_nextToken,
    describeFleetUtilization_fleetIds,
    describeFleetUtilization_limit,

    -- * Destructuring the Response
    DescribeFleetUtilizationResponse (..),
    newDescribeFleetUtilizationResponse,

    -- * Response Lenses
    describeFleetUtilizationResponse_nextToken,
    describeFleetUtilizationResponse_fleetUtilization,
    describeFleetUtilizationResponse_httpStatus,
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
-- /See:/ 'newDescribeFleetUtilization' smart constructor.
data DescribeFleetUtilization = DescribeFleetUtilization'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    -- This parameter is ignored when the request specifies one or a list of
    -- fleet IDs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a fleet(s) to retrieve utilization data for. You
    -- can use either the fleet ID or ARN value. To retrieve attributes for all
    -- current fleets, do not include this parameter. If the list of fleet
    -- identifiers includes fleets that don\'t currently exist, the request
    -- succeeds but no attributes for that fleet are returned.
    fleetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. This parameter
    -- is ignored when the request specifies one or a list of fleet IDs.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeFleetUtilization_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
--
-- 'fleetIds', 'describeFleetUtilization_fleetIds' - A unique identifier for a fleet(s) to retrieve utilization data for. You
-- can use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter. If the list of fleet
-- identifiers includes fleets that don\'t currently exist, the request
-- succeeds but no attributes for that fleet are returned.
--
-- 'limit', 'describeFleetUtilization_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
newDescribeFleetUtilization ::
  DescribeFleetUtilization
newDescribeFleetUtilization =
  DescribeFleetUtilization'
    { nextToken =
        Prelude.Nothing,
      fleetIds = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
describeFleetUtilization_nextToken :: Lens.Lens' DescribeFleetUtilization (Prelude.Maybe Prelude.Text)
describeFleetUtilization_nextToken = Lens.lens (\DescribeFleetUtilization' {nextToken} -> nextToken) (\s@DescribeFleetUtilization' {} a -> s {nextToken = a} :: DescribeFleetUtilization)

-- | A unique identifier for a fleet(s) to retrieve utilization data for. You
-- can use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter. If the list of fleet
-- identifiers includes fleets that don\'t currently exist, the request
-- succeeds but no attributes for that fleet are returned.
describeFleetUtilization_fleetIds :: Lens.Lens' DescribeFleetUtilization (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeFleetUtilization_fleetIds = Lens.lens (\DescribeFleetUtilization' {fleetIds} -> fleetIds) (\s@DescribeFleetUtilization' {} a -> s {fleetIds = a} :: DescribeFleetUtilization) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
describeFleetUtilization_limit :: Lens.Lens' DescribeFleetUtilization (Prelude.Maybe Prelude.Natural)
describeFleetUtilization_limit = Lens.lens (\DescribeFleetUtilization' {limit} -> limit) (\s@DescribeFleetUtilization' {} a -> s {limit = a} :: DescribeFleetUtilization)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFleetUtilization_nextToken
          Lens..~ rs
          Lens.^? describeFleetUtilizationResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFleetUtilization where
  type
    AWSResponse DescribeFleetUtilization =
      DescribeFleetUtilizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetUtilizationResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "FleetUtilization"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetUtilization

instance Prelude.NFData DescribeFleetUtilization

instance Core.ToHeaders DescribeFleetUtilization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeFleetUtilization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFleetUtilization where
  toJSON DescribeFleetUtilization' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("FleetIds" Core..=) Prelude.<$> fleetIds,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeFleetUtilization where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFleetUtilization where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetUtilizationResponse' smart constructor.
data DescribeFleetUtilizationResponse = DescribeFleetUtilizationResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing utilization information for each
    -- requested fleet ID.
    fleetUtilization :: Prelude.Maybe [FleetUtilization],
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
-- 'nextToken', 'describeFleetUtilizationResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'fleetUtilization', 'describeFleetUtilizationResponse_fleetUtilization' - A collection of objects containing utilization information for each
-- requested fleet ID.
--
-- 'httpStatus', 'describeFleetUtilizationResponse_httpStatus' - The response's http status code.
newDescribeFleetUtilizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetUtilizationResponse
newDescribeFleetUtilizationResponse pHttpStatus_ =
  DescribeFleetUtilizationResponse'
    { nextToken =
        Prelude.Nothing,
      fleetUtilization = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeFleetUtilizationResponse_nextToken :: Lens.Lens' DescribeFleetUtilizationResponse (Prelude.Maybe Prelude.Text)
describeFleetUtilizationResponse_nextToken = Lens.lens (\DescribeFleetUtilizationResponse' {nextToken} -> nextToken) (\s@DescribeFleetUtilizationResponse' {} a -> s {nextToken = a} :: DescribeFleetUtilizationResponse)

-- | A collection of objects containing utilization information for each
-- requested fleet ID.
describeFleetUtilizationResponse_fleetUtilization :: Lens.Lens' DescribeFleetUtilizationResponse (Prelude.Maybe [FleetUtilization])
describeFleetUtilizationResponse_fleetUtilization = Lens.lens (\DescribeFleetUtilizationResponse' {fleetUtilization} -> fleetUtilization) (\s@DescribeFleetUtilizationResponse' {} a -> s {fleetUtilization = a} :: DescribeFleetUtilizationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetUtilizationResponse_httpStatus :: Lens.Lens' DescribeFleetUtilizationResponse Prelude.Int
describeFleetUtilizationResponse_httpStatus = Lens.lens (\DescribeFleetUtilizationResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetUtilizationResponse' {} a -> s {httpStatus = a} :: DescribeFleetUtilizationResponse)

instance
  Prelude.NFData
    DescribeFleetUtilizationResponse

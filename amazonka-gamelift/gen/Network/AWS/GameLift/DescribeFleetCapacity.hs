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
-- Module      : Network.AWS.GameLift.DescribeFleetCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current capacity statistics for one or more fleets. These
-- statistics present a snapshot of the fleet\'s instances and provide
-- insight on current or imminent scaling activity. To get statistics on
-- game hosting activity in the fleet, see DescribeFleetUtilization.
--
-- You can request capacity for all fleets or specify a list of one or more
-- fleet identifiers. When requesting multiple fleets, use the pagination
-- parameters to retrieve results as a set of sequential pages. If
-- successful, a FleetCapacity object is returned for each requested fleet
-- ID. When a list of fleet IDs is provided, attribute objects are returned
-- only for fleets that currently exist.
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
module Network.AWS.GameLift.DescribeFleetCapacity
  ( -- * Creating a Request
    DescribeFleetCapacity (..),
    newDescribeFleetCapacity,

    -- * Request Lenses
    describeFleetCapacity_nextToken,
    describeFleetCapacity_fleetIds,
    describeFleetCapacity_limit,

    -- * Destructuring the Response
    DescribeFleetCapacityResponse (..),
    newDescribeFleetCapacityResponse,

    -- * Response Lenses
    describeFleetCapacityResponse_nextToken,
    describeFleetCapacityResponse_fleetCapacity,
    describeFleetCapacityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeFleetCapacity' smart constructor.
data DescribeFleetCapacity = DescribeFleetCapacity'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    -- This parameter is ignored when the request specifies one or a list of
    -- fleet IDs.
    nextToken :: Core.Maybe Core.Text,
    -- | A unique identifier for a fleet(s) to retrieve capacity information for.
    -- You can use either the fleet ID or ARN value.
    fleetIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. This parameter
    -- is ignored when the request specifies one or a list of fleet IDs.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetCapacity_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
--
-- 'fleetIds', 'describeFleetCapacity_fleetIds' - A unique identifier for a fleet(s) to retrieve capacity information for.
-- You can use either the fleet ID or ARN value.
--
-- 'limit', 'describeFleetCapacity_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
newDescribeFleetCapacity ::
  DescribeFleetCapacity
newDescribeFleetCapacity =
  DescribeFleetCapacity'
    { nextToken = Core.Nothing,
      fleetIds = Core.Nothing,
      limit = Core.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
describeFleetCapacity_nextToken :: Lens.Lens' DescribeFleetCapacity (Core.Maybe Core.Text)
describeFleetCapacity_nextToken = Lens.lens (\DescribeFleetCapacity' {nextToken} -> nextToken) (\s@DescribeFleetCapacity' {} a -> s {nextToken = a} :: DescribeFleetCapacity)

-- | A unique identifier for a fleet(s) to retrieve capacity information for.
-- You can use either the fleet ID or ARN value.
describeFleetCapacity_fleetIds :: Lens.Lens' DescribeFleetCapacity (Core.Maybe (Core.NonEmpty Core.Text))
describeFleetCapacity_fleetIds = Lens.lens (\DescribeFleetCapacity' {fleetIds} -> fleetIds) (\s@DescribeFleetCapacity' {} a -> s {fleetIds = a} :: DescribeFleetCapacity) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
describeFleetCapacity_limit :: Lens.Lens' DescribeFleetCapacity (Core.Maybe Core.Natural)
describeFleetCapacity_limit = Lens.lens (\DescribeFleetCapacity' {limit} -> limit) (\s@DescribeFleetCapacity' {} a -> s {limit = a} :: DescribeFleetCapacity)

instance Core.AWSPager DescribeFleetCapacity where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetCapacityResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetCapacityResponse_fleetCapacity
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeFleetCapacity_nextToken
          Lens..~ rs
          Lens.^? describeFleetCapacityResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeFleetCapacity where
  type
    AWSResponse DescribeFleetCapacity =
      DescribeFleetCapacityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetCapacityResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "FleetCapacity" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFleetCapacity

instance Core.NFData DescribeFleetCapacity

instance Core.ToHeaders DescribeFleetCapacity where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeFleetCapacity" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeFleetCapacity where
  toJSON DescribeFleetCapacity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("FleetIds" Core..=) Core.<$> fleetIds,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeFleetCapacity where
  toPath = Core.const "/"

instance Core.ToQuery DescribeFleetCapacity where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetCapacityResponse' smart constructor.
data DescribeFleetCapacityResponse = DescribeFleetCapacityResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of objects containing capacity information for each
    -- requested fleet ID. Leave this parameter empty to retrieve capacity
    -- information for all fleets.
    fleetCapacity :: Core.Maybe [FleetCapacity],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetCapacityResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'fleetCapacity', 'describeFleetCapacityResponse_fleetCapacity' - A collection of objects containing capacity information for each
-- requested fleet ID. Leave this parameter empty to retrieve capacity
-- information for all fleets.
--
-- 'httpStatus', 'describeFleetCapacityResponse_httpStatus' - The response's http status code.
newDescribeFleetCapacityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeFleetCapacityResponse
newDescribeFleetCapacityResponse pHttpStatus_ =
  DescribeFleetCapacityResponse'
    { nextToken =
        Core.Nothing,
      fleetCapacity = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeFleetCapacityResponse_nextToken :: Lens.Lens' DescribeFleetCapacityResponse (Core.Maybe Core.Text)
describeFleetCapacityResponse_nextToken = Lens.lens (\DescribeFleetCapacityResponse' {nextToken} -> nextToken) (\s@DescribeFleetCapacityResponse' {} a -> s {nextToken = a} :: DescribeFleetCapacityResponse)

-- | A collection of objects containing capacity information for each
-- requested fleet ID. Leave this parameter empty to retrieve capacity
-- information for all fleets.
describeFleetCapacityResponse_fleetCapacity :: Lens.Lens' DescribeFleetCapacityResponse (Core.Maybe [FleetCapacity])
describeFleetCapacityResponse_fleetCapacity = Lens.lens (\DescribeFleetCapacityResponse' {fleetCapacity} -> fleetCapacity) (\s@DescribeFleetCapacityResponse' {} a -> s {fleetCapacity = a} :: DescribeFleetCapacityResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetCapacityResponse_httpStatus :: Lens.Lens' DescribeFleetCapacityResponse Core.Int
describeFleetCapacityResponse_httpStatus = Lens.lens (\DescribeFleetCapacityResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetCapacityResponse' {} a -> s {httpStatus = a} :: DescribeFleetCapacityResponse)

instance Core.NFData DescribeFleetCapacityResponse

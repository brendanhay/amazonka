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
-- Module      : Network.AWS.GameLift.DescribeFleetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves core properties, including configuration, status, and
-- metadata, for a fleet.
--
-- To get attributes for one or more fleets, provide a list of fleet IDs or
-- fleet ARNs. To get attributes for all fleets, do not specify a fleet
-- identifier. When requesting attributes for multiple fleets, use the
-- pagination parameters to retrieve results as a set of sequential pages.
-- If successful, a FleetAttributes object is returned for each fleet
-- requested, unless the fleet identifier is not found.
--
-- Some API operations may limit the number of fleet IDs allowed in one
-- request. If a request exceeds this limit, the request fails and the
-- error message includes the maximum allowed number.
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
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetAttributes
  ( -- * Creating a Request
    DescribeFleetAttributes (..),
    newDescribeFleetAttributes,

    -- * Request Lenses
    describeFleetAttributes_nextToken,
    describeFleetAttributes_fleetIds,
    describeFleetAttributes_limit,

    -- * Destructuring the Response
    DescribeFleetAttributesResponse (..),
    newDescribeFleetAttributesResponse,

    -- * Response Lenses
    describeFleetAttributesResponse_nextToken,
    describeFleetAttributesResponse_fleetAttributes,
    describeFleetAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeFleetAttributes' smart constructor.
data DescribeFleetAttributes = DescribeFleetAttributes'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    -- This parameter is ignored when the request specifies one or a list of
    -- fleet IDs.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of unique fleet identifiers to retrieve attributes for. You can
    -- use either the fleet ID or ARN value. To retrieve attributes for all
    -- current fleets, do not include this parameter. If the list of fleet
    -- identifiers includes fleets that don\'t currently exist, the request
    -- succeeds but no attributes for that fleet are returned.
    fleetIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. This parameter
    -- is ignored when the request specifies one or a list of fleet IDs.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetAttributes_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
--
-- 'fleetIds', 'describeFleetAttributes_fleetIds' - A list of unique fleet identifiers to retrieve attributes for. You can
-- use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter. If the list of fleet
-- identifiers includes fleets that don\'t currently exist, the request
-- succeeds but no attributes for that fleet are returned.
--
-- 'limit', 'describeFleetAttributes_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
newDescribeFleetAttributes ::
  DescribeFleetAttributes
newDescribeFleetAttributes =
  DescribeFleetAttributes'
    { nextToken = Core.Nothing,
      fleetIds = Core.Nothing,
      limit = Core.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
describeFleetAttributes_nextToken :: Lens.Lens' DescribeFleetAttributes (Core.Maybe Core.Text)
describeFleetAttributes_nextToken = Lens.lens (\DescribeFleetAttributes' {nextToken} -> nextToken) (\s@DescribeFleetAttributes' {} a -> s {nextToken = a} :: DescribeFleetAttributes)

-- | A list of unique fleet identifiers to retrieve attributes for. You can
-- use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter. If the list of fleet
-- identifiers includes fleets that don\'t currently exist, the request
-- succeeds but no attributes for that fleet are returned.
describeFleetAttributes_fleetIds :: Lens.Lens' DescribeFleetAttributes (Core.Maybe (Core.NonEmpty Core.Text))
describeFleetAttributes_fleetIds = Lens.lens (\DescribeFleetAttributes' {fleetIds} -> fleetIds) (\s@DescribeFleetAttributes' {} a -> s {fleetIds = a} :: DescribeFleetAttributes) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
describeFleetAttributes_limit :: Lens.Lens' DescribeFleetAttributes (Core.Maybe Core.Natural)
describeFleetAttributes_limit = Lens.lens (\DescribeFleetAttributes' {limit} -> limit) (\s@DescribeFleetAttributes' {} a -> s {limit = a} :: DescribeFleetAttributes)

instance Core.AWSPager DescribeFleetAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetAttributesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetAttributesResponse_fleetAttributes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeFleetAttributes_nextToken
          Lens..~ rs
          Lens.^? describeFleetAttributesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeFleetAttributes where
  type
    AWSResponse DescribeFleetAttributes =
      DescribeFleetAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetAttributesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "FleetAttributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFleetAttributes

instance Core.NFData DescribeFleetAttributes

instance Core.ToHeaders DescribeFleetAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeFleetAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeFleetAttributes where
  toJSON DescribeFleetAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("FleetIds" Core..=) Core.<$> fleetIds,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeFleetAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeFleetAttributes where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetAttributesResponse' smart constructor.
data DescribeFleetAttributesResponse = DescribeFleetAttributesResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of objects containing attribute metadata for each requested
    -- fleet ID. Attribute objects are returned only for fleets that currently
    -- exist.
    fleetAttributes :: Core.Maybe [FleetAttributes],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetAttributesResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'fleetAttributes', 'describeFleetAttributesResponse_fleetAttributes' - A collection of objects containing attribute metadata for each requested
-- fleet ID. Attribute objects are returned only for fleets that currently
-- exist.
--
-- 'httpStatus', 'describeFleetAttributesResponse_httpStatus' - The response's http status code.
newDescribeFleetAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeFleetAttributesResponse
newDescribeFleetAttributesResponse pHttpStatus_ =
  DescribeFleetAttributesResponse'
    { nextToken =
        Core.Nothing,
      fleetAttributes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeFleetAttributesResponse_nextToken :: Lens.Lens' DescribeFleetAttributesResponse (Core.Maybe Core.Text)
describeFleetAttributesResponse_nextToken = Lens.lens (\DescribeFleetAttributesResponse' {nextToken} -> nextToken) (\s@DescribeFleetAttributesResponse' {} a -> s {nextToken = a} :: DescribeFleetAttributesResponse)

-- | A collection of objects containing attribute metadata for each requested
-- fleet ID. Attribute objects are returned only for fleets that currently
-- exist.
describeFleetAttributesResponse_fleetAttributes :: Lens.Lens' DescribeFleetAttributesResponse (Core.Maybe [FleetAttributes])
describeFleetAttributesResponse_fleetAttributes = Lens.lens (\DescribeFleetAttributesResponse' {fleetAttributes} -> fleetAttributes) (\s@DescribeFleetAttributesResponse' {} a -> s {fleetAttributes = a} :: DescribeFleetAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetAttributesResponse_httpStatus :: Lens.Lens' DescribeFleetAttributesResponse Core.Int
describeFleetAttributesResponse_httpStatus = Lens.lens (\DescribeFleetAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetAttributesResponse' {} a -> s {httpStatus = a} :: DescribeFleetAttributesResponse)

instance Core.NFData DescribeFleetAttributesResponse

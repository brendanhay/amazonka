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
import qualified Network.AWS.Prelude as Prelude
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of unique fleet identifiers to retrieve attributes for. You can
    -- use either the fleet ID or ARN value. To retrieve attributes for all
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
describeFleetAttributes_nextToken :: Lens.Lens' DescribeFleetAttributes (Prelude.Maybe Prelude.Text)
describeFleetAttributes_nextToken = Lens.lens (\DescribeFleetAttributes' {nextToken} -> nextToken) (\s@DescribeFleetAttributes' {} a -> s {nextToken = a} :: DescribeFleetAttributes)

-- | A list of unique fleet identifiers to retrieve attributes for. You can
-- use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter. If the list of fleet
-- identifiers includes fleets that don\'t currently exist, the request
-- succeeds but no attributes for that fleet are returned.
describeFleetAttributes_fleetIds :: Lens.Lens' DescribeFleetAttributes (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeFleetAttributes_fleetIds = Lens.lens (\DescribeFleetAttributes' {fleetIds} -> fleetIds) (\s@DescribeFleetAttributes' {} a -> s {fleetIds = a} :: DescribeFleetAttributes) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
describeFleetAttributes_limit :: Lens.Lens' DescribeFleetAttributes (Prelude.Maybe Prelude.Natural)
describeFleetAttributes_limit = Lens.lens (\DescribeFleetAttributes' {limit} -> limit) (\s@DescribeFleetAttributes' {} a -> s {limit = a} :: DescribeFleetAttributes)

instance Core.AWSPager DescribeFleetAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetAttributesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetAttributesResponse_fleetAttributes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFleetAttributes_nextToken
          Lens..~ rs
          Lens.^? describeFleetAttributesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFleetAttributes where
  type
    AWSResponse DescribeFleetAttributes =
      DescribeFleetAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetAttributesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "FleetAttributes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetAttributes

instance Prelude.NFData DescribeFleetAttributes

instance Core.ToHeaders DescribeFleetAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeFleetAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFleetAttributes where
  toJSON DescribeFleetAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("FleetIds" Core..=) Prelude.<$> fleetIds,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeFleetAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFleetAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetAttributesResponse' smart constructor.
data DescribeFleetAttributesResponse = DescribeFleetAttributesResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing attribute metadata for each requested
    -- fleet ID. Attribute objects are returned only for fleets that currently
    -- exist.
    fleetAttributes :: Prelude.Maybe [FleetAttributes],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeFleetAttributesResponse
newDescribeFleetAttributesResponse pHttpStatus_ =
  DescribeFleetAttributesResponse'
    { nextToken =
        Prelude.Nothing,
      fleetAttributes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeFleetAttributesResponse_nextToken :: Lens.Lens' DescribeFleetAttributesResponse (Prelude.Maybe Prelude.Text)
describeFleetAttributesResponse_nextToken = Lens.lens (\DescribeFleetAttributesResponse' {nextToken} -> nextToken) (\s@DescribeFleetAttributesResponse' {} a -> s {nextToken = a} :: DescribeFleetAttributesResponse)

-- | A collection of objects containing attribute metadata for each requested
-- fleet ID. Attribute objects are returned only for fleets that currently
-- exist.
describeFleetAttributesResponse_fleetAttributes :: Lens.Lens' DescribeFleetAttributesResponse (Prelude.Maybe [FleetAttributes])
describeFleetAttributesResponse_fleetAttributes = Lens.lens (\DescribeFleetAttributesResponse' {fleetAttributes} -> fleetAttributes) (\s@DescribeFleetAttributesResponse' {} a -> s {fleetAttributes = a} :: DescribeFleetAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetAttributesResponse_httpStatus :: Lens.Lens' DescribeFleetAttributesResponse Prelude.Int
describeFleetAttributesResponse_httpStatus = Lens.lens (\DescribeFleetAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetAttributesResponse' {} a -> s {httpStatus = a} :: DescribeFleetAttributesResponse)

instance
  Prelude.NFData
    DescribeFleetAttributesResponse

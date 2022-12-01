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
-- Module      : Amazonka.GameLift.DescribeScalingPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all scaling policies applied to a fleet.
--
-- To get a fleet\'s scaling policies, specify the fleet ID. You can filter
-- this request by policy status, such as to retrieve only active scaling
-- policies. Use the pagination parameters to retrieve results as a set of
-- sequential pages. If successful, set of ScalingPolicy objects is
-- returned for the fleet.
--
-- A fleet may have all of its scaling policies suspended
-- (StopFleetActions). This operation does not affect the status of the
-- scaling policies, which remains ACTIVE. To see whether a fleet\'s
-- scaling policies are in force or suspended, call DescribeFleetAttributes
-- and check the stopped actions.
--
-- __Related actions__
--
-- DescribeFleetCapacity | UpdateFleetCapacity | DescribeEC2InstanceLimits
-- | PutScalingPolicy | DescribeScalingPolicies | DeleteScalingPolicy |
-- StopFleetActions | StartFleetActions |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeScalingPolicies
  ( -- * Creating a Request
    DescribeScalingPolicies (..),
    newDescribeScalingPolicies,

    -- * Request Lenses
    describeScalingPolicies_nextToken,
    describeScalingPolicies_location,
    describeScalingPolicies_limit,
    describeScalingPolicies_statusFilter,
    describeScalingPolicies_fleetId,

    -- * Destructuring the Response
    DescribeScalingPoliciesResponse (..),
    newDescribeScalingPoliciesResponse,

    -- * Response Lenses
    describeScalingPoliciesResponse_nextToken,
    describeScalingPoliciesResponse_scalingPolicies,
    describeScalingPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The fleet location. If you don\'t specify this value, the response
    -- contains the scaling policies of every location in the fleet.
    location :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Scaling policy status to filter results on. A scaling policy is only in
    -- force when in an @ACTIVE@ status.
    --
    -- -   __ACTIVE__ -- The scaling policy is currently in force.
    --
    -- -   __UPDATEREQUESTED__ -- A request to update the scaling policy has
    --     been received.
    --
    -- -   __UPDATING__ -- A change is being made to the scaling policy.
    --
    -- -   __DELETEREQUESTED__ -- A request to delete the scaling policy has
    --     been received.
    --
    -- -   __DELETING__ -- The scaling policy is being deleted.
    --
    -- -   __DELETED__ -- The scaling policy has been deleted.
    --
    -- -   __ERROR__ -- An error occurred in creating the policy. It should be
    --     removed and recreated.
    statusFilter :: Prelude.Maybe ScalingStatusType,
    -- | A unique identifier for the fleet for which to retrieve scaling
    -- policies. You can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPolicies_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'location', 'describeScalingPolicies_location' - The fleet location. If you don\'t specify this value, the response
-- contains the scaling policies of every location in the fleet.
--
-- 'limit', 'describeScalingPolicies_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'statusFilter', 'describeScalingPolicies_statusFilter' - Scaling policy status to filter results on. A scaling policy is only in
-- force when in an @ACTIVE@ status.
--
-- -   __ACTIVE__ -- The scaling policy is currently in force.
--
-- -   __UPDATEREQUESTED__ -- A request to update the scaling policy has
--     been received.
--
-- -   __UPDATING__ -- A change is being made to the scaling policy.
--
-- -   __DELETEREQUESTED__ -- A request to delete the scaling policy has
--     been received.
--
-- -   __DELETING__ -- The scaling policy is being deleted.
--
-- -   __DELETED__ -- The scaling policy has been deleted.
--
-- -   __ERROR__ -- An error occurred in creating the policy. It should be
--     removed and recreated.
--
-- 'fleetId', 'describeScalingPolicies_fleetId' - A unique identifier for the fleet for which to retrieve scaling
-- policies. You can use either the fleet ID or ARN value.
newDescribeScalingPolicies ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeScalingPolicies
newDescribeScalingPolicies pFleetId_ =
  DescribeScalingPolicies'
    { nextToken =
        Prelude.Nothing,
      location = Prelude.Nothing,
      limit = Prelude.Nothing,
      statusFilter = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeScalingPolicies_nextToken :: Lens.Lens' DescribeScalingPolicies (Prelude.Maybe Prelude.Text)
describeScalingPolicies_nextToken = Lens.lens (\DescribeScalingPolicies' {nextToken} -> nextToken) (\s@DescribeScalingPolicies' {} a -> s {nextToken = a} :: DescribeScalingPolicies)

-- | The fleet location. If you don\'t specify this value, the response
-- contains the scaling policies of every location in the fleet.
describeScalingPolicies_location :: Lens.Lens' DescribeScalingPolicies (Prelude.Maybe Prelude.Text)
describeScalingPolicies_location = Lens.lens (\DescribeScalingPolicies' {location} -> location) (\s@DescribeScalingPolicies' {} a -> s {location = a} :: DescribeScalingPolicies)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeScalingPolicies_limit :: Lens.Lens' DescribeScalingPolicies (Prelude.Maybe Prelude.Natural)
describeScalingPolicies_limit = Lens.lens (\DescribeScalingPolicies' {limit} -> limit) (\s@DescribeScalingPolicies' {} a -> s {limit = a} :: DescribeScalingPolicies)

-- | Scaling policy status to filter results on. A scaling policy is only in
-- force when in an @ACTIVE@ status.
--
-- -   __ACTIVE__ -- The scaling policy is currently in force.
--
-- -   __UPDATEREQUESTED__ -- A request to update the scaling policy has
--     been received.
--
-- -   __UPDATING__ -- A change is being made to the scaling policy.
--
-- -   __DELETEREQUESTED__ -- A request to delete the scaling policy has
--     been received.
--
-- -   __DELETING__ -- The scaling policy is being deleted.
--
-- -   __DELETED__ -- The scaling policy has been deleted.
--
-- -   __ERROR__ -- An error occurred in creating the policy. It should be
--     removed and recreated.
describeScalingPolicies_statusFilter :: Lens.Lens' DescribeScalingPolicies (Prelude.Maybe ScalingStatusType)
describeScalingPolicies_statusFilter = Lens.lens (\DescribeScalingPolicies' {statusFilter} -> statusFilter) (\s@DescribeScalingPolicies' {} a -> s {statusFilter = a} :: DescribeScalingPolicies)

-- | A unique identifier for the fleet for which to retrieve scaling
-- policies. You can use either the fleet ID or ARN value.
describeScalingPolicies_fleetId :: Lens.Lens' DescribeScalingPolicies Prelude.Text
describeScalingPolicies_fleetId = Lens.lens (\DescribeScalingPolicies' {fleetId} -> fleetId) (\s@DescribeScalingPolicies' {} a -> s {fleetId = a} :: DescribeScalingPolicies)

instance Core.AWSPager DescribeScalingPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScalingPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScalingPoliciesResponse_scalingPolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeScalingPolicies_nextToken
          Lens..~ rs
          Lens.^? describeScalingPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeScalingPolicies where
  type
    AWSResponse DescribeScalingPolicies =
      DescribeScalingPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScalingPoliciesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ScalingPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScalingPolicies where
  hashWithSalt _salt DescribeScalingPolicies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` statusFilter
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DescribeScalingPolicies where
  rnf DescribeScalingPolicies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf statusFilter
      `Prelude.seq` Prelude.rnf fleetId

instance Core.ToHeaders DescribeScalingPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeScalingPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeScalingPolicies where
  toJSON DescribeScalingPolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Location" Core..=) Prelude.<$> location,
            ("Limit" Core..=) Prelude.<$> limit,
            ("StatusFilter" Core..=) Prelude.<$> statusFilter,
            Prelude.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath DescribeScalingPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeScalingPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing the scaling policies matching the
    -- request.
    scalingPolicies :: Prelude.Maybe [ScalingPolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPoliciesResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'scalingPolicies', 'describeScalingPoliciesResponse_scalingPolicies' - A collection of objects containing the scaling policies matching the
-- request.
--
-- 'httpStatus', 'describeScalingPoliciesResponse_httpStatus' - The response's http status code.
newDescribeScalingPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScalingPoliciesResponse
newDescribeScalingPoliciesResponse pHttpStatus_ =
  DescribeScalingPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      scalingPolicies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeScalingPoliciesResponse_nextToken :: Lens.Lens' DescribeScalingPoliciesResponse (Prelude.Maybe Prelude.Text)
describeScalingPoliciesResponse_nextToken = Lens.lens (\DescribeScalingPoliciesResponse' {nextToken} -> nextToken) (\s@DescribeScalingPoliciesResponse' {} a -> s {nextToken = a} :: DescribeScalingPoliciesResponse)

-- | A collection of objects containing the scaling policies matching the
-- request.
describeScalingPoliciesResponse_scalingPolicies :: Lens.Lens' DescribeScalingPoliciesResponse (Prelude.Maybe [ScalingPolicy])
describeScalingPoliciesResponse_scalingPolicies = Lens.lens (\DescribeScalingPoliciesResponse' {scalingPolicies} -> scalingPolicies) (\s@DescribeScalingPoliciesResponse' {} a -> s {scalingPolicies = a} :: DescribeScalingPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeScalingPoliciesResponse_httpStatus :: Lens.Lens' DescribeScalingPoliciesResponse Prelude.Int
describeScalingPoliciesResponse_httpStatus = Lens.lens (\DescribeScalingPoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingPoliciesResponse' {} a -> s {httpStatus = a} :: DescribeScalingPoliciesResponse)

instance
  Prelude.NFData
    DescribeScalingPoliciesResponse
  where
  rnf DescribeScalingPoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scalingPolicies
      `Prelude.seq` Prelude.rnf httpStatus

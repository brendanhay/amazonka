{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.DescribeScalingPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- -   DescribeFleetCapacity
--
-- -   UpdateFleetCapacity
--
-- -   DescribeEC2InstanceLimits
--
-- -   Manage scaling policies:
--
--     -   PutScalingPolicy (auto-scaling)
--
--     -   DescribeScalingPolicies (auto-scaling)
--
--     -   DeleteScalingPolicy (auto-scaling)
--
-- -   Manage fleet actions:
--
--     -   StartFleetActions
--
--     -   StopFleetActions
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeScalingPolicies
  ( -- * Creating a Request
    DescribeScalingPolicies (..),
    newDescribeScalingPolicies,

    -- * Request Lenses
    describeScalingPolicies_nextToken,
    describeScalingPolicies_statusFilter,
    describeScalingPolicies_limit,
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for a fleet to retrieve scaling policies for. You
    -- can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPolicies_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
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
-- 'limit', 'describeScalingPolicies_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'fleetId', 'describeScalingPolicies_fleetId' - A unique identifier for a fleet to retrieve scaling policies for. You
-- can use either the fleet ID or ARN value.
newDescribeScalingPolicies ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeScalingPolicies
newDescribeScalingPolicies pFleetId_ =
  DescribeScalingPolicies'
    { nextToken =
        Prelude.Nothing,
      statusFilter = Prelude.Nothing,
      limit = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeScalingPolicies_nextToken :: Lens.Lens' DescribeScalingPolicies (Prelude.Maybe Prelude.Text)
describeScalingPolicies_nextToken = Lens.lens (\DescribeScalingPolicies' {nextToken} -> nextToken) (\s@DescribeScalingPolicies' {} a -> s {nextToken = a} :: DescribeScalingPolicies)

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

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeScalingPolicies_limit :: Lens.Lens' DescribeScalingPolicies (Prelude.Maybe Prelude.Natural)
describeScalingPolicies_limit = Lens.lens (\DescribeScalingPolicies' {limit} -> limit) (\s@DescribeScalingPolicies' {} a -> s {limit = a} :: DescribeScalingPolicies)

-- | A unique identifier for a fleet to retrieve scaling policies for. You
-- can use either the fleet ID or ARN value.
describeScalingPolicies_fleetId :: Lens.Lens' DescribeScalingPolicies Prelude.Text
describeScalingPolicies_fleetId = Lens.lens (\DescribeScalingPolicies' {fleetId} -> fleetId) (\s@DescribeScalingPolicies' {} a -> s {fleetId = a} :: DescribeScalingPolicies)

instance Pager.AWSPager DescribeScalingPolicies where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeScalingPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeScalingPoliciesResponse_scalingPolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeScalingPolicies_nextToken
          Lens..~ rs
          Lens.^? describeScalingPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeScalingPolicies where
  type
    Rs DescribeScalingPolicies =
      DescribeScalingPoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScalingPoliciesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ScalingPolicies"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScalingPolicies

instance Prelude.NFData DescribeScalingPolicies

instance Prelude.ToHeaders DescribeScalingPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.DescribeScalingPolicies" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeScalingPolicies where
  toJSON DescribeScalingPolicies' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("StatusFilter" Prelude..=) Prelude.<$> statusFilter,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just ("FleetId" Prelude..= fleetId)
          ]
      )

instance Prelude.ToPath DescribeScalingPolicies where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeScalingPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing the scaling policies matching the
    -- request.
    scalingPolicies :: Prelude.Maybe [ScalingPolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPoliciesResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
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

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeScalingPoliciesResponse_nextToken :: Lens.Lens' DescribeScalingPoliciesResponse (Prelude.Maybe Prelude.Text)
describeScalingPoliciesResponse_nextToken = Lens.lens (\DescribeScalingPoliciesResponse' {nextToken} -> nextToken) (\s@DescribeScalingPoliciesResponse' {} a -> s {nextToken = a} :: DescribeScalingPoliciesResponse)

-- | A collection of objects containing the scaling policies matching the
-- request.
describeScalingPoliciesResponse_scalingPolicies :: Lens.Lens' DescribeScalingPoliciesResponse (Prelude.Maybe [ScalingPolicy])
describeScalingPoliciesResponse_scalingPolicies = Lens.lens (\DescribeScalingPoliciesResponse' {scalingPolicies} -> scalingPolicies) (\s@DescribeScalingPoliciesResponse' {} a -> s {scalingPolicies = a} :: DescribeScalingPoliciesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeScalingPoliciesResponse_httpStatus :: Lens.Lens' DescribeScalingPoliciesResponse Prelude.Int
describeScalingPoliciesResponse_httpStatus = Lens.lens (\DescribeScalingPoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingPoliciesResponse' {} a -> s {httpStatus = a} :: DescribeScalingPoliciesResponse)

instance
  Prelude.NFData
    DescribeScalingPoliciesResponse

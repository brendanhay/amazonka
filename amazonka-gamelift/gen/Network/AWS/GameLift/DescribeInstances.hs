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
-- Module      : Network.AWS.GameLift.DescribeInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a fleet\'s instances, including instance
-- IDs. Use this operation to get details on all instances in the fleet or
-- get details on one specific instance.
--
-- To get a specific instance, specify fleet ID and instance ID. To get all
-- instances in a fleet, specify a fleet ID only. Use the pagination
-- parameters to retrieve results as a set of sequential pages. If
-- successful, an Instance object is returned for each result.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues>
--
-- __Related operations__
--
-- -   DescribeInstances
--
-- -   GetInstanceAccess
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeInstances
  ( -- * Creating a Request
    DescribeInstances (..),
    newDescribeInstances,

    -- * Request Lenses
    describeInstances_nextToken,
    describeInstances_instanceId,
    describeInstances_limit,
    describeInstances_fleetId,

    -- * Destructuring the Response
    DescribeInstancesResponse (..),
    newDescribeInstancesResponse,

    -- * Response Lenses
    describeInstancesResponse_nextToken,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | A unique identifier for an instance to retrieve. Specify an instance ID
    -- or leave blank to retrieve all instances in the fleet.
    instanceId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural,
    -- | A unique identifier for a fleet to retrieve instance information for.
    -- You can use either the fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstances_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'instanceId', 'describeInstances_instanceId' - A unique identifier for an instance to retrieve. Specify an instance ID
-- or leave blank to retrieve all instances in the fleet.
--
-- 'limit', 'describeInstances_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'fleetId', 'describeInstances_fleetId' - A unique identifier for a fleet to retrieve instance information for.
-- You can use either the fleet ID or ARN value.
newDescribeInstances ::
  -- | 'fleetId'
  Core.Text ->
  DescribeInstances
newDescribeInstances pFleetId_ =
  DescribeInstances'
    { nextToken = Core.Nothing,
      instanceId = Core.Nothing,
      limit = Core.Nothing,
      fleetId = pFleetId_
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeInstances_nextToken :: Lens.Lens' DescribeInstances (Core.Maybe Core.Text)
describeInstances_nextToken = Lens.lens (\DescribeInstances' {nextToken} -> nextToken) (\s@DescribeInstances' {} a -> s {nextToken = a} :: DescribeInstances)

-- | A unique identifier for an instance to retrieve. Specify an instance ID
-- or leave blank to retrieve all instances in the fleet.
describeInstances_instanceId :: Lens.Lens' DescribeInstances (Core.Maybe Core.Text)
describeInstances_instanceId = Lens.lens (\DescribeInstances' {instanceId} -> instanceId) (\s@DescribeInstances' {} a -> s {instanceId = a} :: DescribeInstances)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeInstances_limit :: Lens.Lens' DescribeInstances (Core.Maybe Core.Natural)
describeInstances_limit = Lens.lens (\DescribeInstances' {limit} -> limit) (\s@DescribeInstances' {} a -> s {limit = a} :: DescribeInstances)

-- | A unique identifier for a fleet to retrieve instance information for.
-- You can use either the fleet ID or ARN value.
describeInstances_fleetId :: Lens.Lens' DescribeInstances Core.Text
describeInstances_fleetId = Lens.lens (\DescribeInstances' {fleetId} -> fleetId) (\s@DescribeInstances' {} a -> s {fleetId = a} :: DescribeInstances)

instance Core.AWSPager DescribeInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancesResponse_instances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeInstances_nextToken
          Lens..~ rs
          Lens.^? describeInstancesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeInstances where
  type
    AWSResponse DescribeInstances =
      DescribeInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Instances" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstances

instance Core.NFData DescribeInstances

instance Core.ToHeaders DescribeInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DescribeInstances" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeInstances where
  toJSON DescribeInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("InstanceId" Core..=) Core.<$> instanceId,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath DescribeInstances where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInstances where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of objects containing properties for each instance
    -- returned.
    instances :: Core.Maybe [Instance],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancesResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'instances', 'describeInstancesResponse_instances' - A collection of objects containing properties for each instance
-- returned.
--
-- 'httpStatus', 'describeInstancesResponse_httpStatus' - The response's http status code.
newDescribeInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstancesResponse
newDescribeInstancesResponse pHttpStatus_ =
  DescribeInstancesResponse'
    { nextToken =
        Core.Nothing,
      instances = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeInstancesResponse_nextToken :: Lens.Lens' DescribeInstancesResponse (Core.Maybe Core.Text)
describeInstancesResponse_nextToken = Lens.lens (\DescribeInstancesResponse' {nextToken} -> nextToken) (\s@DescribeInstancesResponse' {} a -> s {nextToken = a} :: DescribeInstancesResponse)

-- | A collection of objects containing properties for each instance
-- returned.
describeInstancesResponse_instances :: Lens.Lens' DescribeInstancesResponse (Core.Maybe [Instance])
describeInstancesResponse_instances = Lens.lens (\DescribeInstancesResponse' {instances} -> instances) (\s@DescribeInstancesResponse' {} a -> s {instances = a} :: DescribeInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstancesResponse_httpStatus :: Lens.Lens' DescribeInstancesResponse Core.Int
describeInstancesResponse_httpStatus = Lens.lens (\DescribeInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancesResponse' {} a -> s {httpStatus = a} :: DescribeInstancesResponse)

instance Core.NFData DescribeInstancesResponse

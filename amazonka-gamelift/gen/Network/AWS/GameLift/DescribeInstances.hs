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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an instance to retrieve. Specify an instance ID
    -- or leave blank to retrieve all instances in the fleet.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for a fleet to retrieve instance information for.
    -- You can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeInstances
newDescribeInstances pFleetId_ =
  DescribeInstances'
    { nextToken = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      limit = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeInstances_nextToken :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_nextToken = Lens.lens (\DescribeInstances' {nextToken} -> nextToken) (\s@DescribeInstances' {} a -> s {nextToken = a} :: DescribeInstances)

-- | A unique identifier for an instance to retrieve. Specify an instance ID
-- or leave blank to retrieve all instances in the fleet.
describeInstances_instanceId :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_instanceId = Lens.lens (\DescribeInstances' {instanceId} -> instanceId) (\s@DescribeInstances' {} a -> s {instanceId = a} :: DescribeInstances)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeInstances_limit :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Natural)
describeInstances_limit = Lens.lens (\DescribeInstances' {limit} -> limit) (\s@DescribeInstances' {} a -> s {limit = a} :: DescribeInstances)

-- | A unique identifier for a fleet to retrieve instance information for.
-- You can use either the fleet ID or ARN value.
describeInstances_fleetId :: Lens.Lens' DescribeInstances Prelude.Text
describeInstances_fleetId = Lens.lens (\DescribeInstances' {fleetId} -> fleetId) (\s@DescribeInstances' {} a -> s {fleetId = a} :: DescribeInstances)

instance Pager.AWSPager DescribeInstances where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeInstancesResponse_instances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeInstances_nextToken
          Lens..~ rs
          Lens.^? describeInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeInstances where
  type Rs DescribeInstances = DescribeInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "Instances"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstances

instance Prelude.NFData DescribeInstances

instance Prelude.ToHeaders DescribeInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.DescribeInstances" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeInstances where
  toJSON DescribeInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("InstanceId" Prelude..=) Prelude.<$> instanceId,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just ("FleetId" Prelude..= fleetId)
          ]
      )

instance Prelude.ToPath DescribeInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeInstances where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing properties for each instance
    -- returned.
    instances :: Prelude.Maybe [Instance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeInstancesResponse
newDescribeInstancesResponse pHttpStatus_ =
  DescribeInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      instances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeInstancesResponse_nextToken :: Lens.Lens' DescribeInstancesResponse (Prelude.Maybe Prelude.Text)
describeInstancesResponse_nextToken = Lens.lens (\DescribeInstancesResponse' {nextToken} -> nextToken) (\s@DescribeInstancesResponse' {} a -> s {nextToken = a} :: DescribeInstancesResponse)

-- | A collection of objects containing properties for each instance
-- returned.
describeInstancesResponse_instances :: Lens.Lens' DescribeInstancesResponse (Prelude.Maybe [Instance])
describeInstancesResponse_instances = Lens.lens (\DescribeInstancesResponse' {instances} -> instances) (\s@DescribeInstancesResponse' {} a -> s {instances = a} :: DescribeInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeInstancesResponse_httpStatus :: Lens.Lens' DescribeInstancesResponse Prelude.Int
describeInstancesResponse_httpStatus = Lens.lens (\DescribeInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancesResponse' {} a -> s {httpStatus = a} :: DescribeInstancesResponse)

instance Prelude.NFData DescribeInstancesResponse

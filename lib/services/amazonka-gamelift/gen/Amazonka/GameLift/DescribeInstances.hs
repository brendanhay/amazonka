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
-- Module      : Amazonka.GameLift.DescribeInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a fleet\'s instances, including instance
-- IDs, connection data, and status.
--
-- This operation can be used in the following ways:
--
-- -   To get information on all instances that are deployed to a fleet\'s
--     home Region, provide the fleet ID.
--
-- -   To get information on all instances that are deployed to a fleet\'s
--     remote location, provide the fleet ID and location name.
--
-- -   To get information on a specific instance in a fleet, provide the
--     fleet ID and instance ID.
--
-- Use the pagination parameters to retrieve results as a set of sequential
-- pages.
--
-- If successful, an @Instance@ object is returned for each requested
-- instance. Instances are not returned in any particular order.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues>
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeInstances
  ( -- * Creating a Request
    DescribeInstances (..),
    newDescribeInstances,

    -- * Request Lenses
    describeInstances_instanceId,
    describeInstances_limit,
    describeInstances_location,
    describeInstances_nextToken,
    describeInstances_fleetId,

    -- * Destructuring the Response
    DescribeInstancesResponse (..),
    newDescribeInstancesResponse,

    -- * Response Lenses
    describeInstancesResponse_instances,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { -- | A unique identifier for an instance to retrieve. Specify an instance ID
    -- or leave blank to retrieve all instances in the fleet.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of a location to retrieve instance information for, in the form
    -- of an Amazon Web Services Region code such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to retrieve instance information for.
    -- You can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeInstances_instanceId' - A unique identifier for an instance to retrieve. Specify an instance ID
-- or leave blank to retrieve all instances in the fleet.
--
-- 'limit', 'describeInstances_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'location', 'describeInstances_location' - The name of a location to retrieve instance information for, in the form
-- of an Amazon Web Services Region code such as @us-west-2@.
--
-- 'nextToken', 'describeInstances_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'fleetId', 'describeInstances_fleetId' - A unique identifier for the fleet to retrieve instance information for.
-- You can use either the fleet ID or ARN value.
newDescribeInstances ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeInstances
newDescribeInstances pFleetId_ =
  DescribeInstances'
    { instanceId = Prelude.Nothing,
      limit = Prelude.Nothing,
      location = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | A unique identifier for an instance to retrieve. Specify an instance ID
-- or leave blank to retrieve all instances in the fleet.
describeInstances_instanceId :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_instanceId = Lens.lens (\DescribeInstances' {instanceId} -> instanceId) (\s@DescribeInstances' {} a -> s {instanceId = a} :: DescribeInstances)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeInstances_limit :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Natural)
describeInstances_limit = Lens.lens (\DescribeInstances' {limit} -> limit) (\s@DescribeInstances' {} a -> s {limit = a} :: DescribeInstances)

-- | The name of a location to retrieve instance information for, in the form
-- of an Amazon Web Services Region code such as @us-west-2@.
describeInstances_location :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_location = Lens.lens (\DescribeInstances' {location} -> location) (\s@DescribeInstances' {} a -> s {location = a} :: DescribeInstances)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeInstances_nextToken :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_nextToken = Lens.lens (\DescribeInstances' {nextToken} -> nextToken) (\s@DescribeInstances' {} a -> s {nextToken = a} :: DescribeInstances)

-- | A unique identifier for the fleet to retrieve instance information for.
-- You can use either the fleet ID or ARN value.
describeInstances_fleetId :: Lens.Lens' DescribeInstances Prelude.Text
describeInstances_fleetId = Lens.lens (\DescribeInstances' {fleetId} -> fleetId) (\s@DescribeInstances' {} a -> s {fleetId = a} :: DescribeInstances)

instance Core.AWSPager DescribeInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancesResponse_instances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeInstances_nextToken
          Lens..~ rs
          Lens.^? describeInstancesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstances where
  type
    AWSResponse DescribeInstances =
      DescribeInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancesResponse'
            Prelude.<$> (x Data..?> "Instances" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstances where
  hashWithSalt _salt DescribeInstances' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DescribeInstances where
  rnf DescribeInstances' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders DescribeInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DescribeInstances" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeInstances where
  toJSON DescribeInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("Limit" Data..=) Prelude.<$> limit,
            ("Location" Data..=) Prelude.<$> location,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("FleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath DescribeInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { -- | A collection of objects containing properties for each instance
    -- returned.
    instances :: Prelude.Maybe [Instance],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'describeInstancesResponse_instances' - A collection of objects containing properties for each instance
-- returned.
--
-- 'nextToken', 'describeInstancesResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'describeInstancesResponse_httpStatus' - The response's http status code.
newDescribeInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstancesResponse
newDescribeInstancesResponse pHttpStatus_ =
  DescribeInstancesResponse'
    { instances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of objects containing properties for each instance
-- returned.
describeInstancesResponse_instances :: Lens.Lens' DescribeInstancesResponse (Prelude.Maybe [Instance])
describeInstancesResponse_instances = Lens.lens (\DescribeInstancesResponse' {instances} -> instances) (\s@DescribeInstancesResponse' {} a -> s {instances = a} :: DescribeInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeInstancesResponse_nextToken :: Lens.Lens' DescribeInstancesResponse (Prelude.Maybe Prelude.Text)
describeInstancesResponse_nextToken = Lens.lens (\DescribeInstancesResponse' {nextToken} -> nextToken) (\s@DescribeInstancesResponse' {} a -> s {nextToken = a} :: DescribeInstancesResponse)

-- | The response's http status code.
describeInstancesResponse_httpStatus :: Lens.Lens' DescribeInstancesResponse Prelude.Int
describeInstancesResponse_httpStatus = Lens.lens (\DescribeInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancesResponse' {} a -> s {httpStatus = a} :: DescribeInstancesResponse)

instance Prelude.NFData DescribeInstancesResponse where
  rnf DescribeInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

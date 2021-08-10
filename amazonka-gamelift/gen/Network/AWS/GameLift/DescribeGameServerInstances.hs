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
-- Module      : Network.AWS.GameLift.DescribeGameServerInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Retrieves status information about the Amazon EC2 instances associated
-- with a GameLift FleetIQ game server group. Use this operation to detect
-- when instances are active or not available to host new game servers. If
-- you are looking for instance configuration information, call
-- DescribeGameServerGroup or access the corresponding Auto Scaling group
-- properties.
--
-- To request status for all instances in the game server group, provide a
-- game server group ID only. To request status for specific instances,
-- provide the game server group ID and one or more instance IDs. Use the
-- pagination parameters to retrieve results in sequential segments. If
-- successful, a collection of @GameServerInstance@ objects is returned.
--
-- This operation is not designed to be called with every game server claim
-- request; this practice can cause you to exceed your API limit, which
-- results in errors. Instead, as a best practice, cache the results and
-- refresh your cache no more than once every 10 seconds.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
-- -   CreateGameServerGroup
--
-- -   ListGameServerGroups
--
-- -   DescribeGameServerGroup
--
-- -   UpdateGameServerGroup
--
-- -   DeleteGameServerGroup
--
-- -   ResumeGameServerGroup
--
-- -   SuspendGameServerGroup
--
-- -   DescribeGameServerInstances
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeGameServerInstances
  ( -- * Creating a Request
    DescribeGameServerInstances (..),
    newDescribeGameServerInstances,

    -- * Request Lenses
    describeGameServerInstances_instanceIds,
    describeGameServerInstances_nextToken,
    describeGameServerInstances_limit,
    describeGameServerInstances_gameServerGroupName,

    -- * Destructuring the Response
    DescribeGameServerInstancesResponse (..),
    newDescribeGameServerInstancesResponse,

    -- * Response Lenses
    describeGameServerInstancesResponse_nextToken,
    describeGameServerInstancesResponse_gameServerInstances,
    describeGameServerInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeGameServerInstances' smart constructor.
data DescribeGameServerInstances = DescribeGameServerInstances'
  { -- | The EC2 instance IDs that you want to retrieve status on. EC2 instance
    -- IDs use a 17-character format, for example: @i-1234567890abcdef0@. To
    -- retrieve all instances in the game server group, leave this parameter
    -- empty.
    instanceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A token that indicates the start of the next sequential segment of
    -- results. Use the token returned with the previous call to this
    -- operation. To start at the beginning of the result set, do not specify a
    -- value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential segments.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the game server group. Use either the
    -- GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameServerInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeGameServerInstances_instanceIds' - The EC2 instance IDs that you want to retrieve status on. EC2 instance
-- IDs use a 17-character format, for example: @i-1234567890abcdef0@. To
-- retrieve all instances in the game server group, leave this parameter
-- empty.
--
-- 'nextToken', 'describeGameServerInstances_nextToken' - A token that indicates the start of the next sequential segment of
-- results. Use the token returned with the previous call to this
-- operation. To start at the beginning of the result set, do not specify a
-- value.
--
-- 'limit', 'describeGameServerInstances_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential segments.
--
-- 'gameServerGroupName', 'describeGameServerInstances_gameServerGroupName' - A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
newDescribeGameServerInstances ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  DescribeGameServerInstances
newDescribeGameServerInstances pGameServerGroupName_ =
  DescribeGameServerInstances'
    { instanceIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      gameServerGroupName = pGameServerGroupName_
    }

-- | The EC2 instance IDs that you want to retrieve status on. EC2 instance
-- IDs use a 17-character format, for example: @i-1234567890abcdef0@. To
-- retrieve all instances in the game server group, leave this parameter
-- empty.
describeGameServerInstances_instanceIds :: Lens.Lens' DescribeGameServerInstances (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeGameServerInstances_instanceIds = Lens.lens (\DescribeGameServerInstances' {instanceIds} -> instanceIds) (\s@DescribeGameServerInstances' {} a -> s {instanceIds = a} :: DescribeGameServerInstances) Prelude.. Lens.mapping Lens._Coerce

-- | A token that indicates the start of the next sequential segment of
-- results. Use the token returned with the previous call to this
-- operation. To start at the beginning of the result set, do not specify a
-- value.
describeGameServerInstances_nextToken :: Lens.Lens' DescribeGameServerInstances (Prelude.Maybe Prelude.Text)
describeGameServerInstances_nextToken = Lens.lens (\DescribeGameServerInstances' {nextToken} -> nextToken) (\s@DescribeGameServerInstances' {} a -> s {nextToken = a} :: DescribeGameServerInstances)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential segments.
describeGameServerInstances_limit :: Lens.Lens' DescribeGameServerInstances (Prelude.Maybe Prelude.Natural)
describeGameServerInstances_limit = Lens.lens (\DescribeGameServerInstances' {limit} -> limit) (\s@DescribeGameServerInstances' {} a -> s {limit = a} :: DescribeGameServerInstances)

-- | A unique identifier for the game server group. Use either the
-- GameServerGroup name or ARN value.
describeGameServerInstances_gameServerGroupName :: Lens.Lens' DescribeGameServerInstances Prelude.Text
describeGameServerInstances_gameServerGroupName = Lens.lens (\DescribeGameServerInstances' {gameServerGroupName} -> gameServerGroupName) (\s@DescribeGameServerInstances' {} a -> s {gameServerGroupName = a} :: DescribeGameServerInstances)

instance Core.AWSPager DescribeGameServerInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGameServerInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGameServerInstancesResponse_gameServerInstances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeGameServerInstances_nextToken
          Lens..~ rs
          Lens.^? describeGameServerInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeGameServerInstances where
  type
    AWSResponse DescribeGameServerInstances =
      DescribeGameServerInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameServerInstancesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "GameServerInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGameServerInstances

instance Prelude.NFData DescribeGameServerInstances

instance Core.ToHeaders DescribeGameServerInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeGameServerInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeGameServerInstances where
  toJSON DescribeGameServerInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InstanceIds" Core..=) Prelude.<$> instanceIds,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("GameServerGroupName" Core..= gameServerGroupName)
          ]
      )

instance Core.ToPath DescribeGameServerInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGameServerInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGameServerInstancesResponse' smart constructor.
data DescribeGameServerInstancesResponse = DescribeGameServerInstancesResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of requested game server instances.
    gameServerInstances :: Prelude.Maybe [GameServerInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameServerInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGameServerInstancesResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'gameServerInstances', 'describeGameServerInstancesResponse_gameServerInstances' - The collection of requested game server instances.
--
-- 'httpStatus', 'describeGameServerInstancesResponse_httpStatus' - The response's http status code.
newDescribeGameServerInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGameServerInstancesResponse
newDescribeGameServerInstancesResponse pHttpStatus_ =
  DescribeGameServerInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      gameServerInstances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeGameServerInstancesResponse_nextToken :: Lens.Lens' DescribeGameServerInstancesResponse (Prelude.Maybe Prelude.Text)
describeGameServerInstancesResponse_nextToken = Lens.lens (\DescribeGameServerInstancesResponse' {nextToken} -> nextToken) (\s@DescribeGameServerInstancesResponse' {} a -> s {nextToken = a} :: DescribeGameServerInstancesResponse)

-- | The collection of requested game server instances.
describeGameServerInstancesResponse_gameServerInstances :: Lens.Lens' DescribeGameServerInstancesResponse (Prelude.Maybe [GameServerInstance])
describeGameServerInstancesResponse_gameServerInstances = Lens.lens (\DescribeGameServerInstancesResponse' {gameServerInstances} -> gameServerInstances) (\s@DescribeGameServerInstancesResponse' {} a -> s {gameServerInstances = a} :: DescribeGameServerInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeGameServerInstancesResponse_httpStatus :: Lens.Lens' DescribeGameServerInstancesResponse Prelude.Int
describeGameServerInstancesResponse_httpStatus = Lens.lens (\DescribeGameServerInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeGameServerInstancesResponse' {} a -> s {httpStatus = a} :: DescribeGameServerInstancesResponse)

instance
  Prelude.NFData
    DescribeGameServerInstancesResponse

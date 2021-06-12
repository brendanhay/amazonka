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
-- Module      : Network.AWS.GameLift.ListGameServerGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Retrieves information on all game servers groups that exist in the
-- current AWS account for the selected Region. Use the pagination
-- parameters to retrieve results in a set of sequential segments.
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
module Network.AWS.GameLift.ListGameServerGroups
  ( -- * Creating a Request
    ListGameServerGroups (..),
    newListGameServerGroups,

    -- * Request Lenses
    listGameServerGroups_nextToken,
    listGameServerGroups_limit,

    -- * Destructuring the Response
    ListGameServerGroupsResponse (..),
    newListGameServerGroupsResponse,

    -- * Response Lenses
    listGameServerGroupsResponse_gameServerGroups,
    listGameServerGroupsResponse_nextToken,
    listGameServerGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGameServerGroups' smart constructor.
data ListGameServerGroups = ListGameServerGroups'
  { -- | A token that indicates the start of the next sequential segment of
    -- results. Use the token returned with the previous call to this
    -- operation. To start at the beginning of the result set, do not specify a
    -- value.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential segments.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGameServerGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGameServerGroups_nextToken' - A token that indicates the start of the next sequential segment of
-- results. Use the token returned with the previous call to this
-- operation. To start at the beginning of the result set, do not specify a
-- value.
--
-- 'limit', 'listGameServerGroups_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential segments.
newListGameServerGroups ::
  ListGameServerGroups
newListGameServerGroups =
  ListGameServerGroups'
    { nextToken = Core.Nothing,
      limit = Core.Nothing
    }

-- | A token that indicates the start of the next sequential segment of
-- results. Use the token returned with the previous call to this
-- operation. To start at the beginning of the result set, do not specify a
-- value.
listGameServerGroups_nextToken :: Lens.Lens' ListGameServerGroups (Core.Maybe Core.Text)
listGameServerGroups_nextToken = Lens.lens (\ListGameServerGroups' {nextToken} -> nextToken) (\s@ListGameServerGroups' {} a -> s {nextToken = a} :: ListGameServerGroups)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential segments.
listGameServerGroups_limit :: Lens.Lens' ListGameServerGroups (Core.Maybe Core.Natural)
listGameServerGroups_limit = Lens.lens (\ListGameServerGroups' {limit} -> limit) (\s@ListGameServerGroups' {} a -> s {limit = a} :: ListGameServerGroups)

instance Core.AWSPager ListGameServerGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGameServerGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listGameServerGroupsResponse_gameServerGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listGameServerGroups_nextToken
          Lens..~ rs
          Lens.^? listGameServerGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListGameServerGroups where
  type
    AWSResponse ListGameServerGroups =
      ListGameServerGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGameServerGroupsResponse'
            Core.<$> (x Core..?> "GameServerGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGameServerGroups

instance Core.NFData ListGameServerGroups

instance Core.ToHeaders ListGameServerGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListGameServerGroups" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGameServerGroups where
  toJSON ListGameServerGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListGameServerGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListGameServerGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGameServerGroupsResponse' smart constructor.
data ListGameServerGroupsResponse = ListGameServerGroupsResponse'
  { -- | A collection of game server group objects that match the request.
    gameServerGroups :: Core.Maybe [GameServerGroup],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGameServerGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameServerGroups', 'listGameServerGroupsResponse_gameServerGroups' - A collection of game server group objects that match the request.
--
-- 'nextToken', 'listGameServerGroupsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'listGameServerGroupsResponse_httpStatus' - The response's http status code.
newListGameServerGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGameServerGroupsResponse
newListGameServerGroupsResponse pHttpStatus_ =
  ListGameServerGroupsResponse'
    { gameServerGroups =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of game server group objects that match the request.
listGameServerGroupsResponse_gameServerGroups :: Lens.Lens' ListGameServerGroupsResponse (Core.Maybe [GameServerGroup])
listGameServerGroupsResponse_gameServerGroups = Lens.lens (\ListGameServerGroupsResponse' {gameServerGroups} -> gameServerGroups) (\s@ListGameServerGroupsResponse' {} a -> s {gameServerGroups = a} :: ListGameServerGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listGameServerGroupsResponse_nextToken :: Lens.Lens' ListGameServerGroupsResponse (Core.Maybe Core.Text)
listGameServerGroupsResponse_nextToken = Lens.lens (\ListGameServerGroupsResponse' {nextToken} -> nextToken) (\s@ListGameServerGroupsResponse' {} a -> s {nextToken = a} :: ListGameServerGroupsResponse)

-- | The response's http status code.
listGameServerGroupsResponse_httpStatus :: Lens.Lens' ListGameServerGroupsResponse Core.Int
listGameServerGroupsResponse_httpStatus = Lens.lens (\ListGameServerGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGameServerGroupsResponse' {} a -> s {httpStatus = a} :: ListGameServerGroupsResponse)

instance Core.NFData ListGameServerGroupsResponse

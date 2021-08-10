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
-- Module      : Network.AWS.GameLift.ListGameServers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and
-- game server groups.__
--
-- Retrieves information on all game servers that are currently active in a
-- specified game server group. You can opt to sort the list by game server
-- age. Use the pagination parameters to retrieve results in a set of
-- sequential segments.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
-- -   RegisterGameServer
--
-- -   ListGameServers
--
-- -   ClaimGameServer
--
-- -   DescribeGameServer
--
-- -   UpdateGameServer
--
-- -   DeregisterGameServer
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListGameServers
  ( -- * Creating a Request
    ListGameServers (..),
    newListGameServers,

    -- * Request Lenses
    listGameServers_sortOrder,
    listGameServers_nextToken,
    listGameServers_limit,
    listGameServers_gameServerGroupName,

    -- * Destructuring the Response
    ListGameServersResponse (..),
    newListGameServersResponse,

    -- * Response Lenses
    listGameServersResponse_nextToken,
    listGameServersResponse_gameServers,
    listGameServersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGameServers' smart constructor.
data ListGameServers = ListGameServers'
  { -- | Indicates how to sort the returned data based on game server
    -- registration timestamp. Use ASCENDING to retrieve oldest game servers
    -- first, or use DESCENDING to retrieve newest game servers first. If this
    -- parameter is left empty, game servers are returned in no particular
    -- order.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A token that indicates the start of the next sequential segment of
    -- results. Use the token returned with the previous call to this
    -- operation. To start at the beginning of the result set, do not specify a
    -- value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential segments.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | An identifier for the game server group to retrieve a list of game
    -- servers from. Use either the GameServerGroup name or ARN value.
    gameServerGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGameServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listGameServers_sortOrder' - Indicates how to sort the returned data based on game server
-- registration timestamp. Use ASCENDING to retrieve oldest game servers
-- first, or use DESCENDING to retrieve newest game servers first. If this
-- parameter is left empty, game servers are returned in no particular
-- order.
--
-- 'nextToken', 'listGameServers_nextToken' - A token that indicates the start of the next sequential segment of
-- results. Use the token returned with the previous call to this
-- operation. To start at the beginning of the result set, do not specify a
-- value.
--
-- 'limit', 'listGameServers_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential segments.
--
-- 'gameServerGroupName', 'listGameServers_gameServerGroupName' - An identifier for the game server group to retrieve a list of game
-- servers from. Use either the GameServerGroup name or ARN value.
newListGameServers ::
  -- | 'gameServerGroupName'
  Prelude.Text ->
  ListGameServers
newListGameServers pGameServerGroupName_ =
  ListGameServers'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      gameServerGroupName = pGameServerGroupName_
    }

-- | Indicates how to sort the returned data based on game server
-- registration timestamp. Use ASCENDING to retrieve oldest game servers
-- first, or use DESCENDING to retrieve newest game servers first. If this
-- parameter is left empty, game servers are returned in no particular
-- order.
listGameServers_sortOrder :: Lens.Lens' ListGameServers (Prelude.Maybe SortOrder)
listGameServers_sortOrder = Lens.lens (\ListGameServers' {sortOrder} -> sortOrder) (\s@ListGameServers' {} a -> s {sortOrder = a} :: ListGameServers)

-- | A token that indicates the start of the next sequential segment of
-- results. Use the token returned with the previous call to this
-- operation. To start at the beginning of the result set, do not specify a
-- value.
listGameServers_nextToken :: Lens.Lens' ListGameServers (Prelude.Maybe Prelude.Text)
listGameServers_nextToken = Lens.lens (\ListGameServers' {nextToken} -> nextToken) (\s@ListGameServers' {} a -> s {nextToken = a} :: ListGameServers)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential segments.
listGameServers_limit :: Lens.Lens' ListGameServers (Prelude.Maybe Prelude.Natural)
listGameServers_limit = Lens.lens (\ListGameServers' {limit} -> limit) (\s@ListGameServers' {} a -> s {limit = a} :: ListGameServers)

-- | An identifier for the game server group to retrieve a list of game
-- servers from. Use either the GameServerGroup name or ARN value.
listGameServers_gameServerGroupName :: Lens.Lens' ListGameServers Prelude.Text
listGameServers_gameServerGroupName = Lens.lens (\ListGameServers' {gameServerGroupName} -> gameServerGroupName) (\s@ListGameServers' {} a -> s {gameServerGroupName = a} :: ListGameServers)

instance Core.AWSPager ListGameServers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGameServersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGameServersResponse_gameServers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGameServers_nextToken
          Lens..~ rs
          Lens.^? listGameServersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListGameServers where
  type
    AWSResponse ListGameServers =
      ListGameServersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGameServersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "GameServers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGameServers

instance Prelude.NFData ListGameServers

instance Core.ToHeaders ListGameServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListGameServers" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListGameServers where
  toJSON ListGameServers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("GameServerGroupName" Core..= gameServerGroupName)
          ]
      )

instance Core.ToPath ListGameServers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListGameServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGameServersResponse' smart constructor.
data ListGameServersResponse = ListGameServersResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of game server objects that match the request.
    gameServers :: Prelude.Maybe [GameServer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGameServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGameServersResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'gameServers', 'listGameServersResponse_gameServers' - A collection of game server objects that match the request.
--
-- 'httpStatus', 'listGameServersResponse_httpStatus' - The response's http status code.
newListGameServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGameServersResponse
newListGameServersResponse pHttpStatus_ =
  ListGameServersResponse'
    { nextToken =
        Prelude.Nothing,
      gameServers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listGameServersResponse_nextToken :: Lens.Lens' ListGameServersResponse (Prelude.Maybe Prelude.Text)
listGameServersResponse_nextToken = Lens.lens (\ListGameServersResponse' {nextToken} -> nextToken) (\s@ListGameServersResponse' {} a -> s {nextToken = a} :: ListGameServersResponse)

-- | A collection of game server objects that match the request.
listGameServersResponse_gameServers :: Lens.Lens' ListGameServersResponse (Prelude.Maybe [GameServer])
listGameServersResponse_gameServers = Lens.lens (\ListGameServersResponse' {gameServers} -> gameServers) (\s@ListGameServersResponse' {} a -> s {gameServers = a} :: ListGameServersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGameServersResponse_httpStatus :: Lens.Lens' ListGameServersResponse Prelude.Int
listGameServersResponse_httpStatus = Lens.lens (\ListGameServersResponse' {httpStatus} -> httpStatus) (\s@ListGameServersResponse' {} a -> s {httpStatus = a} :: ListGameServersResponse)

instance Prelude.NFData ListGameServersResponse

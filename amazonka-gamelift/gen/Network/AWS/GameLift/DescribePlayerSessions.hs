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
-- Module      : Network.AWS.GameLift.DescribePlayerSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for one or more player sessions. This operation can
-- be used in several ways: (1) provide a @PlayerSessionId@ to request
-- properties for a specific player session; (2) provide a @GameSessionId@
-- to request properties for all player sessions in the specified game
-- session; (3) provide a @PlayerId@ to request properties for all player
-- sessions of a specified player.
--
-- To get game session record(s), specify only one of the following: a
-- player session ID, a game session ID, or a player ID. You can filter
-- this request by player session status. Use the pagination parameters to
-- retrieve results as a set of sequential pages. If successful, a
-- PlayerSession object is returned for each session matching the request.
--
-- /Available in Amazon GameLift Local./
--
-- -   CreatePlayerSession
--
-- -   CreatePlayerSessions
--
-- -   DescribePlayerSessions
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribePlayerSessions
  ( -- * Creating a Request
    DescribePlayerSessions (..),
    newDescribePlayerSessions,

    -- * Request Lenses
    describePlayerSessions_nextToken,
    describePlayerSessions_playerSessionStatusFilter,
    describePlayerSessions_playerId,
    describePlayerSessions_playerSessionId,
    describePlayerSessions_gameSessionId,
    describePlayerSessions_limit,

    -- * Destructuring the Response
    DescribePlayerSessionsResponse (..),
    newDescribePlayerSessionsResponse,

    -- * Response Lenses
    describePlayerSessionsResponse_nextToken,
    describePlayerSessionsResponse_playerSessions,
    describePlayerSessionsResponse_httpStatus,
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
-- /See:/ 'newDescribePlayerSessions' smart constructor.
data DescribePlayerSessions = DescribePlayerSessions'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value. If
    -- a player session ID is specified, this parameter is ignored.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Player session status to filter results on.
    --
    -- Possible player session statuses include the following:
    --
    -- -   __RESERVED__ -- The player session request has been received, but
    --     the player has not yet connected to the server process and\/or been
    --     validated.
    --
    -- -   __ACTIVE__ -- The player has been validated by the server process
    --     and is currently connected.
    --
    -- -   __COMPLETED__ -- The player connection has been dropped.
    --
    -- -   __TIMEDOUT__ -- A player session request was received, but the
    --     player did not connect and\/or was not validated within the timeout
    --     limit (60 seconds).
    playerSessionStatusFilter :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player to retrieve player sessions for.
    playerId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player session to retrieve.
    playerSessionId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session to retrieve player sessions
    -- for.
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. If a player
    -- session ID is specified, this parameter is ignored.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePlayerSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePlayerSessions_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value. If
-- a player session ID is specified, this parameter is ignored.
--
-- 'playerSessionStatusFilter', 'describePlayerSessions_playerSessionStatusFilter' - Player session status to filter results on.
--
-- Possible player session statuses include the following:
--
-- -   __RESERVED__ -- The player session request has been received, but
--     the player has not yet connected to the server process and\/or been
--     validated.
--
-- -   __ACTIVE__ -- The player has been validated by the server process
--     and is currently connected.
--
-- -   __COMPLETED__ -- The player connection has been dropped.
--
-- -   __TIMEDOUT__ -- A player session request was received, but the
--     player did not connect and\/or was not validated within the timeout
--     limit (60 seconds).
--
-- 'playerId', 'describePlayerSessions_playerId' - A unique identifier for a player to retrieve player sessions for.
--
-- 'playerSessionId', 'describePlayerSessions_playerSessionId' - A unique identifier for a player session to retrieve.
--
-- 'gameSessionId', 'describePlayerSessions_gameSessionId' - A unique identifier for the game session to retrieve player sessions
-- for.
--
-- 'limit', 'describePlayerSessions_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. If a player
-- session ID is specified, this parameter is ignored.
newDescribePlayerSessions ::
  DescribePlayerSessions
newDescribePlayerSessions =
  DescribePlayerSessions'
    { nextToken =
        Prelude.Nothing,
      playerSessionStatusFilter = Prelude.Nothing,
      playerId = Prelude.Nothing,
      playerSessionId = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value. If
-- a player session ID is specified, this parameter is ignored.
describePlayerSessions_nextToken :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_nextToken = Lens.lens (\DescribePlayerSessions' {nextToken} -> nextToken) (\s@DescribePlayerSessions' {} a -> s {nextToken = a} :: DescribePlayerSessions)

-- | Player session status to filter results on.
--
-- Possible player session statuses include the following:
--
-- -   __RESERVED__ -- The player session request has been received, but
--     the player has not yet connected to the server process and\/or been
--     validated.
--
-- -   __ACTIVE__ -- The player has been validated by the server process
--     and is currently connected.
--
-- -   __COMPLETED__ -- The player connection has been dropped.
--
-- -   __TIMEDOUT__ -- A player session request was received, but the
--     player did not connect and\/or was not validated within the timeout
--     limit (60 seconds).
describePlayerSessions_playerSessionStatusFilter :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_playerSessionStatusFilter = Lens.lens (\DescribePlayerSessions' {playerSessionStatusFilter} -> playerSessionStatusFilter) (\s@DescribePlayerSessions' {} a -> s {playerSessionStatusFilter = a} :: DescribePlayerSessions)

-- | A unique identifier for a player to retrieve player sessions for.
describePlayerSessions_playerId :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_playerId = Lens.lens (\DescribePlayerSessions' {playerId} -> playerId) (\s@DescribePlayerSessions' {} a -> s {playerId = a} :: DescribePlayerSessions)

-- | A unique identifier for a player session to retrieve.
describePlayerSessions_playerSessionId :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_playerSessionId = Lens.lens (\DescribePlayerSessions' {playerSessionId} -> playerSessionId) (\s@DescribePlayerSessions' {} a -> s {playerSessionId = a} :: DescribePlayerSessions)

-- | A unique identifier for the game session to retrieve player sessions
-- for.
describePlayerSessions_gameSessionId :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_gameSessionId = Lens.lens (\DescribePlayerSessions' {gameSessionId} -> gameSessionId) (\s@DescribePlayerSessions' {} a -> s {gameSessionId = a} :: DescribePlayerSessions)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. If a player
-- session ID is specified, this parameter is ignored.
describePlayerSessions_limit :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Natural)
describePlayerSessions_limit = Lens.lens (\DescribePlayerSessions' {limit} -> limit) (\s@DescribePlayerSessions' {} a -> s {limit = a} :: DescribePlayerSessions)

instance Core.AWSPager DescribePlayerSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePlayerSessionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePlayerSessionsResponse_playerSessions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePlayerSessions_nextToken
          Lens..~ rs
          Lens.^? describePlayerSessionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribePlayerSessions where
  type
    AWSResponse DescribePlayerSessions =
      DescribePlayerSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePlayerSessionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "PlayerSessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePlayerSessions

instance Prelude.NFData DescribePlayerSessions

instance Core.ToHeaders DescribePlayerSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribePlayerSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePlayerSessions where
  toJSON DescribePlayerSessions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("PlayerSessionStatusFilter" Core..=)
              Prelude.<$> playerSessionStatusFilter,
            ("PlayerId" Core..=) Prelude.<$> playerId,
            ("PlayerSessionId" Core..=)
              Prelude.<$> playerSessionId,
            ("GameSessionId" Core..=) Prelude.<$> gameSessionId,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribePlayerSessions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePlayerSessions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribePlayerSessionsResponse' smart constructor.
data DescribePlayerSessionsResponse = DescribePlayerSessionsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing properties for each player session
    -- that matches the request.
    playerSessions :: Prelude.Maybe [PlayerSession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePlayerSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePlayerSessionsResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'playerSessions', 'describePlayerSessionsResponse_playerSessions' - A collection of objects containing properties for each player session
-- that matches the request.
--
-- 'httpStatus', 'describePlayerSessionsResponse_httpStatus' - The response's http status code.
newDescribePlayerSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePlayerSessionsResponse
newDescribePlayerSessionsResponse pHttpStatus_ =
  DescribePlayerSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      playerSessions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describePlayerSessionsResponse_nextToken :: Lens.Lens' DescribePlayerSessionsResponse (Prelude.Maybe Prelude.Text)
describePlayerSessionsResponse_nextToken = Lens.lens (\DescribePlayerSessionsResponse' {nextToken} -> nextToken) (\s@DescribePlayerSessionsResponse' {} a -> s {nextToken = a} :: DescribePlayerSessionsResponse)

-- | A collection of objects containing properties for each player session
-- that matches the request.
describePlayerSessionsResponse_playerSessions :: Lens.Lens' DescribePlayerSessionsResponse (Prelude.Maybe [PlayerSession])
describePlayerSessionsResponse_playerSessions = Lens.lens (\DescribePlayerSessionsResponse' {playerSessions} -> playerSessions) (\s@DescribePlayerSessionsResponse' {} a -> s {playerSessions = a} :: DescribePlayerSessionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePlayerSessionsResponse_httpStatus :: Lens.Lens' DescribePlayerSessionsResponse Prelude.Int
describePlayerSessionsResponse_httpStatus = Lens.lens (\DescribePlayerSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribePlayerSessionsResponse' {} a -> s {httpStatus = a} :: DescribePlayerSessionsResponse)

instance
  Prelude.NFData
    DescribePlayerSessionsResponse

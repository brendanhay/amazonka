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
-- Module      : Amazonka.GameLift.DescribePlayerSessions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for one or more player sessions.
--
-- This action can be used in the following ways:
--
-- -   To retrieve a specific player session, provide the player session ID
--     only.
--
-- -   To retrieve all player sessions in a game session, provide the game
--     session ID only.
--
-- -   To retrieve all player sessions for a specific player, provide a
--     player ID only.
--
-- To request player sessions, specify either a player session ID, game
-- session ID, or player ID. You can filter this request by player session
-- status. Use the pagination parameters to retrieve results as a set of
-- sequential pages.
--
-- If successful, a @PlayerSession@ object is returned for each session
-- that matches the request.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribePlayerSessions
  ( -- * Creating a Request
    DescribePlayerSessions (..),
    newDescribePlayerSessions,

    -- * Request Lenses
    describePlayerSessions_gameSessionId,
    describePlayerSessions_limit,
    describePlayerSessions_nextToken,
    describePlayerSessions_playerId,
    describePlayerSessions_playerSessionId,
    describePlayerSessions_playerSessionStatusFilter,

    -- * Destructuring the Response
    DescribePlayerSessionsResponse (..),
    newDescribePlayerSessionsResponse,

    -- * Response Lenses
    describePlayerSessionsResponse_nextToken,
    describePlayerSessionsResponse_playerSessions,
    describePlayerSessionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePlayerSessions' smart constructor.
data DescribePlayerSessions = DescribePlayerSessions'
  { -- | A unique identifier for the game session to retrieve player sessions
    -- for.
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. If a player
    -- session ID is specified, this parameter is ignored.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value. If
    -- a player session ID is specified, this parameter is ignored.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player to retrieve player sessions for.
    playerId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player session to retrieve.
    playerSessionId :: Prelude.Maybe Prelude.Text,
    -- | Player session status to filter results on. Note that when a
    -- PlayerSessionId or PlayerId is provided in a DescribePlayerSessions
    -- request, then the PlayerSessionStatusFilter has no effect on the
    -- response.
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
    playerSessionStatusFilter :: Prelude.Maybe Prelude.Text
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
-- 'gameSessionId', 'describePlayerSessions_gameSessionId' - A unique identifier for the game session to retrieve player sessions
-- for.
--
-- 'limit', 'describePlayerSessions_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. If a player
-- session ID is specified, this parameter is ignored.
--
-- 'nextToken', 'describePlayerSessions_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value. If
-- a player session ID is specified, this parameter is ignored.
--
-- 'playerId', 'describePlayerSessions_playerId' - A unique identifier for a player to retrieve player sessions for.
--
-- 'playerSessionId', 'describePlayerSessions_playerSessionId' - A unique identifier for a player session to retrieve.
--
-- 'playerSessionStatusFilter', 'describePlayerSessions_playerSessionStatusFilter' - Player session status to filter results on. Note that when a
-- PlayerSessionId or PlayerId is provided in a DescribePlayerSessions
-- request, then the PlayerSessionStatusFilter has no effect on the
-- response.
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
newDescribePlayerSessions ::
  DescribePlayerSessions
newDescribePlayerSessions =
  DescribePlayerSessions'
    { gameSessionId =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      playerId = Prelude.Nothing,
      playerSessionId = Prelude.Nothing,
      playerSessionStatusFilter = Prelude.Nothing
    }

-- | A unique identifier for the game session to retrieve player sessions
-- for.
describePlayerSessions_gameSessionId :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_gameSessionId = Lens.lens (\DescribePlayerSessions' {gameSessionId} -> gameSessionId) (\s@DescribePlayerSessions' {} a -> s {gameSessionId = a} :: DescribePlayerSessions)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. If a player
-- session ID is specified, this parameter is ignored.
describePlayerSessions_limit :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Natural)
describePlayerSessions_limit = Lens.lens (\DescribePlayerSessions' {limit} -> limit) (\s@DescribePlayerSessions' {} a -> s {limit = a} :: DescribePlayerSessions)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value. If
-- a player session ID is specified, this parameter is ignored.
describePlayerSessions_nextToken :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_nextToken = Lens.lens (\DescribePlayerSessions' {nextToken} -> nextToken) (\s@DescribePlayerSessions' {} a -> s {nextToken = a} :: DescribePlayerSessions)

-- | A unique identifier for a player to retrieve player sessions for.
describePlayerSessions_playerId :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_playerId = Lens.lens (\DescribePlayerSessions' {playerId} -> playerId) (\s@DescribePlayerSessions' {} a -> s {playerId = a} :: DescribePlayerSessions)

-- | A unique identifier for a player session to retrieve.
describePlayerSessions_playerSessionId :: Lens.Lens' DescribePlayerSessions (Prelude.Maybe Prelude.Text)
describePlayerSessions_playerSessionId = Lens.lens (\DescribePlayerSessions' {playerSessionId} -> playerSessionId) (\s@DescribePlayerSessions' {} a -> s {playerSessionId = a} :: DescribePlayerSessions)

-- | Player session status to filter results on. Note that when a
-- PlayerSessionId or PlayerId is provided in a DescribePlayerSessions
-- request, then the PlayerSessionStatusFilter has no effect on the
-- response.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePlayerSessionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PlayerSessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePlayerSessions where
  hashWithSalt _salt DescribePlayerSessions' {..} =
    _salt `Prelude.hashWithSalt` gameSessionId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` playerId
      `Prelude.hashWithSalt` playerSessionId
      `Prelude.hashWithSalt` playerSessionStatusFilter

instance Prelude.NFData DescribePlayerSessions where
  rnf DescribePlayerSessions' {..} =
    Prelude.rnf gameSessionId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf playerId
      `Prelude.seq` Prelude.rnf playerSessionId
      `Prelude.seq` Prelude.rnf playerSessionStatusFilter

instance Data.ToHeaders DescribePlayerSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribePlayerSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePlayerSessions where
  toJSON DescribePlayerSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GameSessionId" Data..=) Prelude.<$> gameSessionId,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PlayerId" Data..=) Prelude.<$> playerId,
            ("PlayerSessionId" Data..=)
              Prelude.<$> playerSessionId,
            ("PlayerSessionStatusFilter" Data..=)
              Prelude.<$> playerSessionStatusFilter
          ]
      )

instance Data.ToPath DescribePlayerSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePlayerSessions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePlayerSessionsResponse' smart constructor.
data DescribePlayerSessionsResponse = DescribePlayerSessionsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
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
-- 'nextToken', 'describePlayerSessionsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
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

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describePlayerSessionsResponse_nextToken :: Lens.Lens' DescribePlayerSessionsResponse (Prelude.Maybe Prelude.Text)
describePlayerSessionsResponse_nextToken = Lens.lens (\DescribePlayerSessionsResponse' {nextToken} -> nextToken) (\s@DescribePlayerSessionsResponse' {} a -> s {nextToken = a} :: DescribePlayerSessionsResponse)

-- | A collection of objects containing properties for each player session
-- that matches the request.
describePlayerSessionsResponse_playerSessions :: Lens.Lens' DescribePlayerSessionsResponse (Prelude.Maybe [PlayerSession])
describePlayerSessionsResponse_playerSessions = Lens.lens (\DescribePlayerSessionsResponse' {playerSessions} -> playerSessions) (\s@DescribePlayerSessionsResponse' {} a -> s {playerSessions = a} :: DescribePlayerSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePlayerSessionsResponse_httpStatus :: Lens.Lens' DescribePlayerSessionsResponse Prelude.Int
describePlayerSessionsResponse_httpStatus = Lens.lens (\DescribePlayerSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribePlayerSessionsResponse' {} a -> s {httpStatus = a} :: DescribePlayerSessionsResponse)

instance
  Prelude.NFData
    DescribePlayerSessionsResponse
  where
  rnf DescribePlayerSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf playerSessions
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.GameLift.DescribeGameSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a set of one or more game sessions. Request a specific game
-- session or request all game sessions on a fleet. Alternatively, use
-- SearchGameSessions to request a set of active game sessions that are
-- filtered by certain criteria. To retrieve protection policy settings for
-- game sessions, use DescribeGameSessionDetails.
--
-- To get game sessions, specify one of the following: game session ID,
-- fleet ID, or alias ID. You can filter this request by game session
-- status. Use the pagination parameters to retrieve results as a set of
-- sequential pages. If successful, a GameSession object is returned for
-- each game session matching the request.
--
-- /Available in Amazon GameLift Local./
--
-- -   CreateGameSession
--
-- -   DescribeGameSessions
--
-- -   DescribeGameSessionDetails
--
-- -   SearchGameSessions
--
-- -   UpdateGameSession
--
-- -   GetGameSessionLogUrl
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
module Network.AWS.GameLift.DescribeGameSessions
  ( -- * Creating a Request
    DescribeGameSessions (..),
    newDescribeGameSessions,

    -- * Request Lenses
    describeGameSessions_nextToken,
    describeGameSessions_fleetId,
    describeGameSessions_gameSessionId,
    describeGameSessions_statusFilter,
    describeGameSessions_aliasId,
    describeGameSessions_limit,

    -- * Destructuring the Response
    DescribeGameSessionsResponse (..),
    newDescribeGameSessionsResponse,

    -- * Response Lenses
    describeGameSessionsResponse_nextToken,
    describeGameSessionsResponse_gameSessions,
    describeGameSessionsResponse_httpStatus,
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
-- /See:/ 'newDescribeGameSessions' smart constructor.
data DescribeGameSessions = DescribeGameSessions'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a fleet to retrieve all game sessions for. You
    -- can use either the fleet ID or ARN value.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session to retrieve.
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | Game session status to filter results on. Possible game session statuses
    -- include @ACTIVE@, @TERMINATED@, @ACTIVATING@, and @TERMINATING@ (the
    -- last two are transitory).
    statusFilter :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an alias associated with the fleet to retrieve
    -- all game sessions for. You can use either the alias ID or ARN value.
    aliasId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGameSessions_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'fleetId', 'describeGameSessions_fleetId' - A unique identifier for a fleet to retrieve all game sessions for. You
-- can use either the fleet ID or ARN value.
--
-- 'gameSessionId', 'describeGameSessions_gameSessionId' - A unique identifier for the game session to retrieve.
--
-- 'statusFilter', 'describeGameSessions_statusFilter' - Game session status to filter results on. Possible game session statuses
-- include @ACTIVE@, @TERMINATED@, @ACTIVATING@, and @TERMINATING@ (the
-- last two are transitory).
--
-- 'aliasId', 'describeGameSessions_aliasId' - A unique identifier for an alias associated with the fleet to retrieve
-- all game sessions for. You can use either the alias ID or ARN value.
--
-- 'limit', 'describeGameSessions_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newDescribeGameSessions ::
  DescribeGameSessions
newDescribeGameSessions =
  DescribeGameSessions'
    { nextToken = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      statusFilter = Prelude.Nothing,
      aliasId = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeGameSessions_nextToken :: Lens.Lens' DescribeGameSessions (Prelude.Maybe Prelude.Text)
describeGameSessions_nextToken = Lens.lens (\DescribeGameSessions' {nextToken} -> nextToken) (\s@DescribeGameSessions' {} a -> s {nextToken = a} :: DescribeGameSessions)

-- | A unique identifier for a fleet to retrieve all game sessions for. You
-- can use either the fleet ID or ARN value.
describeGameSessions_fleetId :: Lens.Lens' DescribeGameSessions (Prelude.Maybe Prelude.Text)
describeGameSessions_fleetId = Lens.lens (\DescribeGameSessions' {fleetId} -> fleetId) (\s@DescribeGameSessions' {} a -> s {fleetId = a} :: DescribeGameSessions)

-- | A unique identifier for the game session to retrieve.
describeGameSessions_gameSessionId :: Lens.Lens' DescribeGameSessions (Prelude.Maybe Prelude.Text)
describeGameSessions_gameSessionId = Lens.lens (\DescribeGameSessions' {gameSessionId} -> gameSessionId) (\s@DescribeGameSessions' {} a -> s {gameSessionId = a} :: DescribeGameSessions)

-- | Game session status to filter results on. Possible game session statuses
-- include @ACTIVE@, @TERMINATED@, @ACTIVATING@, and @TERMINATING@ (the
-- last two are transitory).
describeGameSessions_statusFilter :: Lens.Lens' DescribeGameSessions (Prelude.Maybe Prelude.Text)
describeGameSessions_statusFilter = Lens.lens (\DescribeGameSessions' {statusFilter} -> statusFilter) (\s@DescribeGameSessions' {} a -> s {statusFilter = a} :: DescribeGameSessions)

-- | A unique identifier for an alias associated with the fleet to retrieve
-- all game sessions for. You can use either the alias ID or ARN value.
describeGameSessions_aliasId :: Lens.Lens' DescribeGameSessions (Prelude.Maybe Prelude.Text)
describeGameSessions_aliasId = Lens.lens (\DescribeGameSessions' {aliasId} -> aliasId) (\s@DescribeGameSessions' {} a -> s {aliasId = a} :: DescribeGameSessions)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeGameSessions_limit :: Lens.Lens' DescribeGameSessions (Prelude.Maybe Prelude.Natural)
describeGameSessions_limit = Lens.lens (\DescribeGameSessions' {limit} -> limit) (\s@DescribeGameSessions' {} a -> s {limit = a} :: DescribeGameSessions)

instance Core.AWSPager DescribeGameSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGameSessionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGameSessionsResponse_gameSessions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeGameSessions_nextToken
          Lens..~ rs
          Lens.^? describeGameSessionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeGameSessions where
  type
    AWSResponse DescribeGameSessions =
      DescribeGameSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "GameSessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGameSessions

instance Prelude.NFData DescribeGameSessions

instance Core.ToHeaders DescribeGameSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeGameSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeGameSessions where
  toJSON DescribeGameSessions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("FleetId" Core..=) Prelude.<$> fleetId,
            ("GameSessionId" Core..=) Prelude.<$> gameSessionId,
            ("StatusFilter" Core..=) Prelude.<$> statusFilter,
            ("AliasId" Core..=) Prelude.<$> aliasId,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeGameSessions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGameSessions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeGameSessionsResponse' smart constructor.
data DescribeGameSessionsResponse = DescribeGameSessionsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing game session properties for each
    -- session matching the request.
    gameSessions :: Prelude.Maybe [GameSession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGameSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGameSessionsResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'gameSessions', 'describeGameSessionsResponse_gameSessions' - A collection of objects containing game session properties for each
-- session matching the request.
--
-- 'httpStatus', 'describeGameSessionsResponse_httpStatus' - The response's http status code.
newDescribeGameSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGameSessionsResponse
newDescribeGameSessionsResponse pHttpStatus_ =
  DescribeGameSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      gameSessions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeGameSessionsResponse_nextToken :: Lens.Lens' DescribeGameSessionsResponse (Prelude.Maybe Prelude.Text)
describeGameSessionsResponse_nextToken = Lens.lens (\DescribeGameSessionsResponse' {nextToken} -> nextToken) (\s@DescribeGameSessionsResponse' {} a -> s {nextToken = a} :: DescribeGameSessionsResponse)

-- | A collection of objects containing game session properties for each
-- session matching the request.
describeGameSessionsResponse_gameSessions :: Lens.Lens' DescribeGameSessionsResponse (Prelude.Maybe [GameSession])
describeGameSessionsResponse_gameSessions = Lens.lens (\DescribeGameSessionsResponse' {gameSessions} -> gameSessions) (\s@DescribeGameSessionsResponse' {} a -> s {gameSessions = a} :: DescribeGameSessionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeGameSessionsResponse_httpStatus :: Lens.Lens' DescribeGameSessionsResponse Prelude.Int
describeGameSessionsResponse_httpStatus = Lens.lens (\DescribeGameSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribeGameSessionsResponse' {} a -> s {httpStatus = a} :: DescribeGameSessionsResponse)

instance Prelude.NFData DescribeGameSessionsResponse

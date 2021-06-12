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
-- Module      : Network.AWS.GameLift.DescribeGameSessionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties, including the protection policy in force, for one
-- or more game sessions. This operation can be used in several ways: (1)
-- provide a @GameSessionId@ or @GameSessionArn@ to request details for a
-- specific game session; (2) provide either a @FleetId@ or an @AliasId@ to
-- request properties for all game sessions running on a fleet.
--
-- To get game session record(s), specify just one of the following: game
-- session ID, fleet ID, or alias ID. You can filter this request by game
-- session status. Use the pagination parameters to retrieve results as a
-- set of sequential pages. If successful, a GameSessionDetail object is
-- returned for each session matching the request.
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
module Network.AWS.GameLift.DescribeGameSessionDetails
  ( -- * Creating a Request
    DescribeGameSessionDetails (..),
    newDescribeGameSessionDetails,

    -- * Request Lenses
    describeGameSessionDetails_nextToken,
    describeGameSessionDetails_fleetId,
    describeGameSessionDetails_gameSessionId,
    describeGameSessionDetails_statusFilter,
    describeGameSessionDetails_aliasId,
    describeGameSessionDetails_limit,

    -- * Destructuring the Response
    DescribeGameSessionDetailsResponse (..),
    newDescribeGameSessionDetailsResponse,

    -- * Response Lenses
    describeGameSessionDetailsResponse_nextToken,
    describeGameSessionDetailsResponse_gameSessionDetails,
    describeGameSessionDetailsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeGameSessionDetails' smart constructor.
data DescribeGameSessionDetails = DescribeGameSessionDetails'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | A unique identifier for a fleet to retrieve all game sessions active on
    -- the fleet. You can use either the fleet ID or ARN value.
    fleetId :: Core.Maybe Core.Text,
    -- | A unique identifier for the game session to retrieve.
    gameSessionId :: Core.Maybe Core.Text,
    -- | Game session status to filter results on. Possible game session statuses
    -- include @ACTIVE@, @TERMINATED@, @ACTIVATING@ and @TERMINATING@ (the last
    -- two are transitory).
    statusFilter :: Core.Maybe Core.Text,
    -- | A unique identifier for an alias associated with the fleet to retrieve
    -- all game sessions for. You can use either the alias ID or ARN value.
    aliasId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGameSessionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGameSessionDetails_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'fleetId', 'describeGameSessionDetails_fleetId' - A unique identifier for a fleet to retrieve all game sessions active on
-- the fleet. You can use either the fleet ID or ARN value.
--
-- 'gameSessionId', 'describeGameSessionDetails_gameSessionId' - A unique identifier for the game session to retrieve.
--
-- 'statusFilter', 'describeGameSessionDetails_statusFilter' - Game session status to filter results on. Possible game session statuses
-- include @ACTIVE@, @TERMINATED@, @ACTIVATING@ and @TERMINATING@ (the last
-- two are transitory).
--
-- 'aliasId', 'describeGameSessionDetails_aliasId' - A unique identifier for an alias associated with the fleet to retrieve
-- all game sessions for. You can use either the alias ID or ARN value.
--
-- 'limit', 'describeGameSessionDetails_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newDescribeGameSessionDetails ::
  DescribeGameSessionDetails
newDescribeGameSessionDetails =
  DescribeGameSessionDetails'
    { nextToken =
        Core.Nothing,
      fleetId = Core.Nothing,
      gameSessionId = Core.Nothing,
      statusFilter = Core.Nothing,
      aliasId = Core.Nothing,
      limit = Core.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeGameSessionDetails_nextToken :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Core.Text)
describeGameSessionDetails_nextToken = Lens.lens (\DescribeGameSessionDetails' {nextToken} -> nextToken) (\s@DescribeGameSessionDetails' {} a -> s {nextToken = a} :: DescribeGameSessionDetails)

-- | A unique identifier for a fleet to retrieve all game sessions active on
-- the fleet. You can use either the fleet ID or ARN value.
describeGameSessionDetails_fleetId :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Core.Text)
describeGameSessionDetails_fleetId = Lens.lens (\DescribeGameSessionDetails' {fleetId} -> fleetId) (\s@DescribeGameSessionDetails' {} a -> s {fleetId = a} :: DescribeGameSessionDetails)

-- | A unique identifier for the game session to retrieve.
describeGameSessionDetails_gameSessionId :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Core.Text)
describeGameSessionDetails_gameSessionId = Lens.lens (\DescribeGameSessionDetails' {gameSessionId} -> gameSessionId) (\s@DescribeGameSessionDetails' {} a -> s {gameSessionId = a} :: DescribeGameSessionDetails)

-- | Game session status to filter results on. Possible game session statuses
-- include @ACTIVE@, @TERMINATED@, @ACTIVATING@ and @TERMINATING@ (the last
-- two are transitory).
describeGameSessionDetails_statusFilter :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Core.Text)
describeGameSessionDetails_statusFilter = Lens.lens (\DescribeGameSessionDetails' {statusFilter} -> statusFilter) (\s@DescribeGameSessionDetails' {} a -> s {statusFilter = a} :: DescribeGameSessionDetails)

-- | A unique identifier for an alias associated with the fleet to retrieve
-- all game sessions for. You can use either the alias ID or ARN value.
describeGameSessionDetails_aliasId :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Core.Text)
describeGameSessionDetails_aliasId = Lens.lens (\DescribeGameSessionDetails' {aliasId} -> aliasId) (\s@DescribeGameSessionDetails' {} a -> s {aliasId = a} :: DescribeGameSessionDetails)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeGameSessionDetails_limit :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Core.Natural)
describeGameSessionDetails_limit = Lens.lens (\DescribeGameSessionDetails' {limit} -> limit) (\s@DescribeGameSessionDetails' {} a -> s {limit = a} :: DescribeGameSessionDetails)

instance Core.AWSPager DescribeGameSessionDetails where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGameSessionDetailsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGameSessionDetailsResponse_gameSessionDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeGameSessionDetails_nextToken
          Lens..~ rs
          Lens.^? describeGameSessionDetailsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeGameSessionDetails where
  type
    AWSResponse DescribeGameSessionDetails =
      DescribeGameSessionDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionDetailsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "GameSessionDetails"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeGameSessionDetails

instance Core.NFData DescribeGameSessionDetails

instance Core.ToHeaders DescribeGameSessionDetails where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeGameSessionDetails" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeGameSessionDetails where
  toJSON DescribeGameSessionDetails' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("FleetId" Core..=) Core.<$> fleetId,
            ("GameSessionId" Core..=) Core.<$> gameSessionId,
            ("StatusFilter" Core..=) Core.<$> statusFilter,
            ("AliasId" Core..=) Core.<$> aliasId,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeGameSessionDetails where
  toPath = Core.const "/"

instance Core.ToQuery DescribeGameSessionDetails where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeGameSessionDetailsResponse' smart constructor.
data DescribeGameSessionDetailsResponse = DescribeGameSessionDetailsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of objects containing game session properties and the
    -- protection policy currently in force for each session matching the
    -- request.
    gameSessionDetails :: Core.Maybe [GameSessionDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGameSessionDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGameSessionDetailsResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'gameSessionDetails', 'describeGameSessionDetailsResponse_gameSessionDetails' - A collection of objects containing game session properties and the
-- protection policy currently in force for each session matching the
-- request.
--
-- 'httpStatus', 'describeGameSessionDetailsResponse_httpStatus' - The response's http status code.
newDescribeGameSessionDetailsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeGameSessionDetailsResponse
newDescribeGameSessionDetailsResponse pHttpStatus_ =
  DescribeGameSessionDetailsResponse'
    { nextToken =
        Core.Nothing,
      gameSessionDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeGameSessionDetailsResponse_nextToken :: Lens.Lens' DescribeGameSessionDetailsResponse (Core.Maybe Core.Text)
describeGameSessionDetailsResponse_nextToken = Lens.lens (\DescribeGameSessionDetailsResponse' {nextToken} -> nextToken) (\s@DescribeGameSessionDetailsResponse' {} a -> s {nextToken = a} :: DescribeGameSessionDetailsResponse)

-- | A collection of objects containing game session properties and the
-- protection policy currently in force for each session matching the
-- request.
describeGameSessionDetailsResponse_gameSessionDetails :: Lens.Lens' DescribeGameSessionDetailsResponse (Core.Maybe [GameSessionDetail])
describeGameSessionDetailsResponse_gameSessionDetails = Lens.lens (\DescribeGameSessionDetailsResponse' {gameSessionDetails} -> gameSessionDetails) (\s@DescribeGameSessionDetailsResponse' {} a -> s {gameSessionDetails = a} :: DescribeGameSessionDetailsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeGameSessionDetailsResponse_httpStatus :: Lens.Lens' DescribeGameSessionDetailsResponse Core.Int
describeGameSessionDetailsResponse_httpStatus = Lens.lens (\DescribeGameSessionDetailsResponse' {httpStatus} -> httpStatus) (\s@DescribeGameSessionDetailsResponse' {} a -> s {httpStatus = a} :: DescribeGameSessionDetailsResponse)

instance
  Core.NFData
    DescribeGameSessionDetailsResponse

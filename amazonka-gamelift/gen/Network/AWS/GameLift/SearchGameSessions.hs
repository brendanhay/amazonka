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
-- Module      : Network.AWS.GameLift.SearchGameSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active game sessions that match a set of search criteria
-- and sorts them in a specified order. You can search or sort by the
-- following game session attributes:
--
-- -   __gameSessionId__ -- A unique identifier for the game session. You
--     can use either a @GameSessionId@ or @GameSessionArn@ value.
--
-- -   __gameSessionName__ -- Name assigned to a game session. This value
--     is set when requesting a new game session with CreateGameSession or
--     updating with UpdateGameSession. Game session names do not need to
--     be unique to a game session.
--
-- -   __gameSessionProperties__ -- Custom data defined in a game
--     session\'s @GameProperty@ parameter. @GameProperty@ values are
--     stored as key:value pairs; the filter expression must indicate the
--     key and a string to search the data values for. For example, to
--     search for game sessions with custom data containing the key:value
--     pair \"gameMode:brawl\", specify the following:
--     @gameSessionProperties.gameMode = \"brawl\"@. All custom data values
--     are searched as strings.
--
-- -   __maximumSessions__ -- Maximum number of player sessions allowed for
--     a game session. This value is set when requesting a new game session
--     with CreateGameSession or updating with UpdateGameSession.
--
-- -   __creationTimeMillis__ -- Value indicating when a game session was
--     created. It is expressed in Unix time as milliseconds.
--
-- -   __playerSessionCount__ -- Number of players currently connected to a
--     game session. This value changes rapidly as players join the session
--     or drop out.
--
-- -   __hasAvailablePlayerSessions__ -- Boolean value indicating whether a
--     game session has reached its maximum number of players. It is highly
--     recommended that all search requests include this filter attribute
--     to optimize search performance and return only sessions that players
--     can join.
--
-- Returned values for @playerSessionCount@ and
-- @hasAvailablePlayerSessions@ change quickly as players join sessions and
-- others drop out. Results should be considered a snapshot in time. Be
-- sure to refresh search results often, and handle sessions that fill up
-- before a player can join.
--
-- To search or sort, specify either a fleet ID or an alias ID, and provide
-- a search filter expression, a sort expression, or both. If successful, a
-- collection of GameSession objects matching the request is returned. Use
-- the pagination parameters to retrieve results as a set of sequential
-- pages.
--
-- You can search for game sessions one fleet at a time only. To find game
-- sessions across multiple fleets, you must search each fleet separately
-- and combine the results. This search feature finds only game sessions
-- that are in @ACTIVE@ status. To locate games in statuses other than
-- active, use DescribeGameSessionDetails.
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
module Network.AWS.GameLift.SearchGameSessions
  ( -- * Creating a Request
    SearchGameSessions (..),
    newSearchGameSessions,

    -- * Request Lenses
    searchGameSessions_nextToken,
    searchGameSessions_fleetId,
    searchGameSessions_sortExpression,
    searchGameSessions_filterExpression,
    searchGameSessions_aliasId,
    searchGameSessions_limit,

    -- * Destructuring the Response
    SearchGameSessionsResponse (..),
    newSearchGameSessionsResponse,

    -- * Response Lenses
    searchGameSessionsResponse_nextToken,
    searchGameSessionsResponse_gameSessions,
    searchGameSessionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newSearchGameSessions' smart constructor.
data SearchGameSessions = SearchGameSessions'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | A unique identifier for a fleet to search for active game sessions. You
    -- can use either the fleet ID or ARN value. Each request must reference
    -- either a fleet ID or alias ID, but not both.
    fleetId :: Core.Maybe Core.Text,
    -- | Instructions on how to sort the search results. If no sort expression is
    -- included, the request returns results in random order. A sort expression
    -- consists of the following elements:
    --
    -- -   __Operand__ -- Name of a game session attribute. Valid values are
    --     @gameSessionName@, @gameSessionId@, @gameSessionProperties@,
    --     @maximumSessions@, @creationTimeMillis@, @playerSessionCount@,
    --     @hasAvailablePlayerSessions@.
    --
    -- -   __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@
    --     (descending).
    --
    -- For example, this sort expression returns the oldest active sessions
    -- first: @\"SortExpression\": \"creationTimeMillis ASC\"@. Results with a
    -- null value for the sort operand are returned at the end of the list.
    sortExpression :: Core.Maybe Core.Text,
    -- | String containing the search criteria for the session search. If no
    -- filter expression is included, the request returns results for all game
    -- sessions in the fleet that are in @ACTIVE@ status.
    --
    -- A filter expression can contain one or multiple conditions. Each
    -- condition consists of the following:
    --
    -- -   __Operand__ -- Name of a game session attribute. Valid values are
    --     @gameSessionName@, @gameSessionId@, @gameSessionProperties@,
    --     @maximumSessions@, @creationTimeMillis@, @playerSessionCount@,
    --     @hasAvailablePlayerSessions@.
    --
    -- -   __Comparator__ -- Valid comparators are: @=@, @\<>@, @\<@, @>@,
    --     @\<=@, @>=@.
    --
    -- -   __Value__ -- Value to be searched for. Values may be numbers,
    --     boolean values (true\/false) or strings depending on the operand.
    --     String values are case sensitive and must be enclosed in single
    --     quotes. Special characters must be escaped. Boolean and string
    --     values can only be used with the comparators @=@ and @\<>@. For
    --     example, the following filter expression searches on
    --     @gameSessionName@:
    --     \"@FilterExpression\": \"gameSessionName = \'Matt\\\\\'s Awesome Game 1\'\"@.
    --
    -- To chain multiple conditions in a single expression, use the logical
    -- keywords @AND@, @OR@, and @NOT@ and parentheses as needed. For example:
    -- @x AND y AND NOT z@, @NOT (x OR y)@.
    --
    -- Session search evaluates conditions from left to right using the
    -- following precedence rules:
    --
    -- 1.  @=@, @\<>@, @\<@, @>@, @\<=@, @>=@
    --
    -- 2.  Parentheses
    --
    -- 3.  NOT
    --
    -- 4.  AND
    --
    -- 5.  OR
    --
    -- For example, this filter expression retrieves game sessions hosting at
    -- least ten players that have an open player slot:
    -- @\"maximumSessions>=10 AND hasAvailablePlayerSessions=true\"@.
    filterExpression :: Core.Maybe Core.Text,
    -- | A unique identifier for an alias associated with the fleet to search for
    -- active game sessions. You can use either the alias ID or ARN value. Each
    -- request must reference either a fleet ID or alias ID, but not both.
    aliasId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. The maximum
    -- number of results returned is 20, even if this value is not set or is
    -- set higher than 20.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchGameSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchGameSessions_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'fleetId', 'searchGameSessions_fleetId' - A unique identifier for a fleet to search for active game sessions. You
-- can use either the fleet ID or ARN value. Each request must reference
-- either a fleet ID or alias ID, but not both.
--
-- 'sortExpression', 'searchGameSessions_sortExpression' - Instructions on how to sort the search results. If no sort expression is
-- included, the request returns results in random order. A sort expression
-- consists of the following elements:
--
-- -   __Operand__ -- Name of a game session attribute. Valid values are
--     @gameSessionName@, @gameSessionId@, @gameSessionProperties@,
--     @maximumSessions@, @creationTimeMillis@, @playerSessionCount@,
--     @hasAvailablePlayerSessions@.
--
-- -   __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@
--     (descending).
--
-- For example, this sort expression returns the oldest active sessions
-- first: @\"SortExpression\": \"creationTimeMillis ASC\"@. Results with a
-- null value for the sort operand are returned at the end of the list.
--
-- 'filterExpression', 'searchGameSessions_filterExpression' - String containing the search criteria for the session search. If no
-- filter expression is included, the request returns results for all game
-- sessions in the fleet that are in @ACTIVE@ status.
--
-- A filter expression can contain one or multiple conditions. Each
-- condition consists of the following:
--
-- -   __Operand__ -- Name of a game session attribute. Valid values are
--     @gameSessionName@, @gameSessionId@, @gameSessionProperties@,
--     @maximumSessions@, @creationTimeMillis@, @playerSessionCount@,
--     @hasAvailablePlayerSessions@.
--
-- -   __Comparator__ -- Valid comparators are: @=@, @\<>@, @\<@, @>@,
--     @\<=@, @>=@.
--
-- -   __Value__ -- Value to be searched for. Values may be numbers,
--     boolean values (true\/false) or strings depending on the operand.
--     String values are case sensitive and must be enclosed in single
--     quotes. Special characters must be escaped. Boolean and string
--     values can only be used with the comparators @=@ and @\<>@. For
--     example, the following filter expression searches on
--     @gameSessionName@:
--     \"@FilterExpression\": \"gameSessionName = \'Matt\\\\\'s Awesome Game 1\'\"@.
--
-- To chain multiple conditions in a single expression, use the logical
-- keywords @AND@, @OR@, and @NOT@ and parentheses as needed. For example:
-- @x AND y AND NOT z@, @NOT (x OR y)@.
--
-- Session search evaluates conditions from left to right using the
-- following precedence rules:
--
-- 1.  @=@, @\<>@, @\<@, @>@, @\<=@, @>=@
--
-- 2.  Parentheses
--
-- 3.  NOT
--
-- 4.  AND
--
-- 5.  OR
--
-- For example, this filter expression retrieves game sessions hosting at
-- least ten players that have an open player slot:
-- @\"maximumSessions>=10 AND hasAvailablePlayerSessions=true\"@.
--
-- 'aliasId', 'searchGameSessions_aliasId' - A unique identifier for an alias associated with the fleet to search for
-- active game sessions. You can use either the alias ID or ARN value. Each
-- request must reference either a fleet ID or alias ID, but not both.
--
-- 'limit', 'searchGameSessions_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. The maximum
-- number of results returned is 20, even if this value is not set or is
-- set higher than 20.
newSearchGameSessions ::
  SearchGameSessions
newSearchGameSessions =
  SearchGameSessions'
    { nextToken = Core.Nothing,
      fleetId = Core.Nothing,
      sortExpression = Core.Nothing,
      filterExpression = Core.Nothing,
      aliasId = Core.Nothing,
      limit = Core.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
searchGameSessions_nextToken :: Lens.Lens' SearchGameSessions (Core.Maybe Core.Text)
searchGameSessions_nextToken = Lens.lens (\SearchGameSessions' {nextToken} -> nextToken) (\s@SearchGameSessions' {} a -> s {nextToken = a} :: SearchGameSessions)

-- | A unique identifier for a fleet to search for active game sessions. You
-- can use either the fleet ID or ARN value. Each request must reference
-- either a fleet ID or alias ID, but not both.
searchGameSessions_fleetId :: Lens.Lens' SearchGameSessions (Core.Maybe Core.Text)
searchGameSessions_fleetId = Lens.lens (\SearchGameSessions' {fleetId} -> fleetId) (\s@SearchGameSessions' {} a -> s {fleetId = a} :: SearchGameSessions)

-- | Instructions on how to sort the search results. If no sort expression is
-- included, the request returns results in random order. A sort expression
-- consists of the following elements:
--
-- -   __Operand__ -- Name of a game session attribute. Valid values are
--     @gameSessionName@, @gameSessionId@, @gameSessionProperties@,
--     @maximumSessions@, @creationTimeMillis@, @playerSessionCount@,
--     @hasAvailablePlayerSessions@.
--
-- -   __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@
--     (descending).
--
-- For example, this sort expression returns the oldest active sessions
-- first: @\"SortExpression\": \"creationTimeMillis ASC\"@. Results with a
-- null value for the sort operand are returned at the end of the list.
searchGameSessions_sortExpression :: Lens.Lens' SearchGameSessions (Core.Maybe Core.Text)
searchGameSessions_sortExpression = Lens.lens (\SearchGameSessions' {sortExpression} -> sortExpression) (\s@SearchGameSessions' {} a -> s {sortExpression = a} :: SearchGameSessions)

-- | String containing the search criteria for the session search. If no
-- filter expression is included, the request returns results for all game
-- sessions in the fleet that are in @ACTIVE@ status.
--
-- A filter expression can contain one or multiple conditions. Each
-- condition consists of the following:
--
-- -   __Operand__ -- Name of a game session attribute. Valid values are
--     @gameSessionName@, @gameSessionId@, @gameSessionProperties@,
--     @maximumSessions@, @creationTimeMillis@, @playerSessionCount@,
--     @hasAvailablePlayerSessions@.
--
-- -   __Comparator__ -- Valid comparators are: @=@, @\<>@, @\<@, @>@,
--     @\<=@, @>=@.
--
-- -   __Value__ -- Value to be searched for. Values may be numbers,
--     boolean values (true\/false) or strings depending on the operand.
--     String values are case sensitive and must be enclosed in single
--     quotes. Special characters must be escaped. Boolean and string
--     values can only be used with the comparators @=@ and @\<>@. For
--     example, the following filter expression searches on
--     @gameSessionName@:
--     \"@FilterExpression\": \"gameSessionName = \'Matt\\\\\'s Awesome Game 1\'\"@.
--
-- To chain multiple conditions in a single expression, use the logical
-- keywords @AND@, @OR@, and @NOT@ and parentheses as needed. For example:
-- @x AND y AND NOT z@, @NOT (x OR y)@.
--
-- Session search evaluates conditions from left to right using the
-- following precedence rules:
--
-- 1.  @=@, @\<>@, @\<@, @>@, @\<=@, @>=@
--
-- 2.  Parentheses
--
-- 3.  NOT
--
-- 4.  AND
--
-- 5.  OR
--
-- For example, this filter expression retrieves game sessions hosting at
-- least ten players that have an open player slot:
-- @\"maximumSessions>=10 AND hasAvailablePlayerSessions=true\"@.
searchGameSessions_filterExpression :: Lens.Lens' SearchGameSessions (Core.Maybe Core.Text)
searchGameSessions_filterExpression = Lens.lens (\SearchGameSessions' {filterExpression} -> filterExpression) (\s@SearchGameSessions' {} a -> s {filterExpression = a} :: SearchGameSessions)

-- | A unique identifier for an alias associated with the fleet to search for
-- active game sessions. You can use either the alias ID or ARN value. Each
-- request must reference either a fleet ID or alias ID, but not both.
searchGameSessions_aliasId :: Lens.Lens' SearchGameSessions (Core.Maybe Core.Text)
searchGameSessions_aliasId = Lens.lens (\SearchGameSessions' {aliasId} -> aliasId) (\s@SearchGameSessions' {} a -> s {aliasId = a} :: SearchGameSessions)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. The maximum
-- number of results returned is 20, even if this value is not set or is
-- set higher than 20.
searchGameSessions_limit :: Lens.Lens' SearchGameSessions (Core.Maybe Core.Natural)
searchGameSessions_limit = Lens.lens (\SearchGameSessions' {limit} -> limit) (\s@SearchGameSessions' {} a -> s {limit = a} :: SearchGameSessions)

instance Core.AWSPager SearchGameSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchGameSessionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? searchGameSessionsResponse_gameSessions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& searchGameSessions_nextToken
          Lens..~ rs
          Lens.^? searchGameSessionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest SearchGameSessions where
  type
    AWSResponse SearchGameSessions =
      SearchGameSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchGameSessionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "GameSessions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchGameSessions

instance Core.NFData SearchGameSessions

instance Core.ToHeaders SearchGameSessions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.SearchGameSessions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchGameSessions where
  toJSON SearchGameSessions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("FleetId" Core..=) Core.<$> fleetId,
            ("SortExpression" Core..=) Core.<$> sortExpression,
            ("FilterExpression" Core..=)
              Core.<$> filterExpression,
            ("AliasId" Core..=) Core.<$> aliasId,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath SearchGameSessions where
  toPath = Core.const "/"

instance Core.ToQuery SearchGameSessions where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newSearchGameSessionsResponse' smart constructor.
data SearchGameSessionsResponse = SearchGameSessionsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of objects containing game session properties for each
    -- session matching the request.
    gameSessions :: Core.Maybe [GameSession],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchGameSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchGameSessionsResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'gameSessions', 'searchGameSessionsResponse_gameSessions' - A collection of objects containing game session properties for each
-- session matching the request.
--
-- 'httpStatus', 'searchGameSessionsResponse_httpStatus' - The response's http status code.
newSearchGameSessionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchGameSessionsResponse
newSearchGameSessionsResponse pHttpStatus_ =
  SearchGameSessionsResponse'
    { nextToken =
        Core.Nothing,
      gameSessions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
searchGameSessionsResponse_nextToken :: Lens.Lens' SearchGameSessionsResponse (Core.Maybe Core.Text)
searchGameSessionsResponse_nextToken = Lens.lens (\SearchGameSessionsResponse' {nextToken} -> nextToken) (\s@SearchGameSessionsResponse' {} a -> s {nextToken = a} :: SearchGameSessionsResponse)

-- | A collection of objects containing game session properties for each
-- session matching the request.
searchGameSessionsResponse_gameSessions :: Lens.Lens' SearchGameSessionsResponse (Core.Maybe [GameSession])
searchGameSessionsResponse_gameSessions = Lens.lens (\SearchGameSessionsResponse' {gameSessions} -> gameSessions) (\s@SearchGameSessionsResponse' {} a -> s {gameSessions = a} :: SearchGameSessionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchGameSessionsResponse_httpStatus :: Lens.Lens' SearchGameSessionsResponse Core.Int
searchGameSessionsResponse_httpStatus = Lens.lens (\SearchGameSessionsResponse' {httpStatus} -> httpStatus) (\s@SearchGameSessionsResponse' {} a -> s {httpStatus = a} :: SearchGameSessionsResponse)

instance Core.NFData SearchGameSessionsResponse

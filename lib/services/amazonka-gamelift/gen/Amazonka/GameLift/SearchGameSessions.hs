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
-- Module      : Amazonka.GameLift.SearchGameSessions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active game sessions that match a set of search criteria
-- and sorts them into a specified order.
--
-- This operation is not designed to be continually called to track game
-- session status. This practice can cause you to exceed your API limit,
-- which results in errors. Instead, you must configure configure an Amazon
-- Simple Notification Service (SNS) topic to receive notifications from
-- FlexMatch or queues. Continuously polling game session status with
-- @DescribeGameSessions@ should only be used for games in development with
-- low game session usage.
--
-- When searching for game sessions, you specify exactly where you want to
-- search and provide a search filter expression, a sort expression, or
-- both. A search request can search only one fleet, but it can search all
-- of a fleet\'s locations.
--
-- This operation can be used in the following ways:
--
-- -   To search all game sessions that are currently running on all
--     locations in a fleet, provide a fleet or alias ID. This approach
--     returns game sessions in the fleet\'s home Region and all remote
--     locations that fit the search criteria.
--
-- -   To search all game sessions that are currently running on a specific
--     fleet location, provide a fleet or alias ID and a location name. For
--     location, you can specify a fleet\'s home Region or any remote
--     location.
--
-- Use the pagination parameters to retrieve results as a set of sequential
-- pages.
--
-- If successful, a @GameSession@ object is returned for each game session
-- that matches the request. Search finds game sessions that are in
-- @ACTIVE@ status only. To retrieve information on game sessions in other
-- statuses, use
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeGameSessions.html DescribeGameSessions>
-- .
--
-- You can search or sort by the following game session attributes:
--
-- -   __gameSessionId__ -- A unique identifier for the game session. You
--     can use either a @GameSessionId@ or @GameSessionArn@ value.
--
-- -   __gameSessionName__ -- Name assigned to a game session. Game session
--     names do not need to be unique to a game session.
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
--     a game session.
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
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.SearchGameSessions
  ( -- * Creating a Request
    SearchGameSessions (..),
    newSearchGameSessions,

    -- * Request Lenses
    searchGameSessions_aliasId,
    searchGameSessions_filterExpression,
    searchGameSessions_fleetId,
    searchGameSessions_limit,
    searchGameSessions_location,
    searchGameSessions_nextToken,
    searchGameSessions_sortExpression,

    -- * Destructuring the Response
    SearchGameSessionsResponse (..),
    newSearchGameSessionsResponse,

    -- * Response Lenses
    searchGameSessionsResponse_gameSessions,
    searchGameSessionsResponse_nextToken,
    searchGameSessionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchGameSessions' smart constructor.
data SearchGameSessions = SearchGameSessions'
  { -- | A unique identifier for the alias associated with the fleet to search
    -- for active game sessions. You can use either the alias ID or ARN value.
    -- Each request must reference either a fleet ID or alias ID, but not both.
    aliasId :: Prelude.Maybe Prelude.Text,
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
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to search for active game sessions.
    -- You can use either the fleet ID or ARN value. Each request must
    -- reference either a fleet ID or alias ID, but not both.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. The maximum
    -- number of results returned is 20, even if this value is not set or is
    -- set higher than 20.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A fleet location to search for game sessions. You can specify a fleet\'s
    -- home Region or a remote location. Use the Amazon Web Services Region
    -- code format, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    sortExpression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchGameSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'searchGameSessions_aliasId' - A unique identifier for the alias associated with the fleet to search
-- for active game sessions. You can use either the alias ID or ARN value.
-- Each request must reference either a fleet ID or alias ID, but not both.
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
-- 'fleetId', 'searchGameSessions_fleetId' - A unique identifier for the fleet to search for active game sessions.
-- You can use either the fleet ID or ARN value. Each request must
-- reference either a fleet ID or alias ID, but not both.
--
-- 'limit', 'searchGameSessions_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. The maximum
-- number of results returned is 20, even if this value is not set or is
-- set higher than 20.
--
-- 'location', 'searchGameSessions_location' - A fleet location to search for game sessions. You can specify a fleet\'s
-- home Region or a remote location. Use the Amazon Web Services Region
-- code format, such as @us-west-2@.
--
-- 'nextToken', 'searchGameSessions_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
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
newSearchGameSessions ::
  SearchGameSessions
newSearchGameSessions =
  SearchGameSessions'
    { aliasId = Prelude.Nothing,
      filterExpression = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      limit = Prelude.Nothing,
      location = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortExpression = Prelude.Nothing
    }

-- | A unique identifier for the alias associated with the fleet to search
-- for active game sessions. You can use either the alias ID or ARN value.
-- Each request must reference either a fleet ID or alias ID, but not both.
searchGameSessions_aliasId :: Lens.Lens' SearchGameSessions (Prelude.Maybe Prelude.Text)
searchGameSessions_aliasId = Lens.lens (\SearchGameSessions' {aliasId} -> aliasId) (\s@SearchGameSessions' {} a -> s {aliasId = a} :: SearchGameSessions)

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
searchGameSessions_filterExpression :: Lens.Lens' SearchGameSessions (Prelude.Maybe Prelude.Text)
searchGameSessions_filterExpression = Lens.lens (\SearchGameSessions' {filterExpression} -> filterExpression) (\s@SearchGameSessions' {} a -> s {filterExpression = a} :: SearchGameSessions)

-- | A unique identifier for the fleet to search for active game sessions.
-- You can use either the fleet ID or ARN value. Each request must
-- reference either a fleet ID or alias ID, but not both.
searchGameSessions_fleetId :: Lens.Lens' SearchGameSessions (Prelude.Maybe Prelude.Text)
searchGameSessions_fleetId = Lens.lens (\SearchGameSessions' {fleetId} -> fleetId) (\s@SearchGameSessions' {} a -> s {fleetId = a} :: SearchGameSessions)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. The maximum
-- number of results returned is 20, even if this value is not set or is
-- set higher than 20.
searchGameSessions_limit :: Lens.Lens' SearchGameSessions (Prelude.Maybe Prelude.Natural)
searchGameSessions_limit = Lens.lens (\SearchGameSessions' {limit} -> limit) (\s@SearchGameSessions' {} a -> s {limit = a} :: SearchGameSessions)

-- | A fleet location to search for game sessions. You can specify a fleet\'s
-- home Region or a remote location. Use the Amazon Web Services Region
-- code format, such as @us-west-2@.
searchGameSessions_location :: Lens.Lens' SearchGameSessions (Prelude.Maybe Prelude.Text)
searchGameSessions_location = Lens.lens (\SearchGameSessions' {location} -> location) (\s@SearchGameSessions' {} a -> s {location = a} :: SearchGameSessions)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
searchGameSessions_nextToken :: Lens.Lens' SearchGameSessions (Prelude.Maybe Prelude.Text)
searchGameSessions_nextToken = Lens.lens (\SearchGameSessions' {nextToken} -> nextToken) (\s@SearchGameSessions' {} a -> s {nextToken = a} :: SearchGameSessions)

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
searchGameSessions_sortExpression :: Lens.Lens' SearchGameSessions (Prelude.Maybe Prelude.Text)
searchGameSessions_sortExpression = Lens.lens (\SearchGameSessions' {sortExpression} -> sortExpression) (\s@SearchGameSessions' {} a -> s {sortExpression = a} :: SearchGameSessions)

instance Core.AWSPager SearchGameSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchGameSessionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchGameSessionsResponse_gameSessions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchGameSessions_nextToken
          Lens..~ rs
          Lens.^? searchGameSessionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchGameSessions where
  type
    AWSResponse SearchGameSessions =
      SearchGameSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchGameSessionsResponse'
            Prelude.<$> (x Data..?> "GameSessions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchGameSessions where
  hashWithSalt _salt SearchGameSessions' {..} =
    _salt
      `Prelude.hashWithSalt` aliasId
      `Prelude.hashWithSalt` filterExpression
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortExpression

instance Prelude.NFData SearchGameSessions where
  rnf SearchGameSessions' {..} =
    Prelude.rnf aliasId
      `Prelude.seq` Prelude.rnf filterExpression
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortExpression

instance Data.ToHeaders SearchGameSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.SearchGameSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchGameSessions where
  toJSON SearchGameSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AliasId" Data..=) Prelude.<$> aliasId,
            ("FilterExpression" Data..=)
              Prelude.<$> filterExpression,
            ("FleetId" Data..=) Prelude.<$> fleetId,
            ("Limit" Data..=) Prelude.<$> limit,
            ("Location" Data..=) Prelude.<$> location,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortExpression" Data..=)
              Prelude.<$> sortExpression
          ]
      )

instance Data.ToPath SearchGameSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchGameSessions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchGameSessionsResponse' smart constructor.
data SearchGameSessionsResponse = SearchGameSessionsResponse'
  { -- | A collection of objects containing game session properties for each
    -- session that matches the request.
    gameSessions :: Prelude.Maybe [GameSession],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchGameSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessions', 'searchGameSessionsResponse_gameSessions' - A collection of objects containing game session properties for each
-- session that matches the request.
--
-- 'nextToken', 'searchGameSessionsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'searchGameSessionsResponse_httpStatus' - The response's http status code.
newSearchGameSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchGameSessionsResponse
newSearchGameSessionsResponse pHttpStatus_ =
  SearchGameSessionsResponse'
    { gameSessions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of objects containing game session properties for each
-- session that matches the request.
searchGameSessionsResponse_gameSessions :: Lens.Lens' SearchGameSessionsResponse (Prelude.Maybe [GameSession])
searchGameSessionsResponse_gameSessions = Lens.lens (\SearchGameSessionsResponse' {gameSessions} -> gameSessions) (\s@SearchGameSessionsResponse' {} a -> s {gameSessions = a} :: SearchGameSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
searchGameSessionsResponse_nextToken :: Lens.Lens' SearchGameSessionsResponse (Prelude.Maybe Prelude.Text)
searchGameSessionsResponse_nextToken = Lens.lens (\SearchGameSessionsResponse' {nextToken} -> nextToken) (\s@SearchGameSessionsResponse' {} a -> s {nextToken = a} :: SearchGameSessionsResponse)

-- | The response's http status code.
searchGameSessionsResponse_httpStatus :: Lens.Lens' SearchGameSessionsResponse Prelude.Int
searchGameSessionsResponse_httpStatus = Lens.lens (\SearchGameSessionsResponse' {httpStatus} -> httpStatus) (\s@SearchGameSessionsResponse' {} a -> s {httpStatus = a} :: SearchGameSessionsResponse)

instance Prelude.NFData SearchGameSessionsResponse where
  rnf SearchGameSessionsResponse' {..} =
    Prelude.rnf gameSessions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

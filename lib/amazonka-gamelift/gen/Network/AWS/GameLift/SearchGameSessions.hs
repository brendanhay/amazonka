{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.SearchGameSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active game sessions that match a set of search criteria and sorts them in a specified order. You can search or sort by the following game session attributes:
--
--
--     * __gameSessionId__ -- A unique identifier for the game session. You can use either a @GameSessionId@ or @GameSessionArn@ value.
--
--
--     * __gameSessionName__ -- Name assigned to a game session. This value is set when requesting a new game session with 'CreateGameSession' or updating with 'UpdateGameSession' . Game session names do not need to be unique to a game session.
--
--
--     * __gameSessionProperties__ -- Custom data defined in a game session's @GameProperty@ parameter. @GameProperty@ values are stored as key:value pairs; the filter expression must indicate the key and a string to search the data values for. For example, to search for game sessions with custom data containing the key:value pair "gameMode:brawl", specify the following: @gameSessionProperties.gameMode = "brawl"@ . All custom data values are searched as strings.
--
--
--     * __maximumSessions__ -- Maximum number of player sessions allowed for a game session. This value is set when requesting a new game session with 'CreateGameSession' or updating with 'UpdateGameSession' .
--
--
--     * __creationTimeMillis__ -- Value indicating when a game session was created. It is expressed in Unix time as milliseconds.
--
--
--     * __playerSessionCount__ -- Number of players currently connected to a game session. This value changes rapidly as players join the session or drop out.
--
--
--     * __hasAvailablePlayerSessions__ -- Boolean value indicating whether a game session has reached its maximum number of players. It is highly recommended that all search requests include this filter attribute to optimize search performance and return only sessions that players can join.
--
--
-- To search or sort, specify either a fleet ID or an alias ID, and provide a search filter expression, a sort expression, or both. If successful, a collection of 'GameSession' objects matching the request is returned. Use the pagination parameters to retrieve results as a set of sequential pages.
-- You can search for game sessions one fleet at a time only. To find game sessions across multiple fleets, you must search each fleet separately and combine the results. This search feature finds only game sessions that are in @ACTIVE@ status. To locate games in statuses other than active, use 'DescribeGameSessionDetails' .
--
--     * 'CreateGameSession'
--
--
--     * 'DescribeGameSessions'
--
--
--     * 'DescribeGameSessionDetails'
--
--
--     * 'SearchGameSessions'
--
--
--     * 'UpdateGameSession'
--
--
--     * 'GetGameSessionLogUrl'
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--
--     * 'DescribeGameSessionPlacement'
--
--
--     * 'StopGameSessionPlacement'
--
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.SearchGameSessions
  ( -- * Creating a request
    SearchGameSessions (..),
    mkSearchGameSessions,

    -- ** Request lenses
    sgsFilterExpression,
    sgsSortExpression,
    sgsAliasId,
    sgsNextToken,
    sgsLimit,
    sgsFleetId,

    -- * Destructuring the response
    SearchGameSessionsResponse (..),
    mkSearchGameSessionsResponse,

    -- ** Response lenses
    sgsrsGameSessions,
    sgsrsNextToken,
    sgsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkSearchGameSessions' smart constructor.
data SearchGameSessions = SearchGameSessions'
  { filterExpression ::
      Lude.Maybe Lude.Text,
    sortExpression :: Lude.Maybe Lude.Text,
    aliasId :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchGameSessions' with the minimum fields required to make a request.
--
-- * 'aliasId' - A unique identifier for an alias associated with the fleet to search for active game sessions. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
-- * 'filterExpression' - String containing the search criteria for the session search. If no filter expression is included, the request returns results for all game sessions in the fleet that are in @ACTIVE@ status.
--
-- A filter expression can contain one or multiple conditions. Each condition consists of the following:
--
--     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .
--
--
--     * __Comparator__ -- Valid comparators are: @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@ .
--
--
--     * __Value__ -- Value to be searched for. Values may be numbers, boolean values (true/false) or strings depending on the operand. String values are case sensitive and must be enclosed in single quotes. Special characters must be escaped. Boolean and string values can only be used with the comparators @=@ and @<>@ . For example, the following filter expression searches on @gameSessionName@ : "@FilterExpression": "gameSessionName = 'Matt\\'s Awesome Game 1'"@ .
--
--
-- To chain multiple conditions in a single expression, use the logical keywords @AND@ , @OR@ , and @NOT@ and parentheses as needed. For example: @x AND y AND NOT z@ , @NOT (x OR y)@ .
-- Session search evaluates conditions from left to right using the following precedence rules:
--
--     * @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@
--
--
--     * Parentheses
--
--
--     * NOT
--
--
--     * AND
--
--
--     * OR
--
--
-- For example, this filter expression retrieves game sessions hosting at least ten players that have an open player slot: @"maximumSessions>=10 AND hasAvailablePlayerSessions=true"@ .
-- * 'fleetId' - A unique identifier for a fleet to search for active game sessions. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. The maximum number of results returned is 20, even if this value is not set or is set higher than 20.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'sortExpression' - Instructions on how to sort the search results. If no sort expression is included, the request returns results in random order. A sort expression consists of the following elements:
--
--
--     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .
--
--
--     * __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@ (descending).
--
--
-- For example, this sort expression returns the oldest active sessions first: @"SortExpression": "creationTimeMillis ASC"@ . Results with a null value for the sort operand are returned at the end of the list.
mkSearchGameSessions ::
  SearchGameSessions
mkSearchGameSessions =
  SearchGameSessions'
    { filterExpression = Lude.Nothing,
      sortExpression = Lude.Nothing,
      aliasId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      fleetId = Lude.Nothing
    }

-- | String containing the search criteria for the session search. If no filter expression is included, the request returns results for all game sessions in the fleet that are in @ACTIVE@ status.
--
-- A filter expression can contain one or multiple conditions. Each condition consists of the following:
--
--     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .
--
--
--     * __Comparator__ -- Valid comparators are: @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@ .
--
--
--     * __Value__ -- Value to be searched for. Values may be numbers, boolean values (true/false) or strings depending on the operand. String values are case sensitive and must be enclosed in single quotes. Special characters must be escaped. Boolean and string values can only be used with the comparators @=@ and @<>@ . For example, the following filter expression searches on @gameSessionName@ : "@FilterExpression": "gameSessionName = 'Matt\\'s Awesome Game 1'"@ .
--
--
-- To chain multiple conditions in a single expression, use the logical keywords @AND@ , @OR@ , and @NOT@ and parentheses as needed. For example: @x AND y AND NOT z@ , @NOT (x OR y)@ .
-- Session search evaluates conditions from left to right using the following precedence rules:
--
--     * @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@
--
--
--     * Parentheses
--
--
--     * NOT
--
--
--     * AND
--
--
--     * OR
--
--
-- For example, this filter expression retrieves game sessions hosting at least ten players that have an open player slot: @"maximumSessions>=10 AND hasAvailablePlayerSessions=true"@ .
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsFilterExpression :: Lens.Lens' SearchGameSessions (Lude.Maybe Lude.Text)
sgsFilterExpression = Lens.lens (filterExpression :: SearchGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {filterExpression = a} :: SearchGameSessions)
{-# DEPRECATED sgsFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | Instructions on how to sort the search results. If no sort expression is included, the request returns results in random order. A sort expression consists of the following elements:
--
--
--     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .
--
--
--     * __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@ (descending).
--
--
-- For example, this sort expression returns the oldest active sessions first: @"SortExpression": "creationTimeMillis ASC"@ . Results with a null value for the sort operand are returned at the end of the list.
--
-- /Note:/ Consider using 'sortExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsSortExpression :: Lens.Lens' SearchGameSessions (Lude.Maybe Lude.Text)
sgsSortExpression = Lens.lens (sortExpression :: SearchGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {sortExpression = a} :: SearchGameSessions)
{-# DEPRECATED sgsSortExpression "Use generic-lens or generic-optics with 'sortExpression' instead." #-}

-- | A unique identifier for an alias associated with the fleet to search for active game sessions. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsAliasId :: Lens.Lens' SearchGameSessions (Lude.Maybe Lude.Text)
sgsAliasId = Lens.lens (aliasId :: SearchGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: SearchGameSessions)
{-# DEPRECATED sgsAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsNextToken :: Lens.Lens' SearchGameSessions (Lude.Maybe Lude.Text)
sgsNextToken = Lens.lens (nextToken :: SearchGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchGameSessions)
{-# DEPRECATED sgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. The maximum number of results returned is 20, even if this value is not set or is set higher than 20.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsLimit :: Lens.Lens' SearchGameSessions (Lude.Maybe Lude.Natural)
sgsLimit = Lens.lens (limit :: SearchGameSessions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: SearchGameSessions)
{-# DEPRECATED sgsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet to search for active game sessions. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsFleetId :: Lens.Lens' SearchGameSessions (Lude.Maybe Lude.Text)
sgsFleetId = Lens.lens (fleetId :: SearchGameSessions -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: SearchGameSessions)
{-# DEPRECATED sgsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Page.AWSPager SearchGameSessions where
  page rq rs
    | Page.stop (rs Lens.^. sgsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. sgsrsGameSessions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& sgsNextToken Lens..~ rs Lens.^. sgsrsNextToken

instance Lude.AWSRequest SearchGameSessions where
  type Rs SearchGameSessions = SearchGameSessionsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchGameSessionsResponse'
            Lude.<$> (x Lude..?> "GameSessions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchGameSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.SearchGameSessions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchGameSessions where
  toJSON SearchGameSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterExpression" Lude..=) Lude.<$> filterExpression,
            ("SortExpression" Lude..=) Lude.<$> sortExpression,
            ("AliasId" Lude..=) Lude.<$> aliasId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("FleetId" Lude..=) Lude.<$> fleetId
          ]
      )

instance Lude.ToPath SearchGameSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchGameSessions where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkSearchGameSessionsResponse' smart constructor.
data SearchGameSessionsResponse = SearchGameSessionsResponse'
  { gameSessions ::
      Lude.Maybe [GameSession],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchGameSessionsResponse' with the minimum fields required to make a request.
--
-- * 'gameSessions' - A collection of objects containing game session properties for each session matching the request.
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkSearchGameSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchGameSessionsResponse
mkSearchGameSessionsResponse pResponseStatus_ =
  SearchGameSessionsResponse'
    { gameSessions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of objects containing game session properties for each session matching the request.
--
-- /Note:/ Consider using 'gameSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsrsGameSessions :: Lens.Lens' SearchGameSessionsResponse (Lude.Maybe [GameSession])
sgsrsGameSessions = Lens.lens (gameSessions :: SearchGameSessionsResponse -> Lude.Maybe [GameSession]) (\s a -> s {gameSessions = a} :: SearchGameSessionsResponse)
{-# DEPRECATED sgsrsGameSessions "Use generic-lens or generic-optics with 'gameSessions' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsrsNextToken :: Lens.Lens' SearchGameSessionsResponse (Lude.Maybe Lude.Text)
sgsrsNextToken = Lens.lens (nextToken :: SearchGameSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchGameSessionsResponse)
{-# DEPRECATED sgsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsrsResponseStatus :: Lens.Lens' SearchGameSessionsResponse Lude.Int
sgsrsResponseStatus = Lens.lens (responseStatus :: SearchGameSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchGameSessionsResponse)
{-# DEPRECATED sgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

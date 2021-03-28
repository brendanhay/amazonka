{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SearchGameSessions (..)
    , mkSearchGameSessions
    -- ** Request lenses
    , sgsAliasId
    , sgsFilterExpression
    , sgsFleetId
    , sgsLimit
    , sgsNextToken
    , sgsSortExpression

    -- * Destructuring the response
    , SearchGameSessionsResponse (..)
    , mkSearchGameSessionsResponse
    -- ** Response lenses
    , sgsrrsGameSessions
    , sgsrrsNextToken
    , sgsrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkSearchGameSessions' smart constructor.
data SearchGameSessions = SearchGameSessions'
  { aliasId :: Core.Maybe Types.AliasIdOrArn
    -- ^ A unique identifier for an alias associated with the fleet to search for active game sessions. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
  , filterExpression :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ String containing the search criteria for the session search. If no filter expression is included, the request returns results for all game sessions in the fleet that are in @ACTIVE@ status.
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
  , fleetId :: Core.Maybe Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to search for active game sessions. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. The maximum number of results returned is 20, even if this value is not set or is set higher than 20. 
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  , sortExpression :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Instructions on how to sort the search results. If no sort expression is included, the request returns results in random order. A sort expression consists of the following elements:
--
--
--     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .
--
--
--     * __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@ (descending).
--
--
-- For example, this sort expression returns the oldest active sessions first: @"SortExpression": "creationTimeMillis ASC"@ . Results with a null value for the sort operand are returned at the end of the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchGameSessions' value with any optional fields omitted.
mkSearchGameSessions
    :: SearchGameSessions
mkSearchGameSessions
  = SearchGameSessions'{aliasId = Core.Nothing,
                        filterExpression = Core.Nothing, fleetId = Core.Nothing,
                        limit = Core.Nothing, nextToken = Core.Nothing,
                        sortExpression = Core.Nothing}

-- | A unique identifier for an alias associated with the fleet to search for active game sessions. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsAliasId :: Lens.Lens' SearchGameSessions (Core.Maybe Types.AliasIdOrArn)
sgsAliasId = Lens.field @"aliasId"
{-# INLINEABLE sgsAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

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
sgsFilterExpression :: Lens.Lens' SearchGameSessions (Core.Maybe Types.NonZeroAndMaxString)
sgsFilterExpression = Lens.field @"filterExpression"
{-# INLINEABLE sgsFilterExpression #-}
{-# DEPRECATED filterExpression "Use generic-lens or generic-optics with 'filterExpression' instead"  #-}

-- | A unique identifier for a fleet to search for active game sessions. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsFleetId :: Lens.Lens' SearchGameSessions (Core.Maybe Types.FleetIdOrArn)
sgsFleetId = Lens.field @"fleetId"
{-# INLINEABLE sgsFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. The maximum number of results returned is 20, even if this value is not set or is set higher than 20. 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsLimit :: Lens.Lens' SearchGameSessions (Core.Maybe Core.Natural)
sgsLimit = Lens.field @"limit"
{-# INLINEABLE sgsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsNextToken :: Lens.Lens' SearchGameSessions (Core.Maybe Types.NonZeroAndMaxString)
sgsNextToken = Lens.field @"nextToken"
{-# INLINEABLE sgsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

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
sgsSortExpression :: Lens.Lens' SearchGameSessions (Core.Maybe Types.NonZeroAndMaxString)
sgsSortExpression = Lens.field @"sortExpression"
{-# INLINEABLE sgsSortExpression #-}
{-# DEPRECATED sortExpression "Use generic-lens or generic-optics with 'sortExpression' instead"  #-}

instance Core.ToQuery SearchGameSessions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchGameSessions where
        toHeaders SearchGameSessions{..}
          = Core.pure ("X-Amz-Target", "GameLift.SearchGameSessions") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SearchGameSessions where
        toJSON SearchGameSessions{..}
          = Core.object
              (Core.catMaybes
                 [("AliasId" Core..=) Core.<$> aliasId,
                  ("FilterExpression" Core..=) Core.<$> filterExpression,
                  ("FleetId" Core..=) Core.<$> fleetId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortExpression" Core..=) Core.<$> sortExpression])

instance Core.AWSRequest SearchGameSessions where
        type Rs SearchGameSessions = SearchGameSessionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchGameSessionsResponse' Core.<$>
                   (x Core..:? "GameSessions") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager SearchGameSessions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"gameSessions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkSearchGameSessionsResponse' smart constructor.
data SearchGameSessionsResponse = SearchGameSessionsResponse'
  { gameSessions :: Core.Maybe [Types.GameSession]
    -- ^ A collection of objects containing game session properties for each session matching the request.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SearchGameSessionsResponse' value with any optional fields omitted.
mkSearchGameSessionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchGameSessionsResponse
mkSearchGameSessionsResponse responseStatus
  = SearchGameSessionsResponse'{gameSessions = Core.Nothing,
                                nextToken = Core.Nothing, responseStatus}

-- | A collection of objects containing game session properties for each session matching the request.
--
-- /Note:/ Consider using 'gameSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsrrsGameSessions :: Lens.Lens' SearchGameSessionsResponse (Core.Maybe [Types.GameSession])
sgsrrsGameSessions = Lens.field @"gameSessions"
{-# INLINEABLE sgsrrsGameSessions #-}
{-# DEPRECATED gameSessions "Use generic-lens or generic-optics with 'gameSessions' instead"  #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsrrsNextToken :: Lens.Lens' SearchGameSessionsResponse (Core.Maybe Types.NonZeroAndMaxString)
sgsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE sgsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsrrsResponseStatus :: Lens.Lens' SearchGameSessionsResponse Core.Int
sgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

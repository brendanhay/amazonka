{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a set of one or more game sessions. Request a specific game session or request all game sessions on a fleet. Alternatively, use 'SearchGameSessions' to request a set of active game sessions that are filtered by certain criteria. To retrieve protection policy settings for game sessions, use 'DescribeGameSessionDetails' .
--
-- To get game sessions, specify one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSession' object is returned for each game session matching the request.
-- /Available in Amazon GameLift Local./ 
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
module Network.AWS.GameLift.DescribeGameSessions
    (
    -- * Creating a request
      DescribeGameSessions (..)
    , mkDescribeGameSessions
    -- ** Request lenses
    , dgsAliasId
    , dgsFleetId
    , dgsGameSessionId
    , dgsLimit
    , dgsNextToken
    , dgsStatusFilter

    -- * Destructuring the response
    , DescribeGameSessionsResponse (..)
    , mkDescribeGameSessionsResponse
    -- ** Response lenses
    , dgsrrsGameSessions
    , dgsrrsNextToken
    , dgsrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeGameSessions' smart constructor.
data DescribeGameSessions = DescribeGameSessions'
  { aliasId :: Core.Maybe Types.AliasIdOrArn
    -- ^ A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
  , fleetId :: Core.Maybe Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to retrieve all game sessions for. You can use either the fleet ID or ARN value. 
  , gameSessionId :: Core.Maybe Types.GameSessionId
    -- ^ A unique identifier for the game session to retrieve. 
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  , statusFilter :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ , and @TERMINATING@ (the last two are transitory). 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameSessions' value with any optional fields omitted.
mkDescribeGameSessions
    :: DescribeGameSessions
mkDescribeGameSessions
  = DescribeGameSessions'{aliasId = Core.Nothing,
                          fleetId = Core.Nothing, gameSessionId = Core.Nothing,
                          limit = Core.Nothing, nextToken = Core.Nothing,
                          statusFilter = Core.Nothing}

-- | A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsAliasId :: Lens.Lens' DescribeGameSessions (Core.Maybe Types.AliasIdOrArn)
dgsAliasId = Lens.field @"aliasId"
{-# INLINEABLE dgsAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | A unique identifier for a fleet to retrieve all game sessions for. You can use either the fleet ID or ARN value. 
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsFleetId :: Lens.Lens' DescribeGameSessions (Core.Maybe Types.FleetIdOrArn)
dgsFleetId = Lens.field @"fleetId"
{-# INLINEABLE dgsFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for the game session to retrieve. 
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsGameSessionId :: Lens.Lens' DescribeGameSessions (Core.Maybe Types.GameSessionId)
dgsGameSessionId = Lens.field @"gameSessionId"
{-# INLINEABLE dgsGameSessionId #-}
{-# DEPRECATED gameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsLimit :: Lens.Lens' DescribeGameSessions (Core.Maybe Core.Natural)
dgsLimit = Lens.field @"limit"
{-# INLINEABLE dgsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsNextToken :: Lens.Lens' DescribeGameSessions (Core.Maybe Types.NonZeroAndMaxString)
dgsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dgsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ , and @TERMINATING@ (the last two are transitory). 
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsStatusFilter :: Lens.Lens' DescribeGameSessions (Core.Maybe Types.NonZeroAndMaxString)
dgsStatusFilter = Lens.field @"statusFilter"
{-# INLINEABLE dgsStatusFilter #-}
{-# DEPRECATED statusFilter "Use generic-lens or generic-optics with 'statusFilter' instead"  #-}

instance Core.ToQuery DescribeGameSessions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeGameSessions where
        toHeaders DescribeGameSessions{..}
          = Core.pure ("X-Amz-Target", "GameLift.DescribeGameSessions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeGameSessions where
        toJSON DescribeGameSessions{..}
          = Core.object
              (Core.catMaybes
                 [("AliasId" Core..=) Core.<$> aliasId,
                  ("FleetId" Core..=) Core.<$> fleetId,
                  ("GameSessionId" Core..=) Core.<$> gameSessionId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("StatusFilter" Core..=) Core.<$> statusFilter])

instance Core.AWSRequest DescribeGameSessions where
        type Rs DescribeGameSessions = DescribeGameSessionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeGameSessionsResponse' Core.<$>
                   (x Core..:? "GameSessions") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeGameSessions where
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
-- /See:/ 'mkDescribeGameSessionsResponse' smart constructor.
data DescribeGameSessionsResponse = DescribeGameSessionsResponse'
  { gameSessions :: Core.Maybe [Types.GameSession]
    -- ^ A collection of objects containing game session properties for each session matching the request.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeGameSessionsResponse' value with any optional fields omitted.
mkDescribeGameSessionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGameSessionsResponse
mkDescribeGameSessionsResponse responseStatus
  = DescribeGameSessionsResponse'{gameSessions = Core.Nothing,
                                  nextToken = Core.Nothing, responseStatus}

-- | A collection of objects containing game session properties for each session matching the request.
--
-- /Note:/ Consider using 'gameSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsrrsGameSessions :: Lens.Lens' DescribeGameSessionsResponse (Core.Maybe [Types.GameSession])
dgsrrsGameSessions = Lens.field @"gameSessions"
{-# INLINEABLE dgsrrsGameSessions #-}
{-# DEPRECATED gameSessions "Use generic-lens or generic-optics with 'gameSessions' instead"  #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsrrsNextToken :: Lens.Lens' DescribeGameSessionsResponse (Core.Maybe Types.NonZeroAndMaxString)
dgsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dgsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsrrsResponseStatus :: Lens.Lens' DescribeGameSessionsResponse Core.Int
dgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

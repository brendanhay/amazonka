{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribePlayerSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for one or more player sessions. This operation can be used in several ways: (1) provide a @PlayerSessionId@ to request properties for a specific player session; (2) provide a @GameSessionId@ to request properties for all player sessions in the specified game session; (3) provide a @PlayerId@ to request properties for all player sessions of a specified player. 
--
-- To get game session record(s), specify only one of the following: a player session ID, a game session ID, or a player ID. You can filter this request by player session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'PlayerSession' object is returned for each session matching the request.
-- /Available in Amazon GameLift Local./ 
--
--     * 'CreatePlayerSession' 
--
--
--     * 'CreatePlayerSessions' 
--
--
--     * 'DescribePlayerSessions' 
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
module Network.AWS.GameLift.DescribePlayerSessions
    (
    -- * Creating a request
      DescribePlayerSessions (..)
    , mkDescribePlayerSessions
    -- ** Request lenses
    , dpssGameSessionId
    , dpssLimit
    , dpssNextToken
    , dpssPlayerId
    , dpssPlayerSessionId
    , dpssPlayerSessionStatusFilter

    -- * Destructuring the response
    , DescribePlayerSessionsResponse (..)
    , mkDescribePlayerSessionsResponse
    -- ** Response lenses
    , dpsrrsNextToken
    , dpsrrsPlayerSessions
    , dpsrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribePlayerSessions' smart constructor.
data DescribePlayerSessions = DescribePlayerSessions'
  { gameSessionId :: Core.Maybe Types.ArnStringModel
    -- ^ A unique identifier for the game session to retrieve player sessions for.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. If a player session ID is specified, this parameter is ignored.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. If a player session ID is specified, this parameter is ignored.
  , playerId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier for a player to retrieve player sessions for.
  , playerSessionId :: Core.Maybe Types.PlayerSessionId
    -- ^ A unique identifier for a player session to retrieve.
  , playerSessionStatusFilter :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Player session status to filter results on.
--
-- Possible player session statuses include the following:
--
--     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated. 
--
--
--     * __ACTIVE__ -- The player has been validated by the server process and is currently connected.
--
--
--     * __COMPLETED__ -- The player connection has been dropped.
--
--
--     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePlayerSessions' value with any optional fields omitted.
mkDescribePlayerSessions
    :: DescribePlayerSessions
mkDescribePlayerSessions
  = DescribePlayerSessions'{gameSessionId = Core.Nothing,
                            limit = Core.Nothing, nextToken = Core.Nothing,
                            playerId = Core.Nothing, playerSessionId = Core.Nothing,
                            playerSessionStatusFilter = Core.Nothing}

-- | A unique identifier for the game session to retrieve player sessions for.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssGameSessionId :: Lens.Lens' DescribePlayerSessions (Core.Maybe Types.ArnStringModel)
dpssGameSessionId = Lens.field @"gameSessionId"
{-# INLINEABLE dpssGameSessionId #-}
{-# DEPRECATED gameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. If a player session ID is specified, this parameter is ignored.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssLimit :: Lens.Lens' DescribePlayerSessions (Core.Maybe Core.Natural)
dpssLimit = Lens.field @"limit"
{-# INLINEABLE dpssLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. If a player session ID is specified, this parameter is ignored.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssNextToken :: Lens.Lens' DescribePlayerSessions (Core.Maybe Types.NonZeroAndMaxString)
dpssNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpssNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A unique identifier for a player to retrieve player sessions for.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPlayerId :: Lens.Lens' DescribePlayerSessions (Core.Maybe Types.NonZeroAndMaxString)
dpssPlayerId = Lens.field @"playerId"
{-# INLINEABLE dpssPlayerId #-}
{-# DEPRECATED playerId "Use generic-lens or generic-optics with 'playerId' instead"  #-}

-- | A unique identifier for a player session to retrieve.
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPlayerSessionId :: Lens.Lens' DescribePlayerSessions (Core.Maybe Types.PlayerSessionId)
dpssPlayerSessionId = Lens.field @"playerSessionId"
{-# INLINEABLE dpssPlayerSessionId #-}
{-# DEPRECATED playerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead"  #-}

-- | Player session status to filter results on.
--
-- Possible player session statuses include the following:
--
--     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated. 
--
--
--     * __ACTIVE__ -- The player has been validated by the server process and is currently connected.
--
--
--     * __COMPLETED__ -- The player connection has been dropped.
--
--
--     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
--
--
--
-- /Note:/ Consider using 'playerSessionStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPlayerSessionStatusFilter :: Lens.Lens' DescribePlayerSessions (Core.Maybe Types.NonZeroAndMaxString)
dpssPlayerSessionStatusFilter = Lens.field @"playerSessionStatusFilter"
{-# INLINEABLE dpssPlayerSessionStatusFilter #-}
{-# DEPRECATED playerSessionStatusFilter "Use generic-lens or generic-optics with 'playerSessionStatusFilter' instead"  #-}

instance Core.ToQuery DescribePlayerSessions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePlayerSessions where
        toHeaders DescribePlayerSessions{..}
          = Core.pure ("X-Amz-Target", "GameLift.DescribePlayerSessions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePlayerSessions where
        toJSON DescribePlayerSessions{..}
          = Core.object
              (Core.catMaybes
                 [("GameSessionId" Core..=) Core.<$> gameSessionId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("PlayerId" Core..=) Core.<$> playerId,
                  ("PlayerSessionId" Core..=) Core.<$> playerSessionId,
                  ("PlayerSessionStatusFilter" Core..=) Core.<$>
                    playerSessionStatusFilter])

instance Core.AWSRequest DescribePlayerSessions where
        type Rs DescribePlayerSessions = DescribePlayerSessionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePlayerSessionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "PlayerSessions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePlayerSessions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"playerSessions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribePlayerSessionsResponse' smart constructor.
data DescribePlayerSessionsResponse = DescribePlayerSessionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , playerSessions :: Core.Maybe [Types.PlayerSession]
    -- ^ A collection of objects containing properties for each player session that matches the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePlayerSessionsResponse' value with any optional fields omitted.
mkDescribePlayerSessionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePlayerSessionsResponse
mkDescribePlayerSessionsResponse responseStatus
  = DescribePlayerSessionsResponse'{nextToken = Core.Nothing,
                                    playerSessions = Core.Nothing, responseStatus}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrrsNextToken :: Lens.Lens' DescribePlayerSessionsResponse (Core.Maybe Types.NextToken)
dpsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A collection of objects containing properties for each player session that matches the request.
--
-- /Note:/ Consider using 'playerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrrsPlayerSessions :: Lens.Lens' DescribePlayerSessionsResponse (Core.Maybe [Types.PlayerSession])
dpsrrsPlayerSessions = Lens.field @"playerSessions"
{-# INLINEABLE dpsrrsPlayerSessions #-}
{-# DEPRECATED playerSessions "Use generic-lens or generic-optics with 'playerSessions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrrsResponseStatus :: Lens.Lens' DescribePlayerSessionsResponse Core.Int
dpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameSessionDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties, including the protection policy in force, for one or more game sessions. This operation can be used in several ways: (1) provide a @GameSessionId@ or @GameSessionArn@ to request details for a specific game session; (2) provide either a @FleetId@ or an @AliasId@ to request properties for all game sessions running on a fleet. 
--
-- To get game session record(s), specify just one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSessionDetail' object is returned for each session matching the request.
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
module Network.AWS.GameLift.DescribeGameSessionDetails
    (
    -- * Creating a request
      DescribeGameSessionDetails (..)
    , mkDescribeGameSessionDetails
    -- ** Request lenses
    , dgsdAliasId
    , dgsdFleetId
    , dgsdGameSessionId
    , dgsdLimit
    , dgsdNextToken
    , dgsdStatusFilter

    -- * Destructuring the response
    , DescribeGameSessionDetailsResponse (..)
    , mkDescribeGameSessionDetailsResponse
    -- ** Response lenses
    , dgsdrrsGameSessionDetails
    , dgsdrrsNextToken
    , dgsdrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeGameSessionDetails' smart constructor.
data DescribeGameSessionDetails = DescribeGameSessionDetails'
  { aliasId :: Core.Maybe Types.AliasIdOrArn
    -- ^ A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
  , fleetId :: Core.Maybe Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to retrieve all game sessions active on the fleet. You can use either the fleet ID or ARN value.
  , gameSessionId :: Core.Maybe Types.ArnStringModel
    -- ^ A unique identifier for the game session to retrieve. 
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  , statusFilter :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ and @TERMINATING@ (the last two are transitory). 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameSessionDetails' value with any optional fields omitted.
mkDescribeGameSessionDetails
    :: DescribeGameSessionDetails
mkDescribeGameSessionDetails
  = DescribeGameSessionDetails'{aliasId = Core.Nothing,
                                fleetId = Core.Nothing, gameSessionId = Core.Nothing,
                                limit = Core.Nothing, nextToken = Core.Nothing,
                                statusFilter = Core.Nothing}

-- | A unique identifier for an alias associated with the fleet to retrieve all game sessions for. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdAliasId :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Types.AliasIdOrArn)
dgsdAliasId = Lens.field @"aliasId"
{-# INLINEABLE dgsdAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | A unique identifier for a fleet to retrieve all game sessions active on the fleet. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdFleetId :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Types.FleetIdOrArn)
dgsdFleetId = Lens.field @"fleetId"
{-# INLINEABLE dgsdFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for the game session to retrieve. 
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdGameSessionId :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Types.ArnStringModel)
dgsdGameSessionId = Lens.field @"gameSessionId"
{-# INLINEABLE dgsdGameSessionId #-}
{-# DEPRECATED gameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdLimit :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Core.Natural)
dgsdLimit = Lens.field @"limit"
{-# INLINEABLE dgsdLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdNextToken :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Types.NonZeroAndMaxString)
dgsdNextToken = Lens.field @"nextToken"
{-# INLINEABLE dgsdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ and @TERMINATING@ (the last two are transitory). 
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdStatusFilter :: Lens.Lens' DescribeGameSessionDetails (Core.Maybe Types.NonZeroAndMaxString)
dgsdStatusFilter = Lens.field @"statusFilter"
{-# INLINEABLE dgsdStatusFilter #-}
{-# DEPRECATED statusFilter "Use generic-lens or generic-optics with 'statusFilter' instead"  #-}

instance Core.ToQuery DescribeGameSessionDetails where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeGameSessionDetails where
        toHeaders DescribeGameSessionDetails{..}
          = Core.pure ("X-Amz-Target", "GameLift.DescribeGameSessionDetails")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeGameSessionDetails where
        toJSON DescribeGameSessionDetails{..}
          = Core.object
              (Core.catMaybes
                 [("AliasId" Core..=) Core.<$> aliasId,
                  ("FleetId" Core..=) Core.<$> fleetId,
                  ("GameSessionId" Core..=) Core.<$> gameSessionId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("StatusFilter" Core..=) Core.<$> statusFilter])

instance Core.AWSRequest DescribeGameSessionDetails where
        type Rs DescribeGameSessionDetails =
             DescribeGameSessionDetailsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeGameSessionDetailsResponse' Core.<$>
                   (x Core..:? "GameSessionDetails") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeGameSessionDetails where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"gameSessionDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeGameSessionDetailsResponse' smart constructor.
data DescribeGameSessionDetailsResponse = DescribeGameSessionDetailsResponse'
  { gameSessionDetails :: Core.Maybe [Types.GameSessionDetail]
    -- ^ A collection of objects containing game session properties and the protection policy currently in force for each session matching the request.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeGameSessionDetailsResponse' value with any optional fields omitted.
mkDescribeGameSessionDetailsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGameSessionDetailsResponse
mkDescribeGameSessionDetailsResponse responseStatus
  = DescribeGameSessionDetailsResponse'{gameSessionDetails =
                                          Core.Nothing,
                                        nextToken = Core.Nothing, responseStatus}

-- | A collection of objects containing game session properties and the protection policy currently in force for each session matching the request.
--
-- /Note:/ Consider using 'gameSessionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdrrsGameSessionDetails :: Lens.Lens' DescribeGameSessionDetailsResponse (Core.Maybe [Types.GameSessionDetail])
dgsdrrsGameSessionDetails = Lens.field @"gameSessionDetails"
{-# INLINEABLE dgsdrrsGameSessionDetails #-}
{-# DEPRECATED gameSessionDetails "Use generic-lens or generic-optics with 'gameSessionDetails' instead"  #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdrrsNextToken :: Lens.Lens' DescribeGameSessionDetailsResponse (Core.Maybe Types.NonZeroAndMaxString)
dgsdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dgsdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsdrrsResponseStatus :: Lens.Lens' DescribeGameSessionDetailsResponse Core.Int
dgsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

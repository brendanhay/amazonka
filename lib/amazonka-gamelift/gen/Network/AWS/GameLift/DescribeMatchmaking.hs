{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeMatchmaking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more matchmaking tickets. Use this operation to retrieve ticket information, including--after a successful match is made--connection information for the resulting new game session. 
--
-- To request matchmaking tickets, provide a list of up to 10 ticket IDs. If the request is successful, a ticket object is returned for each requested ID that currently exists.
-- This operation is not designed to be continually called to track matchmaking ticket status. This practice can cause you to exceed your API limit, which results in errors. Instead, as a best practice, set up an Amazon Simple Notification Service (SNS) to receive notifications, and provide the topic ARN in the matchmaking configuration. Continuously poling ticket status with 'DescribeMatchmaking' should only be used for games in development with low matchmaking usage.
--
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client> 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification> 
-- __Related operations__ 
--
--     * 'StartMatchmaking' 
--
--
--     * 'DescribeMatchmaking' 
--
--
--     * 'StopMatchmaking' 
--
--
--     * 'AcceptMatch' 
--
--
--     * 'StartMatchBackfill' 
--
--
module Network.AWS.GameLift.DescribeMatchmaking
    (
    -- * Creating a request
      DescribeMatchmaking (..)
    , mkDescribeMatchmaking
    -- ** Request lenses
    , dmTicketIds

    -- * Destructuring the response
    , DescribeMatchmakingResponse (..)
    , mkDescribeMatchmakingResponse
    -- ** Response lenses
    , dmrrsTicketList
    , dmrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeMatchmaking' smart constructor.
newtype DescribeMatchmaking = DescribeMatchmaking'
  { ticketIds :: [Types.MatchmakingIdStringModel]
    -- ^ A unique identifier for a matchmaking ticket. You can include up to 10 ID values. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMatchmaking' value with any optional fields omitted.
mkDescribeMatchmaking
    :: DescribeMatchmaking
mkDescribeMatchmaking
  = DescribeMatchmaking'{ticketIds = Core.mempty}

-- | A unique identifier for a matchmaking ticket. You can include up to 10 ID values. 
--
-- /Note:/ Consider using 'ticketIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmTicketIds :: Lens.Lens' DescribeMatchmaking [Types.MatchmakingIdStringModel]
dmTicketIds = Lens.field @"ticketIds"
{-# INLINEABLE dmTicketIds #-}
{-# DEPRECATED ticketIds "Use generic-lens or generic-optics with 'ticketIds' instead"  #-}

instance Core.ToQuery DescribeMatchmaking where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMatchmaking where
        toHeaders DescribeMatchmaking{..}
          = Core.pure ("X-Amz-Target", "GameLift.DescribeMatchmaking")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeMatchmaking where
        toJSON DescribeMatchmaking{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TicketIds" Core..= ticketIds)])

instance Core.AWSRequest DescribeMatchmaking where
        type Rs DescribeMatchmaking = DescribeMatchmakingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMatchmakingResponse' Core.<$>
                   (x Core..:? "TicketList") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeMatchmakingResponse' smart constructor.
data DescribeMatchmakingResponse = DescribeMatchmakingResponse'
  { ticketList :: Core.Maybe [Types.MatchmakingTicket]
    -- ^ A collection of existing matchmaking ticket objects matching the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeMatchmakingResponse' value with any optional fields omitted.
mkDescribeMatchmakingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMatchmakingResponse
mkDescribeMatchmakingResponse responseStatus
  = DescribeMatchmakingResponse'{ticketList = Core.Nothing,
                                 responseStatus}

-- | A collection of existing matchmaking ticket objects matching the request.
--
-- /Note:/ Consider using 'ticketList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsTicketList :: Lens.Lens' DescribeMatchmakingResponse (Core.Maybe [Types.MatchmakingTicket])
dmrrsTicketList = Lens.field @"ticketList"
{-# INLINEABLE dmrrsTicketList #-}
{-# DEPRECATED ticketList "Use generic-lens or generic-optics with 'ticketList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DescribeMatchmakingResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StopMatchmaking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a matchmaking ticket or match backfill ticket that is currently being processed. To stop the matchmaking operation, specify the ticket ID. If successful, work on the ticket is stopped, and the ticket status is changed to @CANCELLED@ .
--
-- This call is also used to turn off automatic backfill for an individual game session. This is for game sessions that are created with a matchmaking configuration that has automatic backfill enabled. The ticket ID is included in the @MatchmakerData@ of an updated game session object, which is provided to the game server.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client> 
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
module Network.AWS.GameLift.StopMatchmaking
    (
    -- * Creating a request
      StopMatchmaking (..)
    , mkStopMatchmaking
    -- ** Request lenses
    , smTicketId

    -- * Destructuring the response
    , StopMatchmakingResponse (..)
    , mkStopMatchmakingResponse
    -- ** Response lenses
    , smrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStopMatchmaking' smart constructor.
newtype StopMatchmaking = StopMatchmaking'
  { ticketId :: Types.TicketId
    -- ^ A unique identifier for a matchmaking ticket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopMatchmaking' value with any optional fields omitted.
mkStopMatchmaking
    :: Types.TicketId -- ^ 'ticketId'
    -> StopMatchmaking
mkStopMatchmaking ticketId = StopMatchmaking'{ticketId}

-- | A unique identifier for a matchmaking ticket.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smTicketId :: Lens.Lens' StopMatchmaking Types.TicketId
smTicketId = Lens.field @"ticketId"
{-# INLINEABLE smTicketId #-}
{-# DEPRECATED ticketId "Use generic-lens or generic-optics with 'ticketId' instead"  #-}

instance Core.ToQuery StopMatchmaking where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopMatchmaking where
        toHeaders StopMatchmaking{..}
          = Core.pure ("X-Amz-Target", "GameLift.StopMatchmaking") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopMatchmaking where
        toJSON StopMatchmaking{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TicketId" Core..= ticketId)])

instance Core.AWSRequest StopMatchmaking where
        type Rs StopMatchmaking = StopMatchmakingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopMatchmakingResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopMatchmakingResponse' smart constructor.
newtype StopMatchmakingResponse = StopMatchmakingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopMatchmakingResponse' value with any optional fields omitted.
mkStopMatchmakingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopMatchmakingResponse
mkStopMatchmakingResponse responseStatus
  = StopMatchmakingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsResponseStatus :: Lens.Lens' StopMatchmakingResponse Core.Int
smrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE smrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.AcceptMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a player's acceptance or rejection of a proposed FlexMatch match. A matchmaking configuration may require player acceptance; if so, then matches built with that configuration cannot be completed unless all players accept the proposed match within a specified time limit.
--
-- When FlexMatch builds a match, all the matchmaking tickets involved in the proposed match are placed into status @REQUIRES_ACCEPTANCE@ . This is a trigger for your game to get acceptance from all players in the ticket. Acceptances are only valid for tickets when they are in this status; all other acceptances result in an error.
-- To register acceptance, specify the ticket ID, a response, and one or more players. Once all players have registered acceptance, the matchmaking tickets advance to status @PLACING@ , where a new game session is created for the match.
-- If any player rejects the match, or if acceptances are not received before a specified timeout, the proposed match is dropped. The matchmaking tickets are then handled in one of two ways: For tickets where one or more players rejected the match, the ticket status is returned to @SEARCHING@ to find a new match. For tickets where one or more players failed to respond, the ticket status is set to @CANCELLED@ , and processing is terminated. A new matchmaking request for these players can be submitted as needed.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client>
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html FlexMatch Events Reference>
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
module Network.AWS.GameLift.AcceptMatch
  ( -- * Creating a request
    AcceptMatch (..),
    mkAcceptMatch,

    -- ** Request lenses
    amTicketId,
    amPlayerIds,
    amAcceptanceType,

    -- * Destructuring the response
    AcceptMatchResponse (..),
    mkAcceptMatchResponse,

    -- ** Response lenses
    amrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkAcceptMatch' smart constructor.
data AcceptMatch = AcceptMatch'
  { -- | A unique identifier for a matchmaking ticket. The ticket must be in status @REQUIRES_ACCEPTANCE@ ; otherwise this request will fail.
    ticketId :: Types.TicketId,
    -- | A unique identifier for a player delivering the response. This parameter can include one or multiple player IDs.
    playerIds :: [Types.NonZeroAndMaxString],
    -- | Player response to the proposed match.
    acceptanceType :: Types.AcceptanceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptMatch' value with any optional fields omitted.
mkAcceptMatch ::
  -- | 'ticketId'
  Types.TicketId ->
  -- | 'acceptanceType'
  Types.AcceptanceType ->
  AcceptMatch
mkAcceptMatch ticketId acceptanceType =
  AcceptMatch' {ticketId, playerIds = Core.mempty, acceptanceType}

-- | A unique identifier for a matchmaking ticket. The ticket must be in status @REQUIRES_ACCEPTANCE@ ; otherwise this request will fail.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amTicketId :: Lens.Lens' AcceptMatch Types.TicketId
amTicketId = Lens.field @"ticketId"
{-# DEPRECATED amTicketId "Use generic-lens or generic-optics with 'ticketId' instead." #-}

-- | A unique identifier for a player delivering the response. This parameter can include one or multiple player IDs.
--
-- /Note:/ Consider using 'playerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amPlayerIds :: Lens.Lens' AcceptMatch [Types.NonZeroAndMaxString]
amPlayerIds = Lens.field @"playerIds"
{-# DEPRECATED amPlayerIds "Use generic-lens or generic-optics with 'playerIds' instead." #-}

-- | Player response to the proposed match.
--
-- /Note:/ Consider using 'acceptanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amAcceptanceType :: Lens.Lens' AcceptMatch Types.AcceptanceType
amAcceptanceType = Lens.field @"acceptanceType"
{-# DEPRECATED amAcceptanceType "Use generic-lens or generic-optics with 'acceptanceType' instead." #-}

instance Core.FromJSON AcceptMatch where
  toJSON AcceptMatch {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TicketId" Core..= ticketId),
            Core.Just ("PlayerIds" Core..= playerIds),
            Core.Just ("AcceptanceType" Core..= acceptanceType)
          ]
      )

instance Core.AWSRequest AcceptMatch where
  type Rs AcceptMatch = AcceptMatchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.AcceptMatch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptMatchResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAcceptMatchResponse' smart constructor.
newtype AcceptMatchResponse = AcceptMatchResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptMatchResponse' value with any optional fields omitted.
mkAcceptMatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptMatchResponse
mkAcceptMatchResponse responseStatus =
  AcceptMatchResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amrrsResponseStatus :: Lens.Lens' AcceptMatchResponse Core.Int
amrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED amrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

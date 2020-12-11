{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    amrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkAcceptMatch' smart constructor.
data AcceptMatch = AcceptMatch'
  { ticketId :: Lude.Text,
    playerIds :: [Lude.Text],
    acceptanceType :: AcceptanceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptMatch' with the minimum fields required to make a request.
--
-- * 'acceptanceType' - Player response to the proposed match.
-- * 'playerIds' - A unique identifier for a player delivering the response. This parameter can include one or multiple player IDs.
-- * 'ticketId' - A unique identifier for a matchmaking ticket. The ticket must be in status @REQUIRES_ACCEPTANCE@ ; otherwise this request will fail.
mkAcceptMatch ::
  -- | 'ticketId'
  Lude.Text ->
  -- | 'acceptanceType'
  AcceptanceType ->
  AcceptMatch
mkAcceptMatch pTicketId_ pAcceptanceType_ =
  AcceptMatch'
    { ticketId = pTicketId_,
      playerIds = Lude.mempty,
      acceptanceType = pAcceptanceType_
    }

-- | A unique identifier for a matchmaking ticket. The ticket must be in status @REQUIRES_ACCEPTANCE@ ; otherwise this request will fail.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amTicketId :: Lens.Lens' AcceptMatch Lude.Text
amTicketId = Lens.lens (ticketId :: AcceptMatch -> Lude.Text) (\s a -> s {ticketId = a} :: AcceptMatch)
{-# DEPRECATED amTicketId "Use generic-lens or generic-optics with 'ticketId' instead." #-}

-- | A unique identifier for a player delivering the response. This parameter can include one or multiple player IDs.
--
-- /Note:/ Consider using 'playerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amPlayerIds :: Lens.Lens' AcceptMatch [Lude.Text]
amPlayerIds = Lens.lens (playerIds :: AcceptMatch -> [Lude.Text]) (\s a -> s {playerIds = a} :: AcceptMatch)
{-# DEPRECATED amPlayerIds "Use generic-lens or generic-optics with 'playerIds' instead." #-}

-- | Player response to the proposed match.
--
-- /Note:/ Consider using 'acceptanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amAcceptanceType :: Lens.Lens' AcceptMatch AcceptanceType
amAcceptanceType = Lens.lens (acceptanceType :: AcceptMatch -> AcceptanceType) (\s a -> s {acceptanceType = a} :: AcceptMatch)
{-# DEPRECATED amAcceptanceType "Use generic-lens or generic-optics with 'acceptanceType' instead." #-}

instance Lude.AWSRequest AcceptMatch where
  type Rs AcceptMatch = AcceptMatchResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AcceptMatchResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptMatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.AcceptMatch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptMatch where
  toJSON AcceptMatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TicketId" Lude..= ticketId),
            Lude.Just ("PlayerIds" Lude..= playerIds),
            Lude.Just ("AcceptanceType" Lude..= acceptanceType)
          ]
      )

instance Lude.ToPath AcceptMatch where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptMatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAcceptMatchResponse' smart constructor.
newtype AcceptMatchResponse = AcceptMatchResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptMatchResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAcceptMatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptMatchResponse
mkAcceptMatchResponse pResponseStatus_ =
  AcceptMatchResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amrsResponseStatus :: Lens.Lens' AcceptMatchResponse Lude.Int
amrsResponseStatus = Lens.lens (responseStatus :: AcceptMatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptMatchResponse)
{-# DEPRECATED amrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

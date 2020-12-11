{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.GameLift.StopMatchmaking
  ( -- * Creating a request
    StopMatchmaking (..),
    mkStopMatchmaking,

    -- ** Request lenses
    smTicketId,

    -- * Destructuring the response
    StopMatchmakingResponse (..),
    mkStopMatchmakingResponse,

    -- ** Response lenses
    smrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStopMatchmaking' smart constructor.
newtype StopMatchmaking = StopMatchmaking' {ticketId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopMatchmaking' with the minimum fields required to make a request.
--
-- * 'ticketId' - A unique identifier for a matchmaking ticket.
mkStopMatchmaking ::
  -- | 'ticketId'
  Lude.Text ->
  StopMatchmaking
mkStopMatchmaking pTicketId_ =
  StopMatchmaking' {ticketId = pTicketId_}

-- | A unique identifier for a matchmaking ticket.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smTicketId :: Lens.Lens' StopMatchmaking Lude.Text
smTicketId = Lens.lens (ticketId :: StopMatchmaking -> Lude.Text) (\s a -> s {ticketId = a} :: StopMatchmaking)
{-# DEPRECATED smTicketId "Use generic-lens or generic-optics with 'ticketId' instead." #-}

instance Lude.AWSRequest StopMatchmaking where
  type Rs StopMatchmaking = StopMatchmakingResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopMatchmakingResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopMatchmaking where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.StopMatchmaking" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopMatchmaking where
  toJSON StopMatchmaking' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TicketId" Lude..= ticketId)])

instance Lude.ToPath StopMatchmaking where
  toPath = Lude.const "/"

instance Lude.ToQuery StopMatchmaking where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopMatchmakingResponse' smart constructor.
newtype StopMatchmakingResponse = StopMatchmakingResponse'
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

-- | Creates a value of 'StopMatchmakingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopMatchmakingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopMatchmakingResponse
mkStopMatchmakingResponse pResponseStatus_ =
  StopMatchmakingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsResponseStatus :: Lens.Lens' StopMatchmakingResponse Lude.Int
smrsResponseStatus = Lens.lens (responseStatus :: StopMatchmakingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopMatchmakingResponse)
{-# DEPRECATED smrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

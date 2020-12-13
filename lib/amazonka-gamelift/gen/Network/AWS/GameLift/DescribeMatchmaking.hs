{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.GameLift.DescribeMatchmaking
  ( -- * Creating a request
    DescribeMatchmaking (..),
    mkDescribeMatchmaking,

    -- ** Request lenses
    dmTicketIds,

    -- * Destructuring the response
    DescribeMatchmakingResponse (..),
    mkDescribeMatchmakingResponse,

    -- ** Response lenses
    dmrsTicketList,
    dmrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeMatchmaking' smart constructor.
newtype DescribeMatchmaking = DescribeMatchmaking'
  { -- | A unique identifier for a matchmaking ticket. You can include up to 10 ID values.
    ticketIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMatchmaking' with the minimum fields required to make a request.
--
-- * 'ticketIds' - A unique identifier for a matchmaking ticket. You can include up to 10 ID values.
mkDescribeMatchmaking ::
  DescribeMatchmaking
mkDescribeMatchmaking =
  DescribeMatchmaking' {ticketIds = Lude.mempty}

-- | A unique identifier for a matchmaking ticket. You can include up to 10 ID values.
--
-- /Note:/ Consider using 'ticketIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmTicketIds :: Lens.Lens' DescribeMatchmaking [Lude.Text]
dmTicketIds = Lens.lens (ticketIds :: DescribeMatchmaking -> [Lude.Text]) (\s a -> s {ticketIds = a} :: DescribeMatchmaking)
{-# DEPRECATED dmTicketIds "Use generic-lens or generic-optics with 'ticketIds' instead." #-}

instance Lude.AWSRequest DescribeMatchmaking where
  type Rs DescribeMatchmaking = DescribeMatchmakingResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMatchmakingResponse'
            Lude.<$> (x Lude..?> "TicketList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMatchmaking where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeMatchmaking" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMatchmaking where
  toJSON DescribeMatchmaking' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TicketIds" Lude..= ticketIds)])

instance Lude.ToPath DescribeMatchmaking where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMatchmaking where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeMatchmakingResponse' smart constructor.
data DescribeMatchmakingResponse = DescribeMatchmakingResponse'
  { -- | A collection of existing matchmaking ticket objects matching the request.
    ticketList :: Lude.Maybe [MatchmakingTicket],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMatchmakingResponse' with the minimum fields required to make a request.
--
-- * 'ticketList' - A collection of existing matchmaking ticket objects matching the request.
-- * 'responseStatus' - The response status code.
mkDescribeMatchmakingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMatchmakingResponse
mkDescribeMatchmakingResponse pResponseStatus_ =
  DescribeMatchmakingResponse'
    { ticketList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of existing matchmaking ticket objects matching the request.
--
-- /Note:/ Consider using 'ticketList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsTicketList :: Lens.Lens' DescribeMatchmakingResponse (Lude.Maybe [MatchmakingTicket])
dmrsTicketList = Lens.lens (ticketList :: DescribeMatchmakingResponse -> Lude.Maybe [MatchmakingTicket]) (\s a -> s {ticketList = a} :: DescribeMatchmakingResponse)
{-# DEPRECATED dmrsTicketList "Use generic-lens or generic-optics with 'ticketList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsResponseStatus :: Lens.Lens' DescribeMatchmakingResponse Lude.Int
dmrsResponseStatus = Lens.lens (responseStatus :: DescribeMatchmakingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMatchmakingResponse)
{-# DEPRECATED dmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

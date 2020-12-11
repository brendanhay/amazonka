{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartMatchmaking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses FlexMatch to create a game match for a group of players based on custom matchmaking rules. If you're also using GameLift hosting, a new game session is started for the matched players. Each matchmaking request identifies one or more players to find a match for, and specifies the type of match to build, including the team configuration and the rules for an acceptable match. When a matchmaking request identifies a group of players who want to play together, FlexMatch finds additional players to fill the match. Match type, rules, and other features are defined in a @MatchmakingConfiguration@ .
--
-- To start matchmaking, provide a unique ticket ID, specify a matchmaking configuration, and include the players to be matched. For each player, you must also include the player attribute values that are required by the matchmaking configuration (in the rule set). If successful, a matchmaking ticket is returned with status set to @QUEUED@ .
-- Track the status of the ticket to respond as needed. If you're also using GameLift hosting, a successfully completed ticket contains game session connection information. Ticket status updates are tracked using event notification through Amazon Simple Notification Service (SNS), which is defined in the matchmaking configuration.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client>
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification>
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-tasks.html FlexMatch Integration Roadmap>
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How GameLift FlexMatch Works>
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
module Network.AWS.GameLift.StartMatchmaking
  ( -- * Creating a request
    StartMatchmaking (..),
    mkStartMatchmaking,

    -- ** Request lenses
    sTicketId,
    sConfigurationName,
    sPlayers,

    -- * Destructuring the response
    StartMatchmakingResponse (..),
    mkStartMatchmakingResponse,

    -- ** Response lenses
    srsMatchmakingTicket,
    srsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStartMatchmaking' smart constructor.
data StartMatchmaking = StartMatchmaking'
  { ticketId ::
      Lude.Maybe Lude.Text,
    configurationName :: Lude.Text,
    players :: [Player]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMatchmaking' with the minimum fields required to make a request.
--
-- * 'configurationName' - Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same Region as this request. You can use either the configuration name or ARN value.
-- * 'players' - Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
-- * 'ticketId' - A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
mkStartMatchmaking ::
  -- | 'configurationName'
  Lude.Text ->
  StartMatchmaking
mkStartMatchmaking pConfigurationName_ =
  StartMatchmaking'
    { ticketId = Lude.Nothing,
      configurationName = pConfigurationName_,
      players = Lude.mempty
    }

-- | A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTicketId :: Lens.Lens' StartMatchmaking (Lude.Maybe Lude.Text)
sTicketId = Lens.lens (ticketId :: StartMatchmaking -> Lude.Maybe Lude.Text) (\s a -> s {ticketId = a} :: StartMatchmaking)
{-# DEPRECATED sTicketId "Use generic-lens or generic-optics with 'ticketId' instead." #-}

-- | Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same Region as this request. You can use either the configuration name or ARN value.
--
-- /Note:/ Consider using 'configurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfigurationName :: Lens.Lens' StartMatchmaking Lude.Text
sConfigurationName = Lens.lens (configurationName :: StartMatchmaking -> Lude.Text) (\s a -> s {configurationName = a} :: StartMatchmaking)
{-# DEPRECATED sConfigurationName "Use generic-lens or generic-optics with 'configurationName' instead." #-}

-- | Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
--
-- /Note:/ Consider using 'players' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPlayers :: Lens.Lens' StartMatchmaking [Player]
sPlayers = Lens.lens (players :: StartMatchmaking -> [Player]) (\s a -> s {players = a} :: StartMatchmaking)
{-# DEPRECATED sPlayers "Use generic-lens or generic-optics with 'players' instead." #-}

instance Lude.AWSRequest StartMatchmaking where
  type Rs StartMatchmaking = StartMatchmakingResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMatchmakingResponse'
            Lude.<$> (x Lude..?> "MatchmakingTicket")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMatchmaking where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.StartMatchmaking" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMatchmaking where
  toJSON StartMatchmaking' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TicketId" Lude..=) Lude.<$> ticketId,
            Lude.Just ("ConfigurationName" Lude..= configurationName),
            Lude.Just ("Players" Lude..= players)
          ]
      )

instance Lude.ToPath StartMatchmaking where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMatchmaking where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStartMatchmakingResponse' smart constructor.
data StartMatchmakingResponse = StartMatchmakingResponse'
  { matchmakingTicket ::
      Lude.Maybe MatchmakingTicket,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMatchmakingResponse' with the minimum fields required to make a request.
--
-- * 'matchmakingTicket' - Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
-- * 'responseStatus' - The response status code.
mkStartMatchmakingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMatchmakingResponse
mkStartMatchmakingResponse pResponseStatus_ =
  StartMatchmakingResponse'
    { matchmakingTicket = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
--
-- /Note:/ Consider using 'matchmakingTicket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsMatchmakingTicket :: Lens.Lens' StartMatchmakingResponse (Lude.Maybe MatchmakingTicket)
srsMatchmakingTicket = Lens.lens (matchmakingTicket :: StartMatchmakingResponse -> Lude.Maybe MatchmakingTicket) (\s a -> s {matchmakingTicket = a} :: StartMatchmakingResponse)
{-# DEPRECATED srsMatchmakingTicket "Use generic-lens or generic-optics with 'matchmakingTicket' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartMatchmakingResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartMatchmakingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMatchmakingResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

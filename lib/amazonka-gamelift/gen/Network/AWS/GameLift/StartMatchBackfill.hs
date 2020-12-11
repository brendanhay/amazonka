{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartMatchBackfill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds new players to fill open slots in an existing game session. This operation can be used to add players to matched games that start with fewer than the maximum number of players or to replace players when they drop out. By backfilling with the same matchmaker used to create the original match, you ensure that new players meet the match criteria and maintain a consistent experience throughout the game session. You can backfill a match anytime after a game session has been created.
--
-- To request a match backfill, specify a unique ticket ID, the existing game session's ARN, a matchmaking configuration, and a set of data that describes all current players in the game session. If successful, a match backfill ticket is created and returned with status set to QUEUED. The ticket is placed in the matchmaker's ticket pool and processed. Track the status of the ticket to respond as needed.
-- The process of finding backfill matches is essentially identical to the initial matchmaking process. The matchmaker searches the pool and groups tickets together to form potential matches, allowing only one backfill ticket per potential match. Once the a match is formed, the matchmaker creates player sessions for the new players. All tickets in the match are updated with the game session's connection information, and the 'GameSession' object is updated to include matchmaker data on the new players. For more detail on how match backfill requests are processed, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How Amazon GameLift FlexMatch Works> .
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch>
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
module Network.AWS.GameLift.StartMatchBackfill
  ( -- * Creating a request
    StartMatchBackfill (..),
    mkStartMatchBackfill,

    -- ** Request lenses
    smbTicketId,
    smbGameSessionARN,
    smbConfigurationName,
    smbPlayers,

    -- * Destructuring the response
    StartMatchBackfillResponse (..),
    mkStartMatchBackfillResponse,

    -- ** Response lenses
    smbrsMatchmakingTicket,
    smbrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStartMatchBackfill' smart constructor.
data StartMatchBackfill = StartMatchBackfill'
  { ticketId ::
      Lude.Maybe Lude.Text,
    gameSessionARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartMatchBackfill' with the minimum fields required to make a request.
--
-- * 'configurationName' - Name of the matchmaker to use for this request. You can use either the configuration name or ARN value. The ARN of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property.
-- * 'gameSessionARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it. This is the same as the game session ID.
-- * 'players' - Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.
--
--
--     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
--
--
--     * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the Region that the game session is currently in. Do not include latency values for any other Region.
--
--
-- * 'ticketId' - A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
mkStartMatchBackfill ::
  -- | 'configurationName'
  Lude.Text ->
  StartMatchBackfill
mkStartMatchBackfill pConfigurationName_ =
  StartMatchBackfill'
    { ticketId = Lude.Nothing,
      gameSessionARN = Lude.Nothing,
      configurationName = pConfigurationName_,
      players = Lude.mempty
    }

-- | A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbTicketId :: Lens.Lens' StartMatchBackfill (Lude.Maybe Lude.Text)
smbTicketId = Lens.lens (ticketId :: StartMatchBackfill -> Lude.Maybe Lude.Text) (\s a -> s {ticketId = a} :: StartMatchBackfill)
{-# DEPRECATED smbTicketId "Use generic-lens or generic-optics with 'ticketId' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it. This is the same as the game session ID.
--
-- /Note:/ Consider using 'gameSessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbGameSessionARN :: Lens.Lens' StartMatchBackfill (Lude.Maybe Lude.Text)
smbGameSessionARN = Lens.lens (gameSessionARN :: StartMatchBackfill -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionARN = a} :: StartMatchBackfill)
{-# DEPRECATED smbGameSessionARN "Use generic-lens or generic-optics with 'gameSessionARN' instead." #-}

-- | Name of the matchmaker to use for this request. You can use either the configuration name or ARN value. The ARN of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property.
--
-- /Note:/ Consider using 'configurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbConfigurationName :: Lens.Lens' StartMatchBackfill Lude.Text
smbConfigurationName = Lens.lens (configurationName :: StartMatchBackfill -> Lude.Text) (\s a -> s {configurationName = a} :: StartMatchBackfill)
{-# DEPRECATED smbConfigurationName "Use generic-lens or generic-optics with 'configurationName' instead." #-}

-- | Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.
--
--
--     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
--
--
--     * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the Region that the game session is currently in. Do not include latency values for any other Region.
--
--
--
-- /Note:/ Consider using 'players' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbPlayers :: Lens.Lens' StartMatchBackfill [Player]
smbPlayers = Lens.lens (players :: StartMatchBackfill -> [Player]) (\s a -> s {players = a} :: StartMatchBackfill)
{-# DEPRECATED smbPlayers "Use generic-lens or generic-optics with 'players' instead." #-}

instance Lude.AWSRequest StartMatchBackfill where
  type Rs StartMatchBackfill = StartMatchBackfillResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMatchBackfillResponse'
            Lude.<$> (x Lude..?> "MatchmakingTicket")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMatchBackfill where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.StartMatchBackfill" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMatchBackfill where
  toJSON StartMatchBackfill' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TicketId" Lude..=) Lude.<$> ticketId,
            ("GameSessionArn" Lude..=) Lude.<$> gameSessionARN,
            Lude.Just ("ConfigurationName" Lude..= configurationName),
            Lude.Just ("Players" Lude..= players)
          ]
      )

instance Lude.ToPath StartMatchBackfill where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMatchBackfill where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStartMatchBackfillResponse' smart constructor.
data StartMatchBackfillResponse = StartMatchBackfillResponse'
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

-- | Creates a value of 'StartMatchBackfillResponse' with the minimum fields required to make a request.
--
-- * 'matchmakingTicket' - Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
-- * 'responseStatus' - The response status code.
mkStartMatchBackfillResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMatchBackfillResponse
mkStartMatchBackfillResponse pResponseStatus_ =
  StartMatchBackfillResponse'
    { matchmakingTicket = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
--
-- /Note:/ Consider using 'matchmakingTicket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrsMatchmakingTicket :: Lens.Lens' StartMatchBackfillResponse (Lude.Maybe MatchmakingTicket)
smbrsMatchmakingTicket = Lens.lens (matchmakingTicket :: StartMatchBackfillResponse -> Lude.Maybe MatchmakingTicket) (\s a -> s {matchmakingTicket = a} :: StartMatchBackfillResponse)
{-# DEPRECATED smbrsMatchmakingTicket "Use generic-lens or generic-optics with 'matchmakingTicket' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrsResponseStatus :: Lens.Lens' StartMatchBackfillResponse Lude.Int
smbrsResponseStatus = Lens.lens (responseStatus :: StartMatchBackfillResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMatchBackfillResponse)
{-# DEPRECATED smbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

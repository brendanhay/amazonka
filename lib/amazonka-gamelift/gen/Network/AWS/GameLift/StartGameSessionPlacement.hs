{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartGameSessionPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places a request for a new game session in a queue (see 'CreateGameSessionQueue' ). When processing a placement request, Amazon GameLift searches for available resources on the queue's destinations, scanning each until it finds resources or the placement request times out.
--
-- A game session placement request can also request player sessions. When a new game session is successfully created, Amazon GameLift creates a player session for each player included in the request.
-- When placing a game session, by default Amazon GameLift tries each fleet in the order they are listed in the queue configuration. Ideally, a queue's destinations are listed in preference order.
-- Alternatively, when requesting a game session with players, you can also provide latency data for each player in relevant Regions. Latency data indicates the performance lag a player experiences when connected to a fleet in the Region. Amazon GameLift uses latency data to reorder the list of destinations to place the game session in a Region with minimal lag. If latency data is provided for multiple players, Amazon GameLift calculates each Region's average lag for all players and reorders to get the best game play across all players.
-- To place a new game session request, specify the following:
--
--     * The queue name and a set of game session properties and settings
--
--
--     * A unique ID (such as a UUID) for the placement. You use this ID to track the status of the placement request
--
--
--     * (Optional) A set of player data and a unique player ID for each player that you are joining to the new game session (player data is optional, but if you include it, you must also provide a unique ID for each player)
--
--
--     * Latency data for all players (if you want to optimize game play for the players)
--
--
-- If successful, a new game session placement is created.
-- To track the status of a placement request, call 'DescribeGameSessionPlacement' and check the request's status. If the status is @FULFILLED@ , a new game session has been created and a game session ARN and Region are referenced. If the placement request times out, you can resubmit the request or retry it with a different queue.
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
module Network.AWS.GameLift.StartGameSessionPlacement
  ( -- * Creating a request
    StartGameSessionPlacement (..),
    mkStartGameSessionPlacement,

    -- ** Request lenses
    sgspPlacementId,
    sgspGameProperties,
    sgspGameSessionName,
    sgspMaximumPlayerSessionCount,
    sgspPlayerLatencies,
    sgspGameSessionData,
    sgspDesiredPlayerSessions,
    sgspGameSessionQueueName,

    -- * Destructuring the response
    StartGameSessionPlacementResponse (..),
    mkStartGameSessionPlacementResponse,

    -- ** Response lenses
    sgsprsGameSessionPlacement,
    sgsprsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStartGameSessionPlacement' smart constructor.
data StartGameSessionPlacement = StartGameSessionPlacement'
  { -- | A unique identifier to assign to the new game session placement. This value is developer-defined. The value must be unique across all Regions and cannot be reused unless you are resubmitting a canceled or timed-out placement request.
    placementId :: Lude.Text,
    -- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameProperties :: Lude.Maybe [GameProperty],
    -- | A descriptive label that is associated with a game session. Session names do not need to be unique.
    gameSessionName :: Lude.Maybe Lude.Text,
    -- | The maximum number of players that can be connected simultaneously to the game session.
    maximumPlayerSessionCount :: Lude.Natural,
    -- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. This information is used to try to place the new game session where it can offer the best possible gameplay experience for the players.
    playerLatencies :: Lude.Maybe [PlayerLatency],
    -- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameSessionData :: Lude.Maybe Lude.Text,
    -- | Set of information on each player to create a player session for.
    desiredPlayerSessions :: Lude.Maybe [DesiredPlayerSession],
    -- | Name of the queue to use to place the new game session. You can use either the queue name or ARN value.
    gameSessionQueueName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartGameSessionPlacement' with the minimum fields required to make a request.
--
-- * 'placementId' - A unique identifier to assign to the new game session placement. This value is developer-defined. The value must be unique across all Regions and cannot be reused unless you are resubmitting a canceled or timed-out placement request.
-- * 'gameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
-- * 'gameSessionName' - A descriptive label that is associated with a game session. Session names do not need to be unique.
-- * 'maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to the game session.
-- * 'playerLatencies' - Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. This information is used to try to place the new game session where it can offer the best possible gameplay experience for the players.
-- * 'gameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
-- * 'desiredPlayerSessions' - Set of information on each player to create a player session for.
-- * 'gameSessionQueueName' - Name of the queue to use to place the new game session. You can use either the queue name or ARN value.
mkStartGameSessionPlacement ::
  -- | 'placementId'
  Lude.Text ->
  -- | 'maximumPlayerSessionCount'
  Lude.Natural ->
  -- | 'gameSessionQueueName'
  Lude.Text ->
  StartGameSessionPlacement
mkStartGameSessionPlacement
  pPlacementId_
  pMaximumPlayerSessionCount_
  pGameSessionQueueName_ =
    StartGameSessionPlacement'
      { placementId = pPlacementId_,
        gameProperties = Lude.Nothing,
        gameSessionName = Lude.Nothing,
        maximumPlayerSessionCount = pMaximumPlayerSessionCount_,
        playerLatencies = Lude.Nothing,
        gameSessionData = Lude.Nothing,
        desiredPlayerSessions = Lude.Nothing,
        gameSessionQueueName = pGameSessionQueueName_
      }

-- | A unique identifier to assign to the new game session placement. This value is developer-defined. The value must be unique across all Regions and cannot be reused unless you are resubmitting a canceled or timed-out placement request.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspPlacementId :: Lens.Lens' StartGameSessionPlacement Lude.Text
sgspPlacementId = Lens.lens (placementId :: StartGameSessionPlacement -> Lude.Text) (\s a -> s {placementId = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameProperties :: Lens.Lens' StartGameSessionPlacement (Lude.Maybe [GameProperty])
sgspGameProperties = Lens.lens (gameProperties :: StartGameSessionPlacement -> Lude.Maybe [GameProperty]) (\s a -> s {gameProperties = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'gameSessionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameSessionName :: Lens.Lens' StartGameSessionPlacement (Lude.Maybe Lude.Text)
sgspGameSessionName = Lens.lens (gameSessionName :: StartGameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionName = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspGameSessionName "Use generic-lens or generic-optics with 'gameSessionName' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspMaximumPlayerSessionCount :: Lens.Lens' StartGameSessionPlacement Lude.Natural
sgspMaximumPlayerSessionCount = Lens.lens (maximumPlayerSessionCount :: StartGameSessionPlacement -> Lude.Natural) (\s a -> s {maximumPlayerSessionCount = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. This information is used to try to place the new game session where it can offer the best possible gameplay experience for the players.
--
-- /Note:/ Consider using 'playerLatencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspPlayerLatencies :: Lens.Lens' StartGameSessionPlacement (Lude.Maybe [PlayerLatency])
sgspPlayerLatencies = Lens.lens (playerLatencies :: StartGameSessionPlacement -> Lude.Maybe [PlayerLatency]) (\s a -> s {playerLatencies = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspPlayerLatencies "Use generic-lens or generic-optics with 'playerLatencies' instead." #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameSessionData :: Lens.Lens' StartGameSessionPlacement (Lude.Maybe Lude.Text)
sgspGameSessionData = Lens.lens (gameSessionData :: StartGameSessionPlacement -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionData = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | Set of information on each player to create a player session for.
--
-- /Note:/ Consider using 'desiredPlayerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspDesiredPlayerSessions :: Lens.Lens' StartGameSessionPlacement (Lude.Maybe [DesiredPlayerSession])
sgspDesiredPlayerSessions = Lens.lens (desiredPlayerSessions :: StartGameSessionPlacement -> Lude.Maybe [DesiredPlayerSession]) (\s a -> s {desiredPlayerSessions = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspDesiredPlayerSessions "Use generic-lens or generic-optics with 'desiredPlayerSessions' instead." #-}

-- | Name of the queue to use to place the new game session. You can use either the queue name or ARN value.
--
-- /Note:/ Consider using 'gameSessionQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameSessionQueueName :: Lens.Lens' StartGameSessionPlacement Lude.Text
sgspGameSessionQueueName = Lens.lens (gameSessionQueueName :: StartGameSessionPlacement -> Lude.Text) (\s a -> s {gameSessionQueueName = a} :: StartGameSessionPlacement)
{-# DEPRECATED sgspGameSessionQueueName "Use generic-lens or generic-optics with 'gameSessionQueueName' instead." #-}

instance Lude.AWSRequest StartGameSessionPlacement where
  type
    Rs StartGameSessionPlacement =
      StartGameSessionPlacementResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartGameSessionPlacementResponse'
            Lude.<$> (x Lude..?> "GameSessionPlacement")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartGameSessionPlacement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.StartGameSessionPlacement" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartGameSessionPlacement where
  toJSON StartGameSessionPlacement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PlacementId" Lude..= placementId),
            ("GameProperties" Lude..=) Lude.<$> gameProperties,
            ("GameSessionName" Lude..=) Lude.<$> gameSessionName,
            Lude.Just
              ("MaximumPlayerSessionCount" Lude..= maximumPlayerSessionCount),
            ("PlayerLatencies" Lude..=) Lude.<$> playerLatencies,
            ("GameSessionData" Lude..=) Lude.<$> gameSessionData,
            ("DesiredPlayerSessions" Lude..=) Lude.<$> desiredPlayerSessions,
            Lude.Just ("GameSessionQueueName" Lude..= gameSessionQueueName)
          ]
      )

instance Lude.ToPath StartGameSessionPlacement where
  toPath = Lude.const "/"

instance Lude.ToQuery StartGameSessionPlacement where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStartGameSessionPlacementResponse' smart constructor.
data StartGameSessionPlacementResponse = StartGameSessionPlacementResponse'
  { -- | Object that describes the newly created game session placement. This object includes all the information provided in the request, as well as start/end time stamps and placement status.
    gameSessionPlacement :: Lude.Maybe GameSessionPlacement,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartGameSessionPlacementResponse' with the minimum fields required to make a request.
--
-- * 'gameSessionPlacement' - Object that describes the newly created game session placement. This object includes all the information provided in the request, as well as start/end time stamps and placement status.
-- * 'responseStatus' - The response status code.
mkStartGameSessionPlacementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartGameSessionPlacementResponse
mkStartGameSessionPlacementResponse pResponseStatus_ =
  StartGameSessionPlacementResponse'
    { gameSessionPlacement =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the newly created game session placement. This object includes all the information provided in the request, as well as start/end time stamps and placement status.
--
-- /Note:/ Consider using 'gameSessionPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsprsGameSessionPlacement :: Lens.Lens' StartGameSessionPlacementResponse (Lude.Maybe GameSessionPlacement)
sgsprsGameSessionPlacement = Lens.lens (gameSessionPlacement :: StartGameSessionPlacementResponse -> Lude.Maybe GameSessionPlacement) (\s a -> s {gameSessionPlacement = a} :: StartGameSessionPlacementResponse)
{-# DEPRECATED sgsprsGameSessionPlacement "Use generic-lens or generic-optics with 'gameSessionPlacement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsprsResponseStatus :: Lens.Lens' StartGameSessionPlacementResponse Lude.Int
sgsprsResponseStatus = Lens.lens (responseStatus :: StartGameSessionPlacementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartGameSessionPlacementResponse)
{-# DEPRECATED sgsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

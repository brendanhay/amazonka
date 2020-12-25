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
    sgspGameSessionQueueName,
    sgspMaximumPlayerSessionCount,
    sgspDesiredPlayerSessions,
    sgspGameProperties,
    sgspGameSessionData,
    sgspGameSessionName,
    sgspPlayerLatencies,

    -- * Destructuring the response
    StartGameSessionPlacementResponse (..),
    mkStartGameSessionPlacementResponse,

    -- ** Response lenses
    sgsprrsGameSessionPlacement,
    sgsprrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStartGameSessionPlacement' smart constructor.
data StartGameSessionPlacement = StartGameSessionPlacement'
  { -- | A unique identifier to assign to the new game session placement. This value is developer-defined. The value must be unique across all Regions and cannot be reused unless you are resubmitting a canceled or timed-out placement request.
    placementId :: Types.IdStringModel,
    -- | Name of the queue to use to place the new game session. You can use either the queue name or ARN value.
    gameSessionQueueName :: Types.GameSessionQueueNameOrArn,
    -- | The maximum number of players that can be connected simultaneously to the game session.
    maximumPlayerSessionCount :: Core.Natural,
    -- | Set of information on each player to create a player session for.
    desiredPlayerSessions :: Core.Maybe [Types.DesiredPlayerSession],
    -- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameProperties :: Core.Maybe [Types.GameProperty],
    -- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameSessionData :: Core.Maybe Types.GameSessionData,
    -- | A descriptive label that is associated with a game session. Session names do not need to be unique.
    gameSessionName :: Core.Maybe Types.GameSessionName,
    -- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. This information is used to try to place the new game session where it can offer the best possible gameplay experience for the players.
    playerLatencies :: Core.Maybe [Types.PlayerLatency]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartGameSessionPlacement' value with any optional fields omitted.
mkStartGameSessionPlacement ::
  -- | 'placementId'
  Types.IdStringModel ->
  -- | 'gameSessionQueueName'
  Types.GameSessionQueueNameOrArn ->
  -- | 'maximumPlayerSessionCount'
  Core.Natural ->
  StartGameSessionPlacement
mkStartGameSessionPlacement
  placementId
  gameSessionQueueName
  maximumPlayerSessionCount =
    StartGameSessionPlacement'
      { placementId,
        gameSessionQueueName,
        maximumPlayerSessionCount,
        desiredPlayerSessions = Core.Nothing,
        gameProperties = Core.Nothing,
        gameSessionData = Core.Nothing,
        gameSessionName = Core.Nothing,
        playerLatencies = Core.Nothing
      }

-- | A unique identifier to assign to the new game session placement. This value is developer-defined. The value must be unique across all Regions and cannot be reused unless you are resubmitting a canceled or timed-out placement request.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspPlacementId :: Lens.Lens' StartGameSessionPlacement Types.IdStringModel
sgspPlacementId = Lens.field @"placementId"
{-# DEPRECATED sgspPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

-- | Name of the queue to use to place the new game session. You can use either the queue name or ARN value.
--
-- /Note:/ Consider using 'gameSessionQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameSessionQueueName :: Lens.Lens' StartGameSessionPlacement Types.GameSessionQueueNameOrArn
sgspGameSessionQueueName = Lens.field @"gameSessionQueueName"
{-# DEPRECATED sgspGameSessionQueueName "Use generic-lens or generic-optics with 'gameSessionQueueName' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspMaximumPlayerSessionCount :: Lens.Lens' StartGameSessionPlacement Core.Natural
sgspMaximumPlayerSessionCount = Lens.field @"maximumPlayerSessionCount"
{-# DEPRECATED sgspMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | Set of information on each player to create a player session for.
--
-- /Note:/ Consider using 'desiredPlayerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspDesiredPlayerSessions :: Lens.Lens' StartGameSessionPlacement (Core.Maybe [Types.DesiredPlayerSession])
sgspDesiredPlayerSessions = Lens.field @"desiredPlayerSessions"
{-# DEPRECATED sgspDesiredPlayerSessions "Use generic-lens or generic-optics with 'desiredPlayerSessions' instead." #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameProperties :: Lens.Lens' StartGameSessionPlacement (Core.Maybe [Types.GameProperty])
sgspGameProperties = Lens.field @"gameProperties"
{-# DEPRECATED sgspGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameSessionData :: Lens.Lens' StartGameSessionPlacement (Core.Maybe Types.GameSessionData)
sgspGameSessionData = Lens.field @"gameSessionData"
{-# DEPRECATED sgspGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'gameSessionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspGameSessionName :: Lens.Lens' StartGameSessionPlacement (Core.Maybe Types.GameSessionName)
sgspGameSessionName = Lens.field @"gameSessionName"
{-# DEPRECATED sgspGameSessionName "Use generic-lens or generic-optics with 'gameSessionName' instead." #-}

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions. This information is used to try to place the new game session where it can offer the best possible gameplay experience for the players.
--
-- /Note:/ Consider using 'playerLatencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspPlayerLatencies :: Lens.Lens' StartGameSessionPlacement (Core.Maybe [Types.PlayerLatency])
sgspPlayerLatencies = Lens.field @"playerLatencies"
{-# DEPRECATED sgspPlayerLatencies "Use generic-lens or generic-optics with 'playerLatencies' instead." #-}

instance Core.FromJSON StartGameSessionPlacement where
  toJSON StartGameSessionPlacement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PlacementId" Core..= placementId),
            Core.Just ("GameSessionQueueName" Core..= gameSessionQueueName),
            Core.Just
              ("MaximumPlayerSessionCount" Core..= maximumPlayerSessionCount),
            ("DesiredPlayerSessions" Core..=) Core.<$> desiredPlayerSessions,
            ("GameProperties" Core..=) Core.<$> gameProperties,
            ("GameSessionData" Core..=) Core.<$> gameSessionData,
            ("GameSessionName" Core..=) Core.<$> gameSessionName,
            ("PlayerLatencies" Core..=) Core.<$> playerLatencies
          ]
      )

instance Core.AWSRequest StartGameSessionPlacement where
  type
    Rs StartGameSessionPlacement =
      StartGameSessionPlacementResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.StartGameSessionPlacement")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartGameSessionPlacementResponse'
            Core.<$> (x Core..:? "GameSessionPlacement")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStartGameSessionPlacementResponse' smart constructor.
data StartGameSessionPlacementResponse = StartGameSessionPlacementResponse'
  { -- | Object that describes the newly created game session placement. This object includes all the information provided in the request, as well as start/end time stamps and placement status.
    gameSessionPlacement :: Core.Maybe Types.GameSessionPlacement,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartGameSessionPlacementResponse' value with any optional fields omitted.
mkStartGameSessionPlacementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartGameSessionPlacementResponse
mkStartGameSessionPlacementResponse responseStatus =
  StartGameSessionPlacementResponse'
    { gameSessionPlacement =
        Core.Nothing,
      responseStatus
    }

-- | Object that describes the newly created game session placement. This object includes all the information provided in the request, as well as start/end time stamps and placement status.
--
-- /Note:/ Consider using 'gameSessionPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsprrsGameSessionPlacement :: Lens.Lens' StartGameSessionPlacementResponse (Core.Maybe Types.GameSessionPlacement)
sgsprrsGameSessionPlacement = Lens.field @"gameSessionPlacement"
{-# DEPRECATED sgsprrsGameSessionPlacement "Use generic-lens or generic-optics with 'gameSessionPlacement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsprrsResponseStatus :: Lens.Lens' StartGameSessionPlacementResponse Core.Int
sgsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sgsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

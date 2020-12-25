{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreatePlayerSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves open slots in a game session for a group of players. Before players can be added, a game session must have an @ACTIVE@ status, have a creation policy of @ALLOW_ALL@ , and have an open player slot. To add a single player to a game session, use 'CreatePlayerSession' . When a player connects to the game server and references a player session ID, the game server contacts the Amazon GameLift service to validate the player reservation and accept the player.
--
-- To create player sessions, specify a game session ID, a list of player IDs, and optionally a set of player data strings. If successful, a slot is reserved in the game session for each player and a set of new 'PlayerSession' objects is returned. Player sessions cannot be updated.
-- /Available in Amazon GameLift Local./
--
--     * 'CreatePlayerSession'
--
--
--     * 'CreatePlayerSessions'
--
--
--     * 'DescribePlayerSessions'
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
module Network.AWS.GameLift.CreatePlayerSessions
  ( -- * Creating a request
    CreatePlayerSessions (..),
    mkCreatePlayerSessions,

    -- ** Request lenses
    cpsGameSessionId,
    cpsPlayerIds,
    cpsPlayerDataMap,

    -- * Destructuring the response
    CreatePlayerSessionsResponse (..),
    mkCreatePlayerSessionsResponse,

    -- ** Response lenses
    cpsrrsPlayerSessions,
    cpsrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreatePlayerSessions' smart constructor.
data CreatePlayerSessions = CreatePlayerSessions'
  { -- | A unique identifier for the game session to add players to.
    gameSessionId :: Types.GameSessionId,
    -- | List of unique identifiers for the players to be added.
    playerIds :: Core.NonEmpty Types.NonZeroAndMaxString,
    -- | Map of string pairs, each specifying a player ID and a set of developer-defined information related to the player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game. Player data strings for player IDs not included in the @PlayerIds@ parameter are ignored.
    playerDataMap :: Core.Maybe (Core.HashMap Types.NonZeroAndMaxString Types.PlayerData)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlayerSessions' value with any optional fields omitted.
mkCreatePlayerSessions ::
  -- | 'gameSessionId'
  Types.GameSessionId ->
  -- | 'playerIds'
  Core.NonEmpty Types.NonZeroAndMaxString ->
  CreatePlayerSessions
mkCreatePlayerSessions gameSessionId playerIds =
  CreatePlayerSessions'
    { gameSessionId,
      playerIds,
      playerDataMap = Core.Nothing
    }

-- | A unique identifier for the game session to add players to.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsGameSessionId :: Lens.Lens' CreatePlayerSessions Types.GameSessionId
cpsGameSessionId = Lens.field @"gameSessionId"
{-# DEPRECATED cpsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | List of unique identifiers for the players to be added.
--
-- /Note:/ Consider using 'playerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsPlayerIds :: Lens.Lens' CreatePlayerSessions (Core.NonEmpty Types.NonZeroAndMaxString)
cpsPlayerIds = Lens.field @"playerIds"
{-# DEPRECATED cpsPlayerIds "Use generic-lens or generic-optics with 'playerIds' instead." #-}

-- | Map of string pairs, each specifying a player ID and a set of developer-defined information related to the player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game. Player data strings for player IDs not included in the @PlayerIds@ parameter are ignored.
--
-- /Note:/ Consider using 'playerDataMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsPlayerDataMap :: Lens.Lens' CreatePlayerSessions (Core.Maybe (Core.HashMap Types.NonZeroAndMaxString Types.PlayerData))
cpsPlayerDataMap = Lens.field @"playerDataMap"
{-# DEPRECATED cpsPlayerDataMap "Use generic-lens or generic-optics with 'playerDataMap' instead." #-}

instance Core.FromJSON CreatePlayerSessions where
  toJSON CreatePlayerSessions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameSessionId" Core..= gameSessionId),
            Core.Just ("PlayerIds" Core..= playerIds),
            ("PlayerDataMap" Core..=) Core.<$> playerDataMap
          ]
      )

instance Core.AWSRequest CreatePlayerSessions where
  type Rs CreatePlayerSessions = CreatePlayerSessionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.CreatePlayerSessions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlayerSessionsResponse'
            Core.<$> (x Core..:? "PlayerSessions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreatePlayerSessionsResponse' smart constructor.
data CreatePlayerSessionsResponse = CreatePlayerSessionsResponse'
  { -- | A collection of player session objects created for the added players.
    playerSessions :: Core.Maybe [Types.PlayerSession],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePlayerSessionsResponse' value with any optional fields omitted.
mkCreatePlayerSessionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePlayerSessionsResponse
mkCreatePlayerSessionsResponse responseStatus =
  CreatePlayerSessionsResponse'
    { playerSessions = Core.Nothing,
      responseStatus
    }

-- | A collection of player session objects created for the added players.
--
-- /Note:/ Consider using 'playerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrrsPlayerSessions :: Lens.Lens' CreatePlayerSessionsResponse (Core.Maybe [Types.PlayerSession])
cpsrrsPlayerSessions = Lens.field @"playerSessions"
{-# DEPRECATED cpsrrsPlayerSessions "Use generic-lens or generic-optics with 'playerSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrrsResponseStatus :: Lens.Lens' CreatePlayerSessionsResponse Core.Int
cpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreatePlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves an open player slot in an active game session. Before a player can be added, a game session must have an @ACTIVE@ status, have a creation policy of @ALLOW_ALL@ , and have an open player slot. To add a group of players to a game session, use 'CreatePlayerSessions' . When the player connects to the game server and references a player session ID, the game server contacts the Amazon GameLift service to validate the player reservation and accept the player.
--
-- To create a player session, specify a game session ID, player ID, and optionally a string of player data. If successful, a slot is reserved in the game session for the player and a new 'PlayerSession' object is returned. Player sessions cannot be updated. 
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
--
--
--
--
module Network.AWS.GameLift.CreatePlayerSession
    (
    -- * Creating a request
      CreatePlayerSession (..)
    , mkCreatePlayerSession
    -- ** Request lenses
    , cGameSessionId
    , cPlayerId
    , cPlayerData

    -- * Destructuring the response
    , CreatePlayerSessionResponse (..)
    , mkCreatePlayerSessionResponse
    -- ** Response lenses
    , cpsrfrsPlayerSession
    , cpsrfrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreatePlayerSession' smart constructor.
data CreatePlayerSession = CreatePlayerSession'
  { gameSessionId :: Types.ArnStringModel
    -- ^ A unique identifier for the game session to add a player to.
  , playerId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for a player. Player IDs are developer-defined.
  , playerData :: Core.Maybe Types.PlayerData
    -- ^ Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlayerSession' value with any optional fields omitted.
mkCreatePlayerSession
    :: Types.ArnStringModel -- ^ 'gameSessionId'
    -> Types.NonZeroAndMaxString -- ^ 'playerId'
    -> CreatePlayerSession
mkCreatePlayerSession gameSessionId playerId
  = CreatePlayerSession'{gameSessionId, playerId,
                         playerData = Core.Nothing}

-- | A unique identifier for the game session to add a player to.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGameSessionId :: Lens.Lens' CreatePlayerSession Types.ArnStringModel
cGameSessionId = Lens.field @"gameSessionId"
{-# INLINEABLE cGameSessionId #-}
{-# DEPRECATED gameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead"  #-}

-- | A unique identifier for a player. Player IDs are developer-defined.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPlayerId :: Lens.Lens' CreatePlayerSession Types.NonZeroAndMaxString
cPlayerId = Lens.field @"playerId"
{-# INLINEABLE cPlayerId #-}
{-# DEPRECATED playerId "Use generic-lens or generic-optics with 'playerId' instead"  #-}

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- /Note:/ Consider using 'playerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPlayerData :: Lens.Lens' CreatePlayerSession (Core.Maybe Types.PlayerData)
cPlayerData = Lens.field @"playerData"
{-# INLINEABLE cPlayerData #-}
{-# DEPRECATED playerData "Use generic-lens or generic-optics with 'playerData' instead"  #-}

instance Core.ToQuery CreatePlayerSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePlayerSession where
        toHeaders CreatePlayerSession{..}
          = Core.pure ("X-Amz-Target", "GameLift.CreatePlayerSession")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePlayerSession where
        toJSON CreatePlayerSession{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameSessionId" Core..= gameSessionId),
                  Core.Just ("PlayerId" Core..= playerId),
                  ("PlayerData" Core..=) Core.<$> playerData])

instance Core.AWSRequest CreatePlayerSession where
        type Rs CreatePlayerSession = CreatePlayerSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePlayerSessionResponse' Core.<$>
                   (x Core..:? "PlayerSession") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreatePlayerSessionResponse' smart constructor.
data CreatePlayerSessionResponse = CreatePlayerSessionResponse'
  { playerSession :: Core.Maybe Types.PlayerSession
    -- ^ Object that describes the newly created player session record.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePlayerSessionResponse' value with any optional fields omitted.
mkCreatePlayerSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePlayerSessionResponse
mkCreatePlayerSessionResponse responseStatus
  = CreatePlayerSessionResponse'{playerSession = Core.Nothing,
                                 responseStatus}

-- | Object that describes the newly created player session record.
--
-- /Note:/ Consider using 'playerSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrfrsPlayerSession :: Lens.Lens' CreatePlayerSessionResponse (Core.Maybe Types.PlayerSession)
cpsrfrsPlayerSession = Lens.field @"playerSession"
{-# INLINEABLE cpsrfrsPlayerSession #-}
{-# DEPRECATED playerSession "Use generic-lens or generic-optics with 'playerSession' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrfrsResponseStatus :: Lens.Lens' CreatePlayerSessionResponse Core.Int
cpsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

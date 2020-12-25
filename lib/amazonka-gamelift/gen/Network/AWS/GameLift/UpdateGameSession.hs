{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates game session properties. This includes the session name, maximum player count, protection policy, which controls whether or not an active game session can be terminated during a scale-down event, and the player session creation policy, which controls whether or not new players can join the session. To update a game session, specify the game session ID and the values you want to change. If successful, an updated 'GameSession' object is returned.
--
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
module Network.AWS.GameLift.UpdateGameSession
  ( -- * Creating a request
    UpdateGameSession (..),
    mkUpdateGameSession,

    -- ** Request lenses
    ugsGameSessionId,
    ugsMaximumPlayerSessionCount,
    ugsName,
    ugsPlayerSessionCreationPolicy,
    ugsProtectionPolicy,

    -- * Destructuring the response
    UpdateGameSessionResponse (..),
    mkUpdateGameSessionResponse,

    -- ** Response lenses
    ursGameSession,
    ursResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateGameSession' smart constructor.
data UpdateGameSession = UpdateGameSession'
  { -- | A unique identifier for the game session to update.
    gameSessionId :: Types.ArnStringModel,
    -- | The maximum number of players that can be connected simultaneously to the game session.
    maximumPlayerSessionCount :: Core.Maybe Core.Natural,
    -- | A descriptive label that is associated with a game session. Session names do not need to be unique.
    name :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Policy determining whether or not the game session accepts new players.
    playerSessionCreationPolicy :: Core.Maybe Types.PlayerSessionCreationPolicy,
    -- | Game session protection policy to apply to this game session only.
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    protectionPolicy :: Core.Maybe Types.ProtectionPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGameSession' value with any optional fields omitted.
mkUpdateGameSession ::
  -- | 'gameSessionId'
  Types.ArnStringModel ->
  UpdateGameSession
mkUpdateGameSession gameSessionId =
  UpdateGameSession'
    { gameSessionId,
      maximumPlayerSessionCount = Core.Nothing,
      name = Core.Nothing,
      playerSessionCreationPolicy = Core.Nothing,
      protectionPolicy = Core.Nothing
    }

-- | A unique identifier for the game session to update.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameSessionId :: Lens.Lens' UpdateGameSession Types.ArnStringModel
ugsGameSessionId = Lens.field @"gameSessionId"
{-# DEPRECATED ugsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsMaximumPlayerSessionCount :: Lens.Lens' UpdateGameSession (Core.Maybe Core.Natural)
ugsMaximumPlayerSessionCount = Lens.field @"maximumPlayerSessionCount"
{-# DEPRECATED ugsMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsName :: Lens.Lens' UpdateGameSession (Core.Maybe Types.NonZeroAndMaxString)
ugsName = Lens.field @"name"
{-# DEPRECATED ugsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Policy determining whether or not the game session accepts new players.
--
-- /Note:/ Consider using 'playerSessionCreationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsPlayerSessionCreationPolicy :: Lens.Lens' UpdateGameSession (Core.Maybe Types.PlayerSessionCreationPolicy)
ugsPlayerSessionCreationPolicy = Lens.field @"playerSessionCreationPolicy"
{-# DEPRECATED ugsPlayerSessionCreationPolicy "Use generic-lens or generic-optics with 'playerSessionCreationPolicy' instead." #-}

-- | Game session protection policy to apply to this game session only.
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
--
-- /Note:/ Consider using 'protectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsProtectionPolicy :: Lens.Lens' UpdateGameSession (Core.Maybe Types.ProtectionPolicy)
ugsProtectionPolicy = Lens.field @"protectionPolicy"
{-# DEPRECATED ugsProtectionPolicy "Use generic-lens or generic-optics with 'protectionPolicy' instead." #-}

instance Core.FromJSON UpdateGameSession where
  toJSON UpdateGameSession {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameSessionId" Core..= gameSessionId),
            ("MaximumPlayerSessionCount" Core..=)
              Core.<$> maximumPlayerSessionCount,
            ("Name" Core..=) Core.<$> name,
            ("PlayerSessionCreationPolicy" Core..=)
              Core.<$> playerSessionCreationPolicy,
            ("ProtectionPolicy" Core..=) Core.<$> protectionPolicy
          ]
      )

instance Core.AWSRequest UpdateGameSession where
  type Rs UpdateGameSession = UpdateGameSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.UpdateGameSession")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameSessionResponse'
            Core.<$> (x Core..:? "GameSession") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateGameSessionResponse' smart constructor.
data UpdateGameSessionResponse = UpdateGameSessionResponse'
  { -- | The updated game session metadata.
    gameSession :: Core.Maybe Types.GameSession,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateGameSessionResponse' value with any optional fields omitted.
mkUpdateGameSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGameSessionResponse
mkUpdateGameSessionResponse responseStatus =
  UpdateGameSessionResponse'
    { gameSession = Core.Nothing,
      responseStatus
    }

-- | The updated game session metadata.
--
-- /Note:/ Consider using 'gameSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursGameSession :: Lens.Lens' UpdateGameSessionResponse (Core.Maybe Types.GameSession)
ursGameSession = Lens.field @"gameSession"
{-# DEPRECATED ursGameSession "Use generic-lens or generic-optics with 'gameSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateGameSessionResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

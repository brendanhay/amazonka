{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchedPlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchedPlayerSession
  ( MatchedPlayerSession (..),

    -- * Smart constructor
    mkMatchedPlayerSession,

    -- * Lenses
    mpsPlayerId,
    mpsPlayerSessionId,
  )
where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.PlayerSessionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a new player session that is created as a result of a successful FlexMatch match. A successful match automatically creates new player sessions for every player ID in the original matchmaking request.
--
-- When players connect to the match's game session, they must include both player ID and player session ID in order to claim their assigned player slot.
--
-- /See:/ 'mkMatchedPlayerSession' smart constructor.
data MatchedPlayerSession = MatchedPlayerSession'
  { -- | A unique identifier for a player
    playerId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | A unique identifier for a player session
    playerSessionId :: Core.Maybe Types.PlayerSessionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MatchedPlayerSession' value with any optional fields omitted.
mkMatchedPlayerSession ::
  MatchedPlayerSession
mkMatchedPlayerSession =
  MatchedPlayerSession'
    { playerId = Core.Nothing,
      playerSessionId = Core.Nothing
    }

-- | A unique identifier for a player
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsPlayerId :: Lens.Lens' MatchedPlayerSession (Core.Maybe Types.NonZeroAndMaxString)
mpsPlayerId = Lens.field @"playerId"
{-# DEPRECATED mpsPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

-- | A unique identifier for a player session
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsPlayerSessionId :: Lens.Lens' MatchedPlayerSession (Core.Maybe Types.PlayerSessionId)
mpsPlayerSessionId = Lens.field @"playerSessionId"
{-# DEPRECATED mpsPlayerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead." #-}

instance Core.FromJSON MatchedPlayerSession where
  parseJSON =
    Core.withObject "MatchedPlayerSession" Core.$
      \x ->
        MatchedPlayerSession'
          Core.<$> (x Core..:? "PlayerId") Core.<*> (x Core..:? "PlayerSessionId")

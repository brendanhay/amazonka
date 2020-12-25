{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionDetail
  ( GameSessionDetail (..),

    -- * Smart constructor
    mkGameSessionDetail,

    -- * Lenses
    gsdGameSession,
    gsdProtectionPolicy,
  )
where

import qualified Network.AWS.GameLift.Types.GameSession as Types
import qualified Network.AWS.GameLift.Types.ProtectionPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A game session's properties plus the protection policy currently in force.
--
-- /See:/ 'mkGameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
  { -- | Object that describes a game session.
    gameSession :: Core.Maybe Types.GameSession,
    -- | Current status of protection for the game session.
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    protectionPolicy :: Core.Maybe Types.ProtectionPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GameSessionDetail' value with any optional fields omitted.
mkGameSessionDetail ::
  GameSessionDetail
mkGameSessionDetail =
  GameSessionDetail'
    { gameSession = Core.Nothing,
      protectionPolicy = Core.Nothing
    }

-- | Object that describes a game session.
--
-- /Note:/ Consider using 'gameSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdGameSession :: Lens.Lens' GameSessionDetail (Core.Maybe Types.GameSession)
gsdGameSession = Lens.field @"gameSession"
{-# DEPRECATED gsdGameSession "Use generic-lens or generic-optics with 'gameSession' instead." #-}

-- | Current status of protection for the game session.
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
gsdProtectionPolicy :: Lens.Lens' GameSessionDetail (Core.Maybe Types.ProtectionPolicy)
gsdProtectionPolicy = Lens.field @"protectionPolicy"
{-# DEPRECATED gsdProtectionPolicy "Use generic-lens or generic-optics with 'protectionPolicy' instead." #-}

instance Core.FromJSON GameSessionDetail where
  parseJSON =
    Core.withObject "GameSessionDetail" Core.$
      \x ->
        GameSessionDetail'
          Core.<$> (x Core..:? "GameSession") Core.<*> (x Core..:? "ProtectionPolicy")

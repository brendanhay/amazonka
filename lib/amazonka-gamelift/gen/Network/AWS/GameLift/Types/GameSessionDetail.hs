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

import Network.AWS.GameLift.Types.GameSession
import Network.AWS.GameLift.Types.ProtectionPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A game session's properties plus the protection policy currently in force.
--
-- /See:/ 'mkGameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
  { -- | Object that describes a game session.
    gameSession :: Lude.Maybe GameSession,
    -- | Current status of protection for the game session.
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    protectionPolicy :: Lude.Maybe ProtectionPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameSessionDetail' with the minimum fields required to make a request.
--
-- * 'gameSession' - Object that describes a game session.
-- * 'protectionPolicy' - Current status of protection for the game session.
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
mkGameSessionDetail ::
  GameSessionDetail
mkGameSessionDetail =
  GameSessionDetail'
    { gameSession = Lude.Nothing,
      protectionPolicy = Lude.Nothing
    }

-- | Object that describes a game session.
--
-- /Note:/ Consider using 'gameSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdGameSession :: Lens.Lens' GameSessionDetail (Lude.Maybe GameSession)
gsdGameSession = Lens.lens (gameSession :: GameSessionDetail -> Lude.Maybe GameSession) (\s a -> s {gameSession = a} :: GameSessionDetail)
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
gsdProtectionPolicy :: Lens.Lens' GameSessionDetail (Lude.Maybe ProtectionPolicy)
gsdProtectionPolicy = Lens.lens (protectionPolicy :: GameSessionDetail -> Lude.Maybe ProtectionPolicy) (\s a -> s {protectionPolicy = a} :: GameSessionDetail)
{-# DEPRECATED gsdProtectionPolicy "Use generic-lens or generic-optics with 'protectionPolicy' instead." #-}

instance Lude.FromJSON GameSessionDetail where
  parseJSON =
    Lude.withObject
      "GameSessionDetail"
      ( \x ->
          GameSessionDetail'
            Lude.<$> (x Lude..:? "GameSession")
            Lude.<*> (x Lude..:? "ProtectionPolicy")
      )

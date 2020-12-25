{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.DesiredPlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.DesiredPlayerSession
  ( DesiredPlayerSession (..),

    -- * Smart constructor
    mkDesiredPlayerSession,

    -- * Lenses
    dpsPlayerData,
    dpsPlayerId,
  )
where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.PlayerData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Player information for use when creating player sessions using a game session placement request with 'StartGameSessionPlacement' .
--
-- /See:/ 'mkDesiredPlayerSession' smart constructor.
data DesiredPlayerSession = DesiredPlayerSession'
  { -- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
    playerData :: Core.Maybe Types.PlayerData,
    -- | A unique identifier for a player to associate with the player session.
    playerId :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DesiredPlayerSession' value with any optional fields omitted.
mkDesiredPlayerSession ::
  DesiredPlayerSession
mkDesiredPlayerSession =
  DesiredPlayerSession'
    { playerData = Core.Nothing,
      playerId = Core.Nothing
    }

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- /Note:/ Consider using 'playerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPlayerData :: Lens.Lens' DesiredPlayerSession (Core.Maybe Types.PlayerData)
dpsPlayerData = Lens.field @"playerData"
{-# DEPRECATED dpsPlayerData "Use generic-lens or generic-optics with 'playerData' instead." #-}

-- | A unique identifier for a player to associate with the player session.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPlayerId :: Lens.Lens' DesiredPlayerSession (Core.Maybe Types.NonZeroAndMaxString)
dpsPlayerId = Lens.field @"playerId"
{-# DEPRECATED dpsPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

instance Core.FromJSON DesiredPlayerSession where
  toJSON DesiredPlayerSession {..} =
    Core.object
      ( Core.catMaybes
          [ ("PlayerData" Core..=) Core.<$> playerData,
            ("PlayerId" Core..=) Core.<$> playerId
          ]
      )

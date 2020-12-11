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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Player information for use when creating player sessions using a game session placement request with 'StartGameSessionPlacement' .
--
-- /See:/ 'mkDesiredPlayerSession' smart constructor.
data DesiredPlayerSession = DesiredPlayerSession'
  { playerData ::
      Lude.Maybe Lude.Text,
    playerId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DesiredPlayerSession' with the minimum fields required to make a request.
--
-- * 'playerData' - Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
-- * 'playerId' - A unique identifier for a player to associate with the player session.
mkDesiredPlayerSession ::
  DesiredPlayerSession
mkDesiredPlayerSession =
  DesiredPlayerSession'
    { playerData = Lude.Nothing,
      playerId = Lude.Nothing
    }

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- /Note:/ Consider using 'playerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPlayerData :: Lens.Lens' DesiredPlayerSession (Lude.Maybe Lude.Text)
dpsPlayerData = Lens.lens (playerData :: DesiredPlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerData = a} :: DesiredPlayerSession)
{-# DEPRECATED dpsPlayerData "Use generic-lens or generic-optics with 'playerData' instead." #-}

-- | A unique identifier for a player to associate with the player session.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPlayerId :: Lens.Lens' DesiredPlayerSession (Lude.Maybe Lude.Text)
dpsPlayerId = Lens.lens (playerId :: DesiredPlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerId = a} :: DesiredPlayerSession)
{-# DEPRECATED dpsPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

instance Lude.ToJSON DesiredPlayerSession where
  toJSON DesiredPlayerSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PlayerData" Lude..=) Lude.<$> playerData,
            ("PlayerId" Lude..=) Lude.<$> playerId
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlacedPlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlacedPlayerSession
  ( PlacedPlayerSession (..),

    -- * Smart constructor
    mkPlacedPlayerSession,

    -- * Lenses
    ppsPlayerSessionId,
    ppsPlayerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a player session that was created as part of a 'StartGameSessionPlacement' request. This object contains only the player ID and player session ID. To retrieve full details on a player session, call 'DescribePlayerSessions' with the player session ID.
--
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
--
-- /See:/ 'mkPlacedPlayerSession' smart constructor.
data PlacedPlayerSession = PlacedPlayerSession'
  { playerSessionId ::
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

-- | Creates a value of 'PlacedPlayerSession' with the minimum fields required to make a request.
--
-- * 'playerId' - A unique identifier for a player that is associated with this player session.
-- * 'playerSessionId' - A unique identifier for a player session.
mkPlacedPlayerSession ::
  PlacedPlayerSession
mkPlacedPlayerSession =
  PlacedPlayerSession'
    { playerSessionId = Lude.Nothing,
      playerId = Lude.Nothing
    }

-- | A unique identifier for a player session.
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppsPlayerSessionId :: Lens.Lens' PlacedPlayerSession (Lude.Maybe Lude.Text)
ppsPlayerSessionId = Lens.lens (playerSessionId :: PlacedPlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerSessionId = a} :: PlacedPlayerSession)
{-# DEPRECATED ppsPlayerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead." #-}

-- | A unique identifier for a player that is associated with this player session.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppsPlayerId :: Lens.Lens' PlacedPlayerSession (Lude.Maybe Lude.Text)
ppsPlayerId = Lens.lens (playerId :: PlacedPlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerId = a} :: PlacedPlayerSession)
{-# DEPRECATED ppsPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

instance Lude.FromJSON PlacedPlayerSession where
  parseJSON =
    Lude.withObject
      "PlacedPlayerSession"
      ( \x ->
          PlacedPlayerSession'
            Lude.<$> (x Lude..:? "PlayerSessionId") Lude.<*> (x Lude..:? "PlayerId")
      )

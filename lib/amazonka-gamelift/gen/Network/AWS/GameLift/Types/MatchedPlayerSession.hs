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
    mpsPlayerSessionId,
    mpsPlayerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a new player session that is created as a result of a successful FlexMatch match. A successful match automatically creates new player sessions for every player ID in the original matchmaking request.
--
-- When players connect to the match's game session, they must include both player ID and player session ID in order to claim their assigned player slot.
--
-- /See:/ 'mkMatchedPlayerSession' smart constructor.
data MatchedPlayerSession = MatchedPlayerSession'
  { -- | A unique identifier for a player session
    playerSessionId :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a player
    playerId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MatchedPlayerSession' with the minimum fields required to make a request.
--
-- * 'playerSessionId' - A unique identifier for a player session
-- * 'playerId' - A unique identifier for a player
mkMatchedPlayerSession ::
  MatchedPlayerSession
mkMatchedPlayerSession =
  MatchedPlayerSession'
    { playerSessionId = Lude.Nothing,
      playerId = Lude.Nothing
    }

-- | A unique identifier for a player session
--
-- /Note:/ Consider using 'playerSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsPlayerSessionId :: Lens.Lens' MatchedPlayerSession (Lude.Maybe Lude.Text)
mpsPlayerSessionId = Lens.lens (playerSessionId :: MatchedPlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerSessionId = a} :: MatchedPlayerSession)
{-# DEPRECATED mpsPlayerSessionId "Use generic-lens or generic-optics with 'playerSessionId' instead." #-}

-- | A unique identifier for a player
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsPlayerId :: Lens.Lens' MatchedPlayerSession (Lude.Maybe Lude.Text)
mpsPlayerId = Lens.lens (playerId :: MatchedPlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerId = a} :: MatchedPlayerSession)
{-# DEPRECATED mpsPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

instance Lude.FromJSON MatchedPlayerSession where
  parseJSON =
    Lude.withObject
      "MatchedPlayerSession"
      ( \x ->
          MatchedPlayerSession'
            Lude.<$> (x Lude..:? "PlayerSessionId") Lude.<*> (x Lude..:? "PlayerId")
      )

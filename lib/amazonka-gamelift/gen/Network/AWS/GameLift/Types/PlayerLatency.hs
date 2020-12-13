{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerLatency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerLatency
  ( PlayerLatency (..),

    -- * Smart constructor
    mkPlayerLatency,

    -- * Lenses
    plLatencyInMilliseconds,
    plRegionIdentifier,
    plPlayerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Regional latency information for a player, used when requesting a new game session with 'StartGameSessionPlacement' . This value indicates the amount of time lag that exists when the player is connected to a fleet in the specified Region. The relative difference between a player's latency values for multiple Regions are used to determine which fleets are best suited to place a new game session for the player.
--
-- /See:/ 'mkPlayerLatency' smart constructor.
data PlayerLatency = PlayerLatency'
  { -- | Amount of time that represents the time lag experienced by the player when connected to the specified Region.
    latencyInMilliseconds :: Lude.Maybe Lude.Double,
    -- | Name of the Region that is associated with the latency value.
    regionIdentifier :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a player associated with the latency data.
    playerId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlayerLatency' with the minimum fields required to make a request.
--
-- * 'latencyInMilliseconds' - Amount of time that represents the time lag experienced by the player when connected to the specified Region.
-- * 'regionIdentifier' - Name of the Region that is associated with the latency value.
-- * 'playerId' - A unique identifier for a player associated with the latency data.
mkPlayerLatency ::
  PlayerLatency
mkPlayerLatency =
  PlayerLatency'
    { latencyInMilliseconds = Lude.Nothing,
      regionIdentifier = Lude.Nothing,
      playerId = Lude.Nothing
    }

-- | Amount of time that represents the time lag experienced by the player when connected to the specified Region.
--
-- /Note:/ Consider using 'latencyInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plLatencyInMilliseconds :: Lens.Lens' PlayerLatency (Lude.Maybe Lude.Double)
plLatencyInMilliseconds = Lens.lens (latencyInMilliseconds :: PlayerLatency -> Lude.Maybe Lude.Double) (\s a -> s {latencyInMilliseconds = a} :: PlayerLatency)
{-# DEPRECATED plLatencyInMilliseconds "Use generic-lens or generic-optics with 'latencyInMilliseconds' instead." #-}

-- | Name of the Region that is associated with the latency value.
--
-- /Note:/ Consider using 'regionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plRegionIdentifier :: Lens.Lens' PlayerLatency (Lude.Maybe Lude.Text)
plRegionIdentifier = Lens.lens (regionIdentifier :: PlayerLatency -> Lude.Maybe Lude.Text) (\s a -> s {regionIdentifier = a} :: PlayerLatency)
{-# DEPRECATED plRegionIdentifier "Use generic-lens or generic-optics with 'regionIdentifier' instead." #-}

-- | A unique identifier for a player associated with the latency data.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plPlayerId :: Lens.Lens' PlayerLatency (Lude.Maybe Lude.Text)
plPlayerId = Lens.lens (playerId :: PlayerLatency -> Lude.Maybe Lude.Text) (\s a -> s {playerId = a} :: PlayerLatency)
{-# DEPRECATED plPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

instance Lude.FromJSON PlayerLatency where
  parseJSON =
    Lude.withObject
      "PlayerLatency"
      ( \x ->
          PlayerLatency'
            Lude.<$> (x Lude..:? "LatencyInMilliseconds")
            Lude.<*> (x Lude..:? "RegionIdentifier")
            Lude.<*> (x Lude..:? "PlayerId")
      )

instance Lude.ToJSON PlayerLatency where
  toJSON PlayerLatency' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LatencyInMilliseconds" Lude..=) Lude.<$> latencyInMilliseconds,
            ("RegionIdentifier" Lude..=) Lude.<$> regionIdentifier,
            ("PlayerId" Lude..=) Lude.<$> playerId
          ]
      )

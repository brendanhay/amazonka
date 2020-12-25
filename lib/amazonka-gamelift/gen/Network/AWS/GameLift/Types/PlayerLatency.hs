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
    plPlayerId,
    plRegionIdentifier,
  )
where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Regional latency information for a player, used when requesting a new game session with 'StartGameSessionPlacement' . This value indicates the amount of time lag that exists when the player is connected to a fleet in the specified Region. The relative difference between a player's latency values for multiple Regions are used to determine which fleets are best suited to place a new game session for the player.
--
-- /See:/ 'mkPlayerLatency' smart constructor.
data PlayerLatency = PlayerLatency'
  { -- | Amount of time that represents the time lag experienced by the player when connected to the specified Region.
    latencyInMilliseconds :: Core.Maybe Core.Double,
    -- | A unique identifier for a player associated with the latency data.
    playerId :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Name of the Region that is associated with the latency value.
    regionIdentifier :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlayerLatency' value with any optional fields omitted.
mkPlayerLatency ::
  PlayerLatency
mkPlayerLatency =
  PlayerLatency'
    { latencyInMilliseconds = Core.Nothing,
      playerId = Core.Nothing,
      regionIdentifier = Core.Nothing
    }

-- | Amount of time that represents the time lag experienced by the player when connected to the specified Region.
--
-- /Note:/ Consider using 'latencyInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plLatencyInMilliseconds :: Lens.Lens' PlayerLatency (Core.Maybe Core.Double)
plLatencyInMilliseconds = Lens.field @"latencyInMilliseconds"
{-# DEPRECATED plLatencyInMilliseconds "Use generic-lens or generic-optics with 'latencyInMilliseconds' instead." #-}

-- | A unique identifier for a player associated with the latency data.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plPlayerId :: Lens.Lens' PlayerLatency (Core.Maybe Types.NonZeroAndMaxString)
plPlayerId = Lens.field @"playerId"
{-# DEPRECATED plPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

-- | Name of the Region that is associated with the latency value.
--
-- /Note:/ Consider using 'regionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plRegionIdentifier :: Lens.Lens' PlayerLatency (Core.Maybe Types.NonZeroAndMaxString)
plRegionIdentifier = Lens.field @"regionIdentifier"
{-# DEPRECATED plRegionIdentifier "Use generic-lens or generic-optics with 'regionIdentifier' instead." #-}

instance Core.FromJSON PlayerLatency where
  toJSON PlayerLatency {..} =
    Core.object
      ( Core.catMaybes
          [ ("LatencyInMilliseconds" Core..=) Core.<$> latencyInMilliseconds,
            ("PlayerId" Core..=) Core.<$> playerId,
            ("RegionIdentifier" Core..=) Core.<$> regionIdentifier
          ]
      )

instance Core.FromJSON PlayerLatency where
  parseJSON =
    Core.withObject "PlayerLatency" Core.$
      \x ->
        PlayerLatency'
          Core.<$> (x Core..:? "LatencyInMilliseconds")
          Core.<*> (x Core..:? "PlayerId")
          Core.<*> (x Core..:? "RegionIdentifier")

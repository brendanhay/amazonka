{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerLatency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerLatency where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Regional latency information for a player, used when requesting a new
-- game session with StartGameSessionPlacement. This value indicates the
-- amount of time lag that exists when the player is connected to a fleet
-- in the specified Region. The relative difference between a player\'s
-- latency values for multiple Regions are used to determine which fleets
-- are best suited to place a new game session for the player.
--
-- /See:/ 'newPlayerLatency' smart constructor.
data PlayerLatency = PlayerLatency'
  { -- | A unique identifier for a player associated with the latency data.
    playerId :: Prelude.Maybe Prelude.Text,
    -- | Amount of time that represents the time lag experienced by the player
    -- when connected to the specified Region.
    latencyInMilliseconds :: Prelude.Maybe Prelude.Double,
    -- | Name of the Region that is associated with the latency value.
    regionIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PlayerLatency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerId', 'playerLatency_playerId' - A unique identifier for a player associated with the latency data.
--
-- 'latencyInMilliseconds', 'playerLatency_latencyInMilliseconds' - Amount of time that represents the time lag experienced by the player
-- when connected to the specified Region.
--
-- 'regionIdentifier', 'playerLatency_regionIdentifier' - Name of the Region that is associated with the latency value.
newPlayerLatency ::
  PlayerLatency
newPlayerLatency =
  PlayerLatency'
    { playerId = Prelude.Nothing,
      latencyInMilliseconds = Prelude.Nothing,
      regionIdentifier = Prelude.Nothing
    }

-- | A unique identifier for a player associated with the latency data.
playerLatency_playerId :: Lens.Lens' PlayerLatency (Prelude.Maybe Prelude.Text)
playerLatency_playerId = Lens.lens (\PlayerLatency' {playerId} -> playerId) (\s@PlayerLatency' {} a -> s {playerId = a} :: PlayerLatency)

-- | Amount of time that represents the time lag experienced by the player
-- when connected to the specified Region.
playerLatency_latencyInMilliseconds :: Lens.Lens' PlayerLatency (Prelude.Maybe Prelude.Double)
playerLatency_latencyInMilliseconds = Lens.lens (\PlayerLatency' {latencyInMilliseconds} -> latencyInMilliseconds) (\s@PlayerLatency' {} a -> s {latencyInMilliseconds = a} :: PlayerLatency)

-- | Name of the Region that is associated with the latency value.
playerLatency_regionIdentifier :: Lens.Lens' PlayerLatency (Prelude.Maybe Prelude.Text)
playerLatency_regionIdentifier = Lens.lens (\PlayerLatency' {regionIdentifier} -> regionIdentifier) (\s@PlayerLatency' {} a -> s {regionIdentifier = a} :: PlayerLatency)

instance Prelude.FromJSON PlayerLatency where
  parseJSON =
    Prelude.withObject
      "PlayerLatency"
      ( \x ->
          PlayerLatency'
            Prelude.<$> (x Prelude..:? "PlayerId")
            Prelude.<*> (x Prelude..:? "LatencyInMilliseconds")
            Prelude.<*> (x Prelude..:? "RegionIdentifier")
      )

instance Prelude.Hashable PlayerLatency

instance Prelude.NFData PlayerLatency

instance Prelude.ToJSON PlayerLatency where
  toJSON PlayerLatency' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PlayerId" Prelude..=) Prelude.<$> playerId,
            ("LatencyInMilliseconds" Prelude..=)
              Prelude.<$> latencyInMilliseconds,
            ("RegionIdentifier" Prelude..=)
              Prelude.<$> regionIdentifier
          ]
      )

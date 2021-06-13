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
-- Module      : Network.AWS.DeviceFarm.Types.NetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.NetworkProfile where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.NetworkProfileType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An array of settings that describes characteristics of a network
-- profile.
--
-- /See:/ 'newNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    uplinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    downlinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    downlinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    downlinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100
    -- percent.
    uplinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the network profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Proportion of received packets that fail to arrive from 0 to 100
    -- percent.
    downlinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | The name of the network profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    uplinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    uplinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | The type of network profile. Valid values are listed here.
    type' :: Prelude.Maybe NetworkProfileType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uplinkJitterMs', 'networkProfile_uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'downlinkDelayMs', 'networkProfile_downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'downlinkBandwidthBits', 'networkProfile_downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'downlinkJitterMs', 'networkProfile_downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'uplinkLossPercent', 'networkProfile_uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
--
-- 'arn', 'networkProfile_arn' - The Amazon Resource Name (ARN) of the network profile.
--
-- 'downlinkLossPercent', 'networkProfile_downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100
-- percent.
--
-- 'name', 'networkProfile_name' - The name of the network profile.
--
-- 'description', 'networkProfile_description' - The description of the network profile.
--
-- 'uplinkDelayMs', 'networkProfile_uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'uplinkBandwidthBits', 'networkProfile_uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'type'', 'networkProfile_type' - The type of network profile. Valid values are listed here.
newNetworkProfile ::
  NetworkProfile
newNetworkProfile =
  NetworkProfile'
    { uplinkJitterMs = Prelude.Nothing,
      downlinkDelayMs = Prelude.Nothing,
      downlinkBandwidthBits = Prelude.Nothing,
      downlinkJitterMs = Prelude.Nothing,
      uplinkLossPercent = Prelude.Nothing,
      arn = Prelude.Nothing,
      downlinkLossPercent = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      uplinkDelayMs = Prelude.Nothing,
      uplinkBandwidthBits = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
networkProfile_uplinkJitterMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_uplinkJitterMs = Lens.lens (\NetworkProfile' {uplinkJitterMs} -> uplinkJitterMs) (\s@NetworkProfile' {} a -> s {uplinkJitterMs = a} :: NetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
networkProfile_downlinkDelayMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_downlinkDelayMs = Lens.lens (\NetworkProfile' {downlinkDelayMs} -> downlinkDelayMs) (\s@NetworkProfile' {} a -> s {downlinkDelayMs = a} :: NetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
networkProfile_downlinkBandwidthBits :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_downlinkBandwidthBits = Lens.lens (\NetworkProfile' {downlinkBandwidthBits} -> downlinkBandwidthBits) (\s@NetworkProfile' {} a -> s {downlinkBandwidthBits = a} :: NetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
networkProfile_downlinkJitterMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_downlinkJitterMs = Lens.lens (\NetworkProfile' {downlinkJitterMs} -> downlinkJitterMs) (\s@NetworkProfile' {} a -> s {downlinkJitterMs = a} :: NetworkProfile)

-- | Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
networkProfile_uplinkLossPercent :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Natural)
networkProfile_uplinkLossPercent = Lens.lens (\NetworkProfile' {uplinkLossPercent} -> uplinkLossPercent) (\s@NetworkProfile' {} a -> s {uplinkLossPercent = a} :: NetworkProfile)

-- | The Amazon Resource Name (ARN) of the network profile.
networkProfile_arn :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_arn = Lens.lens (\NetworkProfile' {arn} -> arn) (\s@NetworkProfile' {} a -> s {arn = a} :: NetworkProfile)

-- | Proportion of received packets that fail to arrive from 0 to 100
-- percent.
networkProfile_downlinkLossPercent :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Natural)
networkProfile_downlinkLossPercent = Lens.lens (\NetworkProfile' {downlinkLossPercent} -> downlinkLossPercent) (\s@NetworkProfile' {} a -> s {downlinkLossPercent = a} :: NetworkProfile)

-- | The name of the network profile.
networkProfile_name :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_name = Lens.lens (\NetworkProfile' {name} -> name) (\s@NetworkProfile' {} a -> s {name = a} :: NetworkProfile)

-- | The description of the network profile.
networkProfile_description :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_description = Lens.lens (\NetworkProfile' {description} -> description) (\s@NetworkProfile' {} a -> s {description = a} :: NetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
networkProfile_uplinkDelayMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_uplinkDelayMs = Lens.lens (\NetworkProfile' {uplinkDelayMs} -> uplinkDelayMs) (\s@NetworkProfile' {} a -> s {uplinkDelayMs = a} :: NetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
networkProfile_uplinkBandwidthBits :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_uplinkBandwidthBits = Lens.lens (\NetworkProfile' {uplinkBandwidthBits} -> uplinkBandwidthBits) (\s@NetworkProfile' {} a -> s {uplinkBandwidthBits = a} :: NetworkProfile)

-- | The type of network profile. Valid values are listed here.
networkProfile_type :: Lens.Lens' NetworkProfile (Prelude.Maybe NetworkProfileType)
networkProfile_type = Lens.lens (\NetworkProfile' {type'} -> type') (\s@NetworkProfile' {} a -> s {type' = a} :: NetworkProfile)

instance Core.FromJSON NetworkProfile where
  parseJSON =
    Core.withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            Prelude.<$> (x Core..:? "uplinkJitterMs")
            Prelude.<*> (x Core..:? "downlinkDelayMs")
            Prelude.<*> (x Core..:? "downlinkBandwidthBits")
            Prelude.<*> (x Core..:? "downlinkJitterMs")
            Prelude.<*> (x Core..:? "uplinkLossPercent")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "downlinkLossPercent")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "uplinkDelayMs")
            Prelude.<*> (x Core..:? "uplinkBandwidthBits")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable NetworkProfile

instance Prelude.NFData NetworkProfile

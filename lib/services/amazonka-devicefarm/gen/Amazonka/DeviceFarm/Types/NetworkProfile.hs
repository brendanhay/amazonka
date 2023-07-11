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
-- Module      : Amazonka.DeviceFarm.Types.NetworkProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.NetworkProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.NetworkProfileType
import qualified Amazonka.Prelude as Prelude

-- | An array of settings that describes characteristics of a network
-- profile.
--
-- /See:/ 'newNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { -- | The Amazon Resource Name (ARN) of the network profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    downlinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    downlinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    downlinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of received packets that fail to arrive from 0 to 100
    -- percent.
    downlinkLossPercent :: Prelude.Maybe Prelude.Natural,
    -- | The name of the network profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of network profile. Valid values are listed here.
    type' :: Prelude.Maybe NetworkProfileType,
    -- | The data throughput rate in bits per second, as an integer from 0 to
    -- 104857600.
    uplinkBandwidthBits :: Prelude.Maybe Prelude.Integer,
    -- | Delay time for all packets to destination in milliseconds as an integer
    -- from 0 to 2000.
    uplinkDelayMs :: Prelude.Maybe Prelude.Integer,
    -- | Time variation in the delay of received packets in milliseconds as an
    -- integer from 0 to 2000.
    uplinkJitterMs :: Prelude.Maybe Prelude.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100
    -- percent.
    uplinkLossPercent :: Prelude.Maybe Prelude.Natural
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
-- 'arn', 'networkProfile_arn' - The Amazon Resource Name (ARN) of the network profile.
--
-- 'description', 'networkProfile_description' - The description of the network profile.
--
-- 'downlinkBandwidthBits', 'networkProfile_downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'downlinkDelayMs', 'networkProfile_downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'downlinkJitterMs', 'networkProfile_downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'downlinkLossPercent', 'networkProfile_downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100
-- percent.
--
-- 'name', 'networkProfile_name' - The name of the network profile.
--
-- 'type'', 'networkProfile_type' - The type of network profile. Valid values are listed here.
--
-- 'uplinkBandwidthBits', 'networkProfile_uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
--
-- 'uplinkDelayMs', 'networkProfile_uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
--
-- 'uplinkJitterMs', 'networkProfile_uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
--
-- 'uplinkLossPercent', 'networkProfile_uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
newNetworkProfile ::
  NetworkProfile
newNetworkProfile =
  NetworkProfile'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      downlinkBandwidthBits = Prelude.Nothing,
      downlinkDelayMs = Prelude.Nothing,
      downlinkJitterMs = Prelude.Nothing,
      downlinkLossPercent = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      uplinkBandwidthBits = Prelude.Nothing,
      uplinkDelayMs = Prelude.Nothing,
      uplinkJitterMs = Prelude.Nothing,
      uplinkLossPercent = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the network profile.
networkProfile_arn :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_arn = Lens.lens (\NetworkProfile' {arn} -> arn) (\s@NetworkProfile' {} a -> s {arn = a} :: NetworkProfile)

-- | The description of the network profile.
networkProfile_description :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_description = Lens.lens (\NetworkProfile' {description} -> description) (\s@NetworkProfile' {} a -> s {description = a} :: NetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
networkProfile_downlinkBandwidthBits :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_downlinkBandwidthBits = Lens.lens (\NetworkProfile' {downlinkBandwidthBits} -> downlinkBandwidthBits) (\s@NetworkProfile' {} a -> s {downlinkBandwidthBits = a} :: NetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
networkProfile_downlinkDelayMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_downlinkDelayMs = Lens.lens (\NetworkProfile' {downlinkDelayMs} -> downlinkDelayMs) (\s@NetworkProfile' {} a -> s {downlinkDelayMs = a} :: NetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
networkProfile_downlinkJitterMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_downlinkJitterMs = Lens.lens (\NetworkProfile' {downlinkJitterMs} -> downlinkJitterMs) (\s@NetworkProfile' {} a -> s {downlinkJitterMs = a} :: NetworkProfile)

-- | Proportion of received packets that fail to arrive from 0 to 100
-- percent.
networkProfile_downlinkLossPercent :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Natural)
networkProfile_downlinkLossPercent = Lens.lens (\NetworkProfile' {downlinkLossPercent} -> downlinkLossPercent) (\s@NetworkProfile' {} a -> s {downlinkLossPercent = a} :: NetworkProfile)

-- | The name of the network profile.
networkProfile_name :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Text)
networkProfile_name = Lens.lens (\NetworkProfile' {name} -> name) (\s@NetworkProfile' {} a -> s {name = a} :: NetworkProfile)

-- | The type of network profile. Valid values are listed here.
networkProfile_type :: Lens.Lens' NetworkProfile (Prelude.Maybe NetworkProfileType)
networkProfile_type = Lens.lens (\NetworkProfile' {type'} -> type') (\s@NetworkProfile' {} a -> s {type' = a} :: NetworkProfile)

-- | The data throughput rate in bits per second, as an integer from 0 to
-- 104857600.
networkProfile_uplinkBandwidthBits :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_uplinkBandwidthBits = Lens.lens (\NetworkProfile' {uplinkBandwidthBits} -> uplinkBandwidthBits) (\s@NetworkProfile' {} a -> s {uplinkBandwidthBits = a} :: NetworkProfile)

-- | Delay time for all packets to destination in milliseconds as an integer
-- from 0 to 2000.
networkProfile_uplinkDelayMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_uplinkDelayMs = Lens.lens (\NetworkProfile' {uplinkDelayMs} -> uplinkDelayMs) (\s@NetworkProfile' {} a -> s {uplinkDelayMs = a} :: NetworkProfile)

-- | Time variation in the delay of received packets in milliseconds as an
-- integer from 0 to 2000.
networkProfile_uplinkJitterMs :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Integer)
networkProfile_uplinkJitterMs = Lens.lens (\NetworkProfile' {uplinkJitterMs} -> uplinkJitterMs) (\s@NetworkProfile' {} a -> s {uplinkJitterMs = a} :: NetworkProfile)

-- | Proportion of transmitted packets that fail to arrive from 0 to 100
-- percent.
networkProfile_uplinkLossPercent :: Lens.Lens' NetworkProfile (Prelude.Maybe Prelude.Natural)
networkProfile_uplinkLossPercent = Lens.lens (\NetworkProfile' {uplinkLossPercent} -> uplinkLossPercent) (\s@NetworkProfile' {} a -> s {uplinkLossPercent = a} :: NetworkProfile)

instance Data.FromJSON NetworkProfile where
  parseJSON =
    Data.withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "downlinkBandwidthBits")
            Prelude.<*> (x Data..:? "downlinkDelayMs")
            Prelude.<*> (x Data..:? "downlinkJitterMs")
            Prelude.<*> (x Data..:? "downlinkLossPercent")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "uplinkBandwidthBits")
            Prelude.<*> (x Data..:? "uplinkDelayMs")
            Prelude.<*> (x Data..:? "uplinkJitterMs")
            Prelude.<*> (x Data..:? "uplinkLossPercent")
      )

instance Prelude.Hashable NetworkProfile where
  hashWithSalt _salt NetworkProfile' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` downlinkBandwidthBits
      `Prelude.hashWithSalt` downlinkDelayMs
      `Prelude.hashWithSalt` downlinkJitterMs
      `Prelude.hashWithSalt` downlinkLossPercent
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` uplinkBandwidthBits
      `Prelude.hashWithSalt` uplinkDelayMs
      `Prelude.hashWithSalt` uplinkJitterMs
      `Prelude.hashWithSalt` uplinkLossPercent

instance Prelude.NFData NetworkProfile where
  rnf NetworkProfile' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf downlinkBandwidthBits
      `Prelude.seq` Prelude.rnf downlinkDelayMs
      `Prelude.seq` Prelude.rnf downlinkJitterMs
      `Prelude.seq` Prelude.rnf downlinkLossPercent
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf uplinkBandwidthBits
      `Prelude.seq` Prelude.rnf uplinkDelayMs
      `Prelude.seq` Prelude.rnf uplinkJitterMs
      `Prelude.seq` Prelude.rnf uplinkLossPercent

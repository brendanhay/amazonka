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
-- Module      : Amazonka.ComputeOptimizer.Types.VolumeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.VolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of an Amazon Elastic Block Store (Amazon
-- EBS) volume.
--
-- /See:/ 'newVolumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { -- | The baseline IOPS of the volume.
    volumeBaselineIOPS :: Prelude.Maybe Prelude.Int,
    -- | The baseline throughput of the volume.
    volumeBaselineThroughput :: Prelude.Maybe Prelude.Int,
    -- | The burst IOPS of the volume.
    volumeBurstIOPS :: Prelude.Maybe Prelude.Int,
    -- | The burst throughput of the volume.
    volumeBurstThroughput :: Prelude.Maybe Prelude.Int,
    -- | The size of the volume, in GiB.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The volume type.
    --
    -- This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for
    -- Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold
    -- HDD, or @standard@ for Magnetic volumes.
    volumeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeBaselineIOPS', 'volumeConfiguration_volumeBaselineIOPS' - The baseline IOPS of the volume.
--
-- 'volumeBaselineThroughput', 'volumeConfiguration_volumeBaselineThroughput' - The baseline throughput of the volume.
--
-- 'volumeBurstIOPS', 'volumeConfiguration_volumeBurstIOPS' - The burst IOPS of the volume.
--
-- 'volumeBurstThroughput', 'volumeConfiguration_volumeBurstThroughput' - The burst throughput of the volume.
--
-- 'volumeSize', 'volumeConfiguration_volumeSize' - The size of the volume, in GiB.
--
-- 'volumeType', 'volumeConfiguration_volumeType' - The volume type.
--
-- This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for
-- Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold
-- HDD, or @standard@ for Magnetic volumes.
newVolumeConfiguration ::
  VolumeConfiguration
newVolumeConfiguration =
  VolumeConfiguration'
    { volumeBaselineIOPS =
        Prelude.Nothing,
      volumeBaselineThroughput = Prelude.Nothing,
      volumeBurstIOPS = Prelude.Nothing,
      volumeBurstThroughput = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | The baseline IOPS of the volume.
volumeConfiguration_volumeBaselineIOPS :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_volumeBaselineIOPS = Lens.lens (\VolumeConfiguration' {volumeBaselineIOPS} -> volumeBaselineIOPS) (\s@VolumeConfiguration' {} a -> s {volumeBaselineIOPS = a} :: VolumeConfiguration)

-- | The baseline throughput of the volume.
volumeConfiguration_volumeBaselineThroughput :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_volumeBaselineThroughput = Lens.lens (\VolumeConfiguration' {volumeBaselineThroughput} -> volumeBaselineThroughput) (\s@VolumeConfiguration' {} a -> s {volumeBaselineThroughput = a} :: VolumeConfiguration)

-- | The burst IOPS of the volume.
volumeConfiguration_volumeBurstIOPS :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_volumeBurstIOPS = Lens.lens (\VolumeConfiguration' {volumeBurstIOPS} -> volumeBurstIOPS) (\s@VolumeConfiguration' {} a -> s {volumeBurstIOPS = a} :: VolumeConfiguration)

-- | The burst throughput of the volume.
volumeConfiguration_volumeBurstThroughput :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_volumeBurstThroughput = Lens.lens (\VolumeConfiguration' {volumeBurstThroughput} -> volumeBurstThroughput) (\s@VolumeConfiguration' {} a -> s {volumeBurstThroughput = a} :: VolumeConfiguration)

-- | The size of the volume, in GiB.
volumeConfiguration_volumeSize :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Int)
volumeConfiguration_volumeSize = Lens.lens (\VolumeConfiguration' {volumeSize} -> volumeSize) (\s@VolumeConfiguration' {} a -> s {volumeSize = a} :: VolumeConfiguration)

-- | The volume type.
--
-- This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for
-- Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold
-- HDD, or @standard@ for Magnetic volumes.
volumeConfiguration_volumeType :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Text)
volumeConfiguration_volumeType = Lens.lens (\VolumeConfiguration' {volumeType} -> volumeType) (\s@VolumeConfiguration' {} a -> s {volumeType = a} :: VolumeConfiguration)

instance Data.FromJSON VolumeConfiguration where
  parseJSON =
    Data.withObject
      "VolumeConfiguration"
      ( \x ->
          VolumeConfiguration'
            Prelude.<$> (x Data..:? "volumeBaselineIOPS")
            Prelude.<*> (x Data..:? "volumeBaselineThroughput")
            Prelude.<*> (x Data..:? "volumeBurstIOPS")
            Prelude.<*> (x Data..:? "volumeBurstThroughput")
            Prelude.<*> (x Data..:? "volumeSize")
            Prelude.<*> (x Data..:? "volumeType")
      )

instance Prelude.Hashable VolumeConfiguration where
  hashWithSalt _salt VolumeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` volumeBaselineIOPS
      `Prelude.hashWithSalt` volumeBaselineThroughput
      `Prelude.hashWithSalt` volumeBurstIOPS
      `Prelude.hashWithSalt` volumeBurstThroughput
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData VolumeConfiguration where
  rnf VolumeConfiguration' {..} =
    Prelude.rnf volumeBaselineIOPS
      `Prelude.seq` Prelude.rnf volumeBaselineThroughput
      `Prelude.seq` Prelude.rnf volumeBurstIOPS
      `Prelude.seq` Prelude.rnf volumeBurstThroughput
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf volumeType

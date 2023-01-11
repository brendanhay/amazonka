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
-- Module      : Amazonka.EMR.Types.VolumeSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.VolumeSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | EBS volume specifications such as volume type, IOPS, size (GiB) and
-- throughput (MiB\/s) that are requested for the EBS volume attached to an
-- EC2 instance in the cluster.
--
-- /See:/ 'newVolumeSpecification' smart constructor.
data VolumeSpecification = VolumeSpecification'
  { -- | The number of I\/O operations per second (IOPS) that the volume
    -- supports.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The throughput, in mebibyte per second (MiB\/s). This optional parameter
    -- can be a number from 125 - 1000 and is valid only for gp3 volumes.
    throughput :: Prelude.Maybe Prelude.Natural,
    -- | The volume type. Volume types supported are gp3, gp2, io1, st1, sc1, and
    -- standard.
    volumeType :: Prelude.Text,
    -- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024.
    -- If the volume type is EBS-optimized, the minimum value is 10.
    sizeInGB :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iops', 'volumeSpecification_iops' - The number of I\/O operations per second (IOPS) that the volume
-- supports.
--
-- 'throughput', 'volumeSpecification_throughput' - The throughput, in mebibyte per second (MiB\/s). This optional parameter
-- can be a number from 125 - 1000 and is valid only for gp3 volumes.
--
-- 'volumeType', 'volumeSpecification_volumeType' - The volume type. Volume types supported are gp3, gp2, io1, st1, sc1, and
-- standard.
--
-- 'sizeInGB', 'volumeSpecification_sizeInGB' - The volume size, in gibibytes (GiB). This can be a number from 1 - 1024.
-- If the volume type is EBS-optimized, the minimum value is 10.
newVolumeSpecification ::
  -- | 'volumeType'
  Prelude.Text ->
  -- | 'sizeInGB'
  Prelude.Int ->
  VolumeSpecification
newVolumeSpecification pVolumeType_ pSizeInGB_ =
  VolumeSpecification'
    { iops = Prelude.Nothing,
      throughput = Prelude.Nothing,
      volumeType = pVolumeType_,
      sizeInGB = pSizeInGB_
    }

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports.
volumeSpecification_iops :: Lens.Lens' VolumeSpecification (Prelude.Maybe Prelude.Int)
volumeSpecification_iops = Lens.lens (\VolumeSpecification' {iops} -> iops) (\s@VolumeSpecification' {} a -> s {iops = a} :: VolumeSpecification)

-- | The throughput, in mebibyte per second (MiB\/s). This optional parameter
-- can be a number from 125 - 1000 and is valid only for gp3 volumes.
volumeSpecification_throughput :: Lens.Lens' VolumeSpecification (Prelude.Maybe Prelude.Natural)
volumeSpecification_throughput = Lens.lens (\VolumeSpecification' {throughput} -> throughput) (\s@VolumeSpecification' {} a -> s {throughput = a} :: VolumeSpecification)

-- | The volume type. Volume types supported are gp3, gp2, io1, st1, sc1, and
-- standard.
volumeSpecification_volumeType :: Lens.Lens' VolumeSpecification Prelude.Text
volumeSpecification_volumeType = Lens.lens (\VolumeSpecification' {volumeType} -> volumeType) (\s@VolumeSpecification' {} a -> s {volumeType = a} :: VolumeSpecification)

-- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024.
-- If the volume type is EBS-optimized, the minimum value is 10.
volumeSpecification_sizeInGB :: Lens.Lens' VolumeSpecification Prelude.Int
volumeSpecification_sizeInGB = Lens.lens (\VolumeSpecification' {sizeInGB} -> sizeInGB) (\s@VolumeSpecification' {} a -> s {sizeInGB = a} :: VolumeSpecification)

instance Data.FromJSON VolumeSpecification where
  parseJSON =
    Data.withObject
      "VolumeSpecification"
      ( \x ->
          VolumeSpecification'
            Prelude.<$> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "Throughput")
            Prelude.<*> (x Data..: "VolumeType")
            Prelude.<*> (x Data..: "SizeInGB")
      )

instance Prelude.Hashable VolumeSpecification where
  hashWithSalt _salt VolumeSpecification' {..} =
    _salt `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` sizeInGB

instance Prelude.NFData VolumeSpecification where
  rnf VolumeSpecification' {..} =
    Prelude.rnf iops
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf sizeInGB

instance Data.ToJSON VolumeSpecification where
  toJSON VolumeSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Iops" Data..=) Prelude.<$> iops,
            ("Throughput" Data..=) Prelude.<$> throughput,
            Prelude.Just ("VolumeType" Data..= volumeType),
            Prelude.Just ("SizeInGB" Data..= sizeInGB)
          ]
      )

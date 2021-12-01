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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.VolumeSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that
-- will be requested for the EBS volume attached to an EC2 instance in the
-- cluster.
--
-- /See:/ 'newVolumeSpecification' smart constructor.
data VolumeSpecification = VolumeSpecification'
  { -- | The number of I\/O operations per second (IOPS) that the volume
    -- supports.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The volume type. Volume types supported are gp2, io1, standard.
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
-- 'volumeType', 'volumeSpecification_volumeType' - The volume type. Volume types supported are gp2, io1, standard.
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
      volumeType = pVolumeType_,
      sizeInGB = pSizeInGB_
    }

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports.
volumeSpecification_iops :: Lens.Lens' VolumeSpecification (Prelude.Maybe Prelude.Int)
volumeSpecification_iops = Lens.lens (\VolumeSpecification' {iops} -> iops) (\s@VolumeSpecification' {} a -> s {iops = a} :: VolumeSpecification)

-- | The volume type. Volume types supported are gp2, io1, standard.
volumeSpecification_volumeType :: Lens.Lens' VolumeSpecification Prelude.Text
volumeSpecification_volumeType = Lens.lens (\VolumeSpecification' {volumeType} -> volumeType) (\s@VolumeSpecification' {} a -> s {volumeType = a} :: VolumeSpecification)

-- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024.
-- If the volume type is EBS-optimized, the minimum value is 10.
volumeSpecification_sizeInGB :: Lens.Lens' VolumeSpecification Prelude.Int
volumeSpecification_sizeInGB = Lens.lens (\VolumeSpecification' {sizeInGB} -> sizeInGB) (\s@VolumeSpecification' {} a -> s {sizeInGB = a} :: VolumeSpecification)

instance Core.FromJSON VolumeSpecification where
  parseJSON =
    Core.withObject
      "VolumeSpecification"
      ( \x ->
          VolumeSpecification'
            Prelude.<$> (x Core..:? "Iops")
            Prelude.<*> (x Core..: "VolumeType")
            Prelude.<*> (x Core..: "SizeInGB")
      )

instance Prelude.Hashable VolumeSpecification where
  hashWithSalt salt' VolumeSpecification' {..} =
    salt' `Prelude.hashWithSalt` sizeInGB
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` iops

instance Prelude.NFData VolumeSpecification where
  rnf VolumeSpecification' {..} =
    Prelude.rnf iops `Prelude.seq` Prelude.rnf sizeInGB
      `Prelude.seq` Prelude.rnf volumeType

instance Core.ToJSON VolumeSpecification where
  toJSON VolumeSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Iops" Core..=) Prelude.<$> iops,
            Prelude.Just ("VolumeType" Core..= volumeType),
            Prelude.Just ("SizeInGB" Core..= sizeInGB)
          ]
      )

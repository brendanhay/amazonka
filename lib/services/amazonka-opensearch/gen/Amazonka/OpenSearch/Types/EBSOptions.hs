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
-- Module      : Amazonka.OpenSearch.Types.EBSOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.EBSOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Options to enable, disable, and specify the properties of EBS storage
-- volumes.
--
-- /See:/ 'newEBSOptions' smart constructor.
data EBSOptions = EBSOptions'
  { -- | Integer to specify the size of an EBS volume.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The IOPD for a Provisioned IOPS EBS volume (SSD).
    iops :: Prelude.Maybe Prelude.Int,
    -- | The volume type for EBS-based storage.
    volumeType :: Prelude.Maybe VolumeType,
    -- | Whether EBS-based storage is enabled.
    eBSEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EBSOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeSize', 'eBSOptions_volumeSize' - Integer to specify the size of an EBS volume.
--
-- 'iops', 'eBSOptions_iops' - The IOPD for a Provisioned IOPS EBS volume (SSD).
--
-- 'volumeType', 'eBSOptions_volumeType' - The volume type for EBS-based storage.
--
-- 'eBSEnabled', 'eBSOptions_eBSEnabled' - Whether EBS-based storage is enabled.
newEBSOptions ::
  EBSOptions
newEBSOptions =
  EBSOptions'
    { volumeSize = Prelude.Nothing,
      iops = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      eBSEnabled = Prelude.Nothing
    }

-- | Integer to specify the size of an EBS volume.
eBSOptions_volumeSize :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_volumeSize = Lens.lens (\EBSOptions' {volumeSize} -> volumeSize) (\s@EBSOptions' {} a -> s {volumeSize = a} :: EBSOptions)

-- | The IOPD for a Provisioned IOPS EBS volume (SSD).
eBSOptions_iops :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Int)
eBSOptions_iops = Lens.lens (\EBSOptions' {iops} -> iops) (\s@EBSOptions' {} a -> s {iops = a} :: EBSOptions)

-- | The volume type for EBS-based storage.
eBSOptions_volumeType :: Lens.Lens' EBSOptions (Prelude.Maybe VolumeType)
eBSOptions_volumeType = Lens.lens (\EBSOptions' {volumeType} -> volumeType) (\s@EBSOptions' {} a -> s {volumeType = a} :: EBSOptions)

-- | Whether EBS-based storage is enabled.
eBSOptions_eBSEnabled :: Lens.Lens' EBSOptions (Prelude.Maybe Prelude.Bool)
eBSOptions_eBSEnabled = Lens.lens (\EBSOptions' {eBSEnabled} -> eBSEnabled) (\s@EBSOptions' {} a -> s {eBSEnabled = a} :: EBSOptions)

instance Core.FromJSON EBSOptions where
  parseJSON =
    Core.withObject
      "EBSOptions"
      ( \x ->
          EBSOptions'
            Prelude.<$> (x Core..:? "VolumeSize")
            Prelude.<*> (x Core..:? "Iops")
            Prelude.<*> (x Core..:? "VolumeType")
            Prelude.<*> (x Core..:? "EBSEnabled")
      )

instance Prelude.Hashable EBSOptions where
  hashWithSalt salt' EBSOptions' {..} =
    salt' `Prelude.hashWithSalt` eBSEnabled
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` volumeSize

instance Prelude.NFData EBSOptions where
  rnf EBSOptions' {..} =
    Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf eBSEnabled
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf iops

instance Core.ToJSON EBSOptions where
  toJSON EBSOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VolumeSize" Core..=) Prelude.<$> volumeSize,
            ("Iops" Core..=) Prelude.<$> iops,
            ("VolumeType" Core..=) Prelude.<$> volumeType,
            ("EBSEnabled" Core..=) Prelude.<$> eBSEnabled
          ]
      )

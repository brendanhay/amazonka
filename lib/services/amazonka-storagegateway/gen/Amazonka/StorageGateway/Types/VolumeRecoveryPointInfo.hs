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
-- Module      : Amazonka.StorageGateway.Types.VolumeRecoveryPointInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.VolumeRecoveryPointInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a storage volume recovery point object.
--
-- /See:/ 'newVolumeRecoveryPointInfo' smart constructor.
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'
  { -- | The Amazon Resource Name (ARN) of the volume target.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The time the recovery point was taken.
    volumeRecoveryPointTime :: Prelude.Maybe Prelude.Text,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The size of the data stored on the volume in bytes.
    --
    -- This value is not available for volumes created prior to May 13, 2015,
    -- until you store data on the volume.
    volumeUsageInBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeRecoveryPointInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'volumeRecoveryPointInfo_volumeARN' - The Amazon Resource Name (ARN) of the volume target.
--
-- 'volumeRecoveryPointTime', 'volumeRecoveryPointInfo_volumeRecoveryPointTime' - The time the recovery point was taken.
--
-- 'volumeSizeInBytes', 'volumeRecoveryPointInfo_volumeSizeInBytes' - The size of the volume in bytes.
--
-- 'volumeUsageInBytes', 'volumeRecoveryPointInfo_volumeUsageInBytes' - The size of the data stored on the volume in bytes.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
newVolumeRecoveryPointInfo ::
  VolumeRecoveryPointInfo
newVolumeRecoveryPointInfo =
  VolumeRecoveryPointInfo'
    { volumeARN =
        Prelude.Nothing,
      volumeRecoveryPointTime = Prelude.Nothing,
      volumeSizeInBytes = Prelude.Nothing,
      volumeUsageInBytes = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume target.
volumeRecoveryPointInfo_volumeARN :: Lens.Lens' VolumeRecoveryPointInfo (Prelude.Maybe Prelude.Text)
volumeRecoveryPointInfo_volumeARN = Lens.lens (\VolumeRecoveryPointInfo' {volumeARN} -> volumeARN) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeARN = a} :: VolumeRecoveryPointInfo)

-- | The time the recovery point was taken.
volumeRecoveryPointInfo_volumeRecoveryPointTime :: Lens.Lens' VolumeRecoveryPointInfo (Prelude.Maybe Prelude.Text)
volumeRecoveryPointInfo_volumeRecoveryPointTime = Lens.lens (\VolumeRecoveryPointInfo' {volumeRecoveryPointTime} -> volumeRecoveryPointTime) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeRecoveryPointTime = a} :: VolumeRecoveryPointInfo)

-- | The size of the volume in bytes.
volumeRecoveryPointInfo_volumeSizeInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Prelude.Maybe Prelude.Integer)
volumeRecoveryPointInfo_volumeSizeInBytes = Lens.lens (\VolumeRecoveryPointInfo' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeSizeInBytes = a} :: VolumeRecoveryPointInfo)

-- | The size of the data stored on the volume in bytes.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
volumeRecoveryPointInfo_volumeUsageInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Prelude.Maybe Prelude.Integer)
volumeRecoveryPointInfo_volumeUsageInBytes = Lens.lens (\VolumeRecoveryPointInfo' {volumeUsageInBytes} -> volumeUsageInBytes) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeUsageInBytes = a} :: VolumeRecoveryPointInfo)

instance Data.FromJSON VolumeRecoveryPointInfo where
  parseJSON =
    Data.withObject
      "VolumeRecoveryPointInfo"
      ( \x ->
          VolumeRecoveryPointInfo'
            Prelude.<$> (x Data..:? "VolumeARN")
            Prelude.<*> (x Data..:? "VolumeRecoveryPointTime")
            Prelude.<*> (x Data..:? "VolumeSizeInBytes")
            Prelude.<*> (x Data..:? "VolumeUsageInBytes")
      )

instance Prelude.Hashable VolumeRecoveryPointInfo where
  hashWithSalt _salt VolumeRecoveryPointInfo' {..} =
    _salt
      `Prelude.hashWithSalt` volumeARN
      `Prelude.hashWithSalt` volumeRecoveryPointTime
      `Prelude.hashWithSalt` volumeSizeInBytes
      `Prelude.hashWithSalt` volumeUsageInBytes

instance Prelude.NFData VolumeRecoveryPointInfo where
  rnf VolumeRecoveryPointInfo' {..} =
    Prelude.rnf volumeARN `Prelude.seq`
      Prelude.rnf volumeRecoveryPointTime `Prelude.seq`
        Prelude.rnf volumeSizeInBytes `Prelude.seq`
          Prelude.rnf volumeUsageInBytes

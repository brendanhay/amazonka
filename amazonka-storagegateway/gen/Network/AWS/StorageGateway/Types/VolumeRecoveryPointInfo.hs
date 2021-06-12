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
-- Module      : Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a storage volume recovery point object.
--
-- /See:/ 'newVolumeRecoveryPointInfo' smart constructor.
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'
  { -- | The Amazon Resource Name (ARN) of the volume target.
    volumeARN :: Core.Maybe Core.Text,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The size of the data stored on the volume in bytes.
    --
    -- This value is not available for volumes created prior to May 13, 2015,
    -- until you store data on the volume.
    volumeUsageInBytes :: Core.Maybe Core.Integer,
    -- | The time the recovery point was taken.
    volumeRecoveryPointTime :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'volumeSizeInBytes', 'volumeRecoveryPointInfo_volumeSizeInBytes' - The size of the volume in bytes.
--
-- 'volumeUsageInBytes', 'volumeRecoveryPointInfo_volumeUsageInBytes' - The size of the data stored on the volume in bytes.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
--
-- 'volumeRecoveryPointTime', 'volumeRecoveryPointInfo_volumeRecoveryPointTime' - The time the recovery point was taken.
newVolumeRecoveryPointInfo ::
  VolumeRecoveryPointInfo
newVolumeRecoveryPointInfo =
  VolumeRecoveryPointInfo'
    { volumeARN = Core.Nothing,
      volumeSizeInBytes = Core.Nothing,
      volumeUsageInBytes = Core.Nothing,
      volumeRecoveryPointTime = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume target.
volumeRecoveryPointInfo_volumeARN :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Core.Text)
volumeRecoveryPointInfo_volumeARN = Lens.lens (\VolumeRecoveryPointInfo' {volumeARN} -> volumeARN) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeARN = a} :: VolumeRecoveryPointInfo)

-- | The size of the volume in bytes.
volumeRecoveryPointInfo_volumeSizeInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Core.Integer)
volumeRecoveryPointInfo_volumeSizeInBytes = Lens.lens (\VolumeRecoveryPointInfo' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeSizeInBytes = a} :: VolumeRecoveryPointInfo)

-- | The size of the data stored on the volume in bytes.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
volumeRecoveryPointInfo_volumeUsageInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Core.Integer)
volumeRecoveryPointInfo_volumeUsageInBytes = Lens.lens (\VolumeRecoveryPointInfo' {volumeUsageInBytes} -> volumeUsageInBytes) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeUsageInBytes = a} :: VolumeRecoveryPointInfo)

-- | The time the recovery point was taken.
volumeRecoveryPointInfo_volumeRecoveryPointTime :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Core.Text)
volumeRecoveryPointInfo_volumeRecoveryPointTime = Lens.lens (\VolumeRecoveryPointInfo' {volumeRecoveryPointTime} -> volumeRecoveryPointTime) (\s@VolumeRecoveryPointInfo' {} a -> s {volumeRecoveryPointTime = a} :: VolumeRecoveryPointInfo)

instance Core.FromJSON VolumeRecoveryPointInfo where
  parseJSON =
    Core.withObject
      "VolumeRecoveryPointInfo"
      ( \x ->
          VolumeRecoveryPointInfo'
            Core.<$> (x Core..:? "VolumeARN")
            Core.<*> (x Core..:? "VolumeSizeInBytes")
            Core.<*> (x Core..:? "VolumeUsageInBytes")
            Core.<*> (x Core..:? "VolumeRecoveryPointTime")
      )

instance Core.Hashable VolumeRecoveryPointInfo

instance Core.NFData VolumeRecoveryPointInfo

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo
  ( VolumeRecoveryPointInfo (..),

    -- * Smart constructor
    mkVolumeRecoveryPointInfo,

    -- * Lenses
    vrpiVolumeARN,
    vrpiVolumeRecoveryPointTime,
    vrpiVolumeSizeInBytes,
    vrpiVolumeUsageInBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.String as Types
import qualified Network.AWS.StorageGateway.Types.VolumeARN as Types

-- | Describes a storage volume recovery point object.
--
-- /See:/ 'mkVolumeRecoveryPointInfo' smart constructor.
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'
  { -- | The Amazon Resource Name (ARN) of the volume target.
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | The time the recovery point was taken.
    volumeRecoveryPointTime :: Core.Maybe Types.String,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The size of the data stored on the volume in bytes.
    volumeUsageInBytes :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeRecoveryPointInfo' value with any optional fields omitted.
mkVolumeRecoveryPointInfo ::
  VolumeRecoveryPointInfo
mkVolumeRecoveryPointInfo =
  VolumeRecoveryPointInfo'
    { volumeARN = Core.Nothing,
      volumeRecoveryPointTime = Core.Nothing,
      volumeSizeInBytes = Core.Nothing,
      volumeUsageInBytes = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume target.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeARN :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Types.VolumeARN)
vrpiVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED vrpiVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The time the recovery point was taken.
--
-- /Note:/ Consider using 'volumeRecoveryPointTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeRecoveryPointTime :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Types.String)
vrpiVolumeRecoveryPointTime = Lens.field @"volumeRecoveryPointTime"
{-# DEPRECATED vrpiVolumeRecoveryPointTime "Use generic-lens or generic-optics with 'volumeRecoveryPointTime' instead." #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeSizeInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Core.Integer)
vrpiVolumeSizeInBytes = Lens.field @"volumeSizeInBytes"
{-# DEPRECATED vrpiVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The size of the data stored on the volume in bytes.
--
-- /Note:/ Consider using 'volumeUsageInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeUsageInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Core.Maybe Core.Integer)
vrpiVolumeUsageInBytes = Lens.field @"volumeUsageInBytes"
{-# DEPRECATED vrpiVolumeUsageInBytes "Use generic-lens or generic-optics with 'volumeUsageInBytes' instead." #-}

instance Core.FromJSON VolumeRecoveryPointInfo where
  parseJSON =
    Core.withObject "VolumeRecoveryPointInfo" Core.$
      \x ->
        VolumeRecoveryPointInfo'
          Core.<$> (x Core..:? "VolumeARN")
          Core.<*> (x Core..:? "VolumeRecoveryPointTime")
          Core.<*> (x Core..:? "VolumeSizeInBytes")
          Core.<*> (x Core..:? "VolumeUsageInBytes")

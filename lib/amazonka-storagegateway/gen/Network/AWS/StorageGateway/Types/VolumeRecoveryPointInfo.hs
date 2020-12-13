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
    vrpiVolumeRecoveryPointTime,
    vrpiVolumeARN,
    vrpiVolumeSizeInBytes,
    vrpiVolumeUsageInBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a storage volume recovery point object.
--
-- /See:/ 'mkVolumeRecoveryPointInfo' smart constructor.
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'
  { -- | The time the recovery point was taken.
    volumeRecoveryPointTime :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the volume target.
    volumeARN :: Lude.Maybe Lude.Text,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Lude.Maybe Lude.Integer,
    -- | The size of the data stored on the volume in bytes.
    volumeUsageInBytes :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeRecoveryPointInfo' with the minimum fields required to make a request.
--
-- * 'volumeRecoveryPointTime' - The time the recovery point was taken.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume target.
-- * 'volumeSizeInBytes' - The size of the volume in bytes.
-- * 'volumeUsageInBytes' - The size of the data stored on the volume in bytes.
mkVolumeRecoveryPointInfo ::
  VolumeRecoveryPointInfo
mkVolumeRecoveryPointInfo =
  VolumeRecoveryPointInfo'
    { volumeRecoveryPointTime = Lude.Nothing,
      volumeARN = Lude.Nothing,
      volumeSizeInBytes = Lude.Nothing,
      volumeUsageInBytes = Lude.Nothing
    }

-- | The time the recovery point was taken.
--
-- /Note:/ Consider using 'volumeRecoveryPointTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeRecoveryPointTime :: Lens.Lens' VolumeRecoveryPointInfo (Lude.Maybe Lude.Text)
vrpiVolumeRecoveryPointTime = Lens.lens (volumeRecoveryPointTime :: VolumeRecoveryPointInfo -> Lude.Maybe Lude.Text) (\s a -> s {volumeRecoveryPointTime = a} :: VolumeRecoveryPointInfo)
{-# DEPRECATED vrpiVolumeRecoveryPointTime "Use generic-lens or generic-optics with 'volumeRecoveryPointTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume target.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeARN :: Lens.Lens' VolumeRecoveryPointInfo (Lude.Maybe Lude.Text)
vrpiVolumeARN = Lens.lens (volumeARN :: VolumeRecoveryPointInfo -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: VolumeRecoveryPointInfo)
{-# DEPRECATED vrpiVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeSizeInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Lude.Maybe Lude.Integer)
vrpiVolumeSizeInBytes = Lens.lens (volumeSizeInBytes :: VolumeRecoveryPointInfo -> Lude.Maybe Lude.Integer) (\s a -> s {volumeSizeInBytes = a} :: VolumeRecoveryPointInfo)
{-# DEPRECATED vrpiVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The size of the data stored on the volume in bytes.
--
-- /Note:/ Consider using 'volumeUsageInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrpiVolumeUsageInBytes :: Lens.Lens' VolumeRecoveryPointInfo (Lude.Maybe Lude.Integer)
vrpiVolumeUsageInBytes = Lens.lens (volumeUsageInBytes :: VolumeRecoveryPointInfo -> Lude.Maybe Lude.Integer) (\s a -> s {volumeUsageInBytes = a} :: VolumeRecoveryPointInfo)
{-# DEPRECATED vrpiVolumeUsageInBytes "Use generic-lens or generic-optics with 'volumeUsageInBytes' instead." #-}

instance Lude.FromJSON VolumeRecoveryPointInfo where
  parseJSON =
    Lude.withObject
      "VolumeRecoveryPointInfo"
      ( \x ->
          VolumeRecoveryPointInfo'
            Lude.<$> (x Lude..:? "VolumeRecoveryPointTime")
            Lude.<*> (x Lude..:? "VolumeARN")
            Lude.<*> (x Lude..:? "VolumeSizeInBytes")
            Lude.<*> (x Lude..:? "VolumeUsageInBytes")
      )

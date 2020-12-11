-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EBSVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EBSVolume
  ( EBSVolume (..),

    -- * Smart constructor
    mkEBSVolume,

    -- * Lenses
    evDevice,
    evVolumeId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | EBS block device that's attached to an EC2 instance.
--
-- /See:/ 'mkEBSVolume' smart constructor.
data EBSVolume = EBSVolume'
  { device :: Lude.Maybe Lude.Text,
    volumeId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSVolume' with the minimum fields required to make a request.
--
-- * 'device' - The device name that is exposed to the instance, such as /dev/sdh.
-- * 'volumeId' - The volume identifier of the EBS volume.
mkEBSVolume ::
  EBSVolume
mkEBSVolume =
  EBSVolume' {device = Lude.Nothing, volumeId = Lude.Nothing}

-- | The device name that is exposed to the instance, such as /dev/sdh.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evDevice :: Lens.Lens' EBSVolume (Lude.Maybe Lude.Text)
evDevice = Lens.lens (device :: EBSVolume -> Lude.Maybe Lude.Text) (\s a -> s {device = a} :: EBSVolume)
{-# DEPRECATED evDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The volume identifier of the EBS volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evVolumeId :: Lens.Lens' EBSVolume (Lude.Maybe Lude.Text)
evVolumeId = Lens.lens (volumeId :: EBSVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: EBSVolume)
{-# DEPRECATED evVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.FromJSON EBSVolume where
  parseJSON =
    Lude.withObject
      "EBSVolume"
      ( \x ->
          EBSVolume'
            Lude.<$> (x Lude..:? "Device") Lude.<*> (x Lude..:? "VolumeId")
      )

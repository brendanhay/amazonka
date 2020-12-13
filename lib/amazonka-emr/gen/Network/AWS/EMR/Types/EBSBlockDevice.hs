{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EBSBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EBSBlockDevice
  ( EBSBlockDevice (..),

    -- * Smart constructor
    mkEBSBlockDevice,

    -- * Lenses
    ebdDevice,
    ebdVolumeSpecification,
  )
where

import Network.AWS.EMR.Types.VolumeSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration of requested EBS block device associated with the instance group.
--
-- /See:/ 'mkEBSBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { -- | The device name that is exposed to the instance, such as /dev/sdh.
    device :: Lude.Maybe Lude.Text,
    -- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
    volumeSpecification :: Lude.Maybe VolumeSpecification
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- * 'device' - The device name that is exposed to the instance, such as /dev/sdh.
-- * 'volumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
mkEBSBlockDevice ::
  EBSBlockDevice
mkEBSBlockDevice =
  EBSBlockDevice'
    { device = Lude.Nothing,
      volumeSpecification = Lude.Nothing
    }

-- | The device name that is exposed to the instance, such as /dev/sdh.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdDevice :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Text)
ebdDevice = Lens.lens (device :: EBSBlockDevice -> Lude.Maybe Lude.Text) (\s a -> s {device = a} :: EBSBlockDevice)
{-# DEPRECATED ebdDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
-- /Note:/ Consider using 'volumeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeSpecification :: Lens.Lens' EBSBlockDevice (Lude.Maybe VolumeSpecification)
ebdVolumeSpecification = Lens.lens (volumeSpecification :: EBSBlockDevice -> Lude.Maybe VolumeSpecification) (\s a -> s {volumeSpecification = a} :: EBSBlockDevice)
{-# DEPRECATED ebdVolumeSpecification "Use generic-lens or generic-optics with 'volumeSpecification' instead." #-}

instance Lude.FromJSON EBSBlockDevice where
  parseJSON =
    Lude.withObject
      "EBSBlockDevice"
      ( \x ->
          EBSBlockDevice'
            Lude.<$> (x Lude..:? "Device") Lude.<*> (x Lude..:? "VolumeSpecification")
      )

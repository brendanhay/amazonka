{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EBSBlockDeviceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EBSBlockDeviceConfig
  ( EBSBlockDeviceConfig (..),

    -- * Smart constructor
    mkEBSBlockDeviceConfig,

    -- * Lenses
    ebdcVolumesPerInstance,
    ebdcVolumeSpecification,
  )
where

import Network.AWS.EMR.Types.VolumeSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration of requested EBS block device associated with the instance group with count of volumes that will be associated to every instance.
--
-- /See:/ 'mkEBSBlockDeviceConfig' smart constructor.
data EBSBlockDeviceConfig = EBSBlockDeviceConfig'
  { volumesPerInstance ::
      Lude.Maybe Lude.Int,
    volumeSpecification :: VolumeSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSBlockDeviceConfig' with the minimum fields required to make a request.
--
-- * 'volumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
-- * 'volumesPerInstance' - Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
mkEBSBlockDeviceConfig ::
  -- | 'volumeSpecification'
  VolumeSpecification ->
  EBSBlockDeviceConfig
mkEBSBlockDeviceConfig pVolumeSpecification_ =
  EBSBlockDeviceConfig'
    { volumesPerInstance = Lude.Nothing,
      volumeSpecification = pVolumeSpecification_
    }

-- | Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
--
-- /Note:/ Consider using 'volumesPerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdcVolumesPerInstance :: Lens.Lens' EBSBlockDeviceConfig (Lude.Maybe Lude.Int)
ebdcVolumesPerInstance = Lens.lens (volumesPerInstance :: EBSBlockDeviceConfig -> Lude.Maybe Lude.Int) (\s a -> s {volumesPerInstance = a} :: EBSBlockDeviceConfig)
{-# DEPRECATED ebdcVolumesPerInstance "Use generic-lens or generic-optics with 'volumesPerInstance' instead." #-}

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
-- /Note:/ Consider using 'volumeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdcVolumeSpecification :: Lens.Lens' EBSBlockDeviceConfig VolumeSpecification
ebdcVolumeSpecification = Lens.lens (volumeSpecification :: EBSBlockDeviceConfig -> VolumeSpecification) (\s a -> s {volumeSpecification = a} :: EBSBlockDeviceConfig)
{-# DEPRECATED ebdcVolumeSpecification "Use generic-lens or generic-optics with 'volumeSpecification' instead." #-}

instance Lude.ToJSON EBSBlockDeviceConfig where
  toJSON EBSBlockDeviceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VolumesPerInstance" Lude..=) Lude.<$> volumesPerInstance,
            Lude.Just ("VolumeSpecification" Lude..= volumeSpecification)
          ]
      )

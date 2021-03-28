{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EbsBlockDeviceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.EbsBlockDeviceConfig
  ( EbsBlockDeviceConfig (..)
  -- * Smart constructor
  , mkEbsBlockDeviceConfig
  -- * Lenses
  , ebdcVolumeSpecification
  , ebdcVolumesPerInstance
  ) where

import qualified Network.AWS.EMR.Types.VolumeSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration of requested EBS block device associated with the instance group with count of volumes that will be associated to every instance.
--
-- /See:/ 'mkEbsBlockDeviceConfig' smart constructor.
data EbsBlockDeviceConfig = EbsBlockDeviceConfig'
  { volumeSpecification :: Types.VolumeSpecification
    -- ^ EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
  , volumesPerInstance :: Core.Maybe Core.Int
    -- ^ Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsBlockDeviceConfig' value with any optional fields omitted.
mkEbsBlockDeviceConfig
    :: Types.VolumeSpecification -- ^ 'volumeSpecification'
    -> EbsBlockDeviceConfig
mkEbsBlockDeviceConfig volumeSpecification
  = EbsBlockDeviceConfig'{volumeSpecification,
                          volumesPerInstance = Core.Nothing}

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
-- /Note:/ Consider using 'volumeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdcVolumeSpecification :: Lens.Lens' EbsBlockDeviceConfig Types.VolumeSpecification
ebdcVolumeSpecification = Lens.field @"volumeSpecification"
{-# INLINEABLE ebdcVolumeSpecification #-}
{-# DEPRECATED volumeSpecification "Use generic-lens or generic-optics with 'volumeSpecification' instead"  #-}

-- | Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
--
-- /Note:/ Consider using 'volumesPerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdcVolumesPerInstance :: Lens.Lens' EbsBlockDeviceConfig (Core.Maybe Core.Int)
ebdcVolumesPerInstance = Lens.field @"volumesPerInstance"
{-# INLINEABLE ebdcVolumesPerInstance #-}
{-# DEPRECATED volumesPerInstance "Use generic-lens or generic-optics with 'volumesPerInstance' instead"  #-}

instance Core.FromJSON EbsBlockDeviceConfig where
        toJSON EbsBlockDeviceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VolumeSpecification" Core..= volumeSpecification),
                  ("VolumesPerInstance" Core..=) Core.<$> volumesPerInstance])

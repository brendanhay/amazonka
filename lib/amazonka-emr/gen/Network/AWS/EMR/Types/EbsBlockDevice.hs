{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.EbsBlockDevice
  ( EbsBlockDevice (..)
  -- * Smart constructor
  , mkEbsBlockDevice
  -- * Lenses
  , ebdDevice
  , ebdVolumeSpecification
  ) where

import qualified Network.AWS.EMR.Types.VolumeSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration of requested EBS block device associated with the instance group.
--
-- /See:/ 'mkEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { device :: Core.Maybe Core.Text
    -- ^ The device name that is exposed to the instance, such as /dev/sdh.
  , volumeSpecification :: Core.Maybe Types.VolumeSpecification
    -- ^ EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsBlockDevice' value with any optional fields omitted.
mkEbsBlockDevice
    :: EbsBlockDevice
mkEbsBlockDevice
  = EbsBlockDevice'{device = Core.Nothing,
                    volumeSpecification = Core.Nothing}

-- | The device name that is exposed to the instance, such as /dev/sdh.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdDevice :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Text)
ebdDevice = Lens.field @"device"
{-# INLINEABLE ebdDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
-- /Note:/ Consider using 'volumeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeSpecification :: Lens.Lens' EbsBlockDevice (Core.Maybe Types.VolumeSpecification)
ebdVolumeSpecification = Lens.field @"volumeSpecification"
{-# INLINEABLE ebdVolumeSpecification #-}
{-# DEPRECATED volumeSpecification "Use generic-lens or generic-optics with 'volumeSpecification' instead"  #-}

instance Core.FromJSON EbsBlockDevice where
        parseJSON
          = Core.withObject "EbsBlockDevice" Core.$
              \ x ->
                EbsBlockDevice' Core.<$>
                  (x Core..:? "Device") Core.<*> x Core..:? "VolumeSpecification"

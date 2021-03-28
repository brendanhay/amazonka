{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EbsVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.EbsVolume
  ( EbsVolume (..)
  -- * Smart constructor
  , mkEbsVolume
  -- * Lenses
  , evDevice
  , evVolumeId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | EBS block device that's attached to an EC2 instance.
--
-- /See:/ 'mkEbsVolume' smart constructor.
data EbsVolume = EbsVolume'
  { device :: Core.Maybe Core.Text
    -- ^ The device name that is exposed to the instance, such as /dev/sdh.
  , volumeId :: Core.Maybe Core.Text
    -- ^ The volume identifier of the EBS volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsVolume' value with any optional fields omitted.
mkEbsVolume
    :: EbsVolume
mkEbsVolume
  = EbsVolume'{device = Core.Nothing, volumeId = Core.Nothing}

-- | The device name that is exposed to the instance, such as /dev/sdh.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evDevice :: Lens.Lens' EbsVolume (Core.Maybe Core.Text)
evDevice = Lens.field @"device"
{-# INLINEABLE evDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | The volume identifier of the EBS volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evVolumeId :: Lens.Lens' EbsVolume (Core.Maybe Core.Text)
evVolumeId = Lens.field @"volumeId"
{-# INLINEABLE evVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

instance Core.FromJSON EbsVolume where
        parseJSON
          = Core.withObject "EbsVolume" Core.$
              \ x ->
                EbsVolume' Core.<$>
                  (x Core..:? "Device") Core.<*> x Core..:? "VolumeId"

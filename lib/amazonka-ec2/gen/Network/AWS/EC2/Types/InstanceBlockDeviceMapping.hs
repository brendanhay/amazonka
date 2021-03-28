{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceBlockDeviceMapping
  ( InstanceBlockDeviceMapping (..)
  -- * Smart constructor
  , mkInstanceBlockDeviceMapping
  -- * Lenses
  , ibdmDeviceName
  , ibdmEbs
  ) where

import qualified Network.AWS.EC2.Types.EbsInstanceBlockDevice as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device mapping.
--
-- /See:/ 'mkInstanceBlockDeviceMapping' smart constructor.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'
  { deviceName :: Core.Maybe Core.Text
    -- ^ The device name (for example, @/dev/sdh@ or @xvdh@ ).
  , ebs :: Core.Maybe Types.EbsInstanceBlockDevice
    -- ^ Parameters used to automatically set up EBS volumes when the instance is launched.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceBlockDeviceMapping' value with any optional fields omitted.
mkInstanceBlockDeviceMapping
    :: InstanceBlockDeviceMapping
mkInstanceBlockDeviceMapping
  = InstanceBlockDeviceMapping'{deviceName = Core.Nothing,
                                ebs = Core.Nothing}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmDeviceName :: Lens.Lens' InstanceBlockDeviceMapping (Core.Maybe Core.Text)
ibdmDeviceName = Lens.field @"deviceName"
{-# INLINEABLE ibdmDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmEbs :: Lens.Lens' InstanceBlockDeviceMapping (Core.Maybe Types.EbsInstanceBlockDevice)
ibdmEbs = Lens.field @"ebs"
{-# INLINEABLE ibdmEbs #-}
{-# DEPRECATED ebs "Use generic-lens or generic-optics with 'ebs' instead"  #-}

instance Core.FromXML InstanceBlockDeviceMapping where
        parseXML x
          = InstanceBlockDeviceMapping' Core.<$>
              (x Core..@? "deviceName") Core.<*> x Core..@? "ebs"

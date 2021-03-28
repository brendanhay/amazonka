{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
  ( LaunchTemplateBlockDeviceMapping (..)
  -- * Smart constructor
  , mkLaunchTemplateBlockDeviceMapping
  -- * Lenses
  , ltbdmDeviceName
  , ltbdmEbs
  , ltbdmNoDevice
  , ltbdmVirtualName
  ) where

import qualified Network.AWS.EC2.Types.LaunchTemplateEbsBlockDevice as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device mapping.
--
-- /See:/ 'mkLaunchTemplateBlockDeviceMapping' smart constructor.
data LaunchTemplateBlockDeviceMapping = LaunchTemplateBlockDeviceMapping'
  { deviceName :: Core.Maybe Core.Text
    -- ^ The device name.
  , ebs :: Core.Maybe Types.LaunchTemplateEbsBlockDevice
    -- ^ Information about the block device for an EBS volume.
  , noDevice :: Core.Maybe Core.Text
    -- ^ Suppresses the specified device included in the block device mapping of the AMI.
  , virtualName :: Core.Maybe Core.Text
    -- ^ The virtual device name (ephemeralN).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateBlockDeviceMapping' value with any optional fields omitted.
mkLaunchTemplateBlockDeviceMapping
    :: LaunchTemplateBlockDeviceMapping
mkLaunchTemplateBlockDeviceMapping
  = LaunchTemplateBlockDeviceMapping'{deviceName = Core.Nothing,
                                      ebs = Core.Nothing, noDevice = Core.Nothing,
                                      virtualName = Core.Nothing}

-- | The device name.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmDeviceName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe Core.Text)
ltbdmDeviceName = Lens.field @"deviceName"
{-# INLINEABLE ltbdmDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | Information about the block device for an EBS volume.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmEbs :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe Types.LaunchTemplateEbsBlockDevice)
ltbdmEbs = Lens.field @"ebs"
{-# INLINEABLE ltbdmEbs #-}
{-# DEPRECATED ebs "Use generic-lens or generic-optics with 'ebs' instead"  #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmNoDevice :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe Core.Text)
ltbdmNoDevice = Lens.field @"noDevice"
{-# INLINEABLE ltbdmNoDevice #-}
{-# DEPRECATED noDevice "Use generic-lens or generic-optics with 'noDevice' instead"  #-}

-- | The virtual device name (ephemeralN).
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltbdmVirtualName :: Lens.Lens' LaunchTemplateBlockDeviceMapping (Core.Maybe Core.Text)
ltbdmVirtualName = Lens.field @"virtualName"
{-# INLINEABLE ltbdmVirtualName #-}
{-# DEPRECATED virtualName "Use generic-lens or generic-optics with 'virtualName' instead"  #-}

instance Core.FromXML LaunchTemplateBlockDeviceMapping where
        parseXML x
          = LaunchTemplateBlockDeviceMapping' Core.<$>
              (x Core..@? "deviceName") Core.<*> x Core..@? "ebs" Core.<*>
                x Core..@? "noDevice"
                Core.<*> x Core..@? "virtualName"

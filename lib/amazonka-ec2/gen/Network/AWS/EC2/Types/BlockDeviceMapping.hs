{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.BlockDeviceMapping
  ( BlockDeviceMapping (..)
  -- * Smart constructor
  , mkBlockDeviceMapping
  -- * Lenses
  , bdmDeviceName
  , bdmEbs
  , bdmNoDevice
  , bdmVirtualName
  ) where

import qualified Network.AWS.EC2.Types.EbsBlockDevice as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device mapping.
--
-- /See:/ 'mkBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { deviceName :: Core.Text
    -- ^ The device name (for example, @/dev/sdh@ or @xvdh@ ).
  , ebs :: Core.Maybe Types.EbsBlockDevice
    -- ^ Parameters used to automatically set up EBS volumes when the instance is launched.
  , noDevice :: Core.Maybe Core.Text
    -- ^ Suppresses the specified device included in the block device mapping of the AMI.
  , virtualName :: Core.Maybe Core.Text
    -- ^ The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- NVMe instance store volumes are automatically enumerated and assigned a device name. Including them in your block device mapping has no effect.
-- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlockDeviceMapping' value with any optional fields omitted.
mkBlockDeviceMapping
    :: Core.Text -- ^ 'deviceName'
    -> BlockDeviceMapping
mkBlockDeviceMapping deviceName
  = BlockDeviceMapping'{deviceName, ebs = Core.Nothing,
                        noDevice = Core.Nothing, virtualName = Core.Nothing}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmDeviceName :: Lens.Lens' BlockDeviceMapping Core.Text
bdmDeviceName = Lens.field @"deviceName"
{-# INLINEABLE bdmDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmEbs :: Lens.Lens' BlockDeviceMapping (Core.Maybe Types.EbsBlockDevice)
bdmEbs = Lens.field @"ebs"
{-# INLINEABLE bdmEbs #-}
{-# DEPRECATED ebs "Use generic-lens or generic-optics with 'ebs' instead"  #-}

-- | Suppresses the specified device included in the block device mapping of the AMI.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmNoDevice :: Lens.Lens' BlockDeviceMapping (Core.Maybe Core.Text)
bdmNoDevice = Lens.field @"noDevice"
{-# INLINEABLE bdmNoDevice #-}
{-# DEPRECATED noDevice "Use generic-lens or generic-optics with 'noDevice' instead"  #-}

-- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- NVMe instance store volumes are automatically enumerated and assigned a device name. Including them in your block device mapping has no effect.
-- Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmVirtualName :: Lens.Lens' BlockDeviceMapping (Core.Maybe Core.Text)
bdmVirtualName = Lens.field @"virtualName"
{-# INLINEABLE bdmVirtualName #-}
{-# DEPRECATED virtualName "Use generic-lens or generic-optics with 'virtualName' instead"  #-}

instance Core.ToQuery BlockDeviceMapping where
        toQuery BlockDeviceMapping{..}
          = Core.toQueryPair "DeviceName" deviceName Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ebs") ebs
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NoDevice") noDevice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VirtualName") virtualName

instance Core.FromXML BlockDeviceMapping where
        parseXML x
          = BlockDeviceMapping' Core.<$>
              (x Core..@ "deviceName") Core.<*> x Core..@? "ebs" Core.<*>
                x Core..@? "noDevice"
                Core.<*> x Core..@? "virtualName"

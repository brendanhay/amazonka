{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.BlockDeviceMapping
  ( BlockDeviceMapping (..)
  -- * Smart constructor
  , mkBlockDeviceMapping
  -- * Lenses
  , bdmDeviceName
  , bdmEbs
  , bdmNoDevice
  , bdmVirtualName
  ) where

import qualified Network.AWS.AutoScaling.Types.DeviceName as Types
import qualified Network.AWS.AutoScaling.Types.Ebs as Types
import qualified Network.AWS.AutoScaling.Types.VirtualName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device mapping.
--
-- /See:/ 'mkBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { deviceName :: Types.DeviceName
    -- ^ The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
  , ebs :: Core.Maybe Types.Ebs
    -- ^ Parameters used to automatically set up EBS volumes when an instance is launched.
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
  , noDevice :: Core.Maybe Core.Bool
    -- ^ Setting this value to @true@ suppresses the specified device included in the block device mapping of the AMI.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches replacement instances.
-- If you specify @NoDevice@ , you cannot specify @Ebs@ .
  , virtualName :: Core.Maybe Types.VirtualName
    -- ^ The name of the virtual device (for example, @ephemeral0@ ).
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlockDeviceMapping' value with any optional fields omitted.
mkBlockDeviceMapping
    :: Types.DeviceName -- ^ 'deviceName'
    -> BlockDeviceMapping
mkBlockDeviceMapping deviceName
  = BlockDeviceMapping'{deviceName, ebs = Core.Nothing,
                        noDevice = Core.Nothing, virtualName = Core.Nothing}

-- | The device name exposed to the EC2 instance (for example, @/dev/sdh@ or @xvdh@ ). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device Naming on Linux Instances> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmDeviceName :: Lens.Lens' BlockDeviceMapping Types.DeviceName
bdmDeviceName = Lens.field @"deviceName"
{-# INLINEABLE bdmDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | Parameters used to automatically set up EBS volumes when an instance is launched.
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmEbs :: Lens.Lens' BlockDeviceMapping (Core.Maybe Types.Ebs)
bdmEbs = Lens.field @"ebs"
{-# INLINEABLE bdmEbs #-}
{-# DEPRECATED ebs "Use generic-lens or generic-optics with 'ebs' instead"  #-}

-- | Setting this value to @true@ suppresses the specified device included in the block device mapping of the AMI.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the EC2 health check. In that case, Amazon EC2 Auto Scaling launches replacement instances.
-- If you specify @NoDevice@ , you cannot specify @Ebs@ .
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmNoDevice :: Lens.Lens' BlockDeviceMapping (Core.Maybe Core.Bool)
bdmNoDevice = Lens.field @"noDevice"
{-# INLINEABLE bdmNoDevice #-}
{-# DEPRECATED noDevice "Use generic-lens or generic-optics with 'noDevice' instead"  #-}

-- | The name of the virtual device (for example, @ephemeral0@ ).
--
-- You can specify either @VirtualName@ or @Ebs@ , but not both.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmVirtualName :: Lens.Lens' BlockDeviceMapping (Core.Maybe Types.VirtualName)
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
              (x Core..@ "DeviceName") Core.<*> x Core..@? "Ebs" Core.<*>
                x Core..@? "NoDevice"
                Core.<*> x Core..@? "VirtualName"

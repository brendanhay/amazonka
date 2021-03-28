{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
  ( InstanceBlockDeviceMappingSpecification (..)
  -- * Smart constructor
  , mkInstanceBlockDeviceMappingSpecification
  -- * Lenses
  , ibdmsDeviceName
  , ibdmsEbs
  , ibdmsNoDevice
  , ibdmsVirtualName
  ) where

import qualified Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a block device mapping entry.
--
-- /See:/ 'mkInstanceBlockDeviceMappingSpecification' smart constructor.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification'
  { deviceName :: Core.Maybe Core.Text
    -- ^ The device name (for example, @/dev/sdh@ or @xvdh@ ).
  , ebs :: Core.Maybe Types.EbsInstanceBlockDeviceSpecification
    -- ^ Parameters used to automatically set up EBS volumes when the instance is launched.
  , noDevice :: Core.Maybe Core.Text
    -- ^ suppress the specified device included in the block device mapping.
  , virtualName :: Core.Maybe Core.Text
    -- ^ The virtual device name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceBlockDeviceMappingSpecification' value with any optional fields omitted.
mkInstanceBlockDeviceMappingSpecification
    :: InstanceBlockDeviceMappingSpecification
mkInstanceBlockDeviceMappingSpecification
  = InstanceBlockDeviceMappingSpecification'{deviceName =
                                               Core.Nothing,
                                             ebs = Core.Nothing, noDevice = Core.Nothing,
                                             virtualName = Core.Nothing}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsDeviceName :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Core.Maybe Core.Text)
ibdmsDeviceName = Lens.field @"deviceName"
{-# INLINEABLE ibdmsDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsEbs :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Core.Maybe Types.EbsInstanceBlockDeviceSpecification)
ibdmsEbs = Lens.field @"ebs"
{-# INLINEABLE ibdmsEbs #-}
{-# DEPRECATED ebs "Use generic-lens or generic-optics with 'ebs' instead"  #-}

-- | suppress the specified device included in the block device mapping.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsNoDevice :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Core.Maybe Core.Text)
ibdmsNoDevice = Lens.field @"noDevice"
{-# INLINEABLE ibdmsNoDevice #-}
{-# DEPRECATED noDevice "Use generic-lens or generic-optics with 'noDevice' instead"  #-}

-- | The virtual device name.
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibdmsVirtualName :: Lens.Lens' InstanceBlockDeviceMappingSpecification (Core.Maybe Core.Text)
ibdmsVirtualName = Lens.field @"virtualName"
{-# INLINEABLE ibdmsVirtualName #-}
{-# DEPRECATED virtualName "Use generic-lens or generic-optics with 'virtualName' instead"  #-}

instance Core.ToQuery InstanceBlockDeviceMappingSpecification where
        toQuery InstanceBlockDeviceMappingSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DeviceName") deviceName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Ebs") ebs
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NoDevice") noDevice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VirtualName") virtualName

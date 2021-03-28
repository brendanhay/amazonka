{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PciId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PciId
  ( PciId (..)
  -- * Smart constructor
  , mkPciId
  -- * Lenses
  , piDeviceId
  , piSubsystemId
  , piSubsystemVendorId
  , piVendorId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the data that identifies an Amazon FPGA image (AFI) on the PCI bus.
--
-- /See:/ 'mkPciId' smart constructor.
data PciId = PciId'
  { deviceId :: Core.Maybe Core.Text
    -- ^ The ID of the device.
  , subsystemId :: Core.Maybe Core.Text
    -- ^ The ID of the subsystem.
  , subsystemVendorId :: Core.Maybe Core.Text
    -- ^ The ID of the vendor for the subsystem.
  , vendorId :: Core.Maybe Core.Text
    -- ^ The ID of the vendor.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PciId' value with any optional fields omitted.
mkPciId
    :: PciId
mkPciId
  = PciId'{deviceId = Core.Nothing, subsystemId = Core.Nothing,
           subsystemVendorId = Core.Nothing, vendorId = Core.Nothing}

-- | The ID of the device.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piDeviceId :: Lens.Lens' PciId (Core.Maybe Core.Text)
piDeviceId = Lens.field @"deviceId"
{-# INLINEABLE piDeviceId #-}
{-# DEPRECATED deviceId "Use generic-lens or generic-optics with 'deviceId' instead"  #-}

-- | The ID of the subsystem.
--
-- /Note:/ Consider using 'subsystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSubsystemId :: Lens.Lens' PciId (Core.Maybe Core.Text)
piSubsystemId = Lens.field @"subsystemId"
{-# INLINEABLE piSubsystemId #-}
{-# DEPRECATED subsystemId "Use generic-lens or generic-optics with 'subsystemId' instead"  #-}

-- | The ID of the vendor for the subsystem.
--
-- /Note:/ Consider using 'subsystemVendorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSubsystemVendorId :: Lens.Lens' PciId (Core.Maybe Core.Text)
piSubsystemVendorId = Lens.field @"subsystemVendorId"
{-# INLINEABLE piSubsystemVendorId #-}
{-# DEPRECATED subsystemVendorId "Use generic-lens or generic-optics with 'subsystemVendorId' instead"  #-}

-- | The ID of the vendor.
--
-- /Note:/ Consider using 'vendorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piVendorId :: Lens.Lens' PciId (Core.Maybe Core.Text)
piVendorId = Lens.field @"vendorId"
{-# INLINEABLE piVendorId #-}
{-# DEPRECATED vendorId "Use generic-lens or generic-optics with 'vendorId' instead"  #-}

instance Core.FromXML PciId where
        parseXML x
          = PciId' Core.<$>
              (x Core..@? "DeviceId") Core.<*> x Core..@? "SubsystemId" Core.<*>
                x Core..@? "SubsystemVendorId"
                Core.<*> x Core..@? "VendorId"

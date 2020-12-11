-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PciId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PciId
  ( PciId (..),

    -- * Smart constructor
    mkPciId,

    -- * Lenses
    piSubsystemId,
    piDeviceId,
    piSubsystemVendorId,
    piVendorId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the data that identifies an Amazon FPGA image (AFI) on the PCI bus.
--
-- /See:/ 'mkPciId' smart constructor.
data PciId = PciId'
  { subsystemId :: Lude.Maybe Lude.Text,
    deviceId :: Lude.Maybe Lude.Text,
    subsystemVendorId :: Lude.Maybe Lude.Text,
    vendorId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PciId' with the minimum fields required to make a request.
--
-- * 'deviceId' - The ID of the device.
-- * 'subsystemId' - The ID of the subsystem.
-- * 'subsystemVendorId' - The ID of the vendor for the subsystem.
-- * 'vendorId' - The ID of the vendor.
mkPciId ::
  PciId
mkPciId =
  PciId'
    { subsystemId = Lude.Nothing,
      deviceId = Lude.Nothing,
      subsystemVendorId = Lude.Nothing,
      vendorId = Lude.Nothing
    }

-- | The ID of the subsystem.
--
-- /Note:/ Consider using 'subsystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSubsystemId :: Lens.Lens' PciId (Lude.Maybe Lude.Text)
piSubsystemId = Lens.lens (subsystemId :: PciId -> Lude.Maybe Lude.Text) (\s a -> s {subsystemId = a} :: PciId)
{-# DEPRECATED piSubsystemId "Use generic-lens or generic-optics with 'subsystemId' instead." #-}

-- | The ID of the device.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piDeviceId :: Lens.Lens' PciId (Lude.Maybe Lude.Text)
piDeviceId = Lens.lens (deviceId :: PciId -> Lude.Maybe Lude.Text) (\s a -> s {deviceId = a} :: PciId)
{-# DEPRECATED piDeviceId "Use generic-lens or generic-optics with 'deviceId' instead." #-}

-- | The ID of the vendor for the subsystem.
--
-- /Note:/ Consider using 'subsystemVendorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSubsystemVendorId :: Lens.Lens' PciId (Lude.Maybe Lude.Text)
piSubsystemVendorId = Lens.lens (subsystemVendorId :: PciId -> Lude.Maybe Lude.Text) (\s a -> s {subsystemVendorId = a} :: PciId)
{-# DEPRECATED piSubsystemVendorId "Use generic-lens or generic-optics with 'subsystemVendorId' instead." #-}

-- | The ID of the vendor.
--
-- /Note:/ Consider using 'vendorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piVendorId :: Lens.Lens' PciId (Lude.Maybe Lude.Text)
piVendorId = Lens.lens (vendorId :: PciId -> Lude.Maybe Lude.Text) (\s a -> s {vendorId = a} :: PciId)
{-# DEPRECATED piVendorId "Use generic-lens or generic-optics with 'vendorId' instead." #-}

instance Lude.FromXML PciId where
  parseXML x =
    PciId'
      Lude.<$> (x Lude..@? "SubsystemId")
      Lude.<*> (x Lude..@? "DeviceId")
      Lude.<*> (x Lude..@? "SubsystemVendorId")
      Lude.<*> (x Lude..@? "VendorId")

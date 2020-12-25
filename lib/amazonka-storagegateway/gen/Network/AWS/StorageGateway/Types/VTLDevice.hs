{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VTLDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VTLDevice
  ( VTLDevice (..),

    -- * Smart constructor
    mkVTLDevice,

    -- * Lenses
    vtldDeviceiSCSIAttributes,
    vtldVTLDeviceARN,
    vtldVTLDeviceProductIdentifier,
    vtldVTLDeviceType,
    vtldVTLDeviceVendor,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes as Types
import qualified Network.AWS.StorageGateway.Types.VTLDeviceARN as Types
import qualified Network.AWS.StorageGateway.Types.VTLDeviceProductIdentifier as Types
import qualified Network.AWS.StorageGateway.Types.VTLDeviceType as Types
import qualified Network.AWS.StorageGateway.Types.VTLDeviceVendor as Types

-- | Represents a device object associated with a tape gateway.
--
-- /See:/ 'mkVTLDevice' smart constructor.
data VTLDevice = VTLDevice'
  { -- | A list of iSCSI information about a VTL device.
    deviceiSCSIAttributes :: Core.Maybe Types.DeviceiSCSIAttributes,
    -- | Specifies the unique Amazon Resource Name (ARN) of the device (tape drive or media changer).
    vTLDeviceARN :: Core.Maybe Types.VTLDeviceARN,
    -- | Specifies the model number of device that the VTL device emulates.
    vTLDeviceProductIdentifier :: Core.Maybe Types.VTLDeviceProductIdentifier,
    -- | Specifies the type of device that the VTL device emulates.
    vTLDeviceType :: Core.Maybe Types.VTLDeviceType,
    -- | Specifies the vendor of the device that the VTL device object emulates.
    vTLDeviceVendor :: Core.Maybe Types.VTLDeviceVendor
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VTLDevice' value with any optional fields omitted.
mkVTLDevice ::
  VTLDevice
mkVTLDevice =
  VTLDevice'
    { deviceiSCSIAttributes = Core.Nothing,
      vTLDeviceARN = Core.Nothing,
      vTLDeviceProductIdentifier = Core.Nothing,
      vTLDeviceType = Core.Nothing,
      vTLDeviceVendor = Core.Nothing
    }

-- | A list of iSCSI information about a VTL device.
--
-- /Note:/ Consider using 'deviceiSCSIAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldDeviceiSCSIAttributes :: Lens.Lens' VTLDevice (Core.Maybe Types.DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes = Lens.field @"deviceiSCSIAttributes"
{-# DEPRECATED vtldDeviceiSCSIAttributes "Use generic-lens or generic-optics with 'deviceiSCSIAttributes' instead." #-}

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape drive or media changer).
--
-- /Note:/ Consider using 'vTLDeviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceARN :: Lens.Lens' VTLDevice (Core.Maybe Types.VTLDeviceARN)
vtldVTLDeviceARN = Lens.field @"vTLDeviceARN"
{-# DEPRECATED vtldVTLDeviceARN "Use generic-lens or generic-optics with 'vTLDeviceARN' instead." #-}

-- | Specifies the model number of device that the VTL device emulates.
--
-- /Note:/ Consider using 'vTLDeviceProductIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceProductIdentifier :: Lens.Lens' VTLDevice (Core.Maybe Types.VTLDeviceProductIdentifier)
vtldVTLDeviceProductIdentifier = Lens.field @"vTLDeviceProductIdentifier"
{-# DEPRECATED vtldVTLDeviceProductIdentifier "Use generic-lens or generic-optics with 'vTLDeviceProductIdentifier' instead." #-}

-- | Specifies the type of device that the VTL device emulates.
--
-- /Note:/ Consider using 'vTLDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceType :: Lens.Lens' VTLDevice (Core.Maybe Types.VTLDeviceType)
vtldVTLDeviceType = Lens.field @"vTLDeviceType"
{-# DEPRECATED vtldVTLDeviceType "Use generic-lens or generic-optics with 'vTLDeviceType' instead." #-}

-- | Specifies the vendor of the device that the VTL device object emulates.
--
-- /Note:/ Consider using 'vTLDeviceVendor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceVendor :: Lens.Lens' VTLDevice (Core.Maybe Types.VTLDeviceVendor)
vtldVTLDeviceVendor = Lens.field @"vTLDeviceVendor"
{-# DEPRECATED vtldVTLDeviceVendor "Use generic-lens or generic-optics with 'vTLDeviceVendor' instead." #-}

instance Core.FromJSON VTLDevice where
  parseJSON =
    Core.withObject "VTLDevice" Core.$
      \x ->
        VTLDevice'
          Core.<$> (x Core..:? "DeviceiSCSIAttributes")
          Core.<*> (x Core..:? "VTLDeviceARN")
          Core.<*> (x Core..:? "VTLDeviceProductIdentifier")
          Core.<*> (x Core..:? "VTLDeviceType")
          Core.<*> (x Core..:? "VTLDeviceVendor")

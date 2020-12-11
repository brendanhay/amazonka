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
    vtldVTLDeviceVendor,
    vtldVTLDeviceARN,
    vtldVTLDeviceType,
    vtldVTLDeviceProductIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes

-- | Represents a device object associated with a tape gateway.
--
-- /See:/ 'mkVTLDevice' smart constructor.
data VTLDevice = VTLDevice'
  { deviceiSCSIAttributes ::
      Lude.Maybe DeviceiSCSIAttributes,
    vTLDeviceVendor :: Lude.Maybe Lude.Text,
    vTLDeviceARN :: Lude.Maybe Lude.Text,
    vTLDeviceType :: Lude.Maybe Lude.Text,
    vTLDeviceProductIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VTLDevice' with the minimum fields required to make a request.
--
-- * 'deviceiSCSIAttributes' - A list of iSCSI information about a VTL device.
-- * 'vTLDeviceARN' - Specifies the unique Amazon Resource Name (ARN) of the device (tape drive or media changer).
-- * 'vTLDeviceProductIdentifier' - Specifies the model number of device that the VTL device emulates.
-- * 'vTLDeviceType' - Specifies the type of device that the VTL device emulates.
-- * 'vTLDeviceVendor' - Specifies the vendor of the device that the VTL device object emulates.
mkVTLDevice ::
  VTLDevice
mkVTLDevice =
  VTLDevice'
    { deviceiSCSIAttributes = Lude.Nothing,
      vTLDeviceVendor = Lude.Nothing,
      vTLDeviceARN = Lude.Nothing,
      vTLDeviceType = Lude.Nothing,
      vTLDeviceProductIdentifier = Lude.Nothing
    }

-- | A list of iSCSI information about a VTL device.
--
-- /Note:/ Consider using 'deviceiSCSIAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldDeviceiSCSIAttributes :: Lens.Lens' VTLDevice (Lude.Maybe DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes = Lens.lens (deviceiSCSIAttributes :: VTLDevice -> Lude.Maybe DeviceiSCSIAttributes) (\s a -> s {deviceiSCSIAttributes = a} :: VTLDevice)
{-# DEPRECATED vtldDeviceiSCSIAttributes "Use generic-lens or generic-optics with 'deviceiSCSIAttributes' instead." #-}

-- | Specifies the vendor of the device that the VTL device object emulates.
--
-- /Note:/ Consider using 'vTLDeviceVendor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceVendor :: Lens.Lens' VTLDevice (Lude.Maybe Lude.Text)
vtldVTLDeviceVendor = Lens.lens (vTLDeviceVendor :: VTLDevice -> Lude.Maybe Lude.Text) (\s a -> s {vTLDeviceVendor = a} :: VTLDevice)
{-# DEPRECATED vtldVTLDeviceVendor "Use generic-lens or generic-optics with 'vTLDeviceVendor' instead." #-}

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape drive or media changer).
--
-- /Note:/ Consider using 'vTLDeviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceARN :: Lens.Lens' VTLDevice (Lude.Maybe Lude.Text)
vtldVTLDeviceARN = Lens.lens (vTLDeviceARN :: VTLDevice -> Lude.Maybe Lude.Text) (\s a -> s {vTLDeviceARN = a} :: VTLDevice)
{-# DEPRECATED vtldVTLDeviceARN "Use generic-lens or generic-optics with 'vTLDeviceARN' instead." #-}

-- | Specifies the type of device that the VTL device emulates.
--
-- /Note:/ Consider using 'vTLDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceType :: Lens.Lens' VTLDevice (Lude.Maybe Lude.Text)
vtldVTLDeviceType = Lens.lens (vTLDeviceType :: VTLDevice -> Lude.Maybe Lude.Text) (\s a -> s {vTLDeviceType = a} :: VTLDevice)
{-# DEPRECATED vtldVTLDeviceType "Use generic-lens or generic-optics with 'vTLDeviceType' instead." #-}

-- | Specifies the model number of device that the VTL device emulates.
--
-- /Note:/ Consider using 'vTLDeviceProductIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtldVTLDeviceProductIdentifier :: Lens.Lens' VTLDevice (Lude.Maybe Lude.Text)
vtldVTLDeviceProductIdentifier = Lens.lens (vTLDeviceProductIdentifier :: VTLDevice -> Lude.Maybe Lude.Text) (\s a -> s {vTLDeviceProductIdentifier = a} :: VTLDevice)
{-# DEPRECATED vtldVTLDeviceProductIdentifier "Use generic-lens or generic-optics with 'vTLDeviceProductIdentifier' instead." #-}

instance Lude.FromJSON VTLDevice where
  parseJSON =
    Lude.withObject
      "VTLDevice"
      ( \x ->
          VTLDevice'
            Lude.<$> (x Lude..:? "DeviceiSCSIAttributes")
            Lude.<*> (x Lude..:? "VTLDeviceVendor")
            Lude.<*> (x Lude..:? "VTLDeviceARN")
            Lude.<*> (x Lude..:? "VTLDeviceType")
            Lude.<*> (x Lude..:? "VTLDeviceProductIdentifier")
      )

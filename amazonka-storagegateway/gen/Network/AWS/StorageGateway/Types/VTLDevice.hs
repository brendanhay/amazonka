{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VTLDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VTLDevice where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes

-- | Represents a device object associated with a tape gateway.
--
-- /See:/ 'newVTLDevice' smart constructor.
data VTLDevice = VTLDevice'
  { -- | Specifies the model number of device that the VTL device emulates.
    vTLDeviceProductIdentifier :: Core.Maybe Core.Text,
    -- | Specifies the vendor of the device that the VTL device object emulates.
    vTLDeviceVendor :: Core.Maybe Core.Text,
    -- | A list of iSCSI information about a VTL device.
    deviceiSCSIAttributes :: Core.Maybe DeviceiSCSIAttributes,
    -- | Specifies the type of device that the VTL device emulates.
    vTLDeviceType :: Core.Maybe Core.Text,
    -- | Specifies the unique Amazon Resource Name (ARN) of the device (tape
    -- drive or media changer).
    vTLDeviceARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VTLDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vTLDeviceProductIdentifier', 'vTLDevice_vTLDeviceProductIdentifier' - Specifies the model number of device that the VTL device emulates.
--
-- 'vTLDeviceVendor', 'vTLDevice_vTLDeviceVendor' - Specifies the vendor of the device that the VTL device object emulates.
--
-- 'deviceiSCSIAttributes', 'vTLDevice_deviceiSCSIAttributes' - A list of iSCSI information about a VTL device.
--
-- 'vTLDeviceType', 'vTLDevice_vTLDeviceType' - Specifies the type of device that the VTL device emulates.
--
-- 'vTLDeviceARN', 'vTLDevice_vTLDeviceARN' - Specifies the unique Amazon Resource Name (ARN) of the device (tape
-- drive or media changer).
newVTLDevice ::
  VTLDevice
newVTLDevice =
  VTLDevice'
    { vTLDeviceProductIdentifier =
        Core.Nothing,
      vTLDeviceVendor = Core.Nothing,
      deviceiSCSIAttributes = Core.Nothing,
      vTLDeviceType = Core.Nothing,
      vTLDeviceARN = Core.Nothing
    }

-- | Specifies the model number of device that the VTL device emulates.
vTLDevice_vTLDeviceProductIdentifier :: Lens.Lens' VTLDevice (Core.Maybe Core.Text)
vTLDevice_vTLDeviceProductIdentifier = Lens.lens (\VTLDevice' {vTLDeviceProductIdentifier} -> vTLDeviceProductIdentifier) (\s@VTLDevice' {} a -> s {vTLDeviceProductIdentifier = a} :: VTLDevice)

-- | Specifies the vendor of the device that the VTL device object emulates.
vTLDevice_vTLDeviceVendor :: Lens.Lens' VTLDevice (Core.Maybe Core.Text)
vTLDevice_vTLDeviceVendor = Lens.lens (\VTLDevice' {vTLDeviceVendor} -> vTLDeviceVendor) (\s@VTLDevice' {} a -> s {vTLDeviceVendor = a} :: VTLDevice)

-- | A list of iSCSI information about a VTL device.
vTLDevice_deviceiSCSIAttributes :: Lens.Lens' VTLDevice (Core.Maybe DeviceiSCSIAttributes)
vTLDevice_deviceiSCSIAttributes = Lens.lens (\VTLDevice' {deviceiSCSIAttributes} -> deviceiSCSIAttributes) (\s@VTLDevice' {} a -> s {deviceiSCSIAttributes = a} :: VTLDevice)

-- | Specifies the type of device that the VTL device emulates.
vTLDevice_vTLDeviceType :: Lens.Lens' VTLDevice (Core.Maybe Core.Text)
vTLDevice_vTLDeviceType = Lens.lens (\VTLDevice' {vTLDeviceType} -> vTLDeviceType) (\s@VTLDevice' {} a -> s {vTLDeviceType = a} :: VTLDevice)

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape
-- drive or media changer).
vTLDevice_vTLDeviceARN :: Lens.Lens' VTLDevice (Core.Maybe Core.Text)
vTLDevice_vTLDeviceARN = Lens.lens (\VTLDevice' {vTLDeviceARN} -> vTLDeviceARN) (\s@VTLDevice' {} a -> s {vTLDeviceARN = a} :: VTLDevice)

instance Core.FromJSON VTLDevice where
  parseJSON =
    Core.withObject
      "VTLDevice"
      ( \x ->
          VTLDevice'
            Core.<$> (x Core..:? "VTLDeviceProductIdentifier")
            Core.<*> (x Core..:? "VTLDeviceVendor")
            Core.<*> (x Core..:? "DeviceiSCSIAttributes")
            Core.<*> (x Core..:? "VTLDeviceType")
            Core.<*> (x Core..:? "VTLDeviceARN")
      )

instance Core.Hashable VTLDevice

instance Core.NFData VTLDevice

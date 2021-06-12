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
-- Module      : Network.AWS.EC2.Types.PciId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PciId where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the data that identifies an Amazon FPGA image (AFI) on the PCI
-- bus.
--
-- /See:/ 'newPciId' smart constructor.
data PciId = PciId'
  { -- | The ID of the subsystem.
    subsystemId :: Core.Maybe Core.Text,
    -- | The ID of the vendor for the subsystem.
    subsystemVendorId :: Core.Maybe Core.Text,
    -- | The ID of the device.
    deviceId :: Core.Maybe Core.Text,
    -- | The ID of the vendor.
    vendorId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PciId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subsystemId', 'pciId_subsystemId' - The ID of the subsystem.
--
-- 'subsystemVendorId', 'pciId_subsystemVendorId' - The ID of the vendor for the subsystem.
--
-- 'deviceId', 'pciId_deviceId' - The ID of the device.
--
-- 'vendorId', 'pciId_vendorId' - The ID of the vendor.
newPciId ::
  PciId
newPciId =
  PciId'
    { subsystemId = Core.Nothing,
      subsystemVendorId = Core.Nothing,
      deviceId = Core.Nothing,
      vendorId = Core.Nothing
    }

-- | The ID of the subsystem.
pciId_subsystemId :: Lens.Lens' PciId (Core.Maybe Core.Text)
pciId_subsystemId = Lens.lens (\PciId' {subsystemId} -> subsystemId) (\s@PciId' {} a -> s {subsystemId = a} :: PciId)

-- | The ID of the vendor for the subsystem.
pciId_subsystemVendorId :: Lens.Lens' PciId (Core.Maybe Core.Text)
pciId_subsystemVendorId = Lens.lens (\PciId' {subsystemVendorId} -> subsystemVendorId) (\s@PciId' {} a -> s {subsystemVendorId = a} :: PciId)

-- | The ID of the device.
pciId_deviceId :: Lens.Lens' PciId (Core.Maybe Core.Text)
pciId_deviceId = Lens.lens (\PciId' {deviceId} -> deviceId) (\s@PciId' {} a -> s {deviceId = a} :: PciId)

-- | The ID of the vendor.
pciId_vendorId :: Lens.Lens' PciId (Core.Maybe Core.Text)
pciId_vendorId = Lens.lens (\PciId' {vendorId} -> vendorId) (\s@PciId' {} a -> s {vendorId = a} :: PciId)

instance Core.FromXML PciId where
  parseXML x =
    PciId'
      Core.<$> (x Core..@? "SubsystemId")
      Core.<*> (x Core..@? "SubsystemVendorId")
      Core.<*> (x Core..@? "DeviceId")
      Core.<*> (x Core..@? "VendorId")

instance Core.Hashable PciId

instance Core.NFData PciId

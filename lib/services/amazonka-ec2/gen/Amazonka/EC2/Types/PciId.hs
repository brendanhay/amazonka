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
-- Module      : Amazonka.EC2.Types.PciId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PciId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the data that identifies an Amazon FPGA image (AFI) on the PCI
-- bus.
--
-- /See:/ 'newPciId' smart constructor.
data PciId = PciId'
  { -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subsystem.
    subsystemId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the vendor for the subsystem.
    subsystemVendorId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the vendor.
    vendorId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PciId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'pciId_deviceId' - The ID of the device.
--
-- 'subsystemId', 'pciId_subsystemId' - The ID of the subsystem.
--
-- 'subsystemVendorId', 'pciId_subsystemVendorId' - The ID of the vendor for the subsystem.
--
-- 'vendorId', 'pciId_vendorId' - The ID of the vendor.
newPciId ::
  PciId
newPciId =
  PciId'
    { deviceId = Prelude.Nothing,
      subsystemId = Prelude.Nothing,
      subsystemVendorId = Prelude.Nothing,
      vendorId = Prelude.Nothing
    }

-- | The ID of the device.
pciId_deviceId :: Lens.Lens' PciId (Prelude.Maybe Prelude.Text)
pciId_deviceId = Lens.lens (\PciId' {deviceId} -> deviceId) (\s@PciId' {} a -> s {deviceId = a} :: PciId)

-- | The ID of the subsystem.
pciId_subsystemId :: Lens.Lens' PciId (Prelude.Maybe Prelude.Text)
pciId_subsystemId = Lens.lens (\PciId' {subsystemId} -> subsystemId) (\s@PciId' {} a -> s {subsystemId = a} :: PciId)

-- | The ID of the vendor for the subsystem.
pciId_subsystemVendorId :: Lens.Lens' PciId (Prelude.Maybe Prelude.Text)
pciId_subsystemVendorId = Lens.lens (\PciId' {subsystemVendorId} -> subsystemVendorId) (\s@PciId' {} a -> s {subsystemVendorId = a} :: PciId)

-- | The ID of the vendor.
pciId_vendorId :: Lens.Lens' PciId (Prelude.Maybe Prelude.Text)
pciId_vendorId = Lens.lens (\PciId' {vendorId} -> vendorId) (\s@PciId' {} a -> s {vendorId = a} :: PciId)

instance Data.FromXML PciId where
  parseXML x =
    PciId'
      Prelude.<$> (x Data..@? "DeviceId")
      Prelude.<*> (x Data..@? "SubsystemId")
      Prelude.<*> (x Data..@? "SubsystemVendorId")
      Prelude.<*> (x Data..@? "VendorId")

instance Prelude.Hashable PciId where
  hashWithSalt _salt PciId' {..} =
    _salt
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` subsystemId
      `Prelude.hashWithSalt` subsystemVendorId
      `Prelude.hashWithSalt` vendorId

instance Prelude.NFData PciId where
  rnf PciId' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf subsystemId
      `Prelude.seq` Prelude.rnf subsystemVendorId
      `Prelude.seq` Prelude.rnf vendorId

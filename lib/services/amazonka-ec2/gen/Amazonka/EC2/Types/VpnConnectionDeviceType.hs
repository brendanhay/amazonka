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
-- Module      : Amazonka.EC2.Types.VpnConnectionDeviceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnConnectionDeviceType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | List of customer gateway devices that have a sample configuration file
-- available for use. You can also see the list of device types with sample
-- configuration files available under
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/your-cgw.html Your customer gateway device>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
--
-- /See:/ 'newVpnConnectionDeviceType' smart constructor.
data VpnConnectionDeviceType = VpnConnectionDeviceType'
  { -- | Customer gateway device software version.
    software :: Prelude.Maybe Prelude.Text,
    -- | Customer gateway device platform.
    platform :: Prelude.Maybe Prelude.Text,
    -- | Customer gateway device identifier.
    vpnConnectionDeviceTypeId :: Prelude.Maybe Prelude.Text,
    -- | Customer gateway device vendor.
    vendor :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnConnectionDeviceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'software', 'vpnConnectionDeviceType_software' - Customer gateway device software version.
--
-- 'platform', 'vpnConnectionDeviceType_platform' - Customer gateway device platform.
--
-- 'vpnConnectionDeviceTypeId', 'vpnConnectionDeviceType_vpnConnectionDeviceTypeId' - Customer gateway device identifier.
--
-- 'vendor', 'vpnConnectionDeviceType_vendor' - Customer gateway device vendor.
newVpnConnectionDeviceType ::
  VpnConnectionDeviceType
newVpnConnectionDeviceType =
  VpnConnectionDeviceType'
    { software =
        Prelude.Nothing,
      platform = Prelude.Nothing,
      vpnConnectionDeviceTypeId = Prelude.Nothing,
      vendor = Prelude.Nothing
    }

-- | Customer gateway device software version.
vpnConnectionDeviceType_software :: Lens.Lens' VpnConnectionDeviceType (Prelude.Maybe Prelude.Text)
vpnConnectionDeviceType_software = Lens.lens (\VpnConnectionDeviceType' {software} -> software) (\s@VpnConnectionDeviceType' {} a -> s {software = a} :: VpnConnectionDeviceType)

-- | Customer gateway device platform.
vpnConnectionDeviceType_platform :: Lens.Lens' VpnConnectionDeviceType (Prelude.Maybe Prelude.Text)
vpnConnectionDeviceType_platform = Lens.lens (\VpnConnectionDeviceType' {platform} -> platform) (\s@VpnConnectionDeviceType' {} a -> s {platform = a} :: VpnConnectionDeviceType)

-- | Customer gateway device identifier.
vpnConnectionDeviceType_vpnConnectionDeviceTypeId :: Lens.Lens' VpnConnectionDeviceType (Prelude.Maybe Prelude.Text)
vpnConnectionDeviceType_vpnConnectionDeviceTypeId = Lens.lens (\VpnConnectionDeviceType' {vpnConnectionDeviceTypeId} -> vpnConnectionDeviceTypeId) (\s@VpnConnectionDeviceType' {} a -> s {vpnConnectionDeviceTypeId = a} :: VpnConnectionDeviceType)

-- | Customer gateway device vendor.
vpnConnectionDeviceType_vendor :: Lens.Lens' VpnConnectionDeviceType (Prelude.Maybe Prelude.Text)
vpnConnectionDeviceType_vendor = Lens.lens (\VpnConnectionDeviceType' {vendor} -> vendor) (\s@VpnConnectionDeviceType' {} a -> s {vendor = a} :: VpnConnectionDeviceType)

instance Core.FromXML VpnConnectionDeviceType where
  parseXML x =
    VpnConnectionDeviceType'
      Prelude.<$> (x Core..@? "software")
      Prelude.<*> (x Core..@? "platform")
      Prelude.<*> (x Core..@? "vpnConnectionDeviceTypeId")
      Prelude.<*> (x Core..@? "vendor")

instance Prelude.Hashable VpnConnectionDeviceType where
  hashWithSalt _salt VpnConnectionDeviceType' {..} =
    _salt `Prelude.hashWithSalt` software
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` vpnConnectionDeviceTypeId
      `Prelude.hashWithSalt` vendor

instance Prelude.NFData VpnConnectionDeviceType where
  rnf VpnConnectionDeviceType' {..} =
    Prelude.rnf software
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf vpnConnectionDeviceTypeId
      `Prelude.seq` Prelude.rnf vendor

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
-- Module      : Amazonka.DirectConnect.Types.NewTransitVirtualInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.NewTransitVirtualInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.AddressFamily
import Amazonka.DirectConnect.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a transit virtual interface.
--
-- /See:/ 'newNewTransitVirtualInterface' smart constructor.
data NewTransitVirtualInterface = NewTransitVirtualInterface'
  { -- | The address family for the BGP peer.
    addressFamily :: Prelude.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Prelude.Maybe Prelude.Int,
    -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to enable or disable SiteLink.
    enableSiteLink :: Prelude.Maybe Prelude.Bool,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 8500. The default value is 1500.
    mtu :: Prelude.Maybe Prelude.Int,
    -- | The tags associated with the transitive virtual interface.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the virtual interface assigned by the customer network. The
    -- name has a maximum of 100 characters. The following are valid
    -- characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VLAN.
    vlan :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewTransitVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressFamily', 'newTransitVirtualInterface_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newTransitVirtualInterface_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'asn', 'newTransitVirtualInterface_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
--
-- 'authKey', 'newTransitVirtualInterface_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'customerAddress', 'newTransitVirtualInterface_customerAddress' - The IP address assigned to the customer interface.
--
-- 'directConnectGatewayId', 'newTransitVirtualInterface_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'enableSiteLink', 'newTransitVirtualInterface_enableSiteLink' - Indicates whether to enable or disable SiteLink.
--
-- 'mtu', 'newTransitVirtualInterface_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 8500. The default value is 1500.
--
-- 'tags', 'newTransitVirtualInterface_tags' - The tags associated with the transitive virtual interface.
--
-- 'virtualInterfaceName', 'newTransitVirtualInterface_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'vlan', 'newTransitVirtualInterface_vlan' - The ID of the VLAN.
newNewTransitVirtualInterface ::
  NewTransitVirtualInterface
newNewTransitVirtualInterface =
  NewTransitVirtualInterface'
    { addressFamily =
        Prelude.Nothing,
      amazonAddress = Prelude.Nothing,
      asn = Prelude.Nothing,
      authKey = Prelude.Nothing,
      customerAddress = Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      enableSiteLink = Prelude.Nothing,
      mtu = Prelude.Nothing,
      tags = Prelude.Nothing,
      virtualInterfaceName = Prelude.Nothing,
      vlan = Prelude.Nothing
    }

-- | The address family for the BGP peer.
newTransitVirtualInterface_addressFamily :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe AddressFamily)
newTransitVirtualInterface_addressFamily = Lens.lens (\NewTransitVirtualInterface' {addressFamily} -> addressFamily) (\s@NewTransitVirtualInterface' {} a -> s {addressFamily = a} :: NewTransitVirtualInterface)

-- | The IP address assigned to the Amazon interface.
newTransitVirtualInterface_amazonAddress :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_amazonAddress = Lens.lens (\NewTransitVirtualInterface' {amazonAddress} -> amazonAddress) (\s@NewTransitVirtualInterface' {} a -> s {amazonAddress = a} :: NewTransitVirtualInterface)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newTransitVirtualInterface_asn :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Int)
newTransitVirtualInterface_asn = Lens.lens (\NewTransitVirtualInterface' {asn} -> asn) (\s@NewTransitVirtualInterface' {} a -> s {asn = a} :: NewTransitVirtualInterface)

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newTransitVirtualInterface_authKey :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_authKey = Lens.lens (\NewTransitVirtualInterface' {authKey} -> authKey) (\s@NewTransitVirtualInterface' {} a -> s {authKey = a} :: NewTransitVirtualInterface)

-- | The IP address assigned to the customer interface.
newTransitVirtualInterface_customerAddress :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_customerAddress = Lens.lens (\NewTransitVirtualInterface' {customerAddress} -> customerAddress) (\s@NewTransitVirtualInterface' {} a -> s {customerAddress = a} :: NewTransitVirtualInterface)

-- | The ID of the Direct Connect gateway.
newTransitVirtualInterface_directConnectGatewayId :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_directConnectGatewayId = Lens.lens (\NewTransitVirtualInterface' {directConnectGatewayId} -> directConnectGatewayId) (\s@NewTransitVirtualInterface' {} a -> s {directConnectGatewayId = a} :: NewTransitVirtualInterface)

-- | Indicates whether to enable or disable SiteLink.
newTransitVirtualInterface_enableSiteLink :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Bool)
newTransitVirtualInterface_enableSiteLink = Lens.lens (\NewTransitVirtualInterface' {enableSiteLink} -> enableSiteLink) (\s@NewTransitVirtualInterface' {} a -> s {enableSiteLink = a} :: NewTransitVirtualInterface)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 8500. The default value is 1500.
newTransitVirtualInterface_mtu :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Int)
newTransitVirtualInterface_mtu = Lens.lens (\NewTransitVirtualInterface' {mtu} -> mtu) (\s@NewTransitVirtualInterface' {} a -> s {mtu = a} :: NewTransitVirtualInterface)

-- | The tags associated with the transitive virtual interface.
newTransitVirtualInterface_tags :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe (Prelude.NonEmpty Tag))
newTransitVirtualInterface_tags = Lens.lens (\NewTransitVirtualInterface' {tags} -> tags) (\s@NewTransitVirtualInterface' {} a -> s {tags = a} :: NewTransitVirtualInterface) Prelude.. Lens.mapping Lens.coerced

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newTransitVirtualInterface_virtualInterfaceName :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_virtualInterfaceName = Lens.lens (\NewTransitVirtualInterface' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewTransitVirtualInterface' {} a -> s {virtualInterfaceName = a} :: NewTransitVirtualInterface)

-- | The ID of the VLAN.
newTransitVirtualInterface_vlan :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Int)
newTransitVirtualInterface_vlan = Lens.lens (\NewTransitVirtualInterface' {vlan} -> vlan) (\s@NewTransitVirtualInterface' {} a -> s {vlan = a} :: NewTransitVirtualInterface)

instance Prelude.Hashable NewTransitVirtualInterface where
  hashWithSalt _salt NewTransitVirtualInterface' {..} =
    _salt
      `Prelude.hashWithSalt` addressFamily
      `Prelude.hashWithSalt` amazonAddress
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` authKey
      `Prelude.hashWithSalt` customerAddress
      `Prelude.hashWithSalt` directConnectGatewayId
      `Prelude.hashWithSalt` enableSiteLink
      `Prelude.hashWithSalt` mtu
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` virtualInterfaceName
      `Prelude.hashWithSalt` vlan

instance Prelude.NFData NewTransitVirtualInterface where
  rnf NewTransitVirtualInterface' {..} =
    Prelude.rnf addressFamily
      `Prelude.seq` Prelude.rnf amazonAddress
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf authKey
      `Prelude.seq` Prelude.rnf customerAddress
      `Prelude.seq` Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf enableSiteLink
      `Prelude.seq` Prelude.rnf mtu
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf virtualInterfaceName
      `Prelude.seq` Prelude.rnf vlan

instance Data.ToJSON NewTransitVirtualInterface where
  toJSON NewTransitVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addressFamily" Data..=) Prelude.<$> addressFamily,
            ("amazonAddress" Data..=) Prelude.<$> amazonAddress,
            ("asn" Data..=) Prelude.<$> asn,
            ("authKey" Data..=) Prelude.<$> authKey,
            ("customerAddress" Data..=)
              Prelude.<$> customerAddress,
            ("directConnectGatewayId" Data..=)
              Prelude.<$> directConnectGatewayId,
            ("enableSiteLink" Data..=)
              Prelude.<$> enableSiteLink,
            ("mtu" Data..=) Prelude.<$> mtu,
            ("tags" Data..=) Prelude.<$> tags,
            ("virtualInterfaceName" Data..=)
              Prelude.<$> virtualInterfaceName,
            ("vlan" Data..=) Prelude.<$> vlan
          ]
      )

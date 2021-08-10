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
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a transit virtual interface.
--
-- /See:/ 'newNewTransitVirtualInterface' smart constructor.
data NewTransitVirtualInterface = NewTransitVirtualInterface'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Prelude.Maybe Prelude.Int,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Prelude.Maybe Prelude.Int,
    -- | The tags associated with the transitive virtual interface.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The name of the virtual interface assigned by the customer network. The
    -- name has a maximum of 100 characters. The following are valid
    -- characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Prelude.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VLAN.
    vlan :: Prelude.Maybe Prelude.Int,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text
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
-- 'authKey', 'newTransitVirtualInterface_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'asn', 'newTransitVirtualInterface_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
--
-- 'mtu', 'newTransitVirtualInterface_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
--
-- 'tags', 'newTransitVirtualInterface_tags' - The tags associated with the transitive virtual interface.
--
-- 'directConnectGatewayId', 'newTransitVirtualInterface_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'virtualInterfaceName', 'newTransitVirtualInterface_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'addressFamily', 'newTransitVirtualInterface_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newTransitVirtualInterface_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'vlan', 'newTransitVirtualInterface_vlan' - The ID of the VLAN.
--
-- 'customerAddress', 'newTransitVirtualInterface_customerAddress' - The IP address assigned to the customer interface.
newNewTransitVirtualInterface ::
  NewTransitVirtualInterface
newNewTransitVirtualInterface =
  NewTransitVirtualInterface'
    { authKey =
        Prelude.Nothing,
      asn = Prelude.Nothing,
      mtu = Prelude.Nothing,
      tags = Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      virtualInterfaceName = Prelude.Nothing,
      addressFamily = Prelude.Nothing,
      amazonAddress = Prelude.Nothing,
      vlan = Prelude.Nothing,
      customerAddress = Prelude.Nothing
    }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newTransitVirtualInterface_authKey :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_authKey = Lens.lens (\NewTransitVirtualInterface' {authKey} -> authKey) (\s@NewTransitVirtualInterface' {} a -> s {authKey = a} :: NewTransitVirtualInterface)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newTransitVirtualInterface_asn :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Int)
newTransitVirtualInterface_asn = Lens.lens (\NewTransitVirtualInterface' {asn} -> asn) (\s@NewTransitVirtualInterface' {} a -> s {asn = a} :: NewTransitVirtualInterface)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
newTransitVirtualInterface_mtu :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Int)
newTransitVirtualInterface_mtu = Lens.lens (\NewTransitVirtualInterface' {mtu} -> mtu) (\s@NewTransitVirtualInterface' {} a -> s {mtu = a} :: NewTransitVirtualInterface)

-- | The tags associated with the transitive virtual interface.
newTransitVirtualInterface_tags :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe (Prelude.NonEmpty Tag))
newTransitVirtualInterface_tags = Lens.lens (\NewTransitVirtualInterface' {tags} -> tags) (\s@NewTransitVirtualInterface' {} a -> s {tags = a} :: NewTransitVirtualInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the Direct Connect gateway.
newTransitVirtualInterface_directConnectGatewayId :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_directConnectGatewayId = Lens.lens (\NewTransitVirtualInterface' {directConnectGatewayId} -> directConnectGatewayId) (\s@NewTransitVirtualInterface' {} a -> s {directConnectGatewayId = a} :: NewTransitVirtualInterface)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newTransitVirtualInterface_virtualInterfaceName :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_virtualInterfaceName = Lens.lens (\NewTransitVirtualInterface' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewTransitVirtualInterface' {} a -> s {virtualInterfaceName = a} :: NewTransitVirtualInterface)

-- | The address family for the BGP peer.
newTransitVirtualInterface_addressFamily :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe AddressFamily)
newTransitVirtualInterface_addressFamily = Lens.lens (\NewTransitVirtualInterface' {addressFamily} -> addressFamily) (\s@NewTransitVirtualInterface' {} a -> s {addressFamily = a} :: NewTransitVirtualInterface)

-- | The IP address assigned to the Amazon interface.
newTransitVirtualInterface_amazonAddress :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_amazonAddress = Lens.lens (\NewTransitVirtualInterface' {amazonAddress} -> amazonAddress) (\s@NewTransitVirtualInterface' {} a -> s {amazonAddress = a} :: NewTransitVirtualInterface)

-- | The ID of the VLAN.
newTransitVirtualInterface_vlan :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Int)
newTransitVirtualInterface_vlan = Lens.lens (\NewTransitVirtualInterface' {vlan} -> vlan) (\s@NewTransitVirtualInterface' {} a -> s {vlan = a} :: NewTransitVirtualInterface)

-- | The IP address assigned to the customer interface.
newTransitVirtualInterface_customerAddress :: Lens.Lens' NewTransitVirtualInterface (Prelude.Maybe Prelude.Text)
newTransitVirtualInterface_customerAddress = Lens.lens (\NewTransitVirtualInterface' {customerAddress} -> customerAddress) (\s@NewTransitVirtualInterface' {} a -> s {customerAddress = a} :: NewTransitVirtualInterface)

instance Prelude.Hashable NewTransitVirtualInterface

instance Prelude.NFData NewTransitVirtualInterface

instance Core.ToJSON NewTransitVirtualInterface where
  toJSON NewTransitVirtualInterface' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("authKey" Core..=) Prelude.<$> authKey,
            ("asn" Core..=) Prelude.<$> asn,
            ("mtu" Core..=) Prelude.<$> mtu,
            ("tags" Core..=) Prelude.<$> tags,
            ("directConnectGatewayId" Core..=)
              Prelude.<$> directConnectGatewayId,
            ("virtualInterfaceName" Core..=)
              Prelude.<$> virtualInterfaceName,
            ("addressFamily" Core..=) Prelude.<$> addressFamily,
            ("amazonAddress" Core..=) Prelude.<$> amazonAddress,
            ("vlan" Core..=) Prelude.<$> vlan,
            ("customerAddress" Core..=)
              Prelude.<$> customerAddress
          ]
      )

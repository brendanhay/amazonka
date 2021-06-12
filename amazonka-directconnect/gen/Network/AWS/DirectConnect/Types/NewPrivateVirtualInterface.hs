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
-- Module      : Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Information about a private virtual interface.
--
-- /See:/ 'newNewPrivateVirtualInterface' smart constructor.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Core.Text,
    -- | The ID of the virtual private gateway.
    virtualGatewayId :: Core.Maybe Core.Text,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int,
    -- | The tags associated with the private virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Core.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Core.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Core.Text,
    -- | The name of the virtual interface assigned by the customer network. The
    -- name has a maximum of 100 characters. The following are valid
    -- characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Core.Text,
    -- | The ID of the VLAN.
    vlan :: Core.Int,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NewPrivateVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'newPrivateVirtualInterface_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'virtualGatewayId', 'newPrivateVirtualInterface_virtualGatewayId' - The ID of the virtual private gateway.
--
-- 'mtu', 'newPrivateVirtualInterface_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
--
-- 'tags', 'newPrivateVirtualInterface_tags' - The tags associated with the private virtual interface.
--
-- 'directConnectGatewayId', 'newPrivateVirtualInterface_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'addressFamily', 'newPrivateVirtualInterface_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newPrivateVirtualInterface_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'customerAddress', 'newPrivateVirtualInterface_customerAddress' - The IP address assigned to the customer interface.
--
-- 'virtualInterfaceName', 'newPrivateVirtualInterface_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'vlan', 'newPrivateVirtualInterface_vlan' - The ID of the VLAN.
--
-- 'asn', 'newPrivateVirtualInterface_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newNewPrivateVirtualInterface ::
  -- | 'virtualInterfaceName'
  Core.Text ->
  -- | 'vlan'
  Core.Int ->
  -- | 'asn'
  Core.Int ->
  NewPrivateVirtualInterface
newNewPrivateVirtualInterface
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPrivateVirtualInterface'
      { authKey = Core.Nothing,
        virtualGatewayId = Core.Nothing,
        mtu = Core.Nothing,
        tags = Core.Nothing,
        directConnectGatewayId = Core.Nothing,
        addressFamily = Core.Nothing,
        amazonAddress = Core.Nothing,
        customerAddress = Core.Nothing,
        virtualInterfaceName = pVirtualInterfaceName_,
        vlan = pVlan_,
        asn = pAsn_
      }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newPrivateVirtualInterface_authKey :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Core.Text)
newPrivateVirtualInterface_authKey = Lens.lens (\NewPrivateVirtualInterface' {authKey} -> authKey) (\s@NewPrivateVirtualInterface' {} a -> s {authKey = a} :: NewPrivateVirtualInterface)

-- | The ID of the virtual private gateway.
newPrivateVirtualInterface_virtualGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Core.Text)
newPrivateVirtualInterface_virtualGatewayId = Lens.lens (\NewPrivateVirtualInterface' {virtualGatewayId} -> virtualGatewayId) (\s@NewPrivateVirtualInterface' {} a -> s {virtualGatewayId = a} :: NewPrivateVirtualInterface)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
newPrivateVirtualInterface_mtu :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Core.Int)
newPrivateVirtualInterface_mtu = Lens.lens (\NewPrivateVirtualInterface' {mtu} -> mtu) (\s@NewPrivateVirtualInterface' {} a -> s {mtu = a} :: NewPrivateVirtualInterface)

-- | The tags associated with the private virtual interface.
newPrivateVirtualInterface_tags :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe (Core.NonEmpty Tag))
newPrivateVirtualInterface_tags = Lens.lens (\NewPrivateVirtualInterface' {tags} -> tags) (\s@NewPrivateVirtualInterface' {} a -> s {tags = a} :: NewPrivateVirtualInterface) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Direct Connect gateway.
newPrivateVirtualInterface_directConnectGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Core.Text)
newPrivateVirtualInterface_directConnectGatewayId = Lens.lens (\NewPrivateVirtualInterface' {directConnectGatewayId} -> directConnectGatewayId) (\s@NewPrivateVirtualInterface' {} a -> s {directConnectGatewayId = a} :: NewPrivateVirtualInterface)

-- | The address family for the BGP peer.
newPrivateVirtualInterface_addressFamily :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe AddressFamily)
newPrivateVirtualInterface_addressFamily = Lens.lens (\NewPrivateVirtualInterface' {addressFamily} -> addressFamily) (\s@NewPrivateVirtualInterface' {} a -> s {addressFamily = a} :: NewPrivateVirtualInterface)

-- | The IP address assigned to the Amazon interface.
newPrivateVirtualInterface_amazonAddress :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Core.Text)
newPrivateVirtualInterface_amazonAddress = Lens.lens (\NewPrivateVirtualInterface' {amazonAddress} -> amazonAddress) (\s@NewPrivateVirtualInterface' {} a -> s {amazonAddress = a} :: NewPrivateVirtualInterface)

-- | The IP address assigned to the customer interface.
newPrivateVirtualInterface_customerAddress :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Core.Text)
newPrivateVirtualInterface_customerAddress = Lens.lens (\NewPrivateVirtualInterface' {customerAddress} -> customerAddress) (\s@NewPrivateVirtualInterface' {} a -> s {customerAddress = a} :: NewPrivateVirtualInterface)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newPrivateVirtualInterface_virtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterface Core.Text
newPrivateVirtualInterface_virtualInterfaceName = Lens.lens (\NewPrivateVirtualInterface' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewPrivateVirtualInterface' {} a -> s {virtualInterfaceName = a} :: NewPrivateVirtualInterface)

-- | The ID of the VLAN.
newPrivateVirtualInterface_vlan :: Lens.Lens' NewPrivateVirtualInterface Core.Int
newPrivateVirtualInterface_vlan = Lens.lens (\NewPrivateVirtualInterface' {vlan} -> vlan) (\s@NewPrivateVirtualInterface' {} a -> s {vlan = a} :: NewPrivateVirtualInterface)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newPrivateVirtualInterface_asn :: Lens.Lens' NewPrivateVirtualInterface Core.Int
newPrivateVirtualInterface_asn = Lens.lens (\NewPrivateVirtualInterface' {asn} -> asn) (\s@NewPrivateVirtualInterface' {} a -> s {asn = a} :: NewPrivateVirtualInterface)

instance Core.Hashable NewPrivateVirtualInterface

instance Core.NFData NewPrivateVirtualInterface

instance Core.ToJSON NewPrivateVirtualInterface where
  toJSON NewPrivateVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ ("authKey" Core..=) Core.<$> authKey,
            ("virtualGatewayId" Core..=)
              Core.<$> virtualGatewayId,
            ("mtu" Core..=) Core.<$> mtu,
            ("tags" Core..=) Core.<$> tags,
            ("directConnectGatewayId" Core..=)
              Core.<$> directConnectGatewayId,
            ("addressFamily" Core..=) Core.<$> addressFamily,
            ("amazonAddress" Core..=) Core.<$> amazonAddress,
            ("customerAddress" Core..=) Core.<$> customerAddress,
            Core.Just
              ( "virtualInterfaceName"
                  Core..= virtualInterfaceName
              ),
            Core.Just ("vlan" Core..= vlan),
            Core.Just ("asn" Core..= asn)
          ]
      )

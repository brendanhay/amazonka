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
-- Module      : Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Information about a private virtual interface to be provisioned on a
-- connection.
--
-- /See:/ 'newNewPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Core.Text,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int,
    -- | The tags associated with the private virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Tag),
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
-- Create a value of 'NewPrivateVirtualInterfaceAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'newPrivateVirtualInterfaceAllocation_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'mtu', 'newPrivateVirtualInterfaceAllocation_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
--
-- 'tags', 'newPrivateVirtualInterfaceAllocation_tags' - The tags associated with the private virtual interface.
--
-- 'addressFamily', 'newPrivateVirtualInterfaceAllocation_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newPrivateVirtualInterfaceAllocation_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'customerAddress', 'newPrivateVirtualInterfaceAllocation_customerAddress' - The IP address assigned to the customer interface.
--
-- 'virtualInterfaceName', 'newPrivateVirtualInterfaceAllocation_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'vlan', 'newPrivateVirtualInterfaceAllocation_vlan' - The ID of the VLAN.
--
-- 'asn', 'newPrivateVirtualInterfaceAllocation_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newNewPrivateVirtualInterfaceAllocation ::
  -- | 'virtualInterfaceName'
  Core.Text ->
  -- | 'vlan'
  Core.Int ->
  -- | 'asn'
  Core.Int ->
  NewPrivateVirtualInterfaceAllocation
newNewPrivateVirtualInterfaceAllocation
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPrivateVirtualInterfaceAllocation'
      { authKey =
          Core.Nothing,
        mtu = Core.Nothing,
        tags = Core.Nothing,
        addressFamily = Core.Nothing,
        amazonAddress = Core.Nothing,
        customerAddress = Core.Nothing,
        virtualInterfaceName =
          pVirtualInterfaceName_,
        vlan = pVlan_,
        asn = pAsn_
      }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newPrivateVirtualInterfaceAllocation_authKey :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Core.Text)
newPrivateVirtualInterfaceAllocation_authKey = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {authKey} -> authKey) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {authKey = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
newPrivateVirtualInterfaceAllocation_mtu :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Core.Int)
newPrivateVirtualInterfaceAllocation_mtu = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {mtu} -> mtu) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {mtu = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The tags associated with the private virtual interface.
newPrivateVirtualInterfaceAllocation_tags :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe (Core.NonEmpty Tag))
newPrivateVirtualInterfaceAllocation_tags = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {tags} -> tags) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {tags = a} :: NewPrivateVirtualInterfaceAllocation) Core.. Lens.mapping Lens._Coerce

-- | The address family for the BGP peer.
newPrivateVirtualInterfaceAllocation_addressFamily :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe AddressFamily)
newPrivateVirtualInterfaceAllocation_addressFamily = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {addressFamily} -> addressFamily) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {addressFamily = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The IP address assigned to the Amazon interface.
newPrivateVirtualInterfaceAllocation_amazonAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Core.Text)
newPrivateVirtualInterfaceAllocation_amazonAddress = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {amazonAddress} -> amazonAddress) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {amazonAddress = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The IP address assigned to the customer interface.
newPrivateVirtualInterfaceAllocation_customerAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Core.Text)
newPrivateVirtualInterfaceAllocation_customerAddress = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {customerAddress} -> customerAddress) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {customerAddress = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newPrivateVirtualInterfaceAllocation_virtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Core.Text
newPrivateVirtualInterfaceAllocation_virtualInterfaceName = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {virtualInterfaceName = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The ID of the VLAN.
newPrivateVirtualInterfaceAllocation_vlan :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Core.Int
newPrivateVirtualInterfaceAllocation_vlan = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {vlan} -> vlan) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {vlan = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newPrivateVirtualInterfaceAllocation_asn :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Core.Int
newPrivateVirtualInterfaceAllocation_asn = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {asn} -> asn) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {asn = a} :: NewPrivateVirtualInterfaceAllocation)

instance
  Core.Hashable
    NewPrivateVirtualInterfaceAllocation

instance
  Core.NFData
    NewPrivateVirtualInterfaceAllocation

instance
  Core.ToJSON
    NewPrivateVirtualInterfaceAllocation
  where
  toJSON NewPrivateVirtualInterfaceAllocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("authKey" Core..=) Core.<$> authKey,
            ("mtu" Core..=) Core.<$> mtu,
            ("tags" Core..=) Core.<$> tags,
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

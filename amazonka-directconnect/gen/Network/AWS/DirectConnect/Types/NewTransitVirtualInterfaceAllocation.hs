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
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Information about a transit virtual interface to be provisioned on a
-- connection.
--
-- /See:/ 'newNewTransitVirtualInterfaceAllocation' smart constructor.
data NewTransitVirtualInterfaceAllocation = NewTransitVirtualInterfaceAllocation'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Core.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Core.Maybe Core.Int,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int,
    -- | The tags associated with the transitive virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The name of the virtual interface assigned by the customer network. The
    -- name has a maximum of 100 characters. The following are valid
    -- characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Core.Maybe Core.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Core.Text,
    -- | The ID of the VLAN.
    vlan :: Core.Maybe Core.Int,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NewTransitVirtualInterfaceAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'newTransitVirtualInterfaceAllocation_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'asn', 'newTransitVirtualInterfaceAllocation_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
--
-- 'mtu', 'newTransitVirtualInterfaceAllocation_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
--
-- 'tags', 'newTransitVirtualInterfaceAllocation_tags' - The tags associated with the transitive virtual interface.
--
-- 'virtualInterfaceName', 'newTransitVirtualInterfaceAllocation_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'addressFamily', 'newTransitVirtualInterfaceAllocation_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newTransitVirtualInterfaceAllocation_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'vlan', 'newTransitVirtualInterfaceAllocation_vlan' - The ID of the VLAN.
--
-- 'customerAddress', 'newTransitVirtualInterfaceAllocation_customerAddress' - The IP address assigned to the customer interface.
newNewTransitVirtualInterfaceAllocation ::
  NewTransitVirtualInterfaceAllocation
newNewTransitVirtualInterfaceAllocation =
  NewTransitVirtualInterfaceAllocation'
    { authKey =
        Core.Nothing,
      asn = Core.Nothing,
      mtu = Core.Nothing,
      tags = Core.Nothing,
      virtualInterfaceName = Core.Nothing,
      addressFamily = Core.Nothing,
      amazonAddress = Core.Nothing,
      vlan = Core.Nothing,
      customerAddress = Core.Nothing
    }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newTransitVirtualInterfaceAllocation_authKey :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Text)
newTransitVirtualInterfaceAllocation_authKey = Lens.lens (\NewTransitVirtualInterfaceAllocation' {authKey} -> authKey) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {authKey = a} :: NewTransitVirtualInterfaceAllocation)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newTransitVirtualInterfaceAllocation_asn :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Int)
newTransitVirtualInterfaceAllocation_asn = Lens.lens (\NewTransitVirtualInterfaceAllocation' {asn} -> asn) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {asn = a} :: NewTransitVirtualInterfaceAllocation)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
newTransitVirtualInterfaceAllocation_mtu :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Int)
newTransitVirtualInterfaceAllocation_mtu = Lens.lens (\NewTransitVirtualInterfaceAllocation' {mtu} -> mtu) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {mtu = a} :: NewTransitVirtualInterfaceAllocation)

-- | The tags associated with the transitive virtual interface.
newTransitVirtualInterfaceAllocation_tags :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe (Core.NonEmpty Tag))
newTransitVirtualInterfaceAllocation_tags = Lens.lens (\NewTransitVirtualInterfaceAllocation' {tags} -> tags) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {tags = a} :: NewTransitVirtualInterfaceAllocation) Core.. Lens.mapping Lens._Coerce

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newTransitVirtualInterfaceAllocation_virtualInterfaceName :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Text)
newTransitVirtualInterfaceAllocation_virtualInterfaceName = Lens.lens (\NewTransitVirtualInterfaceAllocation' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {virtualInterfaceName = a} :: NewTransitVirtualInterfaceAllocation)

-- | The address family for the BGP peer.
newTransitVirtualInterfaceAllocation_addressFamily :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe AddressFamily)
newTransitVirtualInterfaceAllocation_addressFamily = Lens.lens (\NewTransitVirtualInterfaceAllocation' {addressFamily} -> addressFamily) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {addressFamily = a} :: NewTransitVirtualInterfaceAllocation)

-- | The IP address assigned to the Amazon interface.
newTransitVirtualInterfaceAllocation_amazonAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Text)
newTransitVirtualInterfaceAllocation_amazonAddress = Lens.lens (\NewTransitVirtualInterfaceAllocation' {amazonAddress} -> amazonAddress) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {amazonAddress = a} :: NewTransitVirtualInterfaceAllocation)

-- | The ID of the VLAN.
newTransitVirtualInterfaceAllocation_vlan :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Int)
newTransitVirtualInterfaceAllocation_vlan = Lens.lens (\NewTransitVirtualInterfaceAllocation' {vlan} -> vlan) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {vlan = a} :: NewTransitVirtualInterfaceAllocation)

-- | The IP address assigned to the customer interface.
newTransitVirtualInterfaceAllocation_customerAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Core.Maybe Core.Text)
newTransitVirtualInterfaceAllocation_customerAddress = Lens.lens (\NewTransitVirtualInterfaceAllocation' {customerAddress} -> customerAddress) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {customerAddress = a} :: NewTransitVirtualInterfaceAllocation)

instance
  Core.Hashable
    NewTransitVirtualInterfaceAllocation

instance
  Core.NFData
    NewTransitVirtualInterfaceAllocation

instance
  Core.ToJSON
    NewTransitVirtualInterfaceAllocation
  where
  toJSON NewTransitVirtualInterfaceAllocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("authKey" Core..=) Core.<$> authKey,
            ("asn" Core..=) Core.<$> asn,
            ("mtu" Core..=) Core.<$> mtu,
            ("tags" Core..=) Core.<$> tags,
            ("virtualInterfaceName" Core..=)
              Core.<$> virtualInterfaceName,
            ("addressFamily" Core..=) Core.<$> addressFamily,
            ("amazonAddress" Core..=) Core.<$> amazonAddress,
            ("vlan" Core..=) Core.<$> vlan,
            ("customerAddress" Core..=)
              Core.<$> customerAddress
          ]
      )

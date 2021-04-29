{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a private virtual interface to be provisioned on a
-- connection.
--
-- /See:/ 'newNewPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Prelude.Maybe Prelude.Text,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Prelude.Maybe Prelude.Int,
    -- | The tags associated with the private virtual interface.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The address family for the BGP peer.
    addressFamily :: Prelude.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the virtual interface assigned by the customer network. The
    -- name has a maximum of 100 characters. The following are valid
    -- characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Prelude.Text,
    -- | The ID of the VLAN.
    vlan :: Prelude.Int,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'vlan'
  Prelude.Int ->
  -- | 'asn'
  Prelude.Int ->
  NewPrivateVirtualInterfaceAllocation
newNewPrivateVirtualInterfaceAllocation
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPrivateVirtualInterfaceAllocation'
      { authKey =
          Prelude.Nothing,
        mtu = Prelude.Nothing,
        tags = Prelude.Nothing,
        addressFamily = Prelude.Nothing,
        amazonAddress = Prelude.Nothing,
        customerAddress = Prelude.Nothing,
        virtualInterfaceName =
          pVirtualInterfaceName_,
        vlan = pVlan_,
        asn = pAsn_
      }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newPrivateVirtualInterfaceAllocation_authKey :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newPrivateVirtualInterfaceAllocation_authKey = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {authKey} -> authKey) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {authKey = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
newPrivateVirtualInterfaceAllocation_mtu :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Prelude.Maybe Prelude.Int)
newPrivateVirtualInterfaceAllocation_mtu = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {mtu} -> mtu) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {mtu = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The tags associated with the private virtual interface.
newPrivateVirtualInterfaceAllocation_tags :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Prelude.Maybe (Prelude.NonEmpty Tag))
newPrivateVirtualInterfaceAllocation_tags = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {tags} -> tags) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {tags = a} :: NewPrivateVirtualInterfaceAllocation) Prelude.. Lens.mapping Prelude._Coerce

-- | The address family for the BGP peer.
newPrivateVirtualInterfaceAllocation_addressFamily :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Prelude.Maybe AddressFamily)
newPrivateVirtualInterfaceAllocation_addressFamily = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {addressFamily} -> addressFamily) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {addressFamily = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The IP address assigned to the Amazon interface.
newPrivateVirtualInterfaceAllocation_amazonAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newPrivateVirtualInterfaceAllocation_amazonAddress = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {amazonAddress} -> amazonAddress) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {amazonAddress = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The IP address assigned to the customer interface.
newPrivateVirtualInterfaceAllocation_customerAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newPrivateVirtualInterfaceAllocation_customerAddress = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {customerAddress} -> customerAddress) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {customerAddress = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newPrivateVirtualInterfaceAllocation_virtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Prelude.Text
newPrivateVirtualInterfaceAllocation_virtualInterfaceName = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {virtualInterfaceName = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The ID of the VLAN.
newPrivateVirtualInterfaceAllocation_vlan :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Prelude.Int
newPrivateVirtualInterfaceAllocation_vlan = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {vlan} -> vlan) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {vlan = a} :: NewPrivateVirtualInterfaceAllocation)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newPrivateVirtualInterfaceAllocation_asn :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Prelude.Int
newPrivateVirtualInterfaceAllocation_asn = Lens.lens (\NewPrivateVirtualInterfaceAllocation' {asn} -> asn) (\s@NewPrivateVirtualInterfaceAllocation' {} a -> s {asn = a} :: NewPrivateVirtualInterfaceAllocation)

instance
  Prelude.Hashable
    NewPrivateVirtualInterfaceAllocation

instance
  Prelude.NFData
    NewPrivateVirtualInterfaceAllocation

instance
  Prelude.ToJSON
    NewPrivateVirtualInterfaceAllocation
  where
  toJSON NewPrivateVirtualInterfaceAllocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("authKey" Prelude..=) Prelude.<$> authKey,
            ("mtu" Prelude..=) Prelude.<$> mtu,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("addressFamily" Prelude..=)
              Prelude.<$> addressFamily,
            ("amazonAddress" Prelude..=)
              Prelude.<$> amazonAddress,
            ("customerAddress" Prelude..=)
              Prelude.<$> customerAddress,
            Prelude.Just
              ( "virtualInterfaceName"
                  Prelude..= virtualInterfaceName
              ),
            Prelude.Just ("vlan" Prelude..= vlan),
            Prelude.Just ("asn" Prelude..= asn)
          ]
      )

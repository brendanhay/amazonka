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
-- Module      : Amazonka.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.NewTransitVirtualInterfaceAllocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.AddressFamily
import Amazonka.DirectConnect.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a transit virtual interface to be provisioned on a
-- connection.
--
-- /See:/ 'newNewTransitVirtualInterfaceAllocation' smart constructor.
data NewTransitVirtualInterfaceAllocation = NewTransitVirtualInterfaceAllocation'
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
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
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
-- Create a value of 'NewTransitVirtualInterfaceAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressFamily', 'newTransitVirtualInterfaceAllocation_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newTransitVirtualInterfaceAllocation_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'asn', 'newTransitVirtualInterfaceAllocation_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
--
-- 'authKey', 'newTransitVirtualInterfaceAllocation_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'customerAddress', 'newTransitVirtualInterfaceAllocation_customerAddress' - The IP address assigned to the customer interface.
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
-- 'vlan', 'newTransitVirtualInterfaceAllocation_vlan' - The ID of the VLAN.
newNewTransitVirtualInterfaceAllocation ::
  NewTransitVirtualInterfaceAllocation
newNewTransitVirtualInterfaceAllocation =
  NewTransitVirtualInterfaceAllocation'
    { addressFamily =
        Prelude.Nothing,
      amazonAddress = Prelude.Nothing,
      asn = Prelude.Nothing,
      authKey = Prelude.Nothing,
      customerAddress = Prelude.Nothing,
      mtu = Prelude.Nothing,
      tags = Prelude.Nothing,
      virtualInterfaceName =
        Prelude.Nothing,
      vlan = Prelude.Nothing
    }

-- | The address family for the BGP peer.
newTransitVirtualInterfaceAllocation_addressFamily :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe AddressFamily)
newTransitVirtualInterfaceAllocation_addressFamily = Lens.lens (\NewTransitVirtualInterfaceAllocation' {addressFamily} -> addressFamily) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {addressFamily = a} :: NewTransitVirtualInterfaceAllocation)

-- | The IP address assigned to the Amazon interface.
newTransitVirtualInterfaceAllocation_amazonAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newTransitVirtualInterfaceAllocation_amazonAddress = Lens.lens (\NewTransitVirtualInterfaceAllocation' {amazonAddress} -> amazonAddress) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {amazonAddress = a} :: NewTransitVirtualInterfaceAllocation)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newTransitVirtualInterfaceAllocation_asn :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe Prelude.Int)
newTransitVirtualInterfaceAllocation_asn = Lens.lens (\NewTransitVirtualInterfaceAllocation' {asn} -> asn) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {asn = a} :: NewTransitVirtualInterfaceAllocation)

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newTransitVirtualInterfaceAllocation_authKey :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newTransitVirtualInterfaceAllocation_authKey = Lens.lens (\NewTransitVirtualInterfaceAllocation' {authKey} -> authKey) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {authKey = a} :: NewTransitVirtualInterfaceAllocation)

-- | The IP address assigned to the customer interface.
newTransitVirtualInterfaceAllocation_customerAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newTransitVirtualInterfaceAllocation_customerAddress = Lens.lens (\NewTransitVirtualInterfaceAllocation' {customerAddress} -> customerAddress) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {customerAddress = a} :: NewTransitVirtualInterfaceAllocation)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
newTransitVirtualInterfaceAllocation_mtu :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe Prelude.Int)
newTransitVirtualInterfaceAllocation_mtu = Lens.lens (\NewTransitVirtualInterfaceAllocation' {mtu} -> mtu) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {mtu = a} :: NewTransitVirtualInterfaceAllocation)

-- | The tags associated with the transitive virtual interface.
newTransitVirtualInterfaceAllocation_tags :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe (Prelude.NonEmpty Tag))
newTransitVirtualInterfaceAllocation_tags = Lens.lens (\NewTransitVirtualInterfaceAllocation' {tags} -> tags) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {tags = a} :: NewTransitVirtualInterfaceAllocation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newTransitVirtualInterfaceAllocation_virtualInterfaceName :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newTransitVirtualInterfaceAllocation_virtualInterfaceName = Lens.lens (\NewTransitVirtualInterfaceAllocation' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {virtualInterfaceName = a} :: NewTransitVirtualInterfaceAllocation)

-- | The ID of the VLAN.
newTransitVirtualInterfaceAllocation_vlan :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Prelude.Maybe Prelude.Int)
newTransitVirtualInterfaceAllocation_vlan = Lens.lens (\NewTransitVirtualInterfaceAllocation' {vlan} -> vlan) (\s@NewTransitVirtualInterfaceAllocation' {} a -> s {vlan = a} :: NewTransitVirtualInterfaceAllocation)

instance
  Prelude.Hashable
    NewTransitVirtualInterfaceAllocation
  where
  hashWithSalt
    _salt
    NewTransitVirtualInterfaceAllocation' {..} =
      _salt `Prelude.hashWithSalt` addressFamily
        `Prelude.hashWithSalt` amazonAddress
        `Prelude.hashWithSalt` asn
        `Prelude.hashWithSalt` authKey
        `Prelude.hashWithSalt` customerAddress
        `Prelude.hashWithSalt` mtu
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` virtualInterfaceName
        `Prelude.hashWithSalt` vlan

instance
  Prelude.NFData
    NewTransitVirtualInterfaceAllocation
  where
  rnf NewTransitVirtualInterfaceAllocation' {..} =
    Prelude.rnf addressFamily
      `Prelude.seq` Prelude.rnf amazonAddress
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf authKey
      `Prelude.seq` Prelude.rnf customerAddress
      `Prelude.seq` Prelude.rnf mtu
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf virtualInterfaceName
      `Prelude.seq` Prelude.rnf vlan

instance
  Data.ToJSON
    NewTransitVirtualInterfaceAllocation
  where
  toJSON NewTransitVirtualInterfaceAllocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addressFamily" Data..=) Prelude.<$> addressFamily,
            ("amazonAddress" Data..=) Prelude.<$> amazonAddress,
            ("asn" Data..=) Prelude.<$> asn,
            ("authKey" Data..=) Prelude.<$> authKey,
            ("customerAddress" Data..=)
              Prelude.<$> customerAddress,
            ("mtu" Data..=) Prelude.<$> mtu,
            ("tags" Data..=) Prelude.<$> tags,
            ("virtualInterfaceName" Data..=)
              Prelude.<$> virtualInterfaceName,
            ("vlan" Data..=) Prelude.<$> vlan
          ]
      )

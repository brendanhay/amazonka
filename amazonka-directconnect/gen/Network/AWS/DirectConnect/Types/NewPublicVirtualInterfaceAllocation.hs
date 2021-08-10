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
-- Module      : Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a public virtual interface to be provisioned on a
-- connection.
--
-- /See:/ 'newNewPublicVirtualInterfaceAllocation' smart constructor.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Prelude.Maybe Prelude.Text,
    -- | The routes to be advertised to the AWS network in this Region. Applies
    -- to public virtual interfaces.
    routeFilterPrefixes :: Prelude.Maybe [RouteFilterPrefix],
    -- | The tags associated with the public virtual interface.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewPublicVirtualInterfaceAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'newPublicVirtualInterfaceAllocation_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'routeFilterPrefixes', 'newPublicVirtualInterfaceAllocation_routeFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies
-- to public virtual interfaces.
--
-- 'tags', 'newPublicVirtualInterfaceAllocation_tags' - The tags associated with the public virtual interface.
--
-- 'addressFamily', 'newPublicVirtualInterfaceAllocation_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newPublicVirtualInterfaceAllocation_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'customerAddress', 'newPublicVirtualInterfaceAllocation_customerAddress' - The IP address assigned to the customer interface.
--
-- 'virtualInterfaceName', 'newPublicVirtualInterfaceAllocation_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'vlan', 'newPublicVirtualInterfaceAllocation_vlan' - The ID of the VLAN.
--
-- 'asn', 'newPublicVirtualInterfaceAllocation_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newNewPublicVirtualInterfaceAllocation ::
  -- | 'virtualInterfaceName'
  Prelude.Text ->
  -- | 'vlan'
  Prelude.Int ->
  -- | 'asn'
  Prelude.Int ->
  NewPublicVirtualInterfaceAllocation
newNewPublicVirtualInterfaceAllocation
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPublicVirtualInterfaceAllocation'
      { authKey =
          Prelude.Nothing,
        routeFilterPrefixes = Prelude.Nothing,
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
newPublicVirtualInterfaceAllocation_authKey :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newPublicVirtualInterfaceAllocation_authKey = Lens.lens (\NewPublicVirtualInterfaceAllocation' {authKey} -> authKey) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {authKey = a} :: NewPublicVirtualInterfaceAllocation)

-- | The routes to be advertised to the AWS network in this Region. Applies
-- to public virtual interfaces.
newPublicVirtualInterfaceAllocation_routeFilterPrefixes :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Prelude.Maybe [RouteFilterPrefix])
newPublicVirtualInterfaceAllocation_routeFilterPrefixes = Lens.lens (\NewPublicVirtualInterfaceAllocation' {routeFilterPrefixes} -> routeFilterPrefixes) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {routeFilterPrefixes = a} :: NewPublicVirtualInterfaceAllocation) Prelude.. Lens.mapping Lens._Coerce

-- | The tags associated with the public virtual interface.
newPublicVirtualInterfaceAllocation_tags :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Prelude.Maybe (Prelude.NonEmpty Tag))
newPublicVirtualInterfaceAllocation_tags = Lens.lens (\NewPublicVirtualInterfaceAllocation' {tags} -> tags) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {tags = a} :: NewPublicVirtualInterfaceAllocation) Prelude.. Lens.mapping Lens._Coerce

-- | The address family for the BGP peer.
newPublicVirtualInterfaceAllocation_addressFamily :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Prelude.Maybe AddressFamily)
newPublicVirtualInterfaceAllocation_addressFamily = Lens.lens (\NewPublicVirtualInterfaceAllocation' {addressFamily} -> addressFamily) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {addressFamily = a} :: NewPublicVirtualInterfaceAllocation)

-- | The IP address assigned to the Amazon interface.
newPublicVirtualInterfaceAllocation_amazonAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newPublicVirtualInterfaceAllocation_amazonAddress = Lens.lens (\NewPublicVirtualInterfaceAllocation' {amazonAddress} -> amazonAddress) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {amazonAddress = a} :: NewPublicVirtualInterfaceAllocation)

-- | The IP address assigned to the customer interface.
newPublicVirtualInterfaceAllocation_customerAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Prelude.Maybe Prelude.Text)
newPublicVirtualInterfaceAllocation_customerAddress = Lens.lens (\NewPublicVirtualInterfaceAllocation' {customerAddress} -> customerAddress) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {customerAddress = a} :: NewPublicVirtualInterfaceAllocation)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newPublicVirtualInterfaceAllocation_virtualInterfaceName :: Lens.Lens' NewPublicVirtualInterfaceAllocation Prelude.Text
newPublicVirtualInterfaceAllocation_virtualInterfaceName = Lens.lens (\NewPublicVirtualInterfaceAllocation' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {virtualInterfaceName = a} :: NewPublicVirtualInterfaceAllocation)

-- | The ID of the VLAN.
newPublicVirtualInterfaceAllocation_vlan :: Lens.Lens' NewPublicVirtualInterfaceAllocation Prelude.Int
newPublicVirtualInterfaceAllocation_vlan = Lens.lens (\NewPublicVirtualInterfaceAllocation' {vlan} -> vlan) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {vlan = a} :: NewPublicVirtualInterfaceAllocation)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newPublicVirtualInterfaceAllocation_asn :: Lens.Lens' NewPublicVirtualInterfaceAllocation Prelude.Int
newPublicVirtualInterfaceAllocation_asn = Lens.lens (\NewPublicVirtualInterfaceAllocation' {asn} -> asn) (\s@NewPublicVirtualInterfaceAllocation' {} a -> s {asn = a} :: NewPublicVirtualInterfaceAllocation)

instance
  Prelude.Hashable
    NewPublicVirtualInterfaceAllocation

instance
  Prelude.NFData
    NewPublicVirtualInterfaceAllocation

instance
  Core.ToJSON
    NewPublicVirtualInterfaceAllocation
  where
  toJSON NewPublicVirtualInterfaceAllocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("authKey" Core..=) Prelude.<$> authKey,
            ("routeFilterPrefixes" Core..=)
              Prelude.<$> routeFilterPrefixes,
            ("tags" Core..=) Prelude.<$> tags,
            ("addressFamily" Core..=) Prelude.<$> addressFamily,
            ("amazonAddress" Core..=) Prelude.<$> amazonAddress,
            ("customerAddress" Core..=)
              Prelude.<$> customerAddress,
            Prelude.Just
              ( "virtualInterfaceName"
                  Core..= virtualInterfaceName
              ),
            Prelude.Just ("vlan" Core..= vlan),
            Prelude.Just ("asn" Core..= asn)
          ]
      )

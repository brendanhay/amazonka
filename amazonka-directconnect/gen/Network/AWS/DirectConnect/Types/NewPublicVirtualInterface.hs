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
-- Module      : Network.AWS.DirectConnect.Types.NewPublicVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPublicVirtualInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Information about a public virtual interface.
--
-- /See:/ 'newNewPublicVirtualInterface' smart constructor.
data NewPublicVirtualInterface = NewPublicVirtualInterface'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Core.Text,
    -- | The routes to be advertised to the AWS network in this Region. Applies
    -- to public virtual interfaces.
    routeFilterPrefixes :: Core.Maybe [RouteFilterPrefix],
    -- | The tags associated with the public virtual interface.
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
-- Create a value of 'NewPublicVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'newPublicVirtualInterface_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'routeFilterPrefixes', 'newPublicVirtualInterface_routeFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies
-- to public virtual interfaces.
--
-- 'tags', 'newPublicVirtualInterface_tags' - The tags associated with the public virtual interface.
--
-- 'addressFamily', 'newPublicVirtualInterface_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'newPublicVirtualInterface_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'customerAddress', 'newPublicVirtualInterface_customerAddress' - The IP address assigned to the customer interface.
--
-- 'virtualInterfaceName', 'newPublicVirtualInterface_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'vlan', 'newPublicVirtualInterface_vlan' - The ID of the VLAN.
--
-- 'asn', 'newPublicVirtualInterface_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newNewPublicVirtualInterface ::
  -- | 'virtualInterfaceName'
  Core.Text ->
  -- | 'vlan'
  Core.Int ->
  -- | 'asn'
  Core.Int ->
  NewPublicVirtualInterface
newNewPublicVirtualInterface
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPublicVirtualInterface'
      { authKey = Core.Nothing,
        routeFilterPrefixes = Core.Nothing,
        tags = Core.Nothing,
        addressFamily = Core.Nothing,
        amazonAddress = Core.Nothing,
        customerAddress = Core.Nothing,
        virtualInterfaceName = pVirtualInterfaceName_,
        vlan = pVlan_,
        asn = pAsn_
      }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
newPublicVirtualInterface_authKey :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe Core.Text)
newPublicVirtualInterface_authKey = Lens.lens (\NewPublicVirtualInterface' {authKey} -> authKey) (\s@NewPublicVirtualInterface' {} a -> s {authKey = a} :: NewPublicVirtualInterface)

-- | The routes to be advertised to the AWS network in this Region. Applies
-- to public virtual interfaces.
newPublicVirtualInterface_routeFilterPrefixes :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe [RouteFilterPrefix])
newPublicVirtualInterface_routeFilterPrefixes = Lens.lens (\NewPublicVirtualInterface' {routeFilterPrefixes} -> routeFilterPrefixes) (\s@NewPublicVirtualInterface' {} a -> s {routeFilterPrefixes = a} :: NewPublicVirtualInterface) Core.. Lens.mapping Lens._Coerce

-- | The tags associated with the public virtual interface.
newPublicVirtualInterface_tags :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe (Core.NonEmpty Tag))
newPublicVirtualInterface_tags = Lens.lens (\NewPublicVirtualInterface' {tags} -> tags) (\s@NewPublicVirtualInterface' {} a -> s {tags = a} :: NewPublicVirtualInterface) Core.. Lens.mapping Lens._Coerce

-- | The address family for the BGP peer.
newPublicVirtualInterface_addressFamily :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe AddressFamily)
newPublicVirtualInterface_addressFamily = Lens.lens (\NewPublicVirtualInterface' {addressFamily} -> addressFamily) (\s@NewPublicVirtualInterface' {} a -> s {addressFamily = a} :: NewPublicVirtualInterface)

-- | The IP address assigned to the Amazon interface.
newPublicVirtualInterface_amazonAddress :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe Core.Text)
newPublicVirtualInterface_amazonAddress = Lens.lens (\NewPublicVirtualInterface' {amazonAddress} -> amazonAddress) (\s@NewPublicVirtualInterface' {} a -> s {amazonAddress = a} :: NewPublicVirtualInterface)

-- | The IP address assigned to the customer interface.
newPublicVirtualInterface_customerAddress :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe Core.Text)
newPublicVirtualInterface_customerAddress = Lens.lens (\NewPublicVirtualInterface' {customerAddress} -> customerAddress) (\s@NewPublicVirtualInterface' {} a -> s {customerAddress = a} :: NewPublicVirtualInterface)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
newPublicVirtualInterface_virtualInterfaceName :: Lens.Lens' NewPublicVirtualInterface Core.Text
newPublicVirtualInterface_virtualInterfaceName = Lens.lens (\NewPublicVirtualInterface' {virtualInterfaceName} -> virtualInterfaceName) (\s@NewPublicVirtualInterface' {} a -> s {virtualInterfaceName = a} :: NewPublicVirtualInterface)

-- | The ID of the VLAN.
newPublicVirtualInterface_vlan :: Lens.Lens' NewPublicVirtualInterface Core.Int
newPublicVirtualInterface_vlan = Lens.lens (\NewPublicVirtualInterface' {vlan} -> vlan) (\s@NewPublicVirtualInterface' {} a -> s {vlan = a} :: NewPublicVirtualInterface)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
newPublicVirtualInterface_asn :: Lens.Lens' NewPublicVirtualInterface Core.Int
newPublicVirtualInterface_asn = Lens.lens (\NewPublicVirtualInterface' {asn} -> asn) (\s@NewPublicVirtualInterface' {} a -> s {asn = a} :: NewPublicVirtualInterface)

instance Core.Hashable NewPublicVirtualInterface

instance Core.NFData NewPublicVirtualInterface

instance Core.ToJSON NewPublicVirtualInterface where
  toJSON NewPublicVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ ("authKey" Core..=) Core.<$> authKey,
            ("routeFilterPrefixes" Core..=)
              Core.<$> routeFilterPrefixes,
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

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
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.BGPPeer
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.DirectConnect.Types.VirtualInterfaceState
import qualified Network.AWS.Lens as Lens

-- | Information about a virtual interface.
--
-- /See:/ 'newVirtualInterface' smart constructor.
data VirtualInterface = VirtualInterface'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Core.Text,
    -- | The BGP peers configured on this virtual interface.
    bgpPeers :: Core.Maybe [BGPPeer],
    -- | The ID of the virtual private gateway. Applies only to private virtual
    -- interfaces.
    virtualGatewayId :: Core.Maybe Core.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Core.Maybe Core.Int,
    -- | The Direct Connect endpoint on which the virtual interface terminates.
    awsDeviceV2 :: Core.Maybe Core.Text,
    -- | The ID of the connection.
    connectionId :: Core.Maybe Core.Text,
    -- | The customer router configuration.
    customerRouterConfig :: Core.Maybe Core.Text,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Core.Maybe Core.Bool,
    -- | The routes to be advertised to the AWS network in this Region. Applies
    -- to public virtual interfaces.
    routeFilterPrefixes :: Core.Maybe [RouteFilterPrefix],
    -- | The type of virtual interface. The possible values are @private@ and
    -- @public@.
    virtualInterfaceType :: Core.Maybe Core.Text,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int,
    -- | The tags associated with the virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Core.Text,
    -- | The autonomous system number (ASN) for the Amazon side of the
    -- connection.
    amazonSideAsn :: Core.Maybe Core.Integer,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Core.Text,
    -- | The state of the virtual interface. The following are the possible
    -- values:
    --
    -- -   @confirming@: The creation of the virtual interface is pending
    --     confirmation from the virtual interface owner. If the owner of the
    --     virtual interface is different from the owner of the connection on
    --     which it is provisioned, then the virtual interface will remain in
    --     this state until it is confirmed by the virtual interface owner.
    --
    -- -   @verifying@: This state only applies to public virtual interfaces.
    --     Each public virtual interface needs validation before the virtual
    --     interface can be created.
    --
    -- -   @pending@: A virtual interface is in this state from the time that
    --     it is created until the virtual interface is ready to forward
    --     traffic.
    --
    -- -   @available@: A virtual interface that is able to forward traffic.
    --
    -- -   @down@: A virtual interface that is BGP down.
    --
    -- -   @deleting@: A virtual interface is in this state immediately after
    --     calling DeleteVirtualInterface until it can no longer forward
    --     traffic.
    --
    -- -   @deleted@: A virtual interface that cannot forward traffic.
    --
    -- -   @rejected@: The virtual interface owner has declined creation of the
    --     virtual interface. If a virtual interface in the @Confirming@ state
    --     is deleted by the virtual interface owner, the virtual interface
    --     enters the @Rejected@ state.
    --
    -- -   @unknown@: The state of the virtual interface is not available.
    virtualInterfaceState :: Core.Maybe VirtualInterfaceState,
    -- | The name of the virtual interface assigned by the customer network. The
    -- name has a maximum of 100 characters. The following are valid
    -- characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Core.Maybe Core.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Core.Text,
    -- | The ID of the AWS account that owns the virtual interface.
    ownerAccount :: Core.Maybe Core.Text,
    -- | The AWS Region where the virtual interface is located.
    region :: Core.Maybe Core.Text,
    -- | The location of the connection.
    location :: Core.Maybe Core.Text,
    -- | The ID of the VLAN.
    vlan :: Core.Maybe Core.Int,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'virtualInterface_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'bgpPeers', 'virtualInterface_bgpPeers' - The BGP peers configured on this virtual interface.
--
-- 'virtualGatewayId', 'virtualInterface_virtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
--
-- 'asn', 'virtualInterface_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
--
-- 'awsDeviceV2', 'virtualInterface_awsDeviceV2' - The Direct Connect endpoint on which the virtual interface terminates.
--
-- 'connectionId', 'virtualInterface_connectionId' - The ID of the connection.
--
-- 'customerRouterConfig', 'virtualInterface_customerRouterConfig' - The customer router configuration.
--
-- 'jumboFrameCapable', 'virtualInterface_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- 'routeFilterPrefixes', 'virtualInterface_routeFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies
-- to public virtual interfaces.
--
-- 'virtualInterfaceType', 'virtualInterface_virtualInterfaceType' - The type of virtual interface. The possible values are @private@ and
-- @public@.
--
-- 'mtu', 'virtualInterface_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
--
-- 'tags', 'virtualInterface_tags' - The tags associated with the virtual interface.
--
-- 'virtualInterfaceId', 'virtualInterface_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'amazonSideAsn', 'virtualInterface_amazonSideAsn' - The autonomous system number (ASN) for the Amazon side of the
-- connection.
--
-- 'directConnectGatewayId', 'virtualInterface_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'virtualInterfaceState', 'virtualInterface_virtualInterfaceState' - The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
--
-- 'virtualInterfaceName', 'virtualInterface_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'addressFamily', 'virtualInterface_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'virtualInterface_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'ownerAccount', 'virtualInterface_ownerAccount' - The ID of the AWS account that owns the virtual interface.
--
-- 'region', 'virtualInterface_region' - The AWS Region where the virtual interface is located.
--
-- 'location', 'virtualInterface_location' - The location of the connection.
--
-- 'vlan', 'virtualInterface_vlan' - The ID of the VLAN.
--
-- 'customerAddress', 'virtualInterface_customerAddress' - The IP address assigned to the customer interface.
newVirtualInterface ::
  VirtualInterface
newVirtualInterface =
  VirtualInterface'
    { authKey = Core.Nothing,
      bgpPeers = Core.Nothing,
      virtualGatewayId = Core.Nothing,
      asn = Core.Nothing,
      awsDeviceV2 = Core.Nothing,
      connectionId = Core.Nothing,
      customerRouterConfig = Core.Nothing,
      jumboFrameCapable = Core.Nothing,
      routeFilterPrefixes = Core.Nothing,
      virtualInterfaceType = Core.Nothing,
      mtu = Core.Nothing,
      tags = Core.Nothing,
      virtualInterfaceId = Core.Nothing,
      amazonSideAsn = Core.Nothing,
      directConnectGatewayId = Core.Nothing,
      virtualInterfaceState = Core.Nothing,
      virtualInterfaceName = Core.Nothing,
      addressFamily = Core.Nothing,
      amazonAddress = Core.Nothing,
      ownerAccount = Core.Nothing,
      region = Core.Nothing,
      location = Core.Nothing,
      vlan = Core.Nothing,
      customerAddress = Core.Nothing
    }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
virtualInterface_authKey :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_authKey = Lens.lens (\VirtualInterface' {authKey} -> authKey) (\s@VirtualInterface' {} a -> s {authKey = a} :: VirtualInterface)

-- | The BGP peers configured on this virtual interface.
virtualInterface_bgpPeers :: Lens.Lens' VirtualInterface (Core.Maybe [BGPPeer])
virtualInterface_bgpPeers = Lens.lens (\VirtualInterface' {bgpPeers} -> bgpPeers) (\s@VirtualInterface' {} a -> s {bgpPeers = a} :: VirtualInterface) Core.. Lens.mapping Lens._Coerce

-- | The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
virtualInterface_virtualGatewayId :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_virtualGatewayId = Lens.lens (\VirtualInterface' {virtualGatewayId} -> virtualGatewayId) (\s@VirtualInterface' {} a -> s {virtualGatewayId = a} :: VirtualInterface)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
virtualInterface_asn :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
virtualInterface_asn = Lens.lens (\VirtualInterface' {asn} -> asn) (\s@VirtualInterface' {} a -> s {asn = a} :: VirtualInterface)

-- | The Direct Connect endpoint on which the virtual interface terminates.
virtualInterface_awsDeviceV2 :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_awsDeviceV2 = Lens.lens (\VirtualInterface' {awsDeviceV2} -> awsDeviceV2) (\s@VirtualInterface' {} a -> s {awsDeviceV2 = a} :: VirtualInterface)

-- | The ID of the connection.
virtualInterface_connectionId :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_connectionId = Lens.lens (\VirtualInterface' {connectionId} -> connectionId) (\s@VirtualInterface' {} a -> s {connectionId = a} :: VirtualInterface)

-- | The customer router configuration.
virtualInterface_customerRouterConfig :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_customerRouterConfig = Lens.lens (\VirtualInterface' {customerRouterConfig} -> customerRouterConfig) (\s@VirtualInterface' {} a -> s {customerRouterConfig = a} :: VirtualInterface)

-- | Indicates whether jumbo frames (9001 MTU) are supported.
virtualInterface_jumboFrameCapable :: Lens.Lens' VirtualInterface (Core.Maybe Core.Bool)
virtualInterface_jumboFrameCapable = Lens.lens (\VirtualInterface' {jumboFrameCapable} -> jumboFrameCapable) (\s@VirtualInterface' {} a -> s {jumboFrameCapable = a} :: VirtualInterface)

-- | The routes to be advertised to the AWS network in this Region. Applies
-- to public virtual interfaces.
virtualInterface_routeFilterPrefixes :: Lens.Lens' VirtualInterface (Core.Maybe [RouteFilterPrefix])
virtualInterface_routeFilterPrefixes = Lens.lens (\VirtualInterface' {routeFilterPrefixes} -> routeFilterPrefixes) (\s@VirtualInterface' {} a -> s {routeFilterPrefixes = a} :: VirtualInterface) Core.. Lens.mapping Lens._Coerce

-- | The type of virtual interface. The possible values are @private@ and
-- @public@.
virtualInterface_virtualInterfaceType :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_virtualInterfaceType = Lens.lens (\VirtualInterface' {virtualInterfaceType} -> virtualInterfaceType) (\s@VirtualInterface' {} a -> s {virtualInterfaceType = a} :: VirtualInterface)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
virtualInterface_mtu :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
virtualInterface_mtu = Lens.lens (\VirtualInterface' {mtu} -> mtu) (\s@VirtualInterface' {} a -> s {mtu = a} :: VirtualInterface)

-- | The tags associated with the virtual interface.
virtualInterface_tags :: Lens.Lens' VirtualInterface (Core.Maybe (Core.NonEmpty Tag))
virtualInterface_tags = Lens.lens (\VirtualInterface' {tags} -> tags) (\s@VirtualInterface' {} a -> s {tags = a} :: VirtualInterface) Core.. Lens.mapping Lens._Coerce

-- | The ID of the virtual interface.
virtualInterface_virtualInterfaceId :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_virtualInterfaceId = Lens.lens (\VirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@VirtualInterface' {} a -> s {virtualInterfaceId = a} :: VirtualInterface)

-- | The autonomous system number (ASN) for the Amazon side of the
-- connection.
virtualInterface_amazonSideAsn :: Lens.Lens' VirtualInterface (Core.Maybe Core.Integer)
virtualInterface_amazonSideAsn = Lens.lens (\VirtualInterface' {amazonSideAsn} -> amazonSideAsn) (\s@VirtualInterface' {} a -> s {amazonSideAsn = a} :: VirtualInterface)

-- | The ID of the Direct Connect gateway.
virtualInterface_directConnectGatewayId :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_directConnectGatewayId = Lens.lens (\VirtualInterface' {directConnectGatewayId} -> directConnectGatewayId) (\s@VirtualInterface' {} a -> s {directConnectGatewayId = a} :: VirtualInterface)

-- | The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
virtualInterface_virtualInterfaceState :: Lens.Lens' VirtualInterface (Core.Maybe VirtualInterfaceState)
virtualInterface_virtualInterfaceState = Lens.lens (\VirtualInterface' {virtualInterfaceState} -> virtualInterfaceState) (\s@VirtualInterface' {} a -> s {virtualInterfaceState = a} :: VirtualInterface)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
virtualInterface_virtualInterfaceName :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_virtualInterfaceName = Lens.lens (\VirtualInterface' {virtualInterfaceName} -> virtualInterfaceName) (\s@VirtualInterface' {} a -> s {virtualInterfaceName = a} :: VirtualInterface)

-- | The address family for the BGP peer.
virtualInterface_addressFamily :: Lens.Lens' VirtualInterface (Core.Maybe AddressFamily)
virtualInterface_addressFamily = Lens.lens (\VirtualInterface' {addressFamily} -> addressFamily) (\s@VirtualInterface' {} a -> s {addressFamily = a} :: VirtualInterface)

-- | The IP address assigned to the Amazon interface.
virtualInterface_amazonAddress :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_amazonAddress = Lens.lens (\VirtualInterface' {amazonAddress} -> amazonAddress) (\s@VirtualInterface' {} a -> s {amazonAddress = a} :: VirtualInterface)

-- | The ID of the AWS account that owns the virtual interface.
virtualInterface_ownerAccount :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_ownerAccount = Lens.lens (\VirtualInterface' {ownerAccount} -> ownerAccount) (\s@VirtualInterface' {} a -> s {ownerAccount = a} :: VirtualInterface)

-- | The AWS Region where the virtual interface is located.
virtualInterface_region :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_region = Lens.lens (\VirtualInterface' {region} -> region) (\s@VirtualInterface' {} a -> s {region = a} :: VirtualInterface)

-- | The location of the connection.
virtualInterface_location :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_location = Lens.lens (\VirtualInterface' {location} -> location) (\s@VirtualInterface' {} a -> s {location = a} :: VirtualInterface)

-- | The ID of the VLAN.
virtualInterface_vlan :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
virtualInterface_vlan = Lens.lens (\VirtualInterface' {vlan} -> vlan) (\s@VirtualInterface' {} a -> s {vlan = a} :: VirtualInterface)

-- | The IP address assigned to the customer interface.
virtualInterface_customerAddress :: Lens.Lens' VirtualInterface (Core.Maybe Core.Text)
virtualInterface_customerAddress = Lens.lens (\VirtualInterface' {customerAddress} -> customerAddress) (\s@VirtualInterface' {} a -> s {customerAddress = a} :: VirtualInterface)

instance Core.FromJSON VirtualInterface where
  parseJSON =
    Core.withObject
      "VirtualInterface"
      ( \x ->
          VirtualInterface'
            Core.<$> (x Core..:? "authKey")
            Core.<*> (x Core..:? "bgpPeers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "virtualGatewayId")
            Core.<*> (x Core..:? "asn")
            Core.<*> (x Core..:? "awsDeviceV2")
            Core.<*> (x Core..:? "connectionId")
            Core.<*> (x Core..:? "customerRouterConfig")
            Core.<*> (x Core..:? "jumboFrameCapable")
            Core.<*> ( x Core..:? "routeFilterPrefixes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "virtualInterfaceType")
            Core.<*> (x Core..:? "mtu")
            Core.<*> (x Core..:? "tags")
            Core.<*> (x Core..:? "virtualInterfaceId")
            Core.<*> (x Core..:? "amazonSideAsn")
            Core.<*> (x Core..:? "directConnectGatewayId")
            Core.<*> (x Core..:? "virtualInterfaceState")
            Core.<*> (x Core..:? "virtualInterfaceName")
            Core.<*> (x Core..:? "addressFamily")
            Core.<*> (x Core..:? "amazonAddress")
            Core.<*> (x Core..:? "ownerAccount")
            Core.<*> (x Core..:? "region")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "vlan")
            Core.<*> (x Core..:? "customerAddress")
      )

instance Core.Hashable VirtualInterface

instance Core.NFData VirtualInterface

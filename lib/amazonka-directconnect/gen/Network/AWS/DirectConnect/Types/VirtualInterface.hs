{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterface
  ( VirtualInterface (..),

    -- * Smart constructor
    mkVirtualInterface,

    -- * Lenses
    viAddressFamily,
    viAmazonAddress,
    viAmazonSideAsn,
    viAsn,
    viAuthKey,
    viAwsDeviceV2,
    viBgpPeers,
    viConnectionId,
    viCustomerAddress,
    viCustomerRouterConfig,
    viDirectConnectGatewayId,
    viJumboFrameCapable,
    viLocation,
    viMtu,
    viOwnerAccount,
    viRegion,
    viRouteFilterPrefixes,
    viTags,
    viVirtualGatewayId,
    viVirtualInterfaceId,
    viVirtualInterfaceName,
    viVirtualInterfaceState,
    viVirtualInterfaceType,
    viVlan,
  )
where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.AuthKey as Types
import qualified Network.AWS.DirectConnect.Types.AwsDeviceV2 as Types
import qualified Network.AWS.DirectConnect.Types.BGPPeer as Types
import qualified Network.AWS.DirectConnect.Types.ConnectionId as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.DirectConnect.Types.CustomerRouterConfig as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.LocationCode as Types
import qualified Network.AWS.DirectConnect.Types.OwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.Region as Types
import qualified Network.AWS.DirectConnect.Types.RouteFilterPrefix as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.DirectConnect.Types.VirtualGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceId as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceName as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceState as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a virtual interface.
--
-- /See:/ 'mkVirtualInterface' smart constructor.
data VirtualInterface = VirtualInterface'
  { -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe Types.AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Types.AmazonAddress,
    -- | The autonomous system number (ASN) for the Amazon side of the connection.
    amazonSideAsn :: Core.Maybe Core.Integer,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Core.Maybe Core.Int,
    -- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Types.AuthKey,
    -- | The Direct Connect endpoint on which the virtual interface terminates.
    awsDeviceV2 :: Core.Maybe Types.AwsDeviceV2,
    -- | The BGP peers configured on this virtual interface.
    bgpPeers :: Core.Maybe [Types.BGPPeer],
    -- | The ID of the connection.
    connectionId :: Core.Maybe Types.ConnectionId,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Types.CustomerAddress,
    -- | The customer router configuration.
    customerRouterConfig :: Core.Maybe Types.CustomerRouterConfig,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Core.Maybe Core.Bool,
    -- | The location of the connection.
    location :: Core.Maybe Types.LocationCode,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int,
    -- | The ID of the AWS account that owns the virtual interface.
    ownerAccount :: Core.Maybe Types.OwnerAccount,
    -- | The AWS Region where the virtual interface is located.
    region :: Core.Maybe Types.Region,
    -- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
    routeFilterPrefixes :: Core.Maybe [Types.RouteFilterPrefix],
    -- | The tags associated with the virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag),
    -- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
    virtualGatewayId :: Core.Maybe Types.VirtualGatewayId,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId,
    -- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Core.Maybe Types.VirtualInterfaceName,
    -- | The state of the virtual interface. The following are the possible values:
    --
    --
    --     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
    --
    --
    --     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
    --
    --
    --     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
    --
    --
    --     * @available@ : A virtual interface that is able to forward traffic.
    --
    --
    --     * @down@ : A virtual interface that is BGP down.
    --
    --
    --     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.
    --
    --
    --     * @deleted@ : A virtual interface that cannot forward traffic.
    --
    --
    --     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.
    --
    --
    --     * @unknown@ : The state of the virtual interface is not available.
    virtualInterfaceState :: Core.Maybe Types.VirtualInterfaceState,
    -- | The type of virtual interface. The possible values are @private@ and @public@ .
    virtualInterfaceType :: Core.Maybe Types.VirtualInterfaceType,
    -- | The ID of the VLAN.
    vlan :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VirtualInterface' value with any optional fields omitted.
mkVirtualInterface ::
  VirtualInterface
mkVirtualInterface =
  VirtualInterface'
    { addressFamily = Core.Nothing,
      amazonAddress = Core.Nothing,
      amazonSideAsn = Core.Nothing,
      asn = Core.Nothing,
      authKey = Core.Nothing,
      awsDeviceV2 = Core.Nothing,
      bgpPeers = Core.Nothing,
      connectionId = Core.Nothing,
      customerAddress = Core.Nothing,
      customerRouterConfig = Core.Nothing,
      directConnectGatewayId = Core.Nothing,
      jumboFrameCapable = Core.Nothing,
      location = Core.Nothing,
      mtu = Core.Nothing,
      ownerAccount = Core.Nothing,
      region = Core.Nothing,
      routeFilterPrefixes = Core.Nothing,
      tags = Core.Nothing,
      virtualGatewayId = Core.Nothing,
      virtualInterfaceId = Core.Nothing,
      virtualInterfaceName = Core.Nothing,
      virtualInterfaceState = Core.Nothing,
      virtualInterfaceType = Core.Nothing,
      vlan = Core.Nothing
    }

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAddressFamily :: Lens.Lens' VirtualInterface (Core.Maybe Types.AddressFamily)
viAddressFamily = Lens.field @"addressFamily"
{-# DEPRECATED viAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAmazonAddress :: Lens.Lens' VirtualInterface (Core.Maybe Types.AmazonAddress)
viAmazonAddress = Lens.field @"amazonAddress"
{-# DEPRECATED viAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The autonomous system number (ASN) for the Amazon side of the connection.
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAmazonSideAsn :: Lens.Lens' VirtualInterface (Core.Maybe Core.Integer)
viAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# DEPRECATED viAmazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAsn :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
viAsn = Lens.field @"asn"
{-# DEPRECATED viAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAuthKey :: Lens.Lens' VirtualInterface (Core.Maybe Types.AuthKey)
viAuthKey = Lens.field @"authKey"
{-# DEPRECATED viAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The Direct Connect endpoint on which the virtual interface terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAwsDeviceV2 :: Lens.Lens' VirtualInterface (Core.Maybe Types.AwsDeviceV2)
viAwsDeviceV2 = Lens.field @"awsDeviceV2"
{-# DEPRECATED viAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

-- | The BGP peers configured on this virtual interface.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viBgpPeers :: Lens.Lens' VirtualInterface (Core.Maybe [Types.BGPPeer])
viBgpPeers = Lens.field @"bgpPeers"
{-# DEPRECATED viBgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead." #-}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viConnectionId :: Lens.Lens' VirtualInterface (Core.Maybe Types.ConnectionId)
viConnectionId = Lens.field @"connectionId"
{-# DEPRECATED viConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCustomerAddress :: Lens.Lens' VirtualInterface (Core.Maybe Types.CustomerAddress)
viCustomerAddress = Lens.field @"customerAddress"
{-# DEPRECATED viCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The customer router configuration.
--
-- /Note:/ Consider using 'customerRouterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCustomerRouterConfig :: Lens.Lens' VirtualInterface (Core.Maybe Types.CustomerRouterConfig)
viCustomerRouterConfig = Lens.field @"customerRouterConfig"
{-# DEPRECATED viCustomerRouterConfig "Use generic-lens or generic-optics with 'customerRouterConfig' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viDirectConnectGatewayId :: Lens.Lens' VirtualInterface (Core.Maybe Types.DirectConnectGatewayId)
viDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# DEPRECATED viDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viJumboFrameCapable :: Lens.Lens' VirtualInterface (Core.Maybe Core.Bool)
viJumboFrameCapable = Lens.field @"jumboFrameCapable"
{-# DEPRECATED viJumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead." #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viLocation :: Lens.Lens' VirtualInterface (Core.Maybe Types.LocationCode)
viLocation = Lens.field @"location"
{-# DEPRECATED viLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viMtu :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
viMtu = Lens.field @"mtu"
{-# DEPRECATED viMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The ID of the AWS account that owns the virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viOwnerAccount :: Lens.Lens' VirtualInterface (Core.Maybe Types.OwnerAccount)
viOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED viOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The AWS Region where the virtual interface is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viRegion :: Lens.Lens' VirtualInterface (Core.Maybe Types.Region)
viRegion = Lens.field @"region"
{-# DEPRECATED viRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- /Note:/ Consider using 'routeFilterPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viRouteFilterPrefixes :: Lens.Lens' VirtualInterface (Core.Maybe [Types.RouteFilterPrefix])
viRouteFilterPrefixes = Lens.field @"routeFilterPrefixes"
{-# DEPRECATED viRouteFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead." #-}

-- | The tags associated with the virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viTags :: Lens.Lens' VirtualInterface (Core.Maybe (Core.NonEmpty Types.Tag))
viTags = Lens.field @"tags"
{-# DEPRECATED viTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualGatewayId :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualGatewayId)
viVirtualGatewayId = Lens.field @"virtualGatewayId"
{-# DEPRECATED viVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceId :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualInterfaceId)
viVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# DEPRECATED viVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceName :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualInterfaceName)
viVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# DEPRECATED viVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The state of the virtual interface. The following are the possible values:
--
--
--     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
--
--
--     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
--
--
--     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
--
--
--     * @available@ : A virtual interface that is able to forward traffic.
--
--
--     * @down@ : A virtual interface that is BGP down.
--
--
--     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.
--
--
--     * @deleted@ : A virtual interface that cannot forward traffic.
--
--
--     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.
--
--
--     * @unknown@ : The state of the virtual interface is not available.
--
--
--
-- /Note:/ Consider using 'virtualInterfaceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceState :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualInterfaceState)
viVirtualInterfaceState = Lens.field @"virtualInterfaceState"
{-# DEPRECATED viVirtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead." #-}

-- | The type of virtual interface. The possible values are @private@ and @public@ .
--
-- /Note:/ Consider using 'virtualInterfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceType :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualInterfaceType)
viVirtualInterfaceType = Lens.field @"virtualInterfaceType"
{-# DEPRECATED viVirtualInterfaceType "Use generic-lens or generic-optics with 'virtualInterfaceType' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVlan :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
viVlan = Lens.field @"vlan"
{-# DEPRECATED viVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

instance Core.FromJSON VirtualInterface where
  parseJSON =
    Core.withObject "VirtualInterface" Core.$
      \x ->
        VirtualInterface'
          Core.<$> (x Core..:? "addressFamily")
          Core.<*> (x Core..:? "amazonAddress")
          Core.<*> (x Core..:? "amazonSideAsn")
          Core.<*> (x Core..:? "asn")
          Core.<*> (x Core..:? "authKey")
          Core.<*> (x Core..:? "awsDeviceV2")
          Core.<*> (x Core..:? "bgpPeers")
          Core.<*> (x Core..:? "connectionId")
          Core.<*> (x Core..:? "customerAddress")
          Core.<*> (x Core..:? "customerRouterConfig")
          Core.<*> (x Core..:? "directConnectGatewayId")
          Core.<*> (x Core..:? "jumboFrameCapable")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "mtu")
          Core.<*> (x Core..:? "ownerAccount")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "routeFilterPrefixes")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "virtualGatewayId")
          Core.<*> (x Core..:? "virtualInterfaceId")
          Core.<*> (x Core..:? "virtualInterfaceName")
          Core.<*> (x Core..:? "virtualInterfaceState")
          Core.<*> (x Core..:? "virtualInterfaceType")
          Core.<*> (x Core..:? "vlan")

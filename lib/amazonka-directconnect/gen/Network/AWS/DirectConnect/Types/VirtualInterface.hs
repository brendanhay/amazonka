{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.VirtualInterface
  ( VirtualInterface (..)
  -- * Smart constructor
  , mkVirtualInterface
  -- * Lenses
  , viAddressFamily
  , viAmazonAddress
  , viAmazonSideAsn
  , viAsn
  , viAuthKey
  , viAwsDeviceV2
  , viBgpPeers
  , viConnectionId
  , viCustomerAddress
  , viCustomerRouterConfig
  , viDirectConnectGatewayId
  , viJumboFrameCapable
  , viLocation
  , viMtu
  , viOwnerAccount
  , viRegion
  , viRouteFilterPrefixes
  , viTags
  , viVirtualGatewayId
  , viVirtualInterfaceId
  , viVirtualInterfaceName
  , viVirtualInterfaceState
  , viVirtualInterfaceType
  , viVlan
  ) where

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
  { addressFamily :: Core.Maybe Types.AddressFamily
    -- ^ The address family for the BGP peer.
  , amazonAddress :: Core.Maybe Types.AmazonAddress
    -- ^ The IP address assigned to the Amazon interface.
  , amazonSideAsn :: Core.Maybe Core.Integer
    -- ^ The autonomous system number (ASN) for the Amazon side of the connection.
  , asn :: Core.Maybe Core.Int
    -- ^ The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
  , authKey :: Core.Maybe Types.AuthKey
    -- ^ The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
  , awsDeviceV2 :: Core.Maybe Types.AwsDeviceV2
    -- ^ The Direct Connect endpoint on which the virtual interface terminates.
  , bgpPeers :: Core.Maybe [Types.BGPPeer]
    -- ^ The BGP peers configured on this virtual interface.
  , connectionId :: Core.Maybe Types.ConnectionId
    -- ^ The ID of the connection.
  , customerAddress :: Core.Maybe Types.CustomerAddress
    -- ^ The IP address assigned to the customer interface.
  , customerRouterConfig :: Core.Maybe Types.CustomerRouterConfig
    -- ^ The customer router configuration.
  , directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , jumboFrameCapable :: Core.Maybe Core.Bool
    -- ^ Indicates whether jumbo frames (9001 MTU) are supported.
  , location :: Core.Maybe Types.LocationCode
    -- ^ The location of the connection.
  , mtu :: Core.Maybe Core.Int
    -- ^ The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
  , ownerAccount :: Core.Maybe Types.OwnerAccount
    -- ^ The ID of the AWS account that owns the virtual interface.
  , region :: Core.Maybe Types.Region
    -- ^ The AWS Region where the virtual interface is located.
  , routeFilterPrefixes :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags associated with the virtual interface.
  , virtualGatewayId :: Core.Maybe Types.VirtualGatewayId
    -- ^ The ID of the virtual private gateway. Applies only to private virtual interfaces.
  , virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
    -- ^ The ID of the virtual interface.
  , virtualInterfaceName :: Core.Maybe Types.VirtualInterfaceName
    -- ^ The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
  , virtualInterfaceState :: Core.Maybe Types.VirtualInterfaceState
    -- ^ The state of the virtual interface. The following are the possible values:
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
  , virtualInterfaceType :: Core.Maybe Types.VirtualInterfaceType
    -- ^ The type of virtual interface. The possible values are @private@ and @public@ .
  , vlan :: Core.Maybe Core.Int
    -- ^ The ID of the VLAN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VirtualInterface' value with any optional fields omitted.
mkVirtualInterface
    :: VirtualInterface
mkVirtualInterface
  = VirtualInterface'{addressFamily = Core.Nothing,
                      amazonAddress = Core.Nothing, amazonSideAsn = Core.Nothing,
                      asn = Core.Nothing, authKey = Core.Nothing,
                      awsDeviceV2 = Core.Nothing, bgpPeers = Core.Nothing,
                      connectionId = Core.Nothing, customerAddress = Core.Nothing,
                      customerRouterConfig = Core.Nothing,
                      directConnectGatewayId = Core.Nothing,
                      jumboFrameCapable = Core.Nothing, location = Core.Nothing,
                      mtu = Core.Nothing, ownerAccount = Core.Nothing,
                      region = Core.Nothing, routeFilterPrefixes = Core.Nothing,
                      tags = Core.Nothing, virtualGatewayId = Core.Nothing,
                      virtualInterfaceId = Core.Nothing,
                      virtualInterfaceName = Core.Nothing,
                      virtualInterfaceState = Core.Nothing,
                      virtualInterfaceType = Core.Nothing, vlan = Core.Nothing}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAddressFamily :: Lens.Lens' VirtualInterface (Core.Maybe Types.AddressFamily)
viAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE viAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAmazonAddress :: Lens.Lens' VirtualInterface (Core.Maybe Types.AmazonAddress)
viAmazonAddress = Lens.field @"amazonAddress"
{-# INLINEABLE viAmazonAddress #-}
{-# DEPRECATED amazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead"  #-}

-- | The autonomous system number (ASN) for the Amazon side of the connection.
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAmazonSideAsn :: Lens.Lens' VirtualInterface (Core.Maybe Core.Integer)
viAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# INLINEABLE viAmazonSideAsn #-}
{-# DEPRECATED amazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead"  #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAsn :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
viAsn = Lens.field @"asn"
{-# INLINEABLE viAsn #-}
{-# DEPRECATED asn "Use generic-lens or generic-optics with 'asn' instead"  #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAuthKey :: Lens.Lens' VirtualInterface (Core.Maybe Types.AuthKey)
viAuthKey = Lens.field @"authKey"
{-# INLINEABLE viAuthKey #-}
{-# DEPRECATED authKey "Use generic-lens or generic-optics with 'authKey' instead"  #-}

-- | The Direct Connect endpoint on which the virtual interface terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAwsDeviceV2 :: Lens.Lens' VirtualInterface (Core.Maybe Types.AwsDeviceV2)
viAwsDeviceV2 = Lens.field @"awsDeviceV2"
{-# INLINEABLE viAwsDeviceV2 #-}
{-# DEPRECATED awsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead"  #-}

-- | The BGP peers configured on this virtual interface.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viBgpPeers :: Lens.Lens' VirtualInterface (Core.Maybe [Types.BGPPeer])
viBgpPeers = Lens.field @"bgpPeers"
{-# INLINEABLE viBgpPeers #-}
{-# DEPRECATED bgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead"  #-}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viConnectionId :: Lens.Lens' VirtualInterface (Core.Maybe Types.ConnectionId)
viConnectionId = Lens.field @"connectionId"
{-# INLINEABLE viConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCustomerAddress :: Lens.Lens' VirtualInterface (Core.Maybe Types.CustomerAddress)
viCustomerAddress = Lens.field @"customerAddress"
{-# INLINEABLE viCustomerAddress #-}
{-# DEPRECATED customerAddress "Use generic-lens or generic-optics with 'customerAddress' instead"  #-}

-- | The customer router configuration.
--
-- /Note:/ Consider using 'customerRouterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCustomerRouterConfig :: Lens.Lens' VirtualInterface (Core.Maybe Types.CustomerRouterConfig)
viCustomerRouterConfig = Lens.field @"customerRouterConfig"
{-# INLINEABLE viCustomerRouterConfig #-}
{-# DEPRECATED customerRouterConfig "Use generic-lens or generic-optics with 'customerRouterConfig' instead"  #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viDirectConnectGatewayId :: Lens.Lens' VirtualInterface (Core.Maybe Types.DirectConnectGatewayId)
viDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE viDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viJumboFrameCapable :: Lens.Lens' VirtualInterface (Core.Maybe Core.Bool)
viJumboFrameCapable = Lens.field @"jumboFrameCapable"
{-# INLINEABLE viJumboFrameCapable #-}
{-# DEPRECATED jumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead"  #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viLocation :: Lens.Lens' VirtualInterface (Core.Maybe Types.LocationCode)
viLocation = Lens.field @"location"
{-# INLINEABLE viLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viMtu :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
viMtu = Lens.field @"mtu"
{-# INLINEABLE viMtu #-}
{-# DEPRECATED mtu "Use generic-lens or generic-optics with 'mtu' instead"  #-}

-- | The ID of the AWS account that owns the virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viOwnerAccount :: Lens.Lens' VirtualInterface (Core.Maybe Types.OwnerAccount)
viOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE viOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | The AWS Region where the virtual interface is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viRegion :: Lens.Lens' VirtualInterface (Core.Maybe Types.Region)
viRegion = Lens.field @"region"
{-# INLINEABLE viRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- /Note:/ Consider using 'routeFilterPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viRouteFilterPrefixes :: Lens.Lens' VirtualInterface (Core.Maybe [Types.RouteFilterPrefix])
viRouteFilterPrefixes = Lens.field @"routeFilterPrefixes"
{-# INLINEABLE viRouteFilterPrefixes #-}
{-# DEPRECATED routeFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead"  #-}

-- | The tags associated with the virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viTags :: Lens.Lens' VirtualInterface (Core.Maybe (Core.NonEmpty Types.Tag))
viTags = Lens.field @"tags"
{-# INLINEABLE viTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualGatewayId :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualGatewayId)
viVirtualGatewayId = Lens.field @"virtualGatewayId"
{-# INLINEABLE viVirtualGatewayId #-}
{-# DEPRECATED virtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead"  #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceId :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualInterfaceId)
viVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE viVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceName :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualInterfaceName)
viVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# INLINEABLE viVirtualInterfaceName #-}
{-# DEPRECATED virtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead"  #-}

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
{-# INLINEABLE viVirtualInterfaceState #-}
{-# DEPRECATED virtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead"  #-}

-- | The type of virtual interface. The possible values are @private@ and @public@ .
--
-- /Note:/ Consider using 'virtualInterfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceType :: Lens.Lens' VirtualInterface (Core.Maybe Types.VirtualInterfaceType)
viVirtualInterfaceType = Lens.field @"virtualInterfaceType"
{-# INLINEABLE viVirtualInterfaceType #-}
{-# DEPRECATED virtualInterfaceType "Use generic-lens or generic-optics with 'virtualInterfaceType' instead"  #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVlan :: Lens.Lens' VirtualInterface (Core.Maybe Core.Int)
viVlan = Lens.field @"vlan"
{-# INLINEABLE viVlan #-}
{-# DEPRECATED vlan "Use generic-lens or generic-optics with 'vlan' instead"  #-}

instance Core.FromJSON VirtualInterface where
        parseJSON
          = Core.withObject "VirtualInterface" Core.$
              \ x ->
                VirtualInterface' Core.<$>
                  (x Core..:? "addressFamily") Core.<*> x Core..:? "amazonAddress"
                    Core.<*> x Core..:? "amazonSideAsn"
                    Core.<*> x Core..:? "asn"
                    Core.<*> x Core..:? "authKey"
                    Core.<*> x Core..:? "awsDeviceV2"
                    Core.<*> x Core..:? "bgpPeers"
                    Core.<*> x Core..:? "connectionId"
                    Core.<*> x Core..:? "customerAddress"
                    Core.<*> x Core..:? "customerRouterConfig"
                    Core.<*> x Core..:? "directConnectGatewayId"
                    Core.<*> x Core..:? "jumboFrameCapable"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "mtu"
                    Core.<*> x Core..:? "ownerAccount"
                    Core.<*> x Core..:? "region"
                    Core.<*> x Core..:? "routeFilterPrefixes"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "virtualGatewayId"
                    Core.<*> x Core..:? "virtualInterfaceId"
                    Core.<*> x Core..:? "virtualInterfaceName"
                    Core.<*> x Core..:? "virtualInterfaceState"
                    Core.<*> x Core..:? "virtualInterfaceType"
                    Core.<*> x Core..:? "vlan"

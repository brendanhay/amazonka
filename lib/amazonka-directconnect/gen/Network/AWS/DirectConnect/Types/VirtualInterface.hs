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
    viBgpPeers,
    viVirtualGatewayId,
    viMtu,
    viRouteFilterPrefixes,
    viCustomerAddress,
    viVlan,
    viLocation,
    viAmazonAddress,
    viAddressFamily,
    viVirtualInterfaceState,
    viConnectionId,
    viDirectConnectGatewayId,
    viAmazonSideASN,
    viVirtualInterfaceType,
    viAsn,
    viAuthKey,
    viJumboFrameCapable,
    viCustomerRouterConfig,
    viOwnerAccount,
    viRegion,
    viVirtualInterfaceName,
    viAwsDeviceV2,
    viVirtualInterfaceId,
    viTags,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.BGPPeer
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.DirectConnect.Types.VirtualInterfaceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a virtual interface.
--
-- /See:/ 'mkVirtualInterface' smart constructor.
data VirtualInterface = VirtualInterface'
  { -- | The BGP peers configured on this virtual interface.
    bgpPeers :: Lude.Maybe [BGPPeer],
    -- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
    virtualGatewayId :: Lude.Maybe Lude.Text,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Lude.Maybe Lude.Int,
    -- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
    routeFilterPrefixes :: Lude.Maybe [RouteFilterPrefix],
    -- | The IP address assigned to the customer interface.
    customerAddress :: Lude.Maybe Lude.Text,
    -- | The ID of the VLAN.
    vlan :: Lude.Maybe Lude.Int,
    -- | The location of the connection.
    location :: Lude.Maybe Lude.Text,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Lude.Maybe Lude.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Lude.Maybe AddressFamily,
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
    virtualInterfaceState :: Lude.Maybe VirtualInterfaceState,
    -- | The ID of the connection.
    connectionId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text,
    -- | The autonomous system number (ASN) for the Amazon side of the connection.
    amazonSideASN :: Lude.Maybe Lude.Integer,
    -- | The type of virtual interface. The possible values are @private@ and @public@ .
    virtualInterfaceType :: Lude.Maybe Lude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Lude.Maybe Lude.Int,
    -- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Lude.Maybe Lude.Text,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Lude.Maybe Lude.Bool,
    -- | The customer router configuration.
    customerRouterConfig :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS account that owns the virtual interface.
    ownerAccount :: Lude.Maybe Lude.Text,
    -- | The AWS Region where the virtual interface is located.
    region :: Lude.Maybe Lude.Text,
    -- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Lude.Maybe Lude.Text,
    -- | The Direct Connect endpoint on which the virtual interface terminates.
    awsDeviceV2 :: Lude.Maybe Lude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Lude.Maybe Lude.Text,
    -- | The tags associated with the virtual interface.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VirtualInterface' with the minimum fields required to make a request.
--
-- * 'bgpPeers' - The BGP peers configured on this virtual interface.
-- * 'virtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual interfaces.
-- * 'mtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
-- * 'routeFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
-- * 'customerAddress' - The IP address assigned to the customer interface.
-- * 'vlan' - The ID of the VLAN.
-- * 'location' - The location of the connection.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'virtualInterfaceState' - The state of the virtual interface. The following are the possible values:
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
-- * 'connectionId' - The ID of the connection.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'amazonSideASN' - The autonomous system number (ASN) for the Amazon side of the connection.
-- * 'virtualInterfaceType' - The type of virtual interface. The possible values are @private@ and @public@ .
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
-- * 'customerRouterConfig' - The customer router configuration.
-- * 'ownerAccount' - The ID of the AWS account that owns the virtual interface.
-- * 'region' - The AWS Region where the virtual interface is located.
-- * 'virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
-- * 'awsDeviceV2' - The Direct Connect endpoint on which the virtual interface terminates.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
-- * 'tags' - The tags associated with the virtual interface.
mkVirtualInterface ::
  VirtualInterface
mkVirtualInterface =
  VirtualInterface'
    { bgpPeers = Lude.Nothing,
      virtualGatewayId = Lude.Nothing,
      mtu = Lude.Nothing,
      routeFilterPrefixes = Lude.Nothing,
      customerAddress = Lude.Nothing,
      vlan = Lude.Nothing,
      location = Lude.Nothing,
      amazonAddress = Lude.Nothing,
      addressFamily = Lude.Nothing,
      virtualInterfaceState = Lude.Nothing,
      connectionId = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      amazonSideASN = Lude.Nothing,
      virtualInterfaceType = Lude.Nothing,
      asn = Lude.Nothing,
      authKey = Lude.Nothing,
      jumboFrameCapable = Lude.Nothing,
      customerRouterConfig = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      region = Lude.Nothing,
      virtualInterfaceName = Lude.Nothing,
      awsDeviceV2 = Lude.Nothing,
      virtualInterfaceId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The BGP peers configured on this virtual interface.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viBgpPeers :: Lens.Lens' VirtualInterface (Lude.Maybe [BGPPeer])
viBgpPeers = Lens.lens (bgpPeers :: VirtualInterface -> Lude.Maybe [BGPPeer]) (\s a -> s {bgpPeers = a} :: VirtualInterface)
{-# DEPRECATED viBgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead." #-}

-- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualGatewayId :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viVirtualGatewayId = Lens.lens (virtualGatewayId :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: VirtualInterface)
{-# DEPRECATED viVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viMtu :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Int)
viMtu = Lens.lens (mtu :: VirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {mtu = a} :: VirtualInterface)
{-# DEPRECATED viMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- /Note:/ Consider using 'routeFilterPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viRouteFilterPrefixes :: Lens.Lens' VirtualInterface (Lude.Maybe [RouteFilterPrefix])
viRouteFilterPrefixes = Lens.lens (routeFilterPrefixes :: VirtualInterface -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {routeFilterPrefixes = a} :: VirtualInterface)
{-# DEPRECATED viRouteFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCustomerAddress :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viCustomerAddress = Lens.lens (customerAddress :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: VirtualInterface)
{-# DEPRECATED viCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVlan :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Int)
viVlan = Lens.lens (vlan :: VirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {vlan = a} :: VirtualInterface)
{-# DEPRECATED viVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viLocation :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viLocation = Lens.lens (location :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: VirtualInterface)
{-# DEPRECATED viLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAmazonAddress :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viAmazonAddress = Lens.lens (amazonAddress :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: VirtualInterface)
{-# DEPRECATED viAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAddressFamily :: Lens.Lens' VirtualInterface (Lude.Maybe AddressFamily)
viAddressFamily = Lens.lens (addressFamily :: VirtualInterface -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: VirtualInterface)
{-# DEPRECATED viAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

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
viVirtualInterfaceState :: Lens.Lens' VirtualInterface (Lude.Maybe VirtualInterfaceState)
viVirtualInterfaceState = Lens.lens (virtualInterfaceState :: VirtualInterface -> Lude.Maybe VirtualInterfaceState) (\s a -> s {virtualInterfaceState = a} :: VirtualInterface)
{-# DEPRECATED viVirtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead." #-}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viConnectionId :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viConnectionId = Lens.lens (connectionId :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: VirtualInterface)
{-# DEPRECATED viConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viDirectConnectGatewayId :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: VirtualInterface)
{-# DEPRECATED viDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The autonomous system number (ASN) for the Amazon side of the connection.
--
-- /Note:/ Consider using 'amazonSideASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAmazonSideASN :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Integer)
viAmazonSideASN = Lens.lens (amazonSideASN :: VirtualInterface -> Lude.Maybe Lude.Integer) (\s a -> s {amazonSideASN = a} :: VirtualInterface)
{-# DEPRECATED viAmazonSideASN "Use generic-lens or generic-optics with 'amazonSideASN' instead." #-}

-- | The type of virtual interface. The possible values are @private@ and @public@ .
--
-- /Note:/ Consider using 'virtualInterfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceType :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viVirtualInterfaceType = Lens.lens (virtualInterfaceType :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceType = a} :: VirtualInterface)
{-# DEPRECATED viVirtualInterfaceType "Use generic-lens or generic-optics with 'virtualInterfaceType' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAsn :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Int)
viAsn = Lens.lens (asn :: VirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {asn = a} :: VirtualInterface)
{-# DEPRECATED viAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAuthKey :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viAuthKey = Lens.lens (authKey :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: VirtualInterface)
{-# DEPRECATED viAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viJumboFrameCapable :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Bool)
viJumboFrameCapable = Lens.lens (jumboFrameCapable :: VirtualInterface -> Lude.Maybe Lude.Bool) (\s a -> s {jumboFrameCapable = a} :: VirtualInterface)
{-# DEPRECATED viJumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead." #-}

-- | The customer router configuration.
--
-- /Note:/ Consider using 'customerRouterConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCustomerRouterConfig :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viCustomerRouterConfig = Lens.lens (customerRouterConfig :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {customerRouterConfig = a} :: VirtualInterface)
{-# DEPRECATED viCustomerRouterConfig "Use generic-lens or generic-optics with 'customerRouterConfig' instead." #-}

-- | The ID of the AWS account that owns the virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viOwnerAccount :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viOwnerAccount = Lens.lens (ownerAccount :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: VirtualInterface)
{-# DEPRECATED viOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The AWS Region where the virtual interface is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viRegion :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viRegion = Lens.lens (region :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: VirtualInterface)
{-# DEPRECATED viRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceName :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viVirtualInterfaceName = Lens.lens (virtualInterfaceName :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceName = a} :: VirtualInterface)
{-# DEPRECATED viVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The Direct Connect endpoint on which the virtual interface terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viAwsDeviceV2 :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viAwsDeviceV2 = Lens.lens (awsDeviceV2 :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {awsDeviceV2 = a} :: VirtualInterface)
{-# DEPRECATED viAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVirtualInterfaceId :: Lens.Lens' VirtualInterface (Lude.Maybe Lude.Text)
viVirtualInterfaceId = Lens.lens (virtualInterfaceId :: VirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceId = a} :: VirtualInterface)
{-# DEPRECATED viVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

-- | The tags associated with the virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viTags :: Lens.Lens' VirtualInterface (Lude.Maybe (Lude.NonEmpty Tag))
viTags = Lens.lens (tags :: VirtualInterface -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: VirtualInterface)
{-# DEPRECATED viTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON VirtualInterface where
  parseJSON =
    Lude.withObject
      "VirtualInterface"
      ( \x ->
          VirtualInterface'
            Lude.<$> (x Lude..:? "bgpPeers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "virtualGatewayId")
            Lude.<*> (x Lude..:? "mtu")
            Lude.<*> (x Lude..:? "routeFilterPrefixes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "customerAddress")
            Lude.<*> (x Lude..:? "vlan")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "amazonAddress")
            Lude.<*> (x Lude..:? "addressFamily")
            Lude.<*> (x Lude..:? "virtualInterfaceState")
            Lude.<*> (x Lude..:? "connectionId")
            Lude.<*> (x Lude..:? "directConnectGatewayId")
            Lude.<*> (x Lude..:? "amazonSideAsn")
            Lude.<*> (x Lude..:? "virtualInterfaceType")
            Lude.<*> (x Lude..:? "asn")
            Lude.<*> (x Lude..:? "authKey")
            Lude.<*> (x Lude..:? "jumboFrameCapable")
            Lude.<*> (x Lude..:? "customerRouterConfig")
            Lude.<*> (x Lude..:? "ownerAccount")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "virtualInterfaceName")
            Lude.<*> (x Lude..:? "awsDeviceV2")
            Lude.<*> (x Lude..:? "virtualInterfaceId")
            Lude.<*> (x Lude..:? "tags")
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Product where

import Network.AWS.DirectConnect.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a BGP peer.
--
--
--
-- /See:/ 'bgpPeer' smart constructor.
data BGPPeer = BGPPeer'
  { _bpCustomerAddress :: !(Maybe Text)
  , _bpAmazonAddress   :: !(Maybe Text)
  , _bpAddressFamily   :: !(Maybe AddressFamily)
  , _bpBgpStatus       :: !(Maybe BGPStatus)
  , _bpAsn             :: !(Maybe Int)
  , _bpAuthKey         :: !(Maybe Text)
  , _bpBgpPeerId       :: !(Maybe Text)
  , _bpBgpPeerState    :: !(Maybe BGPPeerState)
  , _bpAwsDeviceV2     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BGPPeer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'bpAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'bpAddressFamily' - The address family for the BGP peer.
--
-- * 'bpBgpStatus' - The status of the BGP peer. The following are the possible values:     * @up@ : The BGP peer is established. This state does not indicate the state of the routing function. Ensure that you are receiving routes over the BGP session.     * @down@ : The BGP peer is down.     * @unknown@ : The BGP peer status is not available.
--
-- * 'bpAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- * 'bpAuthKey' - The authentication key for BGP configuration.
--
-- * 'bpBgpPeerId' - The ID of the BGP peer.
--
-- * 'bpBgpPeerState' - The state of the BGP peer. The following are the possible values:     * @verifying@ : The BGP peering addresses or ASN require validation before the BGP peer can be created. This state applies only to public virtual interfaces.     * @pending@ : The BGP peer is created, and remains in this state until it is ready to be established.     * @available@ : The BGP peer is ready to be established.     * @deleting@ : The BGP peer is being deleted.     * @deleted@ : The BGP peer is deleted and cannot be established.
--
-- * 'bpAwsDeviceV2' - The Direct Connect endpoint on which the BGP peer terminates.
bgpPeer
    :: BGPPeer
bgpPeer =
  BGPPeer'
    { _bpCustomerAddress = Nothing
    , _bpAmazonAddress = Nothing
    , _bpAddressFamily = Nothing
    , _bpBgpStatus = Nothing
    , _bpAsn = Nothing
    , _bpAuthKey = Nothing
    , _bpBgpPeerId = Nothing
    , _bpBgpPeerState = Nothing
    , _bpAwsDeviceV2 = Nothing
    }


-- | The IP address assigned to the customer interface.
bpCustomerAddress :: Lens' BGPPeer (Maybe Text)
bpCustomerAddress = lens _bpCustomerAddress (\ s a -> s{_bpCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
bpAmazonAddress :: Lens' BGPPeer (Maybe Text)
bpAmazonAddress = lens _bpAmazonAddress (\ s a -> s{_bpAmazonAddress = a})

-- | The address family for the BGP peer.
bpAddressFamily :: Lens' BGPPeer (Maybe AddressFamily)
bpAddressFamily = lens _bpAddressFamily (\ s a -> s{_bpAddressFamily = a})

-- | The status of the BGP peer. The following are the possible values:     * @up@ : The BGP peer is established. This state does not indicate the state of the routing function. Ensure that you are receiving routes over the BGP session.     * @down@ : The BGP peer is down.     * @unknown@ : The BGP peer status is not available.
bpBgpStatus :: Lens' BGPPeer (Maybe BGPStatus)
bpBgpStatus = lens _bpBgpStatus (\ s a -> s{_bpBgpStatus = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
bpAsn :: Lens' BGPPeer (Maybe Int)
bpAsn = lens _bpAsn (\ s a -> s{_bpAsn = a})

-- | The authentication key for BGP configuration.
bpAuthKey :: Lens' BGPPeer (Maybe Text)
bpAuthKey = lens _bpAuthKey (\ s a -> s{_bpAuthKey = a})

-- | The ID of the BGP peer.
bpBgpPeerId :: Lens' BGPPeer (Maybe Text)
bpBgpPeerId = lens _bpBgpPeerId (\ s a -> s{_bpBgpPeerId = a})

-- | The state of the BGP peer. The following are the possible values:     * @verifying@ : The BGP peering addresses or ASN require validation before the BGP peer can be created. This state applies only to public virtual interfaces.     * @pending@ : The BGP peer is created, and remains in this state until it is ready to be established.     * @available@ : The BGP peer is ready to be established.     * @deleting@ : The BGP peer is being deleted.     * @deleted@ : The BGP peer is deleted and cannot be established.
bpBgpPeerState :: Lens' BGPPeer (Maybe BGPPeerState)
bpBgpPeerState = lens _bpBgpPeerState (\ s a -> s{_bpBgpPeerState = a})

-- | The Direct Connect endpoint on which the BGP peer terminates.
bpAwsDeviceV2 :: Lens' BGPPeer (Maybe Text)
bpAwsDeviceV2 = lens _bpAwsDeviceV2 (\ s a -> s{_bpAwsDeviceV2 = a})

instance FromJSON BGPPeer where
        parseJSON
          = withObject "BGPPeer"
              (\ x ->
                 BGPPeer' <$>
                   (x .:? "customerAddress") <*> (x .:? "amazonAddress")
                     <*> (x .:? "addressFamily")
                     <*> (x .:? "bgpStatus")
                     <*> (x .:? "asn")
                     <*> (x .:? "authKey")
                     <*> (x .:? "bgpPeerId")
                     <*> (x .:? "bgpPeerState")
                     <*> (x .:? "awsDeviceV2"))

instance Hashable BGPPeer where

instance NFData BGPPeer where

-- | Information about an AWS Direct Connect connection.
--
--
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
  { _cLagId                :: !(Maybe Text)
  , _cVlan                 :: !(Maybe Int)
  , _cLocation             :: !(Maybe Text)
  , _cAwsDevice            :: !(Maybe Text)
  , _cHasLogicalRedundancy :: !(Maybe HasLogicalRedundancy)
  , _cConnectionId         :: !(Maybe Text)
  , _cLoaIssueTime         :: !(Maybe POSIX)
  , _cPartnerName          :: !(Maybe Text)
  , _cConnectionName       :: !(Maybe Text)
  , _cBandwidth            :: !(Maybe Text)
  , _cJumboFrameCapable    :: !(Maybe Bool)
  , _cOwnerAccount         :: !(Maybe Text)
  , _cRegion               :: !(Maybe Text)
  , _cAwsDeviceV2          :: !(Maybe Text)
  , _cConnectionState      :: !(Maybe ConnectionState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLagId' - The ID of the LAG.
--
-- * 'cVlan' - The ID of the VLAN.
--
-- * 'cLocation' - The location of the connection.
--
-- * 'cAwsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- * 'cHasLogicalRedundancy' - Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- * 'cConnectionId' - The ID of the connection.
--
-- * 'cLoaIssueTime' - The time of the most recent call to 'DescribeLoa' for this connection.
--
-- * 'cPartnerName' - The name of the AWS Direct Connect service provider associated with the connection.
--
-- * 'cConnectionName' - The name of the connection.
--
-- * 'cBandwidth' - The bandwidth of the connection.
--
-- * 'cJumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- * 'cOwnerAccount' - The ID of the AWS account that owns the connection.
--
-- * 'cRegion' - The AWS Region where the connection is located.
--
-- * 'cAwsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
--
-- * 'cConnectionState' - The state of the connection. The following are the possible values:     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The connection has been approved and is being initialized.     * @available@ : The network link is up and the connection is ready for use.     * @down@ : The network link is down.     * @deleting@ : The connection is being deleted.     * @deleted@ : The connection has been deleted.     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.     * @unknown@ : The state of the connection is not available.
connection
    :: Connection
connection =
  Connection'
    { _cLagId = Nothing
    , _cVlan = Nothing
    , _cLocation = Nothing
    , _cAwsDevice = Nothing
    , _cHasLogicalRedundancy = Nothing
    , _cConnectionId = Nothing
    , _cLoaIssueTime = Nothing
    , _cPartnerName = Nothing
    , _cConnectionName = Nothing
    , _cBandwidth = Nothing
    , _cJumboFrameCapable = Nothing
    , _cOwnerAccount = Nothing
    , _cRegion = Nothing
    , _cAwsDeviceV2 = Nothing
    , _cConnectionState = Nothing
    }


-- | The ID of the LAG.
cLagId :: Lens' Connection (Maybe Text)
cLagId = lens _cLagId (\ s a -> s{_cLagId = a})

-- | The ID of the VLAN.
cVlan :: Lens' Connection (Maybe Int)
cVlan = lens _cVlan (\ s a -> s{_cVlan = a})

-- | The location of the connection.
cLocation :: Lens' Connection (Maybe Text)
cLocation = lens _cLocation (\ s a -> s{_cLocation = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
cAwsDevice :: Lens' Connection (Maybe Text)
cAwsDevice = lens _cAwsDevice (\ s a -> s{_cAwsDevice = a})

-- | Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
cHasLogicalRedundancy :: Lens' Connection (Maybe HasLogicalRedundancy)
cHasLogicalRedundancy = lens _cHasLogicalRedundancy (\ s a -> s{_cHasLogicalRedundancy = a})

-- | The ID of the connection.
cConnectionId :: Lens' Connection (Maybe Text)
cConnectionId = lens _cConnectionId (\ s a -> s{_cConnectionId = a})

-- | The time of the most recent call to 'DescribeLoa' for this connection.
cLoaIssueTime :: Lens' Connection (Maybe UTCTime)
cLoaIssueTime = lens _cLoaIssueTime (\ s a -> s{_cLoaIssueTime = a}) . mapping _Time

-- | The name of the AWS Direct Connect service provider associated with the connection.
cPartnerName :: Lens' Connection (Maybe Text)
cPartnerName = lens _cPartnerName (\ s a -> s{_cPartnerName = a})

-- | The name of the connection.
cConnectionName :: Lens' Connection (Maybe Text)
cConnectionName = lens _cConnectionName (\ s a -> s{_cConnectionName = a})

-- | The bandwidth of the connection.
cBandwidth :: Lens' Connection (Maybe Text)
cBandwidth = lens _cBandwidth (\ s a -> s{_cBandwidth = a})

-- | Indicates whether jumbo frames (9001 MTU) are supported.
cJumboFrameCapable :: Lens' Connection (Maybe Bool)
cJumboFrameCapable = lens _cJumboFrameCapable (\ s a -> s{_cJumboFrameCapable = a})

-- | The ID of the AWS account that owns the connection.
cOwnerAccount :: Lens' Connection (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\ s a -> s{_cOwnerAccount = a})

-- | The AWS Region where the connection is located.
cRegion :: Lens' Connection (Maybe Text)
cRegion = lens _cRegion (\ s a -> s{_cRegion = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
cAwsDeviceV2 :: Lens' Connection (Maybe Text)
cAwsDeviceV2 = lens _cAwsDeviceV2 (\ s a -> s{_cAwsDeviceV2 = a})

-- | The state of the connection. The following are the possible values:     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The connection has been approved and is being initialized.     * @available@ : The network link is up and the connection is ready for use.     * @down@ : The network link is down.     * @deleting@ : The connection is being deleted.     * @deleted@ : The connection has been deleted.     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.     * @unknown@ : The state of the connection is not available.
cConnectionState :: Lens' Connection (Maybe ConnectionState)
cConnectionState = lens _cConnectionState (\ s a -> s{_cConnectionState = a})

instance FromJSON Connection where
        parseJSON
          = withObject "Connection"
              (\ x ->
                 Connection' <$>
                   (x .:? "lagId") <*> (x .:? "vlan") <*>
                     (x .:? "location")
                     <*> (x .:? "awsDevice")
                     <*> (x .:? "hasLogicalRedundancy")
                     <*> (x .:? "connectionId")
                     <*> (x .:? "loaIssueTime")
                     <*> (x .:? "partnerName")
                     <*> (x .:? "connectionName")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "jumboFrameCapable")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "region")
                     <*> (x .:? "awsDeviceV2")
                     <*> (x .:? "connectionState"))

instance Hashable Connection where

instance NFData Connection where

-- | /See:/ 'connections' smart constructor.
newtype Connections = Connections'
  { _cConnections :: Maybe [Connection]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConnections' - The connections.
connections
    :: Connections
connections = Connections' {_cConnections = Nothing}


-- | The connections.
cConnections :: Lens' Connections [Connection]
cConnections = lens _cConnections (\ s a -> s{_cConnections = a}) . _Default . _Coerce

instance FromJSON Connections where
        parseJSON
          = withObject "Connections"
              (\ x ->
                 Connections' <$> (x .:? "connections" .!= mempty))

instance Hashable Connections where

instance NFData Connections where

-- | Information about a Direct Connect gateway, which enables you to connect virtual interfaces and virtual private gateways.
--
--
--
-- /See:/ 'directConnectGateway' smart constructor.
data DirectConnectGateway = DirectConnectGateway'
  { _dcgDirectConnectGatewayId    :: !(Maybe Text)
  , _dcgStateChangeError          :: !(Maybe Text)
  , _dcgAmazonSideASN             :: !(Maybe Integer)
  , _dcgDirectConnectGatewayName  :: !(Maybe Text)
  , _dcgDirectConnectGatewayState :: !(Maybe DirectConnectGatewayState)
  , _dcgOwnerAccount              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectConnectGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'dcgStateChangeError' - The error message if the state of an object failed to advance.
--
-- * 'dcgAmazonSideASN' - The autonomous system number (ASN) for the Amazon side of the connection.
--
-- * 'dcgDirectConnectGatewayName' - The name of the Direct Connect gateway.
--
-- * 'dcgDirectConnectGatewayState' - The state of the Direct Connect gateway. The following are the possible values:     * @pending@ : The initial state after calling 'CreateDirectConnectGateway' .     * @available@ : The Direct Connect gateway is ready for use.     * @deleting@ : The initial state after calling 'DeleteDirectConnectGateway' .     * @deleted@ : The Direct Connect gateway is deleted and cannot pass traffic.
--
-- * 'dcgOwnerAccount' - The ID of the AWS account that owns the Direct Connect gateway.
directConnectGateway
    :: DirectConnectGateway
directConnectGateway =
  DirectConnectGateway'
    { _dcgDirectConnectGatewayId = Nothing
    , _dcgStateChangeError = Nothing
    , _dcgAmazonSideASN = Nothing
    , _dcgDirectConnectGatewayName = Nothing
    , _dcgDirectConnectGatewayState = Nothing
    , _dcgOwnerAccount = Nothing
    }


-- | The ID of the Direct Connect gateway.
dcgDirectConnectGatewayId :: Lens' DirectConnectGateway (Maybe Text)
dcgDirectConnectGatewayId = lens _dcgDirectConnectGatewayId (\ s a -> s{_dcgDirectConnectGatewayId = a})

-- | The error message if the state of an object failed to advance.
dcgStateChangeError :: Lens' DirectConnectGateway (Maybe Text)
dcgStateChangeError = lens _dcgStateChangeError (\ s a -> s{_dcgStateChangeError = a})

-- | The autonomous system number (ASN) for the Amazon side of the connection.
dcgAmazonSideASN :: Lens' DirectConnectGateway (Maybe Integer)
dcgAmazonSideASN = lens _dcgAmazonSideASN (\ s a -> s{_dcgAmazonSideASN = a})

-- | The name of the Direct Connect gateway.
dcgDirectConnectGatewayName :: Lens' DirectConnectGateway (Maybe Text)
dcgDirectConnectGatewayName = lens _dcgDirectConnectGatewayName (\ s a -> s{_dcgDirectConnectGatewayName = a})

-- | The state of the Direct Connect gateway. The following are the possible values:     * @pending@ : The initial state after calling 'CreateDirectConnectGateway' .     * @available@ : The Direct Connect gateway is ready for use.     * @deleting@ : The initial state after calling 'DeleteDirectConnectGateway' .     * @deleted@ : The Direct Connect gateway is deleted and cannot pass traffic.
dcgDirectConnectGatewayState :: Lens' DirectConnectGateway (Maybe DirectConnectGatewayState)
dcgDirectConnectGatewayState = lens _dcgDirectConnectGatewayState (\ s a -> s{_dcgDirectConnectGatewayState = a})

-- | The ID of the AWS account that owns the Direct Connect gateway.
dcgOwnerAccount :: Lens' DirectConnectGateway (Maybe Text)
dcgOwnerAccount = lens _dcgOwnerAccount (\ s a -> s{_dcgOwnerAccount = a})

instance FromJSON DirectConnectGateway where
        parseJSON
          = withObject "DirectConnectGateway"
              (\ x ->
                 DirectConnectGateway' <$>
                   (x .:? "directConnectGatewayId") <*>
                     (x .:? "stateChangeError")
                     <*> (x .:? "amazonSideAsn")
                     <*> (x .:? "directConnectGatewayName")
                     <*> (x .:? "directConnectGatewayState")
                     <*> (x .:? "ownerAccount"))

instance Hashable DirectConnectGateway where

instance NFData DirectConnectGateway where

-- | Information about an association between a Direct Connect gateway and a virtual private gateway.
--
--
--
-- /See:/ 'directConnectGatewayAssociation' smart constructor.
data DirectConnectGatewayAssociation = DirectConnectGatewayAssociation'
  { _dcgaVirtualGatewayId :: !(Maybe Text)
  , _dcgaDirectConnectGatewayId :: !(Maybe Text)
  , _dcgaVirtualGatewayOwnerAccount :: !(Maybe Text)
  , _dcgaStateChangeError :: !(Maybe Text)
  , _dcgaVirtualGatewayRegion :: !(Maybe Text)
  , _dcgaAssociationState :: !(Maybe DirectConnectGatewayAssociationState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgaVirtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual interfaces.
--
-- * 'dcgaDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'dcgaVirtualGatewayOwnerAccount' - The ID of the AWS account that owns the virtual private gateway.
--
-- * 'dcgaStateChangeError' - The error message if the state of an object failed to advance.
--
-- * 'dcgaVirtualGatewayRegion' - The AWS Region where the virtual private gateway is located.
--
-- * 'dcgaAssociationState' - The state of the association. The following are the possible values:     * @associating@ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .     * @associated@ : The Direct Connect gateway and virtual private gateway are successfully associated and ready to pass traffic.     * @disassociating@ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .     * @disassociated@ : The virtual private gateway is disassociated from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual private gateway is stopped.
directConnectGatewayAssociation
    :: DirectConnectGatewayAssociation
directConnectGatewayAssociation =
  DirectConnectGatewayAssociation'
    { _dcgaVirtualGatewayId = Nothing
    , _dcgaDirectConnectGatewayId = Nothing
    , _dcgaVirtualGatewayOwnerAccount = Nothing
    , _dcgaStateChangeError = Nothing
    , _dcgaVirtualGatewayRegion = Nothing
    , _dcgaAssociationState = Nothing
    }


-- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
dcgaVirtualGatewayId :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaVirtualGatewayId = lens _dcgaVirtualGatewayId (\ s a -> s{_dcgaVirtualGatewayId = a})

-- | The ID of the Direct Connect gateway.
dcgaDirectConnectGatewayId :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaDirectConnectGatewayId = lens _dcgaDirectConnectGatewayId (\ s a -> s{_dcgaDirectConnectGatewayId = a})

-- | The ID of the AWS account that owns the virtual private gateway.
dcgaVirtualGatewayOwnerAccount :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaVirtualGatewayOwnerAccount = lens _dcgaVirtualGatewayOwnerAccount (\ s a -> s{_dcgaVirtualGatewayOwnerAccount = a})

-- | The error message if the state of an object failed to advance.
dcgaStateChangeError :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaStateChangeError = lens _dcgaStateChangeError (\ s a -> s{_dcgaStateChangeError = a})

-- | The AWS Region where the virtual private gateway is located.
dcgaVirtualGatewayRegion :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaVirtualGatewayRegion = lens _dcgaVirtualGatewayRegion (\ s a -> s{_dcgaVirtualGatewayRegion = a})

-- | The state of the association. The following are the possible values:     * @associating@ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .     * @associated@ : The Direct Connect gateway and virtual private gateway are successfully associated and ready to pass traffic.     * @disassociating@ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .     * @disassociated@ : The virtual private gateway is disassociated from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual private gateway is stopped.
dcgaAssociationState :: Lens' DirectConnectGatewayAssociation (Maybe DirectConnectGatewayAssociationState)
dcgaAssociationState = lens _dcgaAssociationState (\ s a -> s{_dcgaAssociationState = a})

instance FromJSON DirectConnectGatewayAssociation
         where
        parseJSON
          = withObject "DirectConnectGatewayAssociation"
              (\ x ->
                 DirectConnectGatewayAssociation' <$>
                   (x .:? "virtualGatewayId") <*>
                     (x .:? "directConnectGatewayId")
                     <*> (x .:? "virtualGatewayOwnerAccount")
                     <*> (x .:? "stateChangeError")
                     <*> (x .:? "virtualGatewayRegion")
                     <*> (x .:? "associationState"))

instance Hashable DirectConnectGatewayAssociation
         where

instance NFData DirectConnectGatewayAssociation where

-- | Information about an attachment between a Direct Connect gateway and a virtual interface.
--
--
--
-- /See:/ 'directConnectGatewayAttachment' smart constructor.
data DirectConnectGatewayAttachment = DirectConnectGatewayAttachment'
  { _dDirectConnectGatewayId :: !(Maybe Text)
  , _dAttachmentState :: !(Maybe DirectConnectGatewayAttachmentState)
  , _dStateChangeError :: !(Maybe Text)
  , _dVirtualInterfaceRegion :: !(Maybe Text)
  , _dVirtualInterfaceOwnerAccount :: !(Maybe Text)
  , _dVirtualInterfaceId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectConnectGatewayAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'dAttachmentState' - The state of the attachment. The following are the possible values:     * @attaching@ : The initial state after a virtual interface is created using the Direct Connect gateway.     * @attached@ : The Direct Connect gateway and virtual interface are attached and ready to pass traffic.     * @detaching@ : The initial state after calling 'DeleteVirtualInterface' .     * @detached@ : The virtual interface is detached from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual interface is stopped.
--
-- * 'dStateChangeError' - The error message if the state of an object failed to advance.
--
-- * 'dVirtualInterfaceRegion' - The AWS Region where the virtual interface is located.
--
-- * 'dVirtualInterfaceOwnerAccount' - The ID of the AWS account that owns the virtual interface.
--
-- * 'dVirtualInterfaceId' - The ID of the virtual interface.
directConnectGatewayAttachment
    :: DirectConnectGatewayAttachment
directConnectGatewayAttachment =
  DirectConnectGatewayAttachment'
    { _dDirectConnectGatewayId = Nothing
    , _dAttachmentState = Nothing
    , _dStateChangeError = Nothing
    , _dVirtualInterfaceRegion = Nothing
    , _dVirtualInterfaceOwnerAccount = Nothing
    , _dVirtualInterfaceId = Nothing
    }


-- | The ID of the Direct Connect gateway.
dDirectConnectGatewayId :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dDirectConnectGatewayId = lens _dDirectConnectGatewayId (\ s a -> s{_dDirectConnectGatewayId = a})

-- | The state of the attachment. The following are the possible values:     * @attaching@ : The initial state after a virtual interface is created using the Direct Connect gateway.     * @attached@ : The Direct Connect gateway and virtual interface are attached and ready to pass traffic.     * @detaching@ : The initial state after calling 'DeleteVirtualInterface' .     * @detached@ : The virtual interface is detached from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual interface is stopped.
dAttachmentState :: Lens' DirectConnectGatewayAttachment (Maybe DirectConnectGatewayAttachmentState)
dAttachmentState = lens _dAttachmentState (\ s a -> s{_dAttachmentState = a})

-- | The error message if the state of an object failed to advance.
dStateChangeError :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dStateChangeError = lens _dStateChangeError (\ s a -> s{_dStateChangeError = a})

-- | The AWS Region where the virtual interface is located.
dVirtualInterfaceRegion :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceRegion = lens _dVirtualInterfaceRegion (\ s a -> s{_dVirtualInterfaceRegion = a})

-- | The ID of the AWS account that owns the virtual interface.
dVirtualInterfaceOwnerAccount :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceOwnerAccount = lens _dVirtualInterfaceOwnerAccount (\ s a -> s{_dVirtualInterfaceOwnerAccount = a})

-- | The ID of the virtual interface.
dVirtualInterfaceId :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceId = lens _dVirtualInterfaceId (\ s a -> s{_dVirtualInterfaceId = a})

instance FromJSON DirectConnectGatewayAttachment
         where
        parseJSON
          = withObject "DirectConnectGatewayAttachment"
              (\ x ->
                 DirectConnectGatewayAttachment' <$>
                   (x .:? "directConnectGatewayId") <*>
                     (x .:? "attachmentState")
                     <*> (x .:? "stateChangeError")
                     <*> (x .:? "virtualInterfaceRegion")
                     <*> (x .:? "virtualInterfaceOwnerAccount")
                     <*> (x .:? "virtualInterfaceId"))

instance Hashable DirectConnectGatewayAttachment
         where

instance NFData DirectConnectGatewayAttachment where

-- | Information about an interconnect.
--
--
--
-- /See:/ 'interconnect' smart constructor.
data Interconnect = Interconnect'
  { _iLagId                :: !(Maybe Text)
  , _iInterconnectId       :: !(Maybe Text)
  , _iLocation             :: !(Maybe Text)
  , _iInterconnectName     :: !(Maybe Text)
  , _iAwsDevice            :: !(Maybe Text)
  , _iHasLogicalRedundancy :: !(Maybe HasLogicalRedundancy)
  , _iLoaIssueTime         :: !(Maybe POSIX)
  , _iBandwidth            :: !(Maybe Text)
  , _iJumboFrameCapable    :: !(Maybe Bool)
  , _iInterconnectState    :: !(Maybe InterconnectState)
  , _iRegion               :: !(Maybe Text)
  , _iAwsDeviceV2          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Interconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iLagId' - The ID of the LAG.
--
-- * 'iInterconnectId' - The ID of the interconnect.
--
-- * 'iLocation' - The location of the connection.
--
-- * 'iInterconnectName' - The name of the interconnect.
--
-- * 'iAwsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- * 'iHasLogicalRedundancy' - Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
--
-- * 'iLoaIssueTime' - The time of the most recent call to 'DescribeLoa' for this connection.
--
-- * 'iBandwidth' - The bandwidth of the connection.
--
-- * 'iJumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- * 'iInterconnectState' - The state of the interconnect. The following are the possible values:     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The interconnect is approved, and is being initialized.     * @available@ : The network link is up, and the interconnect is ready for use.     * @down@ : The network link is down.     * @deleting@ : The interconnect is being deleted.     * @deleted@ : The interconnect is deleted.     * @unknown@ : The state of the interconnect is not available.
--
-- * 'iRegion' - The AWS Region where the connection is located.
--
-- * 'iAwsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
interconnect
    :: Interconnect
interconnect =
  Interconnect'
    { _iLagId = Nothing
    , _iInterconnectId = Nothing
    , _iLocation = Nothing
    , _iInterconnectName = Nothing
    , _iAwsDevice = Nothing
    , _iHasLogicalRedundancy = Nothing
    , _iLoaIssueTime = Nothing
    , _iBandwidth = Nothing
    , _iJumboFrameCapable = Nothing
    , _iInterconnectState = Nothing
    , _iRegion = Nothing
    , _iAwsDeviceV2 = Nothing
    }


-- | The ID of the LAG.
iLagId :: Lens' Interconnect (Maybe Text)
iLagId = lens _iLagId (\ s a -> s{_iLagId = a})

-- | The ID of the interconnect.
iInterconnectId :: Lens' Interconnect (Maybe Text)
iInterconnectId = lens _iInterconnectId (\ s a -> s{_iInterconnectId = a})

-- | The location of the connection.
iLocation :: Lens' Interconnect (Maybe Text)
iLocation = lens _iLocation (\ s a -> s{_iLocation = a})

-- | The name of the interconnect.
iInterconnectName :: Lens' Interconnect (Maybe Text)
iInterconnectName = lens _iInterconnectName (\ s a -> s{_iInterconnectName = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
iAwsDevice :: Lens' Interconnect (Maybe Text)
iAwsDevice = lens _iAwsDevice (\ s a -> s{_iAwsDevice = a})

-- | Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
iHasLogicalRedundancy :: Lens' Interconnect (Maybe HasLogicalRedundancy)
iHasLogicalRedundancy = lens _iHasLogicalRedundancy (\ s a -> s{_iHasLogicalRedundancy = a})

-- | The time of the most recent call to 'DescribeLoa' for this connection.
iLoaIssueTime :: Lens' Interconnect (Maybe UTCTime)
iLoaIssueTime = lens _iLoaIssueTime (\ s a -> s{_iLoaIssueTime = a}) . mapping _Time

-- | The bandwidth of the connection.
iBandwidth :: Lens' Interconnect (Maybe Text)
iBandwidth = lens _iBandwidth (\ s a -> s{_iBandwidth = a})

-- | Indicates whether jumbo frames (9001 MTU) are supported.
iJumboFrameCapable :: Lens' Interconnect (Maybe Bool)
iJumboFrameCapable = lens _iJumboFrameCapable (\ s a -> s{_iJumboFrameCapable = a})

-- | The state of the interconnect. The following are the possible values:     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The interconnect is approved, and is being initialized.     * @available@ : The network link is up, and the interconnect is ready for use.     * @down@ : The network link is down.     * @deleting@ : The interconnect is being deleted.     * @deleted@ : The interconnect is deleted.     * @unknown@ : The state of the interconnect is not available.
iInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
iInterconnectState = lens _iInterconnectState (\ s a -> s{_iInterconnectState = a})

-- | The AWS Region where the connection is located.
iRegion :: Lens' Interconnect (Maybe Text)
iRegion = lens _iRegion (\ s a -> s{_iRegion = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
iAwsDeviceV2 :: Lens' Interconnect (Maybe Text)
iAwsDeviceV2 = lens _iAwsDeviceV2 (\ s a -> s{_iAwsDeviceV2 = a})

instance FromJSON Interconnect where
        parseJSON
          = withObject "Interconnect"
              (\ x ->
                 Interconnect' <$>
                   (x .:? "lagId") <*> (x .:? "interconnectId") <*>
                     (x .:? "location")
                     <*> (x .:? "interconnectName")
                     <*> (x .:? "awsDevice")
                     <*> (x .:? "hasLogicalRedundancy")
                     <*> (x .:? "loaIssueTime")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "jumboFrameCapable")
                     <*> (x .:? "interconnectState")
                     <*> (x .:? "region")
                     <*> (x .:? "awsDeviceV2"))

instance Hashable Interconnect where

instance NFData Interconnect where

-- | Information about a link aggregation group (LAG).
--
--
--
-- /See:/ 'lag' smart constructor.
data Lag = Lag'
  { _lagLagId                   :: !(Maybe Text)
  , _lagConnectionsBandwidth    :: !(Maybe Text)
  , _lagMinimumLinks            :: !(Maybe Int)
  , _lagLagName                 :: !(Maybe Text)
  , _lagLocation                :: !(Maybe Text)
  , _lagConnections             :: !(Maybe [Connection])
  , _lagAwsDevice               :: !(Maybe Text)
  , _lagHasLogicalRedundancy    :: !(Maybe HasLogicalRedundancy)
  , _lagAllowsHostedConnections :: !(Maybe Bool)
  , _lagNumberOfConnections     :: !(Maybe Int)
  , _lagJumboFrameCapable       :: !(Maybe Bool)
  , _lagLagState                :: !(Maybe LagState)
  , _lagOwnerAccount            :: !(Maybe Text)
  , _lagRegion                  :: !(Maybe Text)
  , _lagAwsDeviceV2             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Lag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lagLagId' - The ID of the LAG.
--
-- * 'lagConnectionsBandwidth' - The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
--
-- * 'lagMinimumLinks' - The minimum number of physical connections that must be operational for the LAG itself to be operational.
--
-- * 'lagLagName' - The name of the LAG.
--
-- * 'lagLocation' - The location of the LAG.
--
-- * 'lagConnections' - The connections bundled by the LAG.
--
-- * 'lagAwsDevice' - The Direct Connect endpoint that hosts the LAG.
--
-- * 'lagHasLogicalRedundancy' - Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- * 'lagAllowsHostedConnections' - Indicates whether the LAG can host other connections.
--
-- * 'lagNumberOfConnections' - The number of physical connections bundled by the LAG, up to a maximum of 10.
--
-- * 'lagJumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- * 'lagLagState' - The state of the LAG. The following are the possible values:     * @requested@ : The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.     * @pending@ : The LAG has been approved and is being initialized.     * @available@ : The network link is established and the LAG is ready for use.     * @down@ : The network link is down.     * @deleting@ : The LAG is being deleted.     * @deleted@ : The LAG is deleted.     * @unknown@ : The state of the LAG is not available.
--
-- * 'lagOwnerAccount' - The ID of the AWS account that owns the LAG.
--
-- * 'lagRegion' - The AWS Region where the connection is located.
--
-- * 'lagAwsDeviceV2' - The Direct Connect endpoint that hosts the LAG.
lag
    :: Lag
lag =
  Lag'
    { _lagLagId = Nothing
    , _lagConnectionsBandwidth = Nothing
    , _lagMinimumLinks = Nothing
    , _lagLagName = Nothing
    , _lagLocation = Nothing
    , _lagConnections = Nothing
    , _lagAwsDevice = Nothing
    , _lagHasLogicalRedundancy = Nothing
    , _lagAllowsHostedConnections = Nothing
    , _lagNumberOfConnections = Nothing
    , _lagJumboFrameCapable = Nothing
    , _lagLagState = Nothing
    , _lagOwnerAccount = Nothing
    , _lagRegion = Nothing
    , _lagAwsDeviceV2 = Nothing
    }


-- | The ID of the LAG.
lagLagId :: Lens' Lag (Maybe Text)
lagLagId = lens _lagLagId (\ s a -> s{_lagLagId = a})

-- | The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
lagConnectionsBandwidth :: Lens' Lag (Maybe Text)
lagConnectionsBandwidth = lens _lagConnectionsBandwidth (\ s a -> s{_lagConnectionsBandwidth = a})

-- | The minimum number of physical connections that must be operational for the LAG itself to be operational.
lagMinimumLinks :: Lens' Lag (Maybe Int)
lagMinimumLinks = lens _lagMinimumLinks (\ s a -> s{_lagMinimumLinks = a})

-- | The name of the LAG.
lagLagName :: Lens' Lag (Maybe Text)
lagLagName = lens _lagLagName (\ s a -> s{_lagLagName = a})

-- | The location of the LAG.
lagLocation :: Lens' Lag (Maybe Text)
lagLocation = lens _lagLocation (\ s a -> s{_lagLocation = a})

-- | The connections bundled by the LAG.
lagConnections :: Lens' Lag [Connection]
lagConnections = lens _lagConnections (\ s a -> s{_lagConnections = a}) . _Default . _Coerce

-- | The Direct Connect endpoint that hosts the LAG.
lagAwsDevice :: Lens' Lag (Maybe Text)
lagAwsDevice = lens _lagAwsDevice (\ s a -> s{_lagAwsDevice = a})

-- | Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
lagHasLogicalRedundancy :: Lens' Lag (Maybe HasLogicalRedundancy)
lagHasLogicalRedundancy = lens _lagHasLogicalRedundancy (\ s a -> s{_lagHasLogicalRedundancy = a})

-- | Indicates whether the LAG can host other connections.
lagAllowsHostedConnections :: Lens' Lag (Maybe Bool)
lagAllowsHostedConnections = lens _lagAllowsHostedConnections (\ s a -> s{_lagAllowsHostedConnections = a})

-- | The number of physical connections bundled by the LAG, up to a maximum of 10.
lagNumberOfConnections :: Lens' Lag (Maybe Int)
lagNumberOfConnections = lens _lagNumberOfConnections (\ s a -> s{_lagNumberOfConnections = a})

-- | Indicates whether jumbo frames (9001 MTU) are supported.
lagJumboFrameCapable :: Lens' Lag (Maybe Bool)
lagJumboFrameCapable = lens _lagJumboFrameCapable (\ s a -> s{_lagJumboFrameCapable = a})

-- | The state of the LAG. The following are the possible values:     * @requested@ : The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.     * @pending@ : The LAG has been approved and is being initialized.     * @available@ : The network link is established and the LAG is ready for use.     * @down@ : The network link is down.     * @deleting@ : The LAG is being deleted.     * @deleted@ : The LAG is deleted.     * @unknown@ : The state of the LAG is not available.
lagLagState :: Lens' Lag (Maybe LagState)
lagLagState = lens _lagLagState (\ s a -> s{_lagLagState = a})

-- | The ID of the AWS account that owns the LAG.
lagOwnerAccount :: Lens' Lag (Maybe Text)
lagOwnerAccount = lens _lagOwnerAccount (\ s a -> s{_lagOwnerAccount = a})

-- | The AWS Region where the connection is located.
lagRegion :: Lens' Lag (Maybe Text)
lagRegion = lens _lagRegion (\ s a -> s{_lagRegion = a})

-- | The Direct Connect endpoint that hosts the LAG.
lagAwsDeviceV2 :: Lens' Lag (Maybe Text)
lagAwsDeviceV2 = lens _lagAwsDeviceV2 (\ s a -> s{_lagAwsDeviceV2 = a})

instance FromJSON Lag where
        parseJSON
          = withObject "Lag"
              (\ x ->
                 Lag' <$>
                   (x .:? "lagId") <*> (x .:? "connectionsBandwidth")
                     <*> (x .:? "minimumLinks")
                     <*> (x .:? "lagName")
                     <*> (x .:? "location")
                     <*> (x .:? "connections" .!= mempty)
                     <*> (x .:? "awsDevice")
                     <*> (x .:? "hasLogicalRedundancy")
                     <*> (x .:? "allowsHostedConnections")
                     <*> (x .:? "numberOfConnections")
                     <*> (x .:? "jumboFrameCapable")
                     <*> (x .:? "lagState")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "region")
                     <*> (x .:? "awsDeviceV2"))

instance Hashable Lag where

instance NFData Lag where

-- | Information about an AWS Direct Connect location.
--
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lAvailablePortSpeeds :: !(Maybe [Text])
  , _lLocationName        :: !(Maybe Text)
  , _lLocationCode        :: !(Maybe Text)
  , _lRegion              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lAvailablePortSpeeds' - The available port speeds for the location.
--
-- * 'lLocationName' - The name of the location. This includes the name of the colocation partner and the physical site of the building.
--
-- * 'lLocationCode' - The code for the location.
--
-- * 'lRegion' - The AWS Region for the location.
location
    :: Location
location =
  Location'
    { _lAvailablePortSpeeds = Nothing
    , _lLocationName = Nothing
    , _lLocationCode = Nothing
    , _lRegion = Nothing
    }


-- | The available port speeds for the location.
lAvailablePortSpeeds :: Lens' Location [Text]
lAvailablePortSpeeds = lens _lAvailablePortSpeeds (\ s a -> s{_lAvailablePortSpeeds = a}) . _Default . _Coerce

-- | The name of the location. This includes the name of the colocation partner and the physical site of the building.
lLocationName :: Lens' Location (Maybe Text)
lLocationName = lens _lLocationName (\ s a -> s{_lLocationName = a})

-- | The code for the location.
lLocationCode :: Lens' Location (Maybe Text)
lLocationCode = lens _lLocationCode (\ s a -> s{_lLocationCode = a})

-- | The AWS Region for the location.
lRegion :: Lens' Location (Maybe Text)
lRegion = lens _lRegion (\ s a -> s{_lRegion = a})

instance FromJSON Location where
        parseJSON
          = withObject "Location"
              (\ x ->
                 Location' <$>
                   (x .:? "availablePortSpeeds" .!= mempty) <*>
                     (x .:? "locationName")
                     <*> (x .:? "locationCode")
                     <*> (x .:? "region"))

instance Hashable Location where

instance NFData Location where

-- | Information about a new BGP peer.
--
--
--
-- /See:/ 'newBGPPeer' smart constructor.
data NewBGPPeer = NewBGPPeer'
  { _nbpCustomerAddress :: !(Maybe Text)
  , _nbpAmazonAddress   :: !(Maybe Text)
  , _nbpAddressFamily   :: !(Maybe AddressFamily)
  , _nbpAsn             :: !(Maybe Int)
  , _nbpAuthKey         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NewBGPPeer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nbpCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'nbpAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'nbpAddressFamily' - The address family for the BGP peer.
--
-- * 'nbpAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- * 'nbpAuthKey' - The authentication key for BGP configuration.
newBGPPeer
    :: NewBGPPeer
newBGPPeer =
  NewBGPPeer'
    { _nbpCustomerAddress = Nothing
    , _nbpAmazonAddress = Nothing
    , _nbpAddressFamily = Nothing
    , _nbpAsn = Nothing
    , _nbpAuthKey = Nothing
    }


-- | The IP address assigned to the customer interface.
nbpCustomerAddress :: Lens' NewBGPPeer (Maybe Text)
nbpCustomerAddress = lens _nbpCustomerAddress (\ s a -> s{_nbpCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
nbpAmazonAddress :: Lens' NewBGPPeer (Maybe Text)
nbpAmazonAddress = lens _nbpAmazonAddress (\ s a -> s{_nbpAmazonAddress = a})

-- | The address family for the BGP peer.
nbpAddressFamily :: Lens' NewBGPPeer (Maybe AddressFamily)
nbpAddressFamily = lens _nbpAddressFamily (\ s a -> s{_nbpAddressFamily = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
nbpAsn :: Lens' NewBGPPeer (Maybe Int)
nbpAsn = lens _nbpAsn (\ s a -> s{_nbpAsn = a})

-- | The authentication key for BGP configuration.
nbpAuthKey :: Lens' NewBGPPeer (Maybe Text)
nbpAuthKey = lens _nbpAuthKey (\ s a -> s{_nbpAuthKey = a})

instance Hashable NewBGPPeer where

instance NFData NewBGPPeer where

instance ToJSON NewBGPPeer where
        toJSON NewBGPPeer'{..}
          = object
              (catMaybes
                 [("customerAddress" .=) <$> _nbpCustomerAddress,
                  ("amazonAddress" .=) <$> _nbpAmazonAddress,
                  ("addressFamily" .=) <$> _nbpAddressFamily,
                  ("asn" .=) <$> _nbpAsn,
                  ("authKey" .=) <$> _nbpAuthKey])

-- | Information about a private virtual interface.
--
--
--
-- /See:/ 'newPrivateVirtualInterface' smart constructor.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
  { _nVirtualGatewayId       :: !(Maybe Text)
  , _nMtu                    :: !(Maybe Int)
  , _nCustomerAddress        :: !(Maybe Text)
  , _nAmazonAddress          :: !(Maybe Text)
  , _nAddressFamily          :: !(Maybe AddressFamily)
  , _nDirectConnectGatewayId :: !(Maybe Text)
  , _nAuthKey                :: !(Maybe Text)
  , _nVirtualInterfaceName   :: !Text
  , _nVlan                   :: !Int
  , _nAsn                    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NewPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nVirtualGatewayId' - The ID of the virtual private gateway.
--
-- * 'nMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'nCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'nAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'nAddressFamily' - The address family for the BGP peer.
--
-- * 'nDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'nAuthKey' - The authentication key for BGP configuration.
--
-- * 'nVirtualInterfaceName' - The name of the virtual interface assigned by the customer network.
--
-- * 'nVlan' - The ID of the VLAN.
--
-- * 'nAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
newPrivateVirtualInterface
    :: Text -- ^ 'nVirtualInterfaceName'
    -> Int -- ^ 'nVlan'
    -> Int -- ^ 'nAsn'
    -> NewPrivateVirtualInterface
newPrivateVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPrivateVirtualInterface'
    { _nVirtualGatewayId = Nothing
    , _nMtu = Nothing
    , _nCustomerAddress = Nothing
    , _nAmazonAddress = Nothing
    , _nAddressFamily = Nothing
    , _nDirectConnectGatewayId = Nothing
    , _nAuthKey = Nothing
    , _nVirtualInterfaceName = pVirtualInterfaceName_
    , _nVlan = pVlan_
    , _nAsn = pAsn_
    }


-- | The ID of the virtual private gateway.
nVirtualGatewayId :: Lens' NewPrivateVirtualInterface (Maybe Text)
nVirtualGatewayId = lens _nVirtualGatewayId (\ s a -> s{_nVirtualGatewayId = a})

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
nMtu :: Lens' NewPrivateVirtualInterface (Maybe Int)
nMtu = lens _nMtu (\ s a -> s{_nMtu = a})

-- | The IP address assigned to the customer interface.
nCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nCustomerAddress = lens _nCustomerAddress (\ s a -> s{_nCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
nAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAmazonAddress = lens _nAmazonAddress (\ s a -> s{_nAmazonAddress = a})

-- | The address family for the BGP peer.
nAddressFamily :: Lens' NewPrivateVirtualInterface (Maybe AddressFamily)
nAddressFamily = lens _nAddressFamily (\ s a -> s{_nAddressFamily = a})

-- | The ID of the Direct Connect gateway.
nDirectConnectGatewayId :: Lens' NewPrivateVirtualInterface (Maybe Text)
nDirectConnectGatewayId = lens _nDirectConnectGatewayId (\ s a -> s{_nDirectConnectGatewayId = a})

-- | The authentication key for BGP configuration.
nAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAuthKey = lens _nAuthKey (\ s a -> s{_nAuthKey = a})

-- | The name of the virtual interface assigned by the customer network.
nVirtualInterfaceName :: Lens' NewPrivateVirtualInterface Text
nVirtualInterfaceName = lens _nVirtualInterfaceName (\ s a -> s{_nVirtualInterfaceName = a})

-- | The ID of the VLAN.
nVlan :: Lens' NewPrivateVirtualInterface Int
nVlan = lens _nVlan (\ s a -> s{_nVlan = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
nAsn :: Lens' NewPrivateVirtualInterface Int
nAsn = lens _nAsn (\ s a -> s{_nAsn = a})

instance Hashable NewPrivateVirtualInterface where

instance NFData NewPrivateVirtualInterface where

instance ToJSON NewPrivateVirtualInterface where
        toJSON NewPrivateVirtualInterface'{..}
          = object
              (catMaybes
                 [("virtualGatewayId" .=) <$> _nVirtualGatewayId,
                  ("mtu" .=) <$> _nMtu,
                  ("customerAddress" .=) <$> _nCustomerAddress,
                  ("amazonAddress" .=) <$> _nAmazonAddress,
                  ("addressFamily" .=) <$> _nAddressFamily,
                  ("directConnectGatewayId" .=) <$>
                    _nDirectConnectGatewayId,
                  ("authKey" .=) <$> _nAuthKey,
                  Just
                    ("virtualInterfaceName" .= _nVirtualInterfaceName),
                  Just ("vlan" .= _nVlan), Just ("asn" .= _nAsn)])

-- | Information about a private virtual interface to be provisioned on a connection.
--
--
--
-- /See:/ 'newPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
  { _npviaMtu                  :: !(Maybe Int)
  , _npviaCustomerAddress      :: !(Maybe Text)
  , _npviaAmazonAddress        :: !(Maybe Text)
  , _npviaAddressFamily        :: !(Maybe AddressFamily)
  , _npviaAuthKey              :: !(Maybe Text)
  , _npviaVirtualInterfaceName :: !Text
  , _npviaVlan                 :: !Int
  , _npviaAsn                  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NewPrivateVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npviaMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'npviaCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'npviaAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'npviaAddressFamily' - The address family for the BGP peer.
--
-- * 'npviaAuthKey' - The authentication key for BGP configuration.
--
-- * 'npviaVirtualInterfaceName' - The name of the virtual interface assigned by the customer network.
--
-- * 'npviaVlan' - The ID of the VLAN.
--
-- * 'npviaAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
newPrivateVirtualInterfaceAllocation
    :: Text -- ^ 'npviaVirtualInterfaceName'
    -> Int -- ^ 'npviaVlan'
    -> Int -- ^ 'npviaAsn'
    -> NewPrivateVirtualInterfaceAllocation
newPrivateVirtualInterfaceAllocation pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPrivateVirtualInterfaceAllocation'
    { _npviaMtu = Nothing
    , _npviaCustomerAddress = Nothing
    , _npviaAmazonAddress = Nothing
    , _npviaAddressFamily = Nothing
    , _npviaAuthKey = Nothing
    , _npviaVirtualInterfaceName = pVirtualInterfaceName_
    , _npviaVlan = pVlan_
    , _npviaAsn = pAsn_
    }


-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
npviaMtu :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Int)
npviaMtu = lens _npviaMtu (\ s a -> s{_npviaMtu = a})

-- | The IP address assigned to the customer interface.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress = lens _npviaCustomerAddress (\ s a -> s{_npviaCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress = lens _npviaAmazonAddress (\ s a -> s{_npviaAmazonAddress = a})

-- | The address family for the BGP peer.
npviaAddressFamily :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe AddressFamily)
npviaAddressFamily = lens _npviaAddressFamily (\ s a -> s{_npviaAddressFamily = a})

-- | The authentication key for BGP configuration.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\ s a -> s{_npviaAuthKey = a})

-- | The name of the virtual interface assigned by the customer network.
npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation Text
npviaVirtualInterfaceName = lens _npviaVirtualInterfaceName (\ s a -> s{_npviaVirtualInterfaceName = a})

-- | The ID of the VLAN.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaVlan = lens _npviaVlan (\ s a -> s{_npviaVlan = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
npviaAsn :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaAsn = lens _npviaAsn (\ s a -> s{_npviaAsn = a})

instance Hashable
           NewPrivateVirtualInterfaceAllocation
         where

instance NFData NewPrivateVirtualInterfaceAllocation
         where

instance ToJSON NewPrivateVirtualInterfaceAllocation
         where
        toJSON NewPrivateVirtualInterfaceAllocation'{..}
          = object
              (catMaybes
                 [("mtu" .=) <$> _npviaMtu,
                  ("customerAddress" .=) <$> _npviaCustomerAddress,
                  ("amazonAddress" .=) <$> _npviaAmazonAddress,
                  ("addressFamily" .=) <$> _npviaAddressFamily,
                  ("authKey" .=) <$> _npviaAuthKey,
                  Just
                    ("virtualInterfaceName" .=
                       _npviaVirtualInterfaceName),
                  Just ("vlan" .= _npviaVlan),
                  Just ("asn" .= _npviaAsn)])

-- | Information about a public virtual interface.
--
--
--
-- /See:/ 'newPublicVirtualInterface' smart constructor.
data NewPublicVirtualInterface = NewPublicVirtualInterface'
  { _npviRouteFilterPrefixes  :: !(Maybe [RouteFilterPrefix])
  , _npviCustomerAddress      :: !(Maybe Text)
  , _npviAmazonAddress        :: !(Maybe Text)
  , _npviAddressFamily        :: !(Maybe AddressFamily)
  , _npviAuthKey              :: !(Maybe Text)
  , _npviVirtualInterfaceName :: !Text
  , _npviVlan                 :: !Int
  , _npviAsn                  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NewPublicVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npviRouteFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- * 'npviCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'npviAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'npviAddressFamily' - The address family for the BGP peer.
--
-- * 'npviAuthKey' - The authentication key for BGP configuration.
--
-- * 'npviVirtualInterfaceName' - The name of the virtual interface assigned by the customer network.
--
-- * 'npviVlan' - The ID of the VLAN.
--
-- * 'npviAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
newPublicVirtualInterface
    :: Text -- ^ 'npviVirtualInterfaceName'
    -> Int -- ^ 'npviVlan'
    -> Int -- ^ 'npviAsn'
    -> NewPublicVirtualInterface
newPublicVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPublicVirtualInterface'
    { _npviRouteFilterPrefixes = Nothing
    , _npviCustomerAddress = Nothing
    , _npviAmazonAddress = Nothing
    , _npviAddressFamily = Nothing
    , _npviAuthKey = Nothing
    , _npviVirtualInterfaceName = pVirtualInterfaceName_
    , _npviVlan = pVlan_
    , _npviAsn = pAsn_
    }


-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
npviRouteFilterPrefixes :: Lens' NewPublicVirtualInterface [RouteFilterPrefix]
npviRouteFilterPrefixes = lens _npviRouteFilterPrefixes (\ s a -> s{_npviRouteFilterPrefixes = a}) . _Default . _Coerce

-- | The IP address assigned to the customer interface.
npviCustomerAddress :: Lens' NewPublicVirtualInterface (Maybe Text)
npviCustomerAddress = lens _npviCustomerAddress (\ s a -> s{_npviCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
npviAmazonAddress :: Lens' NewPublicVirtualInterface (Maybe Text)
npviAmazonAddress = lens _npviAmazonAddress (\ s a -> s{_npviAmazonAddress = a})

-- | The address family for the BGP peer.
npviAddressFamily :: Lens' NewPublicVirtualInterface (Maybe AddressFamily)
npviAddressFamily = lens _npviAddressFamily (\ s a -> s{_npviAddressFamily = a})

-- | The authentication key for BGP configuration.
npviAuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npviAuthKey = lens _npviAuthKey (\ s a -> s{_npviAuthKey = a})

-- | The name of the virtual interface assigned by the customer network.
npviVirtualInterfaceName :: Lens' NewPublicVirtualInterface Text
npviVirtualInterfaceName = lens _npviVirtualInterfaceName (\ s a -> s{_npviVirtualInterfaceName = a})

-- | The ID of the VLAN.
npviVlan :: Lens' NewPublicVirtualInterface Int
npviVlan = lens _npviVlan (\ s a -> s{_npviVlan = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
npviAsn :: Lens' NewPublicVirtualInterface Int
npviAsn = lens _npviAsn (\ s a -> s{_npviAsn = a})

instance Hashable NewPublicVirtualInterface where

instance NFData NewPublicVirtualInterface where

instance ToJSON NewPublicVirtualInterface where
        toJSON NewPublicVirtualInterface'{..}
          = object
              (catMaybes
                 [("routeFilterPrefixes" .=) <$>
                    _npviRouteFilterPrefixes,
                  ("customerAddress" .=) <$> _npviCustomerAddress,
                  ("amazonAddress" .=) <$> _npviAmazonAddress,
                  ("addressFamily" .=) <$> _npviAddressFamily,
                  ("authKey" .=) <$> _npviAuthKey,
                  Just
                    ("virtualInterfaceName" .=
                       _npviVirtualInterfaceName),
                  Just ("vlan" .= _npviVlan),
                  Just ("asn" .= _npviAsn)])

-- | Information about a public virtual interface to be provisioned on a connection.
--
--
--
-- /See:/ 'newPublicVirtualInterfaceAllocation' smart constructor.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
  { _newRouteFilterPrefixes  :: !(Maybe [RouteFilterPrefix])
  , _newCustomerAddress      :: !(Maybe Text)
  , _newAmazonAddress        :: !(Maybe Text)
  , _newAddressFamily        :: !(Maybe AddressFamily)
  , _newAuthKey              :: !(Maybe Text)
  , _newVirtualInterfaceName :: !Text
  , _newVlan                 :: !Int
  , _newAsn                  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NewPublicVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'newRouteFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- * 'newCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'newAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'newAddressFamily' - The address family for the BGP peer.
--
-- * 'newAuthKey' - The authentication key for BGP configuration.
--
-- * 'newVirtualInterfaceName' - The name of the virtual interface assigned by the customer network.
--
-- * 'newVlan' - The ID of the VLAN.
--
-- * 'newAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
newPublicVirtualInterfaceAllocation
    :: Text -- ^ 'newVirtualInterfaceName'
    -> Int -- ^ 'newVlan'
    -> Int -- ^ 'newAsn'
    -> NewPublicVirtualInterfaceAllocation
newPublicVirtualInterfaceAllocation pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPublicVirtualInterfaceAllocation'
    { _newRouteFilterPrefixes = Nothing
    , _newCustomerAddress = Nothing
    , _newAmazonAddress = Nothing
    , _newAddressFamily = Nothing
    , _newAuthKey = Nothing
    , _newVirtualInterfaceName = pVirtualInterfaceName_
    , _newVlan = pVlan_
    , _newAsn = pAsn_
    }


-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
newRouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation [RouteFilterPrefix]
newRouteFilterPrefixes = lens _newRouteFilterPrefixes (\ s a -> s{_newRouteFilterPrefixes = a}) . _Default . _Coerce

-- | The IP address assigned to the customer interface.
newCustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newCustomerAddress = lens _newCustomerAddress (\ s a -> s{_newCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
newAmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newAmazonAddress = lens _newAmazonAddress (\ s a -> s{_newAmazonAddress = a})

-- | The address family for the BGP peer.
newAddressFamily :: Lens' NewPublicVirtualInterfaceAllocation (Maybe AddressFamily)
newAddressFamily = lens _newAddressFamily (\ s a -> s{_newAddressFamily = a})

-- | The authentication key for BGP configuration.
newAuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newAuthKey = lens _newAuthKey (\ s a -> s{_newAuthKey = a})

-- | The name of the virtual interface assigned by the customer network.
newVirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation Text
newVirtualInterfaceName = lens _newVirtualInterfaceName (\ s a -> s{_newVirtualInterfaceName = a})

-- | The ID of the VLAN.
newVlan :: Lens' NewPublicVirtualInterfaceAllocation Int
newVlan = lens _newVlan (\ s a -> s{_newVlan = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
newAsn :: Lens' NewPublicVirtualInterfaceAllocation Int
newAsn = lens _newAsn (\ s a -> s{_newAsn = a})

instance Hashable NewPublicVirtualInterfaceAllocation
         where

instance NFData NewPublicVirtualInterfaceAllocation
         where

instance ToJSON NewPublicVirtualInterfaceAllocation
         where
        toJSON NewPublicVirtualInterfaceAllocation'{..}
          = object
              (catMaybes
                 [("routeFilterPrefixes" .=) <$>
                    _newRouteFilterPrefixes,
                  ("customerAddress" .=) <$> _newCustomerAddress,
                  ("amazonAddress" .=) <$> _newAmazonAddress,
                  ("addressFamily" .=) <$> _newAddressFamily,
                  ("authKey" .=) <$> _newAuthKey,
                  Just
                    ("virtualInterfaceName" .= _newVirtualInterfaceName),
                  Just ("vlan" .= _newVlan), Just ("asn" .= _newAsn)])

-- | Information about a tag associated with an AWS Direct Connect resource.
--
--
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { _rtResourceARN :: !(Maybe Text)
  , _rtTags        :: !(Maybe (List1 Tag))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtResourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'rtTags' - The tags.
resourceTag
    :: ResourceTag
resourceTag = ResourceTag' {_rtResourceARN = Nothing, _rtTags = Nothing}


-- | The Amazon Resource Name (ARN) of the resource.
rtResourceARN :: Lens' ResourceTag (Maybe Text)
rtResourceARN = lens _rtResourceARN (\ s a -> s{_rtResourceARN = a})

-- | The tags.
rtTags :: Lens' ResourceTag (Maybe (NonEmpty Tag))
rtTags = lens _rtTags (\ s a -> s{_rtTags = a}) . mapping _List1

instance FromJSON ResourceTag where
        parseJSON
          = withObject "ResourceTag"
              (\ x ->
                 ResourceTag' <$>
                   (x .:? "resourceArn") <*> (x .:? "tags"))

instance Hashable ResourceTag where

instance NFData ResourceTag where

-- | Information about a route filter prefix that a customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.
--
--
--
-- /See:/ 'routeFilterPrefix' smart constructor.
newtype RouteFilterPrefix = RouteFilterPrefix'
  { _rfpCidr :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteFilterPrefix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfpCidr' - The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
routeFilterPrefix
    :: RouteFilterPrefix
routeFilterPrefix = RouteFilterPrefix' {_rfpCidr = Nothing}


-- | The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
rfpCidr :: Lens' RouteFilterPrefix (Maybe Text)
rfpCidr = lens _rfpCidr (\ s a -> s{_rfpCidr = a})

instance FromJSON RouteFilterPrefix where
        parseJSON
          = withObject "RouteFilterPrefix"
              (\ x -> RouteFilterPrefix' <$> (x .:? "cidr"))

instance Hashable RouteFilterPrefix where

instance NFData RouteFilterPrefix where

instance ToJSON RouteFilterPrefix where
        toJSON RouteFilterPrefix'{..}
          = object (catMaybes [("cidr" .=) <$> _rfpCidr])

-- | Information about a tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value.
--
-- * 'tagKey' - The key.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | The value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "value") <*> (x .: "key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _tagValue,
                  Just ("key" .= _tagKey)])

-- | Information about a virtual private gateway for a private virtual interface.
--
--
--
-- /See:/ 'virtualGateway' smart constructor.
data VirtualGateway = VirtualGateway'
  { _vgVirtualGatewayId    :: !(Maybe Text)
  , _vgVirtualGatewayState :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vgVirtualGatewayId' - The ID of the virtual private gateway.
--
-- * 'vgVirtualGatewayState' - The state of the virtual private gateway. The following are the possible values:     * @pending@ : Initial state after creating the virtual private gateway.     * @available@ : Ready for use by a private virtual interface.     * @deleting@ : Initial state after deleting the virtual private gateway.     * @deleted@ : The virtual private gateway is deleted. The private virtual interface is unable to send traffic over this gateway.
virtualGateway
    :: VirtualGateway
virtualGateway =
  VirtualGateway'
    {_vgVirtualGatewayId = Nothing, _vgVirtualGatewayState = Nothing}


-- | The ID of the virtual private gateway.
vgVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayId = lens _vgVirtualGatewayId (\ s a -> s{_vgVirtualGatewayId = a})

-- | The state of the virtual private gateway. The following are the possible values:     * @pending@ : Initial state after creating the virtual private gateway.     * @available@ : Ready for use by a private virtual interface.     * @deleting@ : Initial state after deleting the virtual private gateway.     * @deleted@ : The virtual private gateway is deleted. The private virtual interface is unable to send traffic over this gateway.
vgVirtualGatewayState :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayState = lens _vgVirtualGatewayState (\ s a -> s{_vgVirtualGatewayState = a})

instance FromJSON VirtualGateway where
        parseJSON
          = withObject "VirtualGateway"
              (\ x ->
                 VirtualGateway' <$>
                   (x .:? "virtualGatewayId") <*>
                     (x .:? "virtualGatewayState"))

instance Hashable VirtualGateway where

instance NFData VirtualGateway where

-- | Information about a virtual interface.
--
--
--
-- /See:/ 'virtualInterface' smart constructor.
data VirtualInterface = VirtualInterface'
  { _viBgpPeers               :: !(Maybe [BGPPeer])
  , _viVirtualGatewayId       :: !(Maybe Text)
  , _viMtu                    :: !(Maybe Int)
  , _viRouteFilterPrefixes    :: !(Maybe [RouteFilterPrefix])
  , _viCustomerAddress        :: !(Maybe Text)
  , _viVlan                   :: !(Maybe Int)
  , _viLocation               :: !(Maybe Text)
  , _viAmazonAddress          :: !(Maybe Text)
  , _viAddressFamily          :: !(Maybe AddressFamily)
  , _viVirtualInterfaceState  :: !(Maybe VirtualInterfaceState)
  , _viConnectionId           :: !(Maybe Text)
  , _viDirectConnectGatewayId :: !(Maybe Text)
  , _viAmazonSideASN          :: !(Maybe Integer)
  , _viVirtualInterfaceType   :: !(Maybe Text)
  , _viAsn                    :: !(Maybe Int)
  , _viAuthKey                :: !(Maybe Text)
  , _viJumboFrameCapable      :: !(Maybe Bool)
  , _viCustomerRouterConfig   :: !(Maybe Text)
  , _viOwnerAccount           :: !(Maybe Text)
  , _viRegion                 :: !(Maybe Text)
  , _viVirtualInterfaceName   :: !(Maybe Text)
  , _viAwsDeviceV2            :: !(Maybe Text)
  , _viVirtualInterfaceId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viBgpPeers' - The BGP peers configured on this virtual interface.
--
-- * 'viVirtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual interfaces.
--
-- * 'viMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'viRouteFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- * 'viCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'viVlan' - The ID of the VLAN.
--
-- * 'viLocation' - The location of the connection.
--
-- * 'viAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'viAddressFamily' - The address family for the BGP peer.
--
-- * 'viVirtualInterfaceState' - The state of the virtual interface. The following are the possible values:     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.     * @available@ : A virtual interface that is able to forward traffic.     * @down@ : A virtual interface that is BGP down.     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.     * @deleted@ : A virtual interface that cannot forward traffic.     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.     * @unknown@ : The state of the virtual interface is not available.
--
-- * 'viConnectionId' - The ID of the connection.
--
-- * 'viDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'viAmazonSideASN' - The autonomous system number (ASN) for the Amazon side of the connection.
--
-- * 'viVirtualInterfaceType' - The type of virtual interface. The possible values are @private@ and @public@ .
--
-- * 'viAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- * 'viAuthKey' - The authentication key for BGP configuration.
--
-- * 'viJumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- * 'viCustomerRouterConfig' - The customer router configuration.
--
-- * 'viOwnerAccount' - The ID of the AWS account that owns the virtual interface.
--
-- * 'viRegion' - The AWS Region where the virtual interface is located.
--
-- * 'viVirtualInterfaceName' - The name of the virtual interface assigned by the customer network.
--
-- * 'viAwsDeviceV2' - The Direct Connect endpoint on which the virtual interface terminates.
--
-- * 'viVirtualInterfaceId' - The ID of the virtual interface.
virtualInterface
    :: VirtualInterface
virtualInterface =
  VirtualInterface'
    { _viBgpPeers = Nothing
    , _viVirtualGatewayId = Nothing
    , _viMtu = Nothing
    , _viRouteFilterPrefixes = Nothing
    , _viCustomerAddress = Nothing
    , _viVlan = Nothing
    , _viLocation = Nothing
    , _viAmazonAddress = Nothing
    , _viAddressFamily = Nothing
    , _viVirtualInterfaceState = Nothing
    , _viConnectionId = Nothing
    , _viDirectConnectGatewayId = Nothing
    , _viAmazonSideASN = Nothing
    , _viVirtualInterfaceType = Nothing
    , _viAsn = Nothing
    , _viAuthKey = Nothing
    , _viJumboFrameCapable = Nothing
    , _viCustomerRouterConfig = Nothing
    , _viOwnerAccount = Nothing
    , _viRegion = Nothing
    , _viVirtualInterfaceName = Nothing
    , _viAwsDeviceV2 = Nothing
    , _viVirtualInterfaceId = Nothing
    }


-- | The BGP peers configured on this virtual interface.
viBgpPeers :: Lens' VirtualInterface [BGPPeer]
viBgpPeers = lens _viBgpPeers (\ s a -> s{_viBgpPeers = a}) . _Default . _Coerce

-- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
viVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
viVirtualGatewayId = lens _viVirtualGatewayId (\ s a -> s{_viVirtualGatewayId = a})

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
viMtu :: Lens' VirtualInterface (Maybe Int)
viMtu = lens _viMtu (\ s a -> s{_viMtu = a})

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
viRouteFilterPrefixes :: Lens' VirtualInterface [RouteFilterPrefix]
viRouteFilterPrefixes = lens _viRouteFilterPrefixes (\ s a -> s{_viRouteFilterPrefixes = a}) . _Default . _Coerce

-- | The IP address assigned to the customer interface.
viCustomerAddress :: Lens' VirtualInterface (Maybe Text)
viCustomerAddress = lens _viCustomerAddress (\ s a -> s{_viCustomerAddress = a})

-- | The ID of the VLAN.
viVlan :: Lens' VirtualInterface (Maybe Int)
viVlan = lens _viVlan (\ s a -> s{_viVlan = a})

-- | The location of the connection.
viLocation :: Lens' VirtualInterface (Maybe Text)
viLocation = lens _viLocation (\ s a -> s{_viLocation = a})

-- | The IP address assigned to the Amazon interface.
viAmazonAddress :: Lens' VirtualInterface (Maybe Text)
viAmazonAddress = lens _viAmazonAddress (\ s a -> s{_viAmazonAddress = a})

-- | The address family for the BGP peer.
viAddressFamily :: Lens' VirtualInterface (Maybe AddressFamily)
viAddressFamily = lens _viAddressFamily (\ s a -> s{_viAddressFamily = a})

-- | The state of the virtual interface. The following are the possible values:     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.     * @available@ : A virtual interface that is able to forward traffic.     * @down@ : A virtual interface that is BGP down.     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.     * @deleted@ : A virtual interface that cannot forward traffic.     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.     * @unknown@ : The state of the virtual interface is not available.
viVirtualInterfaceState :: Lens' VirtualInterface (Maybe VirtualInterfaceState)
viVirtualInterfaceState = lens _viVirtualInterfaceState (\ s a -> s{_viVirtualInterfaceState = a})

-- | The ID of the connection.
viConnectionId :: Lens' VirtualInterface (Maybe Text)
viConnectionId = lens _viConnectionId (\ s a -> s{_viConnectionId = a})

-- | The ID of the Direct Connect gateway.
viDirectConnectGatewayId :: Lens' VirtualInterface (Maybe Text)
viDirectConnectGatewayId = lens _viDirectConnectGatewayId (\ s a -> s{_viDirectConnectGatewayId = a})

-- | The autonomous system number (ASN) for the Amazon side of the connection.
viAmazonSideASN :: Lens' VirtualInterface (Maybe Integer)
viAmazonSideASN = lens _viAmazonSideASN (\ s a -> s{_viAmazonSideASN = a})

-- | The type of virtual interface. The possible values are @private@ and @public@ .
viVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceType = lens _viVirtualInterfaceType (\ s a -> s{_viVirtualInterfaceType = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
viAsn :: Lens' VirtualInterface (Maybe Int)
viAsn = lens _viAsn (\ s a -> s{_viAsn = a})

-- | The authentication key for BGP configuration.
viAuthKey :: Lens' VirtualInterface (Maybe Text)
viAuthKey = lens _viAuthKey (\ s a -> s{_viAuthKey = a})

-- | Indicates whether jumbo frames (9001 MTU) are supported.
viJumboFrameCapable :: Lens' VirtualInterface (Maybe Bool)
viJumboFrameCapable = lens _viJumboFrameCapable (\ s a -> s{_viJumboFrameCapable = a})

-- | The customer router configuration.
viCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
viCustomerRouterConfig = lens _viCustomerRouterConfig (\ s a -> s{_viCustomerRouterConfig = a})

-- | The ID of the AWS account that owns the virtual interface.
viOwnerAccount :: Lens' VirtualInterface (Maybe Text)
viOwnerAccount = lens _viOwnerAccount (\ s a -> s{_viOwnerAccount = a})

-- | The AWS Region where the virtual interface is located.
viRegion :: Lens' VirtualInterface (Maybe Text)
viRegion = lens _viRegion (\ s a -> s{_viRegion = a})

-- | The name of the virtual interface assigned by the customer network.
viVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceName = lens _viVirtualInterfaceName (\ s a -> s{_viVirtualInterfaceName = a})

-- | The Direct Connect endpoint on which the virtual interface terminates.
viAwsDeviceV2 :: Lens' VirtualInterface (Maybe Text)
viAwsDeviceV2 = lens _viAwsDeviceV2 (\ s a -> s{_viAwsDeviceV2 = a})

-- | The ID of the virtual interface.
viVirtualInterfaceId :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceId = lens _viVirtualInterfaceId (\ s a -> s{_viVirtualInterfaceId = a})

instance FromJSON VirtualInterface where
        parseJSON
          = withObject "VirtualInterface"
              (\ x ->
                 VirtualInterface' <$>
                   (x .:? "bgpPeers" .!= mempty) <*>
                     (x .:? "virtualGatewayId")
                     <*> (x .:? "mtu")
                     <*> (x .:? "routeFilterPrefixes" .!= mempty)
                     <*> (x .:? "customerAddress")
                     <*> (x .:? "vlan")
                     <*> (x .:? "location")
                     <*> (x .:? "amazonAddress")
                     <*> (x .:? "addressFamily")
                     <*> (x .:? "virtualInterfaceState")
                     <*> (x .:? "connectionId")
                     <*> (x .:? "directConnectGatewayId")
                     <*> (x .:? "amazonSideAsn")
                     <*> (x .:? "virtualInterfaceType")
                     <*> (x .:? "asn")
                     <*> (x .:? "authKey")
                     <*> (x .:? "jumboFrameCapable")
                     <*> (x .:? "customerRouterConfig")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "region")
                     <*> (x .:? "virtualInterfaceName")
                     <*> (x .:? "awsDeviceV2")
                     <*> (x .:? "virtualInterfaceId"))

instance Hashable VirtualInterface where

instance NFData VirtualInterface where

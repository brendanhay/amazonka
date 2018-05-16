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

-- | A structure containing information about a BGP peer.
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
  , _bpBgpPeerState    :: !(Maybe BGPPeerState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BGPPeer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpCustomerAddress' - Undocumented member.
--
-- * 'bpAmazonAddress' - Undocumented member.
--
-- * 'bpAddressFamily' - Undocumented member.
--
-- * 'bpBgpStatus' - Undocumented member.
--
-- * 'bpAsn' - Undocumented member.
--
-- * 'bpAuthKey' - Undocumented member.
--
-- * 'bpBgpPeerState' - Undocumented member.
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
    , _bpBgpPeerState = Nothing
    }


-- | Undocumented member.
bpCustomerAddress :: Lens' BGPPeer (Maybe Text)
bpCustomerAddress = lens _bpCustomerAddress (\ s a -> s{_bpCustomerAddress = a})

-- | Undocumented member.
bpAmazonAddress :: Lens' BGPPeer (Maybe Text)
bpAmazonAddress = lens _bpAmazonAddress (\ s a -> s{_bpAmazonAddress = a})

-- | Undocumented member.
bpAddressFamily :: Lens' BGPPeer (Maybe AddressFamily)
bpAddressFamily = lens _bpAddressFamily (\ s a -> s{_bpAddressFamily = a})

-- | Undocumented member.
bpBgpStatus :: Lens' BGPPeer (Maybe BGPStatus)
bpBgpStatus = lens _bpBgpStatus (\ s a -> s{_bpBgpStatus = a})

-- | Undocumented member.
bpAsn :: Lens' BGPPeer (Maybe Int)
bpAsn = lens _bpAsn (\ s a -> s{_bpAsn = a})

-- | Undocumented member.
bpAuthKey :: Lens' BGPPeer (Maybe Text)
bpAuthKey = lens _bpAuthKey (\ s a -> s{_bpAuthKey = a})

-- | Undocumented member.
bpBgpPeerState :: Lens' BGPPeer (Maybe BGPPeerState)
bpBgpPeerState = lens _bpBgpPeerState (\ s a -> s{_bpBgpPeerState = a})

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
                     <*> (x .:? "bgpPeerState"))

instance Hashable BGPPeer where

instance NFData BGPPeer where

-- | A connection represents the physical network connection between the AWS Direct Connect location and the customer.
--
--
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
  { _cLagId           :: !(Maybe Text)
  , _cVlan            :: !(Maybe Int)
  , _cLocation        :: !(Maybe Text)
  , _cAwsDevice       :: !(Maybe Text)
  , _cConnectionId    :: !(Maybe Text)
  , _cLoaIssueTime    :: !(Maybe POSIX)
  , _cPartnerName     :: !(Maybe Text)
  , _cConnectionName  :: !(Maybe Text)
  , _cBandwidth       :: !(Maybe Text)
  , _cOwnerAccount    :: !(Maybe Text)
  , _cRegion          :: !(Maybe Text)
  , _cConnectionState :: !(Maybe ConnectionState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLagId' - Undocumented member.
--
-- * 'cVlan' - Undocumented member.
--
-- * 'cLocation' - Undocumented member.
--
-- * 'cAwsDevice' - The Direct Connection endpoint which the physical connection terminates on.
--
-- * 'cConnectionId' - Undocumented member.
--
-- * 'cLoaIssueTime' - The time of the most recent call to 'DescribeLoa' for this connection.
--
-- * 'cPartnerName' - The name of the AWS Direct Connect service provider associated with the connection.
--
-- * 'cConnectionName' - Undocumented member.
--
-- * 'cBandwidth' - Bandwidth of the connection. Example: 1Gbps (for regular connections), or 500Mbps (for hosted connections) Default: None
--
-- * 'cOwnerAccount' - The AWS account that will own the new connection.
--
-- * 'cRegion' - Undocumented member.
--
-- * 'cConnectionState' - Undocumented member.
connection
    :: Connection
connection =
  Connection'
    { _cLagId = Nothing
    , _cVlan = Nothing
    , _cLocation = Nothing
    , _cAwsDevice = Nothing
    , _cConnectionId = Nothing
    , _cLoaIssueTime = Nothing
    , _cPartnerName = Nothing
    , _cConnectionName = Nothing
    , _cBandwidth = Nothing
    , _cOwnerAccount = Nothing
    , _cRegion = Nothing
    , _cConnectionState = Nothing
    }


-- | Undocumented member.
cLagId :: Lens' Connection (Maybe Text)
cLagId = lens _cLagId (\ s a -> s{_cLagId = a})

-- | Undocumented member.
cVlan :: Lens' Connection (Maybe Int)
cVlan = lens _cVlan (\ s a -> s{_cVlan = a})

-- | Undocumented member.
cLocation :: Lens' Connection (Maybe Text)
cLocation = lens _cLocation (\ s a -> s{_cLocation = a})

-- | The Direct Connection endpoint which the physical connection terminates on.
cAwsDevice :: Lens' Connection (Maybe Text)
cAwsDevice = lens _cAwsDevice (\ s a -> s{_cAwsDevice = a})

-- | Undocumented member.
cConnectionId :: Lens' Connection (Maybe Text)
cConnectionId = lens _cConnectionId (\ s a -> s{_cConnectionId = a})

-- | The time of the most recent call to 'DescribeLoa' for this connection.
cLoaIssueTime :: Lens' Connection (Maybe UTCTime)
cLoaIssueTime = lens _cLoaIssueTime (\ s a -> s{_cLoaIssueTime = a}) . mapping _Time

-- | The name of the AWS Direct Connect service provider associated with the connection.
cPartnerName :: Lens' Connection (Maybe Text)
cPartnerName = lens _cPartnerName (\ s a -> s{_cPartnerName = a})

-- | Undocumented member.
cConnectionName :: Lens' Connection (Maybe Text)
cConnectionName = lens _cConnectionName (\ s a -> s{_cConnectionName = a})

-- | Bandwidth of the connection. Example: 1Gbps (for regular connections), or 500Mbps (for hosted connections) Default: None
cBandwidth :: Lens' Connection (Maybe Text)
cBandwidth = lens _cBandwidth (\ s a -> s{_cBandwidth = a})

-- | The AWS account that will own the new connection.
cOwnerAccount :: Lens' Connection (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\ s a -> s{_cOwnerAccount = a})

-- | Undocumented member.
cRegion :: Lens' Connection (Maybe Text)
cRegion = lens _cRegion (\ s a -> s{_cRegion = a})

-- | Undocumented member.
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
                     <*> (x .:? "connectionId")
                     <*> (x .:? "loaIssueTime")
                     <*> (x .:? "partnerName")
                     <*> (x .:? "connectionName")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "region")
                     <*> (x .:? "connectionState"))

instance Hashable Connection where

instance NFData Connection where

-- | A structure containing a list of connections.
--
--
--
-- /See:/ 'connections' smart constructor.
newtype Connections = Connections'
  { _cConnections :: Maybe [Connection]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConnections' - A list of connections.
connections
    :: Connections
connections = Connections' {_cConnections = Nothing}


-- | A list of connections.
cConnections :: Lens' Connections [Connection]
cConnections = lens _cConnections (\ s a -> s{_cConnections = a}) . _Default . _Coerce

instance FromJSON Connections where
        parseJSON
          = withObject "Connections"
              (\ x ->
                 Connections' <$> (x .:? "connections" .!= mempty))

instance Hashable Connections where

instance NFData Connections where

-- | A direct connect gateway is an intermediate object that enables you to connect virtual interfaces and virtual private gateways.
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
-- * 'dcgDirectConnectGatewayId' - Undocumented member.
--
-- * 'dcgStateChangeError' - Undocumented member.
--
-- * 'dcgAmazonSideASN' - The autonomous system number (ASN) for the Amazon side of the connection.
--
-- * 'dcgDirectConnectGatewayName' - Undocumented member.
--
-- * 'dcgDirectConnectGatewayState' - Undocumented member.
--
-- * 'dcgOwnerAccount' - The AWS account ID of the owner of the direct connect gateway.
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


-- | Undocumented member.
dcgDirectConnectGatewayId :: Lens' DirectConnectGateway (Maybe Text)
dcgDirectConnectGatewayId = lens _dcgDirectConnectGatewayId (\ s a -> s{_dcgDirectConnectGatewayId = a})

-- | Undocumented member.
dcgStateChangeError :: Lens' DirectConnectGateway (Maybe Text)
dcgStateChangeError = lens _dcgStateChangeError (\ s a -> s{_dcgStateChangeError = a})

-- | The autonomous system number (ASN) for the Amazon side of the connection.
dcgAmazonSideASN :: Lens' DirectConnectGateway (Maybe Integer)
dcgAmazonSideASN = lens _dcgAmazonSideASN (\ s a -> s{_dcgAmazonSideASN = a})

-- | Undocumented member.
dcgDirectConnectGatewayName :: Lens' DirectConnectGateway (Maybe Text)
dcgDirectConnectGatewayName = lens _dcgDirectConnectGatewayName (\ s a -> s{_dcgDirectConnectGatewayName = a})

-- | Undocumented member.
dcgDirectConnectGatewayState :: Lens' DirectConnectGateway (Maybe DirectConnectGatewayState)
dcgDirectConnectGatewayState = lens _dcgDirectConnectGatewayState (\ s a -> s{_dcgDirectConnectGatewayState = a})

-- | The AWS account ID of the owner of the direct connect gateway.
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

-- | The association between a direct connect gateway and virtual private gateway.
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
-- * 'dcgaVirtualGatewayId' - Undocumented member.
--
-- * 'dcgaDirectConnectGatewayId' - Undocumented member.
--
-- * 'dcgaVirtualGatewayOwnerAccount' - The AWS account ID of the owner of the virtual private gateway.
--
-- * 'dcgaStateChangeError' - Undocumented member.
--
-- * 'dcgaVirtualGatewayRegion' - Undocumented member.
--
-- * 'dcgaAssociationState' - Undocumented member.
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


-- | Undocumented member.
dcgaVirtualGatewayId :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaVirtualGatewayId = lens _dcgaVirtualGatewayId (\ s a -> s{_dcgaVirtualGatewayId = a})

-- | Undocumented member.
dcgaDirectConnectGatewayId :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaDirectConnectGatewayId = lens _dcgaDirectConnectGatewayId (\ s a -> s{_dcgaDirectConnectGatewayId = a})

-- | The AWS account ID of the owner of the virtual private gateway.
dcgaVirtualGatewayOwnerAccount :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaVirtualGatewayOwnerAccount = lens _dcgaVirtualGatewayOwnerAccount (\ s a -> s{_dcgaVirtualGatewayOwnerAccount = a})

-- | Undocumented member.
dcgaStateChangeError :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaStateChangeError = lens _dcgaStateChangeError (\ s a -> s{_dcgaStateChangeError = a})

-- | Undocumented member.
dcgaVirtualGatewayRegion :: Lens' DirectConnectGatewayAssociation (Maybe Text)
dcgaVirtualGatewayRegion = lens _dcgaVirtualGatewayRegion (\ s a -> s{_dcgaVirtualGatewayRegion = a})

-- | Undocumented member.
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

-- | The association between a direct connect gateway and virtual interface.
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
-- * 'dDirectConnectGatewayId' - Undocumented member.
--
-- * 'dAttachmentState' - Undocumented member.
--
-- * 'dStateChangeError' - Undocumented member.
--
-- * 'dVirtualInterfaceRegion' - Undocumented member.
--
-- * 'dVirtualInterfaceOwnerAccount' - The AWS account ID of the owner of the virtual interface.
--
-- * 'dVirtualInterfaceId' - Undocumented member.
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


-- | Undocumented member.
dDirectConnectGatewayId :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dDirectConnectGatewayId = lens _dDirectConnectGatewayId (\ s a -> s{_dDirectConnectGatewayId = a})

-- | Undocumented member.
dAttachmentState :: Lens' DirectConnectGatewayAttachment (Maybe DirectConnectGatewayAttachmentState)
dAttachmentState = lens _dAttachmentState (\ s a -> s{_dAttachmentState = a})

-- | Undocumented member.
dStateChangeError :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dStateChangeError = lens _dStateChangeError (\ s a -> s{_dStateChangeError = a})

-- | Undocumented member.
dVirtualInterfaceRegion :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceRegion = lens _dVirtualInterfaceRegion (\ s a -> s{_dVirtualInterfaceRegion = a})

-- | The AWS account ID of the owner of the virtual interface.
dVirtualInterfaceOwnerAccount :: Lens' DirectConnectGatewayAttachment (Maybe Text)
dVirtualInterfaceOwnerAccount = lens _dVirtualInterfaceOwnerAccount (\ s a -> s{_dVirtualInterfaceOwnerAccount = a})

-- | Undocumented member.
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

-- | An interconnect is a connection that can host other connections.
--
--
-- Like a standard AWS Direct Connect connection, an interconnect represents the physical connection between an AWS Direct Connect partner's network and a specific Direct Connect location. An AWS Direct Connect partner who owns an interconnect can provision hosted connections on the interconnect for their end customers, thereby providing the end customers with connectivity to AWS services.
--
-- The resources of the interconnect, including bandwidth and VLAN numbers, are shared by all of the hosted connections on the interconnect, and the owner of the interconnect determines how these resources are assigned.
--
--
-- /See:/ 'interconnect' smart constructor.
data Interconnect = Interconnect'
  { _iLagId             :: !(Maybe Text)
  , _iInterconnectId    :: !(Maybe Text)
  , _iLocation          :: !(Maybe Text)
  , _iInterconnectName  :: !(Maybe Text)
  , _iAwsDevice         :: !(Maybe Text)
  , _iLoaIssueTime      :: !(Maybe POSIX)
  , _iBandwidth         :: !(Maybe Text)
  , _iInterconnectState :: !(Maybe InterconnectState)
  , _iRegion            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Interconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iLagId' - Undocumented member.
--
-- * 'iInterconnectId' - Undocumented member.
--
-- * 'iLocation' - Undocumented member.
--
-- * 'iInterconnectName' - Undocumented member.
--
-- * 'iAwsDevice' - The Direct Connection endpoint which the physical connection terminates on.
--
-- * 'iLoaIssueTime' - The time of the most recent call to DescribeInterconnectLoa for this Interconnect.
--
-- * 'iBandwidth' - Undocumented member.
--
-- * 'iInterconnectState' - Undocumented member.
--
-- * 'iRegion' - Undocumented member.
interconnect
    :: Interconnect
interconnect =
  Interconnect'
    { _iLagId = Nothing
    , _iInterconnectId = Nothing
    , _iLocation = Nothing
    , _iInterconnectName = Nothing
    , _iAwsDevice = Nothing
    , _iLoaIssueTime = Nothing
    , _iBandwidth = Nothing
    , _iInterconnectState = Nothing
    , _iRegion = Nothing
    }


-- | Undocumented member.
iLagId :: Lens' Interconnect (Maybe Text)
iLagId = lens _iLagId (\ s a -> s{_iLagId = a})

-- | Undocumented member.
iInterconnectId :: Lens' Interconnect (Maybe Text)
iInterconnectId = lens _iInterconnectId (\ s a -> s{_iInterconnectId = a})

-- | Undocumented member.
iLocation :: Lens' Interconnect (Maybe Text)
iLocation = lens _iLocation (\ s a -> s{_iLocation = a})

-- | Undocumented member.
iInterconnectName :: Lens' Interconnect (Maybe Text)
iInterconnectName = lens _iInterconnectName (\ s a -> s{_iInterconnectName = a})

-- | The Direct Connection endpoint which the physical connection terminates on.
iAwsDevice :: Lens' Interconnect (Maybe Text)
iAwsDevice = lens _iAwsDevice (\ s a -> s{_iAwsDevice = a})

-- | The time of the most recent call to DescribeInterconnectLoa for this Interconnect.
iLoaIssueTime :: Lens' Interconnect (Maybe UTCTime)
iLoaIssueTime = lens _iLoaIssueTime (\ s a -> s{_iLoaIssueTime = a}) . mapping _Time

-- | Undocumented member.
iBandwidth :: Lens' Interconnect (Maybe Text)
iBandwidth = lens _iBandwidth (\ s a -> s{_iBandwidth = a})

-- | Undocumented member.
iInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
iInterconnectState = lens _iInterconnectState (\ s a -> s{_iInterconnectState = a})

-- | Undocumented member.
iRegion :: Lens' Interconnect (Maybe Text)
iRegion = lens _iRegion (\ s a -> s{_iRegion = a})

instance FromJSON Interconnect where
        parseJSON
          = withObject "Interconnect"
              (\ x ->
                 Interconnect' <$>
                   (x .:? "lagId") <*> (x .:? "interconnectId") <*>
                     (x .:? "location")
                     <*> (x .:? "interconnectName")
                     <*> (x .:? "awsDevice")
                     <*> (x .:? "loaIssueTime")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "interconnectState")
                     <*> (x .:? "region"))

instance Hashable Interconnect where

instance NFData Interconnect where

-- | Describes a link aggregation group (LAG). A LAG is a connection that uses the Link Aggregation Control Protocol (LACP) to logically aggregate a bundle of physical connections. Like an interconnect, it can host other connections. All connections in a LAG must terminate on the same physical AWS Direct Connect endpoint, and must be the same bandwidth.
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
  , _lagAllowsHostedConnections :: !(Maybe Bool)
  , _lagNumberOfConnections     :: !(Maybe Int)
  , _lagLagState                :: !(Maybe LagState)
  , _lagOwnerAccount            :: !(Maybe Text)
  , _lagRegion                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Lag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lagLagId' - Undocumented member.
--
-- * 'lagConnectionsBandwidth' - The individual bandwidth of the physical connections bundled by the LAG. Available values: 1Gbps, 10Gbps
--
-- * 'lagMinimumLinks' - The minimum number of physical connections that must be operational for the LAG itself to be operational. If the number of operational connections drops below this setting, the LAG state changes to @down@ . This value can help to ensure that a LAG is not overutilized if a significant number of its bundled connections go down.
--
-- * 'lagLagName' - The name of the LAG.
--
-- * 'lagLocation' - Undocumented member.
--
-- * 'lagConnections' - A list of connections bundled by this LAG.
--
-- * 'lagAwsDevice' - The AWS Direct Connection endpoint that hosts the LAG.
--
-- * 'lagAllowsHostedConnections' - Indicates whether the LAG can host other connections.
--
-- * 'lagNumberOfConnections' - The number of physical connections bundled by the LAG, up to a maximum of 10.
--
-- * 'lagLagState' - Undocumented member.
--
-- * 'lagOwnerAccount' - The owner of the LAG.
--
-- * 'lagRegion' - Undocumented member.
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
    , _lagAllowsHostedConnections = Nothing
    , _lagNumberOfConnections = Nothing
    , _lagLagState = Nothing
    , _lagOwnerAccount = Nothing
    , _lagRegion = Nothing
    }


-- | Undocumented member.
lagLagId :: Lens' Lag (Maybe Text)
lagLagId = lens _lagLagId (\ s a -> s{_lagLagId = a})

-- | The individual bandwidth of the physical connections bundled by the LAG. Available values: 1Gbps, 10Gbps
lagConnectionsBandwidth :: Lens' Lag (Maybe Text)
lagConnectionsBandwidth = lens _lagConnectionsBandwidth (\ s a -> s{_lagConnectionsBandwidth = a})

-- | The minimum number of physical connections that must be operational for the LAG itself to be operational. If the number of operational connections drops below this setting, the LAG state changes to @down@ . This value can help to ensure that a LAG is not overutilized if a significant number of its bundled connections go down.
lagMinimumLinks :: Lens' Lag (Maybe Int)
lagMinimumLinks = lens _lagMinimumLinks (\ s a -> s{_lagMinimumLinks = a})

-- | The name of the LAG.
lagLagName :: Lens' Lag (Maybe Text)
lagLagName = lens _lagLagName (\ s a -> s{_lagLagName = a})

-- | Undocumented member.
lagLocation :: Lens' Lag (Maybe Text)
lagLocation = lens _lagLocation (\ s a -> s{_lagLocation = a})

-- | A list of connections bundled by this LAG.
lagConnections :: Lens' Lag [Connection]
lagConnections = lens _lagConnections (\ s a -> s{_lagConnections = a}) . _Default . _Coerce

-- | The AWS Direct Connection endpoint that hosts the LAG.
lagAwsDevice :: Lens' Lag (Maybe Text)
lagAwsDevice = lens _lagAwsDevice (\ s a -> s{_lagAwsDevice = a})

-- | Indicates whether the LAG can host other connections.
lagAllowsHostedConnections :: Lens' Lag (Maybe Bool)
lagAllowsHostedConnections = lens _lagAllowsHostedConnections (\ s a -> s{_lagAllowsHostedConnections = a})

-- | The number of physical connections bundled by the LAG, up to a maximum of 10.
lagNumberOfConnections :: Lens' Lag (Maybe Int)
lagNumberOfConnections = lens _lagNumberOfConnections (\ s a -> s{_lagNumberOfConnections = a})

-- | Undocumented member.
lagLagState :: Lens' Lag (Maybe LagState)
lagLagState = lens _lagLagState (\ s a -> s{_lagLagState = a})

-- | The owner of the LAG.
lagOwnerAccount :: Lens' Lag (Maybe Text)
lagOwnerAccount = lens _lagOwnerAccount (\ s a -> s{_lagOwnerAccount = a})

-- | Undocumented member.
lagRegion :: Lens' Lag (Maybe Text)
lagRegion = lens _lagRegion (\ s a -> s{_lagRegion = a})

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
                     <*> (x .:? "allowsHostedConnections")
                     <*> (x .:? "numberOfConnections")
                     <*> (x .:? "lagState")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "region"))

instance Hashable Lag where

instance NFData Lag where

-- | An AWS Direct Connect location where connections and interconnects can be requested.
--
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lLocationName :: !(Maybe Text)
  , _lLocationCode :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lLocationName' - The name of the AWS Direct Connect location. The name includes the colocation partner name and the physical site of the lit building.
--
-- * 'lLocationCode' - The code used to indicate the AWS Direct Connect location.
location
    :: Location
location = Location' {_lLocationName = Nothing, _lLocationCode = Nothing}


-- | The name of the AWS Direct Connect location. The name includes the colocation partner name and the physical site of the lit building.
lLocationName :: Lens' Location (Maybe Text)
lLocationName = lens _lLocationName (\ s a -> s{_lLocationName = a})

-- | The code used to indicate the AWS Direct Connect location.
lLocationCode :: Lens' Location (Maybe Text)
lLocationCode = lens _lLocationCode (\ s a -> s{_lLocationCode = a})

instance FromJSON Location where
        parseJSON
          = withObject "Location"
              (\ x ->
                 Location' <$>
                   (x .:? "locationName") <*> (x .:? "locationCode"))

instance Hashable Location where

instance NFData Location where

-- | A structure containing information about a new BGP peer.
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
-- * 'nbpCustomerAddress' - Undocumented member.
--
-- * 'nbpAmazonAddress' - Undocumented member.
--
-- * 'nbpAddressFamily' - Undocumented member.
--
-- * 'nbpAsn' - Undocumented member.
--
-- * 'nbpAuthKey' - Undocumented member.
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


-- | Undocumented member.
nbpCustomerAddress :: Lens' NewBGPPeer (Maybe Text)
nbpCustomerAddress = lens _nbpCustomerAddress (\ s a -> s{_nbpCustomerAddress = a})

-- | Undocumented member.
nbpAmazonAddress :: Lens' NewBGPPeer (Maybe Text)
nbpAmazonAddress = lens _nbpAmazonAddress (\ s a -> s{_nbpAmazonAddress = a})

-- | Undocumented member.
nbpAddressFamily :: Lens' NewBGPPeer (Maybe AddressFamily)
nbpAddressFamily = lens _nbpAddressFamily (\ s a -> s{_nbpAddressFamily = a})

-- | Undocumented member.
nbpAsn :: Lens' NewBGPPeer (Maybe Int)
nbpAsn = lens _nbpAsn (\ s a -> s{_nbpAsn = a})

-- | Undocumented member.
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

-- | A structure containing information about a new private virtual interface.
--
--
--
-- /See:/ 'newPrivateVirtualInterface' smart constructor.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
  { _nVirtualGatewayId       :: !(Maybe Text)
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
-- * 'nVirtualGatewayId' - Undocumented member.
--
-- * 'nCustomerAddress' - Undocumented member.
--
-- * 'nAmazonAddress' - Undocumented member.
--
-- * 'nAddressFamily' - Undocumented member.
--
-- * 'nDirectConnectGatewayId' - Undocumented member.
--
-- * 'nAuthKey' - Undocumented member.
--
-- * 'nVirtualInterfaceName' - Undocumented member.
--
-- * 'nVlan' - Undocumented member.
--
-- * 'nAsn' - Undocumented member.
newPrivateVirtualInterface
    :: Text -- ^ 'nVirtualInterfaceName'
    -> Int -- ^ 'nVlan'
    -> Int -- ^ 'nAsn'
    -> NewPrivateVirtualInterface
newPrivateVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPrivateVirtualInterface'
    { _nVirtualGatewayId = Nothing
    , _nCustomerAddress = Nothing
    , _nAmazonAddress = Nothing
    , _nAddressFamily = Nothing
    , _nDirectConnectGatewayId = Nothing
    , _nAuthKey = Nothing
    , _nVirtualInterfaceName = pVirtualInterfaceName_
    , _nVlan = pVlan_
    , _nAsn = pAsn_
    }


-- | Undocumented member.
nVirtualGatewayId :: Lens' NewPrivateVirtualInterface (Maybe Text)
nVirtualGatewayId = lens _nVirtualGatewayId (\ s a -> s{_nVirtualGatewayId = a})

-- | Undocumented member.
nCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nCustomerAddress = lens _nCustomerAddress (\ s a -> s{_nCustomerAddress = a})

-- | Undocumented member.
nAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAmazonAddress = lens _nAmazonAddress (\ s a -> s{_nAmazonAddress = a})

-- | Undocumented member.
nAddressFamily :: Lens' NewPrivateVirtualInterface (Maybe AddressFamily)
nAddressFamily = lens _nAddressFamily (\ s a -> s{_nAddressFamily = a})

-- | Undocumented member.
nDirectConnectGatewayId :: Lens' NewPrivateVirtualInterface (Maybe Text)
nDirectConnectGatewayId = lens _nDirectConnectGatewayId (\ s a -> s{_nDirectConnectGatewayId = a})

-- | Undocumented member.
nAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAuthKey = lens _nAuthKey (\ s a -> s{_nAuthKey = a})

-- | Undocumented member.
nVirtualInterfaceName :: Lens' NewPrivateVirtualInterface Text
nVirtualInterfaceName = lens _nVirtualInterfaceName (\ s a -> s{_nVirtualInterfaceName = a})

-- | Undocumented member.
nVlan :: Lens' NewPrivateVirtualInterface Int
nVlan = lens _nVlan (\ s a -> s{_nVlan = a})

-- | Undocumented member.
nAsn :: Lens' NewPrivateVirtualInterface Int
nAsn = lens _nAsn (\ s a -> s{_nAsn = a})

instance Hashable NewPrivateVirtualInterface where

instance NFData NewPrivateVirtualInterface where

instance ToJSON NewPrivateVirtualInterface where
        toJSON NewPrivateVirtualInterface'{..}
          = object
              (catMaybes
                 [("virtualGatewayId" .=) <$> _nVirtualGatewayId,
                  ("customerAddress" .=) <$> _nCustomerAddress,
                  ("amazonAddress" .=) <$> _nAmazonAddress,
                  ("addressFamily" .=) <$> _nAddressFamily,
                  ("directConnectGatewayId" .=) <$>
                    _nDirectConnectGatewayId,
                  ("authKey" .=) <$> _nAuthKey,
                  Just
                    ("virtualInterfaceName" .= _nVirtualInterfaceName),
                  Just ("vlan" .= _nVlan), Just ("asn" .= _nAsn)])

-- | A structure containing information about a private virtual interface that will be provisioned on a connection.
--
--
--
-- /See:/ 'newPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
  { _npviaCustomerAddress      :: !(Maybe Text)
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
-- * 'npviaCustomerAddress' - Undocumented member.
--
-- * 'npviaAmazonAddress' - Undocumented member.
--
-- * 'npviaAddressFamily' - Undocumented member.
--
-- * 'npviaAuthKey' - Undocumented member.
--
-- * 'npviaVirtualInterfaceName' - Undocumented member.
--
-- * 'npviaVlan' - Undocumented member.
--
-- * 'npviaAsn' - Undocumented member.
newPrivateVirtualInterfaceAllocation
    :: Text -- ^ 'npviaVirtualInterfaceName'
    -> Int -- ^ 'npviaVlan'
    -> Int -- ^ 'npviaAsn'
    -> NewPrivateVirtualInterfaceAllocation
newPrivateVirtualInterfaceAllocation pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPrivateVirtualInterfaceAllocation'
    { _npviaCustomerAddress = Nothing
    , _npviaAmazonAddress = Nothing
    , _npviaAddressFamily = Nothing
    , _npviaAuthKey = Nothing
    , _npviaVirtualInterfaceName = pVirtualInterfaceName_
    , _npviaVlan = pVlan_
    , _npviaAsn = pAsn_
    }


-- | Undocumented member.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress = lens _npviaCustomerAddress (\ s a -> s{_npviaCustomerAddress = a})

-- | Undocumented member.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress = lens _npviaAmazonAddress (\ s a -> s{_npviaAmazonAddress = a})

-- | Undocumented member.
npviaAddressFamily :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe AddressFamily)
npviaAddressFamily = lens _npviaAddressFamily (\ s a -> s{_npviaAddressFamily = a})

-- | Undocumented member.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\ s a -> s{_npviaAuthKey = a})

-- | Undocumented member.
npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation Text
npviaVirtualInterfaceName = lens _npviaVirtualInterfaceName (\ s a -> s{_npviaVirtualInterfaceName = a})

-- | Undocumented member.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaVlan = lens _npviaVlan (\ s a -> s{_npviaVlan = a})

-- | Undocumented member.
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
                 [("customerAddress" .=) <$> _npviaCustomerAddress,
                  ("amazonAddress" .=) <$> _npviaAmazonAddress,
                  ("addressFamily" .=) <$> _npviaAddressFamily,
                  ("authKey" .=) <$> _npviaAuthKey,
                  Just
                    ("virtualInterfaceName" .=
                       _npviaVirtualInterfaceName),
                  Just ("vlan" .= _npviaVlan),
                  Just ("asn" .= _npviaAsn)])

-- | A structure containing information about a new public virtual interface.
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
-- * 'npviRouteFilterPrefixes' - Undocumented member.
--
-- * 'npviCustomerAddress' - Undocumented member.
--
-- * 'npviAmazonAddress' - Undocumented member.
--
-- * 'npviAddressFamily' - Undocumented member.
--
-- * 'npviAuthKey' - Undocumented member.
--
-- * 'npviVirtualInterfaceName' - Undocumented member.
--
-- * 'npviVlan' - Undocumented member.
--
-- * 'npviAsn' - Undocumented member.
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


-- | Undocumented member.
npviRouteFilterPrefixes :: Lens' NewPublicVirtualInterface [RouteFilterPrefix]
npviRouteFilterPrefixes = lens _npviRouteFilterPrefixes (\ s a -> s{_npviRouteFilterPrefixes = a}) . _Default . _Coerce

-- | Undocumented member.
npviCustomerAddress :: Lens' NewPublicVirtualInterface (Maybe Text)
npviCustomerAddress = lens _npviCustomerAddress (\ s a -> s{_npviCustomerAddress = a})

-- | Undocumented member.
npviAmazonAddress :: Lens' NewPublicVirtualInterface (Maybe Text)
npviAmazonAddress = lens _npviAmazonAddress (\ s a -> s{_npviAmazonAddress = a})

-- | Undocumented member.
npviAddressFamily :: Lens' NewPublicVirtualInterface (Maybe AddressFamily)
npviAddressFamily = lens _npviAddressFamily (\ s a -> s{_npviAddressFamily = a})

-- | Undocumented member.
npviAuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npviAuthKey = lens _npviAuthKey (\ s a -> s{_npviAuthKey = a})

-- | Undocumented member.
npviVirtualInterfaceName :: Lens' NewPublicVirtualInterface Text
npviVirtualInterfaceName = lens _npviVirtualInterfaceName (\ s a -> s{_npviVirtualInterfaceName = a})

-- | Undocumented member.
npviVlan :: Lens' NewPublicVirtualInterface Int
npviVlan = lens _npviVlan (\ s a -> s{_npviVlan = a})

-- | Undocumented member.
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

-- | A structure containing information about a public virtual interface that will be provisioned on a connection.
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
-- * 'newRouteFilterPrefixes' - Undocumented member.
--
-- * 'newCustomerAddress' - Undocumented member.
--
-- * 'newAmazonAddress' - Undocumented member.
--
-- * 'newAddressFamily' - Undocumented member.
--
-- * 'newAuthKey' - Undocumented member.
--
-- * 'newVirtualInterfaceName' - Undocumented member.
--
-- * 'newVlan' - Undocumented member.
--
-- * 'newAsn' - Undocumented member.
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


-- | Undocumented member.
newRouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation [RouteFilterPrefix]
newRouteFilterPrefixes = lens _newRouteFilterPrefixes (\ s a -> s{_newRouteFilterPrefixes = a}) . _Default . _Coerce

-- | Undocumented member.
newCustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newCustomerAddress = lens _newCustomerAddress (\ s a -> s{_newCustomerAddress = a})

-- | Undocumented member.
newAmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newAmazonAddress = lens _newAmazonAddress (\ s a -> s{_newAmazonAddress = a})

-- | Undocumented member.
newAddressFamily :: Lens' NewPublicVirtualInterfaceAllocation (Maybe AddressFamily)
newAddressFamily = lens _newAddressFamily (\ s a -> s{_newAddressFamily = a})

-- | Undocumented member.
newAuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newAuthKey = lens _newAuthKey (\ s a -> s{_newAuthKey = a})

-- | Undocumented member.
newVirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation Text
newVirtualInterfaceName = lens _newVirtualInterfaceName (\ s a -> s{_newVirtualInterfaceName = a})

-- | Undocumented member.
newVlan :: Lens' NewPublicVirtualInterfaceAllocation Int
newVlan = lens _newVlan (\ s a -> s{_newVlan = a})

-- | Undocumented member.
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

-- | The tags associated with a Direct Connect resource.
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
-- * 'rtResourceARN' - The Amazon Resource Name (ARN) of the Direct Connect resource.
--
-- * 'rtTags' - The tags.
resourceTag
    :: ResourceTag
resourceTag = ResourceTag' {_rtResourceARN = Nothing, _rtTags = Nothing}


-- | The Amazon Resource Name (ARN) of the Direct Connect resource.
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

-- | A route filter prefix that the customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.
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
-- * 'rfpCidr' - CIDR notation for the advertised route. Multiple routes are separated by commas. IPv6 CIDRs must be at least a /64 or shorter Example: 10.10.10.0/24,10.10.11.0/24,2001:db8::/64
routeFilterPrefix
    :: RouteFilterPrefix
routeFilterPrefix = RouteFilterPrefix' {_rfpCidr = Nothing}


-- | CIDR notation for the advertised route. Multiple routes are separated by commas. IPv6 CIDRs must be at least a /64 or shorter Example: 10.10.10.0/24,10.10.11.0/24,2001:db8::/64
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
-- * 'tagValue' - The value of the tag.
--
-- * 'tagKey' - The key of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key of the tag.
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

-- | You can create one or more AWS Direct Connect private virtual interfaces linking to your virtual private gateway.
--
--
-- Virtual private gateways can be managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html Amazon EC2 CreateVpnGateway action> .
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
-- * 'vgVirtualGatewayId' - Undocumented member.
--
-- * 'vgVirtualGatewayState' - Undocumented member.
virtualGateway
    :: VirtualGateway
virtualGateway =
  VirtualGateway'
    {_vgVirtualGatewayId = Nothing, _vgVirtualGatewayState = Nothing}


-- | Undocumented member.
vgVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayId = lens _vgVirtualGatewayId (\ s a -> s{_vgVirtualGatewayId = a})

-- | Undocumented member.
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

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.
--
--
--
-- /See:/ 'virtualInterface' smart constructor.
data VirtualInterface = VirtualInterface'
  { _viBgpPeers               :: !(Maybe [BGPPeer])
  , _viVirtualGatewayId       :: !(Maybe Text)
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
  , _viCustomerRouterConfig   :: !(Maybe Text)
  , _viOwnerAccount           :: !(Maybe Text)
  , _viVirtualInterfaceName   :: !(Maybe Text)
  , _viVirtualInterfaceId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viBgpPeers' - Undocumented member.
--
-- * 'viVirtualGatewayId' - Undocumented member.
--
-- * 'viRouteFilterPrefixes' - Undocumented member.
--
-- * 'viCustomerAddress' - Undocumented member.
--
-- * 'viVlan' - Undocumented member.
--
-- * 'viLocation' - Undocumented member.
--
-- * 'viAmazonAddress' - Undocumented member.
--
-- * 'viAddressFamily' - Undocumented member.
--
-- * 'viVirtualInterfaceState' - Undocumented member.
--
-- * 'viConnectionId' - Undocumented member.
--
-- * 'viDirectConnectGatewayId' - Undocumented member.
--
-- * 'viAmazonSideASN' - The autonomous system number (ASN) for the Amazon side of the connection.
--
-- * 'viVirtualInterfaceType' - Undocumented member.
--
-- * 'viAsn' - Undocumented member.
--
-- * 'viAuthKey' - Undocumented member.
--
-- * 'viCustomerRouterConfig' - Information for generating the customer router configuration.
--
-- * 'viOwnerAccount' - The AWS account that will own the new virtual interface.
--
-- * 'viVirtualInterfaceName' - Undocumented member.
--
-- * 'viVirtualInterfaceId' - Undocumented member.
virtualInterface
    :: VirtualInterface
virtualInterface =
  VirtualInterface'
    { _viBgpPeers = Nothing
    , _viVirtualGatewayId = Nothing
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
    , _viCustomerRouterConfig = Nothing
    , _viOwnerAccount = Nothing
    , _viVirtualInterfaceName = Nothing
    , _viVirtualInterfaceId = Nothing
    }


-- | Undocumented member.
viBgpPeers :: Lens' VirtualInterface [BGPPeer]
viBgpPeers = lens _viBgpPeers (\ s a -> s{_viBgpPeers = a}) . _Default . _Coerce

-- | Undocumented member.
viVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
viVirtualGatewayId = lens _viVirtualGatewayId (\ s a -> s{_viVirtualGatewayId = a})

-- | Undocumented member.
viRouteFilterPrefixes :: Lens' VirtualInterface [RouteFilterPrefix]
viRouteFilterPrefixes = lens _viRouteFilterPrefixes (\ s a -> s{_viRouteFilterPrefixes = a}) . _Default . _Coerce

-- | Undocumented member.
viCustomerAddress :: Lens' VirtualInterface (Maybe Text)
viCustomerAddress = lens _viCustomerAddress (\ s a -> s{_viCustomerAddress = a})

-- | Undocumented member.
viVlan :: Lens' VirtualInterface (Maybe Int)
viVlan = lens _viVlan (\ s a -> s{_viVlan = a})

-- | Undocumented member.
viLocation :: Lens' VirtualInterface (Maybe Text)
viLocation = lens _viLocation (\ s a -> s{_viLocation = a})

-- | Undocumented member.
viAmazonAddress :: Lens' VirtualInterface (Maybe Text)
viAmazonAddress = lens _viAmazonAddress (\ s a -> s{_viAmazonAddress = a})

-- | Undocumented member.
viAddressFamily :: Lens' VirtualInterface (Maybe AddressFamily)
viAddressFamily = lens _viAddressFamily (\ s a -> s{_viAddressFamily = a})

-- | Undocumented member.
viVirtualInterfaceState :: Lens' VirtualInterface (Maybe VirtualInterfaceState)
viVirtualInterfaceState = lens _viVirtualInterfaceState (\ s a -> s{_viVirtualInterfaceState = a})

-- | Undocumented member.
viConnectionId :: Lens' VirtualInterface (Maybe Text)
viConnectionId = lens _viConnectionId (\ s a -> s{_viConnectionId = a})

-- | Undocumented member.
viDirectConnectGatewayId :: Lens' VirtualInterface (Maybe Text)
viDirectConnectGatewayId = lens _viDirectConnectGatewayId (\ s a -> s{_viDirectConnectGatewayId = a})

-- | The autonomous system number (ASN) for the Amazon side of the connection.
viAmazonSideASN :: Lens' VirtualInterface (Maybe Integer)
viAmazonSideASN = lens _viAmazonSideASN (\ s a -> s{_viAmazonSideASN = a})

-- | Undocumented member.
viVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceType = lens _viVirtualInterfaceType (\ s a -> s{_viVirtualInterfaceType = a})

-- | Undocumented member.
viAsn :: Lens' VirtualInterface (Maybe Int)
viAsn = lens _viAsn (\ s a -> s{_viAsn = a})

-- | Undocumented member.
viAuthKey :: Lens' VirtualInterface (Maybe Text)
viAuthKey = lens _viAuthKey (\ s a -> s{_viAuthKey = a})

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
viCustomerRouterConfig = lens _viCustomerRouterConfig (\ s a -> s{_viCustomerRouterConfig = a})

-- | The AWS account that will own the new virtual interface.
viOwnerAccount :: Lens' VirtualInterface (Maybe Text)
viOwnerAccount = lens _viOwnerAccount (\ s a -> s{_viOwnerAccount = a})

-- | Undocumented member.
viVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceName = lens _viVirtualInterfaceName (\ s a -> s{_viVirtualInterfaceName = a})

-- | Undocumented member.
viVirtualInterfaceId :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceId = lens _viVirtualInterfaceId (\ s a -> s{_viVirtualInterfaceId = a})

instance FromJSON VirtualInterface where
        parseJSON
          = withObject "VirtualInterface"
              (\ x ->
                 VirtualInterface' <$>
                   (x .:? "bgpPeers" .!= mempty) <*>
                     (x .:? "virtualGatewayId")
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
                     <*> (x .:? "customerRouterConfig")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "virtualInterfaceName")
                     <*> (x .:? "virtualInterfaceId"))

instance Hashable VirtualInterface where

instance NFData VirtualInterface where

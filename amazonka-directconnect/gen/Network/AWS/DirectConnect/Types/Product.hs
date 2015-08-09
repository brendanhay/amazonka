{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Product where

import Network.AWS.DirectConnect.Types.Sum
import Network.AWS.Prelude

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
--
-- /See:/ 'connection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cVlan'
--
-- * 'cLocation'
--
-- * 'cConnectionId'
--
-- * 'cConnectionName'
--
-- * 'cPartnerName'
--
-- * 'cBandwidth'
--
-- * 'cRegion'
--
-- * 'cOwnerAccount'
--
-- * 'cConnectionState'
data Connection = Connection'
    { _cVlan :: !(Maybe Int)
    , _cLocation :: !(Maybe Text)
    , _cConnectionId :: !(Maybe Text)
    , _cConnectionName :: !(Maybe Text)
    , _cPartnerName :: !(Maybe Text)
    , _cBandwidth :: !(Maybe Text)
    , _cRegion :: !(Maybe Text)
    , _cOwnerAccount :: !(Maybe Text)
    , _cConnectionState :: !(Maybe ConnectionState)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Connection' smart constructor.
connection :: Connection
connection = 
    Connection'
    { _cVlan = Nothing
    , _cLocation = Nothing
    , _cConnectionId = Nothing
    , _cConnectionName = Nothing
    , _cPartnerName = Nothing
    , _cBandwidth = Nothing
    , _cRegion = Nothing
    , _cOwnerAccount = Nothing
    , _cConnectionState = Nothing
    }

-- | Undocumented member.
cVlan :: Lens' Connection (Maybe Int)
cVlan = lens _cVlan (\ s a -> s{_cVlan = a});

-- | Undocumented member.
cLocation :: Lens' Connection (Maybe Text)
cLocation = lens _cLocation (\ s a -> s{_cLocation = a});

-- | Undocumented member.
cConnectionId :: Lens' Connection (Maybe Text)
cConnectionId = lens _cConnectionId (\ s a -> s{_cConnectionId = a});

-- | Undocumented member.
cConnectionName :: Lens' Connection (Maybe Text)
cConnectionName = lens _cConnectionName (\ s a -> s{_cConnectionName = a});

-- | Undocumented member.
cPartnerName :: Lens' Connection (Maybe Text)
cPartnerName = lens _cPartnerName (\ s a -> s{_cPartnerName = a});

-- | Bandwidth of the connection.
--
-- Example: 1Gbps (for regular connections), or 500Mbps (for hosted
-- connections)
--
-- Default: None
cBandwidth :: Lens' Connection (Maybe Text)
cBandwidth = lens _cBandwidth (\ s a -> s{_cBandwidth = a});

-- | Undocumented member.
cRegion :: Lens' Connection (Maybe Text)
cRegion = lens _cRegion (\ s a -> s{_cRegion = a});

-- | Undocumented member.
cOwnerAccount :: Lens' Connection (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\ s a -> s{_cOwnerAccount = a});

-- | Undocumented member.
cConnectionState :: Lens' Connection (Maybe ConnectionState)
cConnectionState = lens _cConnectionState (\ s a -> s{_cConnectionState = a});

instance FromJSON Connection where
        parseJSON
          = withObject "Connection"
              (\ x ->
                 Connection' <$>
                   (x .:? "vlan") <*> (x .:? "location") <*>
                     (x .:? "connectionId")
                     <*> (x .:? "connectionName")
                     <*> (x .:? "partnerName")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "region")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "connectionState"))

-- | A structure containing a list of connections.
--
-- /See:/ 'connections' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cConnections'
newtype Connections = Connections'
    { _cConnections :: Maybe [Connection]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Connections' smart constructor.
connections :: Connections
connections = 
    Connections'
    { _cConnections = Nothing
    }

-- | A list of connections.
cConnections :: Lens' Connections [Connection]
cConnections = lens _cConnections (\ s a -> s{_cConnections = a}) . _Default . _Coerce;

instance FromJSON Connections where
        parseJSON
          = withObject "Connections"
              (\ x ->
                 Connections' <$> (x .:? "connections" .!= mempty))

-- | An interconnect is a connection that can host other connections.
--
-- Like a standard AWS Direct Connect connection, an interconnect
-- represents the physical connection between an AWS Direct Connect
-- partner\'s network and a specific Direct Connect location. An AWS Direct
-- Connect partner who owns an interconnect can provision hosted
-- connections on the interconnect for their end customers, thereby
-- providing the end customers with connectivity to AWS services.
--
-- The resources of the interconnect, including bandwidth and VLAN numbers,
-- are shared by all of the hosted connections on the interconnect, and the
-- owner of the interconnect determines how these resources are assigned.
--
-- /See:/ 'interconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iInterconnectId'
--
-- * 'iInterconnectName'
--
-- * 'iLocation'
--
-- * 'iBandwidth'
--
-- * 'iInterconnectState'
--
-- * 'iRegion'
data Interconnect = Interconnect'
    { _iInterconnectId :: !(Maybe Text)
    , _iInterconnectName :: !(Maybe Text)
    , _iLocation :: !(Maybe Text)
    , _iBandwidth :: !(Maybe Text)
    , _iInterconnectState :: !(Maybe InterconnectState)
    , _iRegion :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Interconnect' smart constructor.
interconnect :: Interconnect
interconnect = 
    Interconnect'
    { _iInterconnectId = Nothing
    , _iInterconnectName = Nothing
    , _iLocation = Nothing
    , _iBandwidth = Nothing
    , _iInterconnectState = Nothing
    , _iRegion = Nothing
    }

-- | Undocumented member.
iInterconnectId :: Lens' Interconnect (Maybe Text)
iInterconnectId = lens _iInterconnectId (\ s a -> s{_iInterconnectId = a});

-- | Undocumented member.
iInterconnectName :: Lens' Interconnect (Maybe Text)
iInterconnectName = lens _iInterconnectName (\ s a -> s{_iInterconnectName = a});

-- | Undocumented member.
iLocation :: Lens' Interconnect (Maybe Text)
iLocation = lens _iLocation (\ s a -> s{_iLocation = a});

-- | Undocumented member.
iBandwidth :: Lens' Interconnect (Maybe Text)
iBandwidth = lens _iBandwidth (\ s a -> s{_iBandwidth = a});

-- | Undocumented member.
iInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
iInterconnectState = lens _iInterconnectState (\ s a -> s{_iInterconnectState = a});

-- | Undocumented member.
iRegion :: Lens' Interconnect (Maybe Text)
iRegion = lens _iRegion (\ s a -> s{_iRegion = a});

instance FromJSON Interconnect where
        parseJSON
          = withObject "Interconnect"
              (\ x ->
                 Interconnect' <$>
                   (x .:? "interconnectId") <*>
                     (x .:? "interconnectName")
                     <*> (x .:? "location")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "interconnectState")
                     <*> (x .:? "region"))

-- | An AWS Direct Connect location where connections and interconnects can
-- be requested.
--
-- /See:/ 'location' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lLocationName'
--
-- * 'lLocationCode'
data Location = Location'
    { _lLocationName :: !(Maybe Text)
    , _lLocationCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Location' smart constructor.
location :: Location
location = 
    Location'
    { _lLocationName = Nothing
    , _lLocationCode = Nothing
    }

-- | The name of the AWS Direct Connect location. The name includes the
-- colocation partner name and the physical site of the lit building.
lLocationName :: Lens' Location (Maybe Text)
lLocationName = lens _lLocationName (\ s a -> s{_lLocationName = a});

-- | The code used to indicate the AWS Direct Connect location.
lLocationCode :: Lens' Location (Maybe Text)
lLocationCode = lens _lLocationCode (\ s a -> s{_lLocationCode = a});

instance FromJSON Location where
        parseJSON
          = withObject "Location"
              (\ x ->
                 Location' <$>
                   (x .:? "locationName") <*> (x .:? "locationCode"))

-- | A structure containing information about a new private virtual
-- interface.
--
-- /See:/ 'newPrivateVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nCustomerAddress'
--
-- * 'nAmazonAddress'
--
-- * 'nAuthKey'
--
-- * 'nVirtualInterfaceName'
--
-- * 'nVlan'
--
-- * 'nAsn'
--
-- * 'nVirtualGatewayId'
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
    { _nCustomerAddress :: !(Maybe Text)
    , _nAmazonAddress :: !(Maybe Text)
    , _nAuthKey :: !(Maybe Text)
    , _nVirtualInterfaceName :: !Text
    , _nVlan :: !Int
    , _nAsn :: !Int
    , _nVirtualGatewayId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NewPrivateVirtualInterface' smart constructor.
newPrivateVirtualInterface :: Text -> Int -> Int -> Text -> NewPrivateVirtualInterface
newPrivateVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ pVirtualGatewayId_ = 
    NewPrivateVirtualInterface'
    { _nCustomerAddress = Nothing
    , _nAmazonAddress = Nothing
    , _nAuthKey = Nothing
    , _nVirtualInterfaceName = pVirtualInterfaceName_
    , _nVlan = pVlan_
    , _nAsn = pAsn_
    , _nVirtualGatewayId = pVirtualGatewayId_
    }

-- | Undocumented member.
nCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nCustomerAddress = lens _nCustomerAddress (\ s a -> s{_nCustomerAddress = a});

-- | Undocumented member.
nAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAmazonAddress = lens _nAmazonAddress (\ s a -> s{_nAmazonAddress = a});

-- | Undocumented member.
nAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
nAuthKey = lens _nAuthKey (\ s a -> s{_nAuthKey = a});

-- | Undocumented member.
nVirtualInterfaceName :: Lens' NewPrivateVirtualInterface Text
nVirtualInterfaceName = lens _nVirtualInterfaceName (\ s a -> s{_nVirtualInterfaceName = a});

-- | Undocumented member.
nVlan :: Lens' NewPrivateVirtualInterface Int
nVlan = lens _nVlan (\ s a -> s{_nVlan = a});

-- | Undocumented member.
nAsn :: Lens' NewPrivateVirtualInterface Int
nAsn = lens _nAsn (\ s a -> s{_nAsn = a});

-- | Undocumented member.
nVirtualGatewayId :: Lens' NewPrivateVirtualInterface Text
nVirtualGatewayId = lens _nVirtualGatewayId (\ s a -> s{_nVirtualGatewayId = a});

instance ToJSON NewPrivateVirtualInterface where
        toJSON NewPrivateVirtualInterface'{..}
          = object
              ["customerAddress" .= _nCustomerAddress,
               "amazonAddress" .= _nAmazonAddress,
               "authKey" .= _nAuthKey,
               "virtualInterfaceName" .= _nVirtualInterfaceName,
               "vlan" .= _nVlan, "asn" .= _nAsn,
               "virtualGatewayId" .= _nVirtualGatewayId]

-- | A structure containing information about a private virtual interface
-- that will be provisioned on a connection.
--
-- /See:/ 'newPrivateVirtualInterfaceAllocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'npviaCustomerAddress'
--
-- * 'npviaAmazonAddress'
--
-- * 'npviaAuthKey'
--
-- * 'npviaVirtualInterfaceName'
--
-- * 'npviaVlan'
--
-- * 'npviaAsn'
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
    { _npviaCustomerAddress :: !(Maybe Text)
    , _npviaAmazonAddress :: !(Maybe Text)
    , _npviaAuthKey :: !(Maybe Text)
    , _npviaVirtualInterfaceName :: !Text
    , _npviaVlan :: !Int
    , _npviaAsn :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NewPrivateVirtualInterfaceAllocation' smart constructor.
newPrivateVirtualInterfaceAllocation :: Text -> Int -> Int -> NewPrivateVirtualInterfaceAllocation
newPrivateVirtualInterfaceAllocation pVirtualInterfaceName_ pVlan_ pAsn_ = 
    NewPrivateVirtualInterfaceAllocation'
    { _npviaCustomerAddress = Nothing
    , _npviaAmazonAddress = Nothing
    , _npviaAuthKey = Nothing
    , _npviaVirtualInterfaceName = pVirtualInterfaceName_
    , _npviaVlan = pVlan_
    , _npviaAsn = pAsn_
    }

-- | Undocumented member.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress = lens _npviaCustomerAddress (\ s a -> s{_npviaCustomerAddress = a});

-- | Undocumented member.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress = lens _npviaAmazonAddress (\ s a -> s{_npviaAmazonAddress = a});

-- | Undocumented member.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\ s a -> s{_npviaAuthKey = a});

-- | Undocumented member.
npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation Text
npviaVirtualInterfaceName = lens _npviaVirtualInterfaceName (\ s a -> s{_npviaVirtualInterfaceName = a});

-- | Undocumented member.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaVlan = lens _npviaVlan (\ s a -> s{_npviaVlan = a});

-- | Undocumented member.
npviaAsn :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaAsn = lens _npviaAsn (\ s a -> s{_npviaAsn = a});

instance ToJSON NewPrivateVirtualInterfaceAllocation
         where
        toJSON NewPrivateVirtualInterfaceAllocation'{..}
          = object
              ["customerAddress" .= _npviaCustomerAddress,
               "amazonAddress" .= _npviaAmazonAddress,
               "authKey" .= _npviaAuthKey,
               "virtualInterfaceName" .= _npviaVirtualInterfaceName,
               "vlan" .= _npviaVlan, "asn" .= _npviaAsn]

-- | A structure containing information about a new public virtual interface.
--
-- /See:/ 'newPublicVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'npviAuthKey'
--
-- * 'npviVirtualInterfaceName'
--
-- * 'npviVlan'
--
-- * 'npviAsn'
--
-- * 'npviAmazonAddress'
--
-- * 'npviCustomerAddress'
--
-- * 'npviRouteFilterPrefixes'
data NewPublicVirtualInterface = NewPublicVirtualInterface'
    { _npviAuthKey :: !(Maybe Text)
    , _npviVirtualInterfaceName :: !Text
    , _npviVlan :: !Int
    , _npviAsn :: !Int
    , _npviAmazonAddress :: !Text
    , _npviCustomerAddress :: !Text
    , _npviRouteFilterPrefixes :: ![RouteFilterPrefix]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NewPublicVirtualInterface' smart constructor.
newPublicVirtualInterface :: Text -> Int -> Int -> Text -> Text -> NewPublicVirtualInterface
newPublicVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ pAmazonAddress_ pCustomerAddress_ = 
    NewPublicVirtualInterface'
    { _npviAuthKey = Nothing
    , _npviVirtualInterfaceName = pVirtualInterfaceName_
    , _npviVlan = pVlan_
    , _npviAsn = pAsn_
    , _npviAmazonAddress = pAmazonAddress_
    , _npviCustomerAddress = pCustomerAddress_
    , _npviRouteFilterPrefixes = mempty
    }

-- | Undocumented member.
npviAuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npviAuthKey = lens _npviAuthKey (\ s a -> s{_npviAuthKey = a});

-- | Undocumented member.
npviVirtualInterfaceName :: Lens' NewPublicVirtualInterface Text
npviVirtualInterfaceName = lens _npviVirtualInterfaceName (\ s a -> s{_npviVirtualInterfaceName = a});

-- | Undocumented member.
npviVlan :: Lens' NewPublicVirtualInterface Int
npviVlan = lens _npviVlan (\ s a -> s{_npviVlan = a});

-- | Undocumented member.
npviAsn :: Lens' NewPublicVirtualInterface Int
npviAsn = lens _npviAsn (\ s a -> s{_npviAsn = a});

-- | Undocumented member.
npviAmazonAddress :: Lens' NewPublicVirtualInterface Text
npviAmazonAddress = lens _npviAmazonAddress (\ s a -> s{_npviAmazonAddress = a});

-- | Undocumented member.
npviCustomerAddress :: Lens' NewPublicVirtualInterface Text
npviCustomerAddress = lens _npviCustomerAddress (\ s a -> s{_npviCustomerAddress = a});

-- | Undocumented member.
npviRouteFilterPrefixes :: Lens' NewPublicVirtualInterface [RouteFilterPrefix]
npviRouteFilterPrefixes = lens _npviRouteFilterPrefixes (\ s a -> s{_npviRouteFilterPrefixes = a}) . _Coerce;

instance ToJSON NewPublicVirtualInterface where
        toJSON NewPublicVirtualInterface'{..}
          = object
              ["authKey" .= _npviAuthKey,
               "virtualInterfaceName" .= _npviVirtualInterfaceName,
               "vlan" .= _npviVlan, "asn" .= _npviAsn,
               "amazonAddress" .= _npviAmazonAddress,
               "customerAddress" .= _npviCustomerAddress,
               "routeFilterPrefixes" .= _npviRouteFilterPrefixes]

-- | A structure containing information about a public virtual interface that
-- will be provisioned on a connection.
--
-- /See:/ 'newPublicVirtualInterfaceAllocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'newAuthKey'
--
-- * 'newVirtualInterfaceName'
--
-- * 'newVlan'
--
-- * 'newAsn'
--
-- * 'newAmazonAddress'
--
-- * 'newCustomerAddress'
--
-- * 'newRouteFilterPrefixes'
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
    { _newAuthKey :: !(Maybe Text)
    , _newVirtualInterfaceName :: !Text
    , _newVlan :: !Int
    , _newAsn :: !Int
    , _newAmazonAddress :: !Text
    , _newCustomerAddress :: !Text
    , _newRouteFilterPrefixes :: ![RouteFilterPrefix]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NewPublicVirtualInterfaceAllocation' smart constructor.
newPublicVirtualInterfaceAllocation :: Text -> Int -> Int -> Text -> Text -> NewPublicVirtualInterfaceAllocation
newPublicVirtualInterfaceAllocation pVirtualInterfaceName_ pVlan_ pAsn_ pAmazonAddress_ pCustomerAddress_ = 
    NewPublicVirtualInterfaceAllocation'
    { _newAuthKey = Nothing
    , _newVirtualInterfaceName = pVirtualInterfaceName_
    , _newVlan = pVlan_
    , _newAsn = pAsn_
    , _newAmazonAddress = pAmazonAddress_
    , _newCustomerAddress = pCustomerAddress_
    , _newRouteFilterPrefixes = mempty
    }

-- | Undocumented member.
newAuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
newAuthKey = lens _newAuthKey (\ s a -> s{_newAuthKey = a});

-- | Undocumented member.
newVirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation Text
newVirtualInterfaceName = lens _newVirtualInterfaceName (\ s a -> s{_newVirtualInterfaceName = a});

-- | Undocumented member.
newVlan :: Lens' NewPublicVirtualInterfaceAllocation Int
newVlan = lens _newVlan (\ s a -> s{_newVlan = a});

-- | Undocumented member.
newAsn :: Lens' NewPublicVirtualInterfaceAllocation Int
newAsn = lens _newAsn (\ s a -> s{_newAsn = a});

-- | Undocumented member.
newAmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
newAmazonAddress = lens _newAmazonAddress (\ s a -> s{_newAmazonAddress = a});

-- | Undocumented member.
newCustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
newCustomerAddress = lens _newCustomerAddress (\ s a -> s{_newCustomerAddress = a});

-- | Undocumented member.
newRouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation [RouteFilterPrefix]
newRouteFilterPrefixes = lens _newRouteFilterPrefixes (\ s a -> s{_newRouteFilterPrefixes = a}) . _Coerce;

instance ToJSON NewPublicVirtualInterfaceAllocation
         where
        toJSON NewPublicVirtualInterfaceAllocation'{..}
          = object
              ["authKey" .= _newAuthKey,
               "virtualInterfaceName" .= _newVirtualInterfaceName,
               "vlan" .= _newVlan, "asn" .= _newAsn,
               "amazonAddress" .= _newAmazonAddress,
               "customerAddress" .= _newCustomerAddress,
               "routeFilterPrefixes" .= _newRouteFilterPrefixes]

-- | A route filter prefix that the customer can advertise through Border
-- Gateway Protocol (BGP) over a public virtual interface.
--
-- /See:/ 'routeFilterPrefix' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rfpCidr'
newtype RouteFilterPrefix = RouteFilterPrefix'
    { _rfpCidr :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RouteFilterPrefix' smart constructor.
routeFilterPrefix :: RouteFilterPrefix
routeFilterPrefix = 
    RouteFilterPrefix'
    { _rfpCidr = Nothing
    }

-- | CIDR notation for the advertised route. Multiple routes are separated by
-- commas.
--
-- Example: 10.10.10.0\/24,10.10.11.0\/24
rfpCidr :: Lens' RouteFilterPrefix (Maybe Text)
rfpCidr = lens _rfpCidr (\ s a -> s{_rfpCidr = a});

instance FromJSON RouteFilterPrefix where
        parseJSON
          = withObject "RouteFilterPrefix"
              (\ x -> RouteFilterPrefix' <$> (x .:? "cidr"))

instance ToJSON RouteFilterPrefix where
        toJSON RouteFilterPrefix'{..}
          = object ["cidr" .= _rfpCidr]

-- | You can create one or more AWS Direct Connect private virtual interfaces
-- linking to your virtual private gateway.
--
-- Virtual private gateways can be managed using the Amazon Virtual Private
-- Cloud (Amazon VPC) console or the
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html Amazon EC2 CreateVpnGateway action>.
--
-- /See:/ 'virtualGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vgVirtualGatewayId'
--
-- * 'vgVirtualGatewayState'
data VirtualGateway = VirtualGateway'
    { _vgVirtualGatewayId :: !(Maybe Text)
    , _vgVirtualGatewayState :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VirtualGateway' smart constructor.
virtualGateway :: VirtualGateway
virtualGateway = 
    VirtualGateway'
    { _vgVirtualGatewayId = Nothing
    , _vgVirtualGatewayState = Nothing
    }

-- | Undocumented member.
vgVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayId = lens _vgVirtualGatewayId (\ s a -> s{_vgVirtualGatewayId = a});

-- | Undocumented member.
vgVirtualGatewayState :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayState = lens _vgVirtualGatewayState (\ s a -> s{_vgVirtualGatewayState = a});

instance FromJSON VirtualGateway where
        parseJSON
          = withObject "VirtualGateway"
              (\ x ->
                 VirtualGateway' <$>
                   (x .:? "virtualGatewayId") <*>
                     (x .:? "virtualGatewayState"))

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
--
-- /See:/ 'virtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'viVirtualGatewayId'
--
-- * 'viRouteFilterPrefixes'
--
-- * 'viCustomerAddress'
--
-- * 'viVlan'
--
-- * 'viLocation'
--
-- * 'viAmazonAddress'
--
-- * 'viVirtualInterfaceState'
--
-- * 'viConnectionId'
--
-- * 'viAsn'
--
-- * 'viVirtualInterfaceType'
--
-- * 'viAuthKey'
--
-- * 'viCustomerRouterConfig'
--
-- * 'viOwnerAccount'
--
-- * 'viVirtualInterfaceName'
--
-- * 'viVirtualInterfaceId'
data VirtualInterface = VirtualInterface'
    { _viVirtualGatewayId :: !(Maybe Text)
    , _viRouteFilterPrefixes :: !(Maybe [RouteFilterPrefix])
    , _viCustomerAddress :: !(Maybe Text)
    , _viVlan :: !(Maybe Int)
    , _viLocation :: !(Maybe Text)
    , _viAmazonAddress :: !(Maybe Text)
    , _viVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _viConnectionId :: !(Maybe Text)
    , _viAsn :: !(Maybe Int)
    , _viVirtualInterfaceType :: !(Maybe Text)
    , _viAuthKey :: !(Maybe Text)
    , _viCustomerRouterConfig :: !(Maybe Text)
    , _viOwnerAccount :: !(Maybe Text)
    , _viVirtualInterfaceName :: !(Maybe Text)
    , _viVirtualInterfaceId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VirtualInterface' smart constructor.
virtualInterface :: VirtualInterface
virtualInterface = 
    VirtualInterface'
    { _viVirtualGatewayId = Nothing
    , _viRouteFilterPrefixes = Nothing
    , _viCustomerAddress = Nothing
    , _viVlan = Nothing
    , _viLocation = Nothing
    , _viAmazonAddress = Nothing
    , _viVirtualInterfaceState = Nothing
    , _viConnectionId = Nothing
    , _viAsn = Nothing
    , _viVirtualInterfaceType = Nothing
    , _viAuthKey = Nothing
    , _viCustomerRouterConfig = Nothing
    , _viOwnerAccount = Nothing
    , _viVirtualInterfaceName = Nothing
    , _viVirtualInterfaceId = Nothing
    }

-- | Undocumented member.
viVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
viVirtualGatewayId = lens _viVirtualGatewayId (\ s a -> s{_viVirtualGatewayId = a});

-- | Undocumented member.
viRouteFilterPrefixes :: Lens' VirtualInterface [RouteFilterPrefix]
viRouteFilterPrefixes = lens _viRouteFilterPrefixes (\ s a -> s{_viRouteFilterPrefixes = a}) . _Default . _Coerce;

-- | Undocumented member.
viCustomerAddress :: Lens' VirtualInterface (Maybe Text)
viCustomerAddress = lens _viCustomerAddress (\ s a -> s{_viCustomerAddress = a});

-- | Undocumented member.
viVlan :: Lens' VirtualInterface (Maybe Int)
viVlan = lens _viVlan (\ s a -> s{_viVlan = a});

-- | Undocumented member.
viLocation :: Lens' VirtualInterface (Maybe Text)
viLocation = lens _viLocation (\ s a -> s{_viLocation = a});

-- | Undocumented member.
viAmazonAddress :: Lens' VirtualInterface (Maybe Text)
viAmazonAddress = lens _viAmazonAddress (\ s a -> s{_viAmazonAddress = a});

-- | Undocumented member.
viVirtualInterfaceState :: Lens' VirtualInterface (Maybe VirtualInterfaceState)
viVirtualInterfaceState = lens _viVirtualInterfaceState (\ s a -> s{_viVirtualInterfaceState = a});

-- | Undocumented member.
viConnectionId :: Lens' VirtualInterface (Maybe Text)
viConnectionId = lens _viConnectionId (\ s a -> s{_viConnectionId = a});

-- | Undocumented member.
viAsn :: Lens' VirtualInterface (Maybe Int)
viAsn = lens _viAsn (\ s a -> s{_viAsn = a});

-- | Undocumented member.
viVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceType = lens _viVirtualInterfaceType (\ s a -> s{_viVirtualInterfaceType = a});

-- | Undocumented member.
viAuthKey :: Lens' VirtualInterface (Maybe Text)
viAuthKey = lens _viAuthKey (\ s a -> s{_viAuthKey = a});

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
viCustomerRouterConfig = lens _viCustomerRouterConfig (\ s a -> s{_viCustomerRouterConfig = a});

-- | Undocumented member.
viOwnerAccount :: Lens' VirtualInterface (Maybe Text)
viOwnerAccount = lens _viOwnerAccount (\ s a -> s{_viOwnerAccount = a});

-- | Undocumented member.
viVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceName = lens _viVirtualInterfaceName (\ s a -> s{_viVirtualInterfaceName = a});

-- | Undocumented member.
viVirtualInterfaceId :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceId = lens _viVirtualInterfaceId (\ s a -> s{_viVirtualInterfaceId = a});

instance FromJSON VirtualInterface where
        parseJSON
          = withObject "VirtualInterface"
              (\ x ->
                 VirtualInterface' <$>
                   (x .:? "virtualGatewayId") <*>
                     (x .:? "routeFilterPrefixes" .!= mempty)
                     <*> (x .:? "customerAddress")
                     <*> (x .:? "vlan")
                     <*> (x .:? "location")
                     <*> (x .:? "amazonAddress")
                     <*> (x .:? "virtualInterfaceState")
                     <*> (x .:? "connectionId")
                     <*> (x .:? "asn")
                     <*> (x .:? "virtualInterfaceType")
                     <*> (x .:? "authKey")
                     <*> (x .:? "customerRouterConfig")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "virtualInterfaceName")
                     <*> (x .:? "virtualInterfaceId"))

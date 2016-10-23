{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Product where

import           Network.AWS.DirectConnect.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | A connection represents the physical network connection between the AWS Direct Connect location and the customer.
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
    { _cVlan            :: !(Maybe Int)
    , _cLocation        :: !(Maybe Text)
    , _cConnectionId    :: !(Maybe Text)
    , _cLoaIssueTime    :: !(Maybe POSIX)
    , _cPartnerName     :: !(Maybe Text)
    , _cConnectionName  :: !(Maybe Text)
    , _cBandwidth       :: !(Maybe Text)
    , _cOwnerAccount    :: !(Maybe Text)
    , _cRegion          :: !(Maybe Text)
    , _cConnectionState :: !(Maybe ConnectionState)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cVlan'
--
-- * 'cLocation'
--
-- * 'cConnectionId'
--
-- * 'cLoaIssueTime'
--
-- * 'cPartnerName'
--
-- * 'cConnectionName'
--
-- * 'cBandwidth'
--
-- * 'cOwnerAccount'
--
-- * 'cRegion'
--
-- * 'cConnectionState'
connection
    :: Connection
connection =
    Connection'
    { _cVlan = Nothing
    , _cLocation = Nothing
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
cVlan :: Lens' Connection (Maybe Int)
cVlan = lens _cVlan (\ s a -> s{_cVlan = a});

-- | Undocumented member.
cLocation :: Lens' Connection (Maybe Text)
cLocation = lens _cLocation (\ s a -> s{_cLocation = a});

-- | Undocumented member.
cConnectionId :: Lens' Connection (Maybe Text)
cConnectionId = lens _cConnectionId (\ s a -> s{_cConnectionId = a});

-- | The time of the most recent call to DescribeConnectionLoa for this Connection.
cLoaIssueTime :: Lens' Connection (Maybe UTCTime)
cLoaIssueTime = lens _cLoaIssueTime (\ s a -> s{_cLoaIssueTime = a}) . mapping _Time;

-- | The name of the AWS Direct Connect service provider associated with the connection.
cPartnerName :: Lens' Connection (Maybe Text)
cPartnerName = lens _cPartnerName (\ s a -> s{_cPartnerName = a});

-- | Undocumented member.
cConnectionName :: Lens' Connection (Maybe Text)
cConnectionName = lens _cConnectionName (\ s a -> s{_cConnectionName = a});

-- | Bandwidth of the connection.
--
-- Example: 1Gbps (for regular connections), or 500Mbps (for hosted connections)
--
-- Default: None
cBandwidth :: Lens' Connection (Maybe Text)
cBandwidth = lens _cBandwidth (\ s a -> s{_cBandwidth = a});

-- | The AWS account that will own the new connection.
cOwnerAccount :: Lens' Connection (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\ s a -> s{_cOwnerAccount = a});

-- | Undocumented member.
cRegion :: Lens' Connection (Maybe Text)
cRegion = lens _cRegion (\ s a -> s{_cRegion = a});

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
                     <*> (x .:? "loaIssueTime")
                     <*> (x .:? "partnerName")
                     <*> (x .:? "connectionName")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "region")
                     <*> (x .:? "connectionState"))

instance Hashable Connection

instance NFData Connection

-- | A structure containing a list of connections.
--
-- /See:/ 'connections' smart constructor.
newtype Connections = Connections'
    { _cConnections :: Maybe [Connection]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Connections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConnections'
connections
    :: Connections
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

instance Hashable Connections

instance NFData Connections

-- | An interconnect is a connection that can host other connections.
--
-- Like a standard AWS Direct Connect connection, an interconnect represents the physical connection between an AWS Direct Connect partner\'s network and a specific Direct Connect location. An AWS Direct Connect partner who owns an interconnect can provision hosted connections on the interconnect for their end customers, thereby providing the end customers with connectivity to AWS services.
--
-- The resources of the interconnect, including bandwidth and VLAN numbers, are shared by all of the hosted connections on the interconnect, and the owner of the interconnect determines how these resources are assigned.
--
-- /See:/ 'interconnect' smart constructor.
data Interconnect = Interconnect'
    { _iInterconnectId    :: !(Maybe Text)
    , _iLocation          :: !(Maybe Text)
    , _iInterconnectName  :: !(Maybe Text)
    , _iLoaIssueTime      :: !(Maybe POSIX)
    , _iBandwidth         :: !(Maybe Text)
    , _iInterconnectState :: !(Maybe InterconnectState)
    , _iRegion            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Interconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInterconnectId'
--
-- * 'iLocation'
--
-- * 'iInterconnectName'
--
-- * 'iLoaIssueTime'
--
-- * 'iBandwidth'
--
-- * 'iInterconnectState'
--
-- * 'iRegion'
interconnect
    :: Interconnect
interconnect =
    Interconnect'
    { _iInterconnectId = Nothing
    , _iLocation = Nothing
    , _iInterconnectName = Nothing
    , _iLoaIssueTime = Nothing
    , _iBandwidth = Nothing
    , _iInterconnectState = Nothing
    , _iRegion = Nothing
    }

-- | Undocumented member.
iInterconnectId :: Lens' Interconnect (Maybe Text)
iInterconnectId = lens _iInterconnectId (\ s a -> s{_iInterconnectId = a});

-- | Undocumented member.
iLocation :: Lens' Interconnect (Maybe Text)
iLocation = lens _iLocation (\ s a -> s{_iLocation = a});

-- | Undocumented member.
iInterconnectName :: Lens' Interconnect (Maybe Text)
iInterconnectName = lens _iInterconnectName (\ s a -> s{_iInterconnectName = a});

-- | The time of the most recent call to DescribeInterconnectLoa for this Interconnect.
iLoaIssueTime :: Lens' Interconnect (Maybe UTCTime)
iLoaIssueTime = lens _iLoaIssueTime (\ s a -> s{_iLoaIssueTime = a}) . mapping _Time;

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
                   (x .:? "interconnectId") <*> (x .:? "location") <*>
                     (x .:? "interconnectName")
                     <*> (x .:? "loaIssueTime")
                     <*> (x .:? "bandwidth")
                     <*> (x .:? "interconnectState")
                     <*> (x .:? "region"))

instance Hashable Interconnect

instance NFData Interconnect

-- | A structure containing the Letter of Authorization - Connecting Facility Assignment (LOA-CFA) for a connection.
--
-- /See:/ 'loa' smart constructor.
data Loa = Loa'
    { _loaLoaContent     :: !(Maybe Base64)
    , _loaLoaContentType :: !(Maybe LoaContentType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Loa' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loaLoaContent'
--
-- * 'loaLoaContentType'
loa
    :: Loa
loa =
    Loa'
    { _loaLoaContent = Nothing
    , _loaLoaContentType = Nothing
    }

-- | Undocumented member.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
loaLoaContent :: Lens' Loa (Maybe ByteString)
loaLoaContent = lens _loaLoaContent (\ s a -> s{_loaLoaContent = a}) . mapping _Base64;

-- | Undocumented member.
loaLoaContentType :: Lens' Loa (Maybe LoaContentType)
loaLoaContentType = lens _loaLoaContentType (\ s a -> s{_loaLoaContentType = a});

instance FromJSON Loa where
        parseJSON
          = withObject "Loa"
              (\ x ->
                 Loa' <$>
                   (x .:? "loaContent") <*> (x .:? "loaContentType"))

instance Hashable Loa

instance NFData Loa

-- | An AWS Direct Connect location where connections and interconnects can be requested.
--
-- /See:/ 'location' smart constructor.
data Location = Location'
    { _lLocationName :: !(Maybe Text)
    , _lLocationCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lLocationName'
--
-- * 'lLocationCode'
location
    :: Location
location =
    Location'
    { _lLocationName = Nothing
    , _lLocationCode = Nothing
    }

-- | The name of the AWS Direct Connect location. The name includes the colocation partner name and the physical site of the lit building.
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

instance Hashable Location

instance NFData Location

-- | A structure containing information about a new private virtual interface.
--
-- /See:/ 'newPrivateVirtualInterface' smart constructor.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
    { _nCustomerAddress      :: !(Maybe Text)
    , _nAmazonAddress        :: !(Maybe Text)
    , _nAuthKey              :: !(Maybe Text)
    , _nVirtualInterfaceName :: !Text
    , _nVlan                 :: !Int
    , _nAsn                  :: !Int
    , _nVirtualGatewayId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NewPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
newPrivateVirtualInterface
    :: Text -- ^ 'nVirtualInterfaceName'
    -> Int -- ^ 'nVlan'
    -> Int -- ^ 'nAsn'
    -> Text -- ^ 'nVirtualGatewayId'
    -> NewPrivateVirtualInterface
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

instance Hashable NewPrivateVirtualInterface

instance NFData NewPrivateVirtualInterface

instance ToJSON NewPrivateVirtualInterface where
        toJSON NewPrivateVirtualInterface'{..}
          = object
              (catMaybes
                 [("customerAddress" .=) <$> _nCustomerAddress,
                  ("amazonAddress" .=) <$> _nAmazonAddress,
                  ("authKey" .=) <$> _nAuthKey,
                  Just
                    ("virtualInterfaceName" .= _nVirtualInterfaceName),
                  Just ("vlan" .= _nVlan), Just ("asn" .= _nAsn),
                  Just ("virtualGatewayId" .= _nVirtualGatewayId)])

-- | A structure containing information about a private virtual interface that will be provisioned on a connection.
--
-- /See:/ 'newPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
    { _npviaCustomerAddress      :: !(Maybe Text)
    , _npviaAmazonAddress        :: !(Maybe Text)
    , _npviaAuthKey              :: !(Maybe Text)
    , _npviaVirtualInterfaceName :: !Text
    , _npviaVlan                 :: !Int
    , _npviaAsn                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NewPrivateVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
newPrivateVirtualInterfaceAllocation
    :: Text -- ^ 'npviaVirtualInterfaceName'
    -> Int -- ^ 'npviaVlan'
    -> Int -- ^ 'npviaAsn'
    -> NewPrivateVirtualInterfaceAllocation
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

instance Hashable
         NewPrivateVirtualInterfaceAllocation

instance NFData NewPrivateVirtualInterfaceAllocation

instance ToJSON NewPrivateVirtualInterfaceAllocation
         where
        toJSON NewPrivateVirtualInterfaceAllocation'{..}
          = object
              (catMaybes
                 [("customerAddress" .=) <$> _npviaCustomerAddress,
                  ("amazonAddress" .=) <$> _npviaAmazonAddress,
                  ("authKey" .=) <$> _npviaAuthKey,
                  Just
                    ("virtualInterfaceName" .=
                       _npviaVirtualInterfaceName),
                  Just ("vlan" .= _npviaVlan),
                  Just ("asn" .= _npviaAsn)])

-- | A structure containing information about a new public virtual interface.
--
-- /See:/ 'newPublicVirtualInterface' smart constructor.
data NewPublicVirtualInterface = NewPublicVirtualInterface'
    { _npviAuthKey              :: !(Maybe Text)
    , _npviVirtualInterfaceName :: !Text
    , _npviVlan                 :: !Int
    , _npviAsn                  :: !Int
    , _npviAmazonAddress        :: !Text
    , _npviCustomerAddress      :: !Text
    , _npviRouteFilterPrefixes  :: ![RouteFilterPrefix]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NewPublicVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
newPublicVirtualInterface
    :: Text -- ^ 'npviVirtualInterfaceName'
    -> Int -- ^ 'npviVlan'
    -> Int -- ^ 'npviAsn'
    -> Text -- ^ 'npviAmazonAddress'
    -> Text -- ^ 'npviCustomerAddress'
    -> NewPublicVirtualInterface
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

instance Hashable NewPublicVirtualInterface

instance NFData NewPublicVirtualInterface

instance ToJSON NewPublicVirtualInterface where
        toJSON NewPublicVirtualInterface'{..}
          = object
              (catMaybes
                 [("authKey" .=) <$> _npviAuthKey,
                  Just
                    ("virtualInterfaceName" .=
                       _npviVirtualInterfaceName),
                  Just ("vlan" .= _npviVlan), Just ("asn" .= _npviAsn),
                  Just ("amazonAddress" .= _npviAmazonAddress),
                  Just ("customerAddress" .= _npviCustomerAddress),
                  Just
                    ("routeFilterPrefixes" .= _npviRouteFilterPrefixes)])

-- | A structure containing information about a public virtual interface that will be provisioned on a connection.
--
-- /See:/ 'newPublicVirtualInterfaceAllocation' smart constructor.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
    { _newAuthKey              :: !(Maybe Text)
    , _newVirtualInterfaceName :: !Text
    , _newVlan                 :: !Int
    , _newAsn                  :: !Int
    , _newAmazonAddress        :: !Text
    , _newCustomerAddress      :: !Text
    , _newRouteFilterPrefixes  :: ![RouteFilterPrefix]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NewPublicVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
newPublicVirtualInterfaceAllocation
    :: Text -- ^ 'newVirtualInterfaceName'
    -> Int -- ^ 'newVlan'
    -> Int -- ^ 'newAsn'
    -> Text -- ^ 'newAmazonAddress'
    -> Text -- ^ 'newCustomerAddress'
    -> NewPublicVirtualInterfaceAllocation
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

instance Hashable NewPublicVirtualInterfaceAllocation

instance NFData NewPublicVirtualInterfaceAllocation

instance ToJSON NewPublicVirtualInterfaceAllocation
         where
        toJSON NewPublicVirtualInterfaceAllocation'{..}
          = object
              (catMaybes
                 [("authKey" .=) <$> _newAuthKey,
                  Just
                    ("virtualInterfaceName" .= _newVirtualInterfaceName),
                  Just ("vlan" .= _newVlan), Just ("asn" .= _newAsn),
                  Just ("amazonAddress" .= _newAmazonAddress),
                  Just ("customerAddress" .= _newCustomerAddress),
                  Just
                    ("routeFilterPrefixes" .= _newRouteFilterPrefixes)])

-- | A route filter prefix that the customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.
--
-- /See:/ 'routeFilterPrefix' smart constructor.
newtype RouteFilterPrefix = RouteFilterPrefix'
    { _rfpCidr :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RouteFilterPrefix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfpCidr'
routeFilterPrefix
    :: RouteFilterPrefix
routeFilterPrefix =
    RouteFilterPrefix'
    { _rfpCidr = Nothing
    }

-- | CIDR notation for the advertised route. Multiple routes are separated by commas.
--
-- Example: 10.10.10.0\/24,10.10.11.0\/24
rfpCidr :: Lens' RouteFilterPrefix (Maybe Text)
rfpCidr = lens _rfpCidr (\ s a -> s{_rfpCidr = a});

instance FromJSON RouteFilterPrefix where
        parseJSON
          = withObject "RouteFilterPrefix"
              (\ x -> RouteFilterPrefix' <$> (x .:? "cidr"))

instance Hashable RouteFilterPrefix

instance NFData RouteFilterPrefix

instance ToJSON RouteFilterPrefix where
        toJSON RouteFilterPrefix'{..}
          = object (catMaybes [("cidr" .=) <$> _rfpCidr])

-- | You can create one or more AWS Direct Connect private virtual interfaces linking to your virtual private gateway.
--
-- Virtual private gateways can be managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html Amazon EC2 CreateVpnGateway action>.
--
-- /See:/ 'virtualGateway' smart constructor.
data VirtualGateway = VirtualGateway'
    { _vgVirtualGatewayId    :: !(Maybe Text)
    , _vgVirtualGatewayState :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VirtualGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vgVirtualGatewayId'
--
-- * 'vgVirtualGatewayState'
virtualGateway
    :: VirtualGateway
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

instance Hashable VirtualGateway

instance NFData VirtualGateway

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer.
--
-- /See:/ 'virtualInterface' smart constructor.
data VirtualInterface = VirtualInterface'
    { _viVirtualGatewayId      :: !(Maybe Text)
    , _viRouteFilterPrefixes   :: !(Maybe [RouteFilterPrefix])
    , _viCustomerAddress       :: !(Maybe Text)
    , _viVlan                  :: !(Maybe Int)
    , _viLocation              :: !(Maybe Text)
    , _viAmazonAddress         :: !(Maybe Text)
    , _viVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _viConnectionId          :: !(Maybe Text)
    , _viVirtualInterfaceType  :: !(Maybe Text)
    , _viAsn                   :: !(Maybe Int)
    , _viAuthKey               :: !(Maybe Text)
    , _viCustomerRouterConfig  :: !(Maybe Text)
    , _viOwnerAccount          :: !(Maybe Text)
    , _viVirtualInterfaceName  :: !(Maybe Text)
    , _viVirtualInterfaceId    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
-- * 'viVirtualInterfaceType'
--
-- * 'viAsn'
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
virtualInterface
    :: VirtualInterface
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
    , _viVirtualInterfaceType = Nothing
    , _viAsn = Nothing
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
viVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceType = lens _viVirtualInterfaceType (\ s a -> s{_viVirtualInterfaceType = a});

-- | Undocumented member.
viAsn :: Lens' VirtualInterface (Maybe Int)
viAsn = lens _viAsn (\ s a -> s{_viAsn = a});

-- | Undocumented member.
viAuthKey :: Lens' VirtualInterface (Maybe Text)
viAuthKey = lens _viAuthKey (\ s a -> s{_viAuthKey = a});

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
viCustomerRouterConfig = lens _viCustomerRouterConfig (\ s a -> s{_viCustomerRouterConfig = a});

-- | The AWS account that will own the new virtual interface.
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
                     <*> (x .:? "virtualInterfaceType")
                     <*> (x .:? "asn")
                     <*> (x .:? "authKey")
                     <*> (x .:? "customerRouterConfig")
                     <*> (x .:? "ownerAccount")
                     <*> (x .:? "virtualInterfaceName")
                     <*> (x .:? "virtualInterfaceId"))

instance Hashable VirtualInterface

instance NFData VirtualInterface

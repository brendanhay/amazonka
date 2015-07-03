{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.DirectConnect.Types
    (
    -- * Service
      DirectConnect

    -- * Errors
    , _DirectConnectClientException
    , _DirectConnectServerException

    -- * ConnectionState
    , ConnectionState (..)

    -- * InterconnectState
    , InterconnectState (..)

    -- * VirtualInterfaceState
    , VirtualInterfaceState (..)

    -- * Connection
    , Connection
    , connection
    , conVlan
    , conLocation
    , conConnectionId
    , conConnectionName
    , conPartnerName
    , conBandwidth
    , conRegion
    , conOwnerAccount
    , conConnectionState

    -- * Connections
    , Connections
    , connections
    , conConnections

    -- * Interconnect
    , Interconnect
    , interconnect
    , intInterconnectId
    , intInterconnectName
    , intLocation
    , intBandwidth
    , intInterconnectState
    , intRegion

    -- * Location
    , Location
    , location
    , locLocationName
    , locLocationCode

    -- * NewPrivateVirtualInterface
    , NewPrivateVirtualInterface
    , newPrivateVirtualInterface
    , newCustomerAddress
    , newAmazonAddress
    , newAuthKey
    , newVirtualInterfaceName
    , newVlan
    , newAsn
    , newVirtualGatewayId

    -- * NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation
    , newPrivateVirtualInterfaceAllocation
    , npviaCustomerAddress
    , npviaAmazonAddress
    , npviaAuthKey
    , npviaVirtualInterfaceName
    , npviaVlan
    , npviaAsn

    -- * NewPublicVirtualInterface
    , NewPublicVirtualInterface
    , newPublicVirtualInterface
    , npviAuthKey
    , npviVirtualInterfaceName
    , npviVlan
    , npviAsn
    , npviAmazonAddress
    , npviCustomerAddress
    , npviRouteFilterPrefixes

    -- * NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation
    , newPublicVirtualInterfaceAllocation
    , nAuthKey
    , nVirtualInterfaceName
    , nVlan
    , nAsn
    , nAmazonAddress
    , nCustomerAddress
    , nRouteFilterPrefixes

    -- * RouteFilterPrefix
    , RouteFilterPrefix
    , routeFilterPrefix
    , rfpCidr

    -- * VirtualGateway
    , VirtualGateway
    , virtualGateway
    , vgVirtualGatewayId
    , vgVirtualGatewayState

    -- * VirtualInterface
    , VirtualInterface
    , virtualInterface
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    , viCustomerAddress
    , viVlan
    , viLocation
    , viAmazonAddress
    , viVirtualInterfaceState
    , viConnectionId
    , viAsn
    , viVirtualInterfaceType
    , viAuthKey
    , viCustomerRouterConfig
    , viOwnerAccount
    , viVirtualInterfaceName
    , viVirtualInterfaceId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-10-25@ of the Amazon Direct Connect SDK.
data DirectConnect

instance AWSService DirectConnect where
    type Sg DirectConnect = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "DirectConnect"
            , _svcPrefix = "directconnect"
            , _svcVersion = "2012-10-25"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The API was called with invalid parameters. The error message will
-- contain additional details about the cause.
_DirectConnectClientException :: AWSError a => Getting (First ServiceError) a ServiceError
_DirectConnectClientException =
    _ServiceError . hasCode "DirectConnectClientException"

-- | A server-side error occurred during the API call. The error message will
-- contain additional details about the cause.
_DirectConnectServerException :: AWSError a => Getting (First ServiceError) a ServiceError
_DirectConnectServerException =
    _ServiceError . hasCode "DirectConnectServerException"

-- | State of the connection.
--
-- -   __Ordering__: The initial state of a hosted connection provisioned
--     on an interconnect. The connection stays in the ordering state until
--     the owner of the hosted connection confirms or declines the
--     connection order.
-- -   __Requested__: The initial state of a standard connection. The
--     connection stays in the requested state until the Letter of
--     Authorization (LOA) is sent to the customer.
-- -   __Pending__: The connection has been approved, and is being
--     initialized.
-- -   __Available__: The network link is up, and the connection is ready
--     for use.
-- -   __Down__: The network link is down.
-- -   __Deleted__: The connection has been deleted.
-- -   __Rejected__: A hosted connection in the \'Ordering\' state will
--     enter the \'Rejected\' state if it is deleted by the end customer.
data ConnectionState
    = CSDeleted
    | CSOrdering
    | CSAvailable
    | CSDeleting
    | CSPending
    | CSDown
    | CSRequested
    | CSRejected
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ConnectionState where
    parser = takeLowerText >>= \case
        "available" -> pure CSAvailable
        "deleted" -> pure CSDeleted
        "deleting" -> pure CSDeleting
        "down" -> pure CSDown
        "ordering" -> pure CSOrdering
        "pending" -> pure CSPending
        "rejected" -> pure CSRejected
        "requested" -> pure CSRequested
        e -> fromTextError $ "Failure parsing ConnectionState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, ordering, pending, rejected, requested"

instance ToText ConnectionState where
    toText = \case
        CSAvailable -> "available"
        CSDeleted -> "deleted"
        CSDeleting -> "deleting"
        CSDown -> "down"
        CSOrdering -> "ordering"
        CSPending -> "pending"
        CSRejected -> "rejected"
        CSRequested -> "requested"

instance Hashable ConnectionState where
    hashWithSalt = hashUsing fromEnum

instance ToQuery ConnectionState
instance ToHeader ConnectionState

instance FromJSON ConnectionState where
    parseJSON = parseJSONText "ConnectionState"

-- | State of the interconnect.
--
-- -   __Requested__: The initial state of an interconnect. The
--     interconnect stays in the requested state until the Letter of
--     Authorization (LOA) is sent to the customer.
-- -   __Pending__: The interconnect has been approved, and is being
--     initialized.
-- -   __Available__: The network link is up, and the interconnect is ready
--     for use.
-- -   __Down__: The network link is down.
-- -   __Deleted__: The interconnect has been deleted.
data InterconnectState
    = ISDeleted
    | ISAvailable
    | ISDeleting
    | ISRequested
    | ISPending
    | ISDown
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText InterconnectState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deleted" -> pure ISDeleted
        "deleting" -> pure ISDeleting
        "down" -> pure ISDown
        "pending" -> pure ISPending
        "requested" -> pure ISRequested
        e -> fromTextError $ "Failure parsing InterconnectState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, pending, requested"

instance ToText InterconnectState where
    toText = \case
        ISAvailable -> "available"
        ISDeleted -> "deleted"
        ISDeleting -> "deleting"
        ISDown -> "down"
        ISPending -> "pending"
        ISRequested -> "requested"

instance Hashable InterconnectState where
    hashWithSalt = hashUsing fromEnum

instance ToQuery InterconnectState
instance ToHeader InterconnectState

instance FromJSON InterconnectState where
    parseJSON = parseJSONText "InterconnectState"

-- | State of the virtual interface.
--
-- -   __Confirming__: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
-- -   __Verifying__: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
-- -   __Pending__: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
-- -   __Available__: A virtual interface that is able to forward traffic.
-- -   __Deleting__: A virtual interface is in this state immediately after
--     calling /DeleteVirtualInterface/ until it can no longer forward
--     traffic.
-- -   __Deleted__: A virtual interface that cannot forward traffic.
-- -   __Rejected__: The virtual interface owner has declined creation of
--     the virtual interface. If a virtual interface in the \'Confirming\'
--     state is deleted by the virtual interface owner, the virtual
--     interface will enter the \'Rejected\' state.
data VirtualInterfaceState
    = Deleting
    | Pending
    | Confirming
    | Rejected
    | Verifying
    | Deleted
    | Available
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText VirtualInterfaceState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "confirming" -> pure Confirming
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "pending" -> pure Pending
        "rejected" -> pure Rejected
        "verifying" -> pure Verifying
        e -> fromTextError $ "Failure parsing VirtualInterfaceState from value: '" <> e
           <> "'. Accepted values: available, confirming, deleted, deleting, pending, rejected, verifying"

instance ToText VirtualInterfaceState where
    toText = \case
        Available -> "available"
        Confirming -> "confirming"
        Deleted -> "deleted"
        Deleting -> "deleting"
        Pending -> "pending"
        Rejected -> "rejected"
        Verifying -> "verifying"

instance Hashable VirtualInterfaceState where
    hashWithSalt = hashUsing fromEnum

instance ToQuery VirtualInterfaceState
instance ToHeader VirtualInterfaceState

instance FromJSON VirtualInterfaceState where
    parseJSON = parseJSONText "VirtualInterfaceState"

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
--
-- /See:/ 'connection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conVlan'
--
-- * 'conLocation'
--
-- * 'conConnectionId'
--
-- * 'conConnectionName'
--
-- * 'conPartnerName'
--
-- * 'conBandwidth'
--
-- * 'conRegion'
--
-- * 'conOwnerAccount'
--
-- * 'conConnectionState'
data Connection = Connection'
    { _conVlan            :: !(Maybe Int)
    , _conLocation        :: !(Maybe Text)
    , _conConnectionId    :: !(Maybe Text)
    , _conConnectionName  :: !(Maybe Text)
    , _conPartnerName     :: !(Maybe Text)
    , _conBandwidth       :: !(Maybe Text)
    , _conRegion          :: !(Maybe Text)
    , _conOwnerAccount    :: !(Maybe Text)
    , _conConnectionState :: !(Maybe ConnectionState)
    } deriving (Eq,Read,Show)

-- | 'Connection' smart constructor.
connection :: Connection
connection =
    Connection'
    { _conVlan = Nothing
    , _conLocation = Nothing
    , _conConnectionId = Nothing
    , _conConnectionName = Nothing
    , _conPartnerName = Nothing
    , _conBandwidth = Nothing
    , _conRegion = Nothing
    , _conOwnerAccount = Nothing
    , _conConnectionState = Nothing
    }

-- | FIXME: Undocumented member.
conVlan :: Lens' Connection (Maybe Int)
conVlan = lens _conVlan (\ s a -> s{_conVlan = a});

-- | FIXME: Undocumented member.
conLocation :: Lens' Connection (Maybe Text)
conLocation = lens _conLocation (\ s a -> s{_conLocation = a});

-- | FIXME: Undocumented member.
conConnectionId :: Lens' Connection (Maybe Text)
conConnectionId = lens _conConnectionId (\ s a -> s{_conConnectionId = a});

-- | FIXME: Undocumented member.
conConnectionName :: Lens' Connection (Maybe Text)
conConnectionName = lens _conConnectionName (\ s a -> s{_conConnectionName = a});

-- | FIXME: Undocumented member.
conPartnerName :: Lens' Connection (Maybe Text)
conPartnerName = lens _conPartnerName (\ s a -> s{_conPartnerName = a});

-- | Bandwidth of the connection.
--
-- Example: 1Gbps (for regular connections), or 500Mbps (for hosted
-- connections)
--
-- Default: None
conBandwidth :: Lens' Connection (Maybe Text)
conBandwidth = lens _conBandwidth (\ s a -> s{_conBandwidth = a});

-- | FIXME: Undocumented member.
conRegion :: Lens' Connection (Maybe Text)
conRegion = lens _conRegion (\ s a -> s{_conRegion = a});

-- | FIXME: Undocumented member.
conOwnerAccount :: Lens' Connection (Maybe Text)
conOwnerAccount = lens _conOwnerAccount (\ s a -> s{_conOwnerAccount = a});

-- | FIXME: Undocumented member.
conConnectionState :: Lens' Connection (Maybe ConnectionState)
conConnectionState = lens _conConnectionState (\ s a -> s{_conConnectionState = a});

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
-- * 'conConnections'
newtype Connections = Connections'
    { _conConnections :: Maybe [Connection]
    } deriving (Eq,Read,Show)

-- | 'Connections' smart constructor.
connections :: Connections
connections =
    Connections'
    { _conConnections = Nothing
    }

-- | A list of connections.
conConnections :: Lens' Connections [Connection]
conConnections = lens _conConnections (\ s a -> s{_conConnections = a}) . _Default;

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
-- * 'intInterconnectId'
--
-- * 'intInterconnectName'
--
-- * 'intLocation'
--
-- * 'intBandwidth'
--
-- * 'intInterconnectState'
--
-- * 'intRegion'
data Interconnect = Interconnect'
    { _intInterconnectId    :: !(Maybe Text)
    , _intInterconnectName  :: !(Maybe Text)
    , _intLocation          :: !(Maybe Text)
    , _intBandwidth         :: !(Maybe Text)
    , _intInterconnectState :: !(Maybe InterconnectState)
    , _intRegion            :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Interconnect' smart constructor.
interconnect :: Interconnect
interconnect =
    Interconnect'
    { _intInterconnectId = Nothing
    , _intInterconnectName = Nothing
    , _intLocation = Nothing
    , _intBandwidth = Nothing
    , _intInterconnectState = Nothing
    , _intRegion = Nothing
    }

-- | FIXME: Undocumented member.
intInterconnectId :: Lens' Interconnect (Maybe Text)
intInterconnectId = lens _intInterconnectId (\ s a -> s{_intInterconnectId = a});

-- | FIXME: Undocumented member.
intInterconnectName :: Lens' Interconnect (Maybe Text)
intInterconnectName = lens _intInterconnectName (\ s a -> s{_intInterconnectName = a});

-- | FIXME: Undocumented member.
intLocation :: Lens' Interconnect (Maybe Text)
intLocation = lens _intLocation (\ s a -> s{_intLocation = a});

-- | FIXME: Undocumented member.
intBandwidth :: Lens' Interconnect (Maybe Text)
intBandwidth = lens _intBandwidth (\ s a -> s{_intBandwidth = a});

-- | FIXME: Undocumented member.
intInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
intInterconnectState = lens _intInterconnectState (\ s a -> s{_intInterconnectState = a});

-- | FIXME: Undocumented member.
intRegion :: Lens' Interconnect (Maybe Text)
intRegion = lens _intRegion (\ s a -> s{_intRegion = a});

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
-- * 'locLocationName'
--
-- * 'locLocationCode'
data Location = Location'
    { _locLocationName :: !(Maybe Text)
    , _locLocationCode :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Location' smart constructor.
location :: Location
location =
    Location'
    { _locLocationName = Nothing
    , _locLocationCode = Nothing
    }

-- | The name of the AWS Direct Connect location. The name includes the
-- colocation partner name and the physical site of the lit building.
locLocationName :: Lens' Location (Maybe Text)
locLocationName = lens _locLocationName (\ s a -> s{_locLocationName = a});

-- | The code used to indicate the AWS Direct Connect location.
locLocationCode :: Lens' Location (Maybe Text)
locLocationCode = lens _locLocationCode (\ s a -> s{_locLocationCode = a});

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
-- * 'newCustomerAddress'
--
-- * 'newAmazonAddress'
--
-- * 'newAuthKey'
--
-- * 'newVirtualInterfaceName'
--
-- * 'newVlan'
--
-- * 'newAsn'
--
-- * 'newVirtualGatewayId'
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
    { _newCustomerAddress      :: !(Maybe Text)
    , _newAmazonAddress        :: !(Maybe Text)
    , _newAuthKey              :: !(Maybe Text)
    , _newVirtualInterfaceName :: !Text
    , _newVlan                 :: !Int
    , _newAsn                  :: !Int
    , _newVirtualGatewayId     :: !Text
    } deriving (Eq,Read,Show)

-- | 'NewPrivateVirtualInterface' smart constructor.
newPrivateVirtualInterface :: Text -> Int -> Int -> Text -> NewPrivateVirtualInterface
newPrivateVirtualInterface pVirtualInterfaceName pVlan pAsn pVirtualGatewayId =
    NewPrivateVirtualInterface'
    { _newCustomerAddress = Nothing
    , _newAmazonAddress = Nothing
    , _newAuthKey = Nothing
    , _newVirtualInterfaceName = pVirtualInterfaceName
    , _newVlan = pVlan
    , _newAsn = pAsn
    , _newVirtualGatewayId = pVirtualGatewayId
    }

-- | FIXME: Undocumented member.
newCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
newCustomerAddress = lens _newCustomerAddress (\ s a -> s{_newCustomerAddress = a});

-- | FIXME: Undocumented member.
newAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
newAmazonAddress = lens _newAmazonAddress (\ s a -> s{_newAmazonAddress = a});

-- | FIXME: Undocumented member.
newAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
newAuthKey = lens _newAuthKey (\ s a -> s{_newAuthKey = a});

-- | FIXME: Undocumented member.
newVirtualInterfaceName :: Lens' NewPrivateVirtualInterface Text
newVirtualInterfaceName = lens _newVirtualInterfaceName (\ s a -> s{_newVirtualInterfaceName = a});

-- | FIXME: Undocumented member.
newVlan :: Lens' NewPrivateVirtualInterface Int
newVlan = lens _newVlan (\ s a -> s{_newVlan = a});

-- | FIXME: Undocumented member.
newAsn :: Lens' NewPrivateVirtualInterface Int
newAsn = lens _newAsn (\ s a -> s{_newAsn = a});

-- | FIXME: Undocumented member.
newVirtualGatewayId :: Lens' NewPrivateVirtualInterface Text
newVirtualGatewayId = lens _newVirtualGatewayId (\ s a -> s{_newVirtualGatewayId = a});

instance ToJSON NewPrivateVirtualInterface where
        toJSON NewPrivateVirtualInterface'{..}
          = object
              ["customerAddress" .= _newCustomerAddress,
               "amazonAddress" .= _newAmazonAddress,
               "authKey" .= _newAuthKey,
               "virtualInterfaceName" .= _newVirtualInterfaceName,
               "vlan" .= _newVlan, "asn" .= _newAsn,
               "virtualGatewayId" .= _newVirtualGatewayId]

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
    { _npviaCustomerAddress      :: !(Maybe Text)
    , _npviaAmazonAddress        :: !(Maybe Text)
    , _npviaAuthKey              :: !(Maybe Text)
    , _npviaVirtualInterfaceName :: !Text
    , _npviaVlan                 :: !Int
    , _npviaAsn                  :: !Int
    } deriving (Eq,Read,Show)

-- | 'NewPrivateVirtualInterfaceAllocation' smart constructor.
newPrivateVirtualInterfaceAllocation :: Text -> Int -> Int -> NewPrivateVirtualInterfaceAllocation
newPrivateVirtualInterfaceAllocation pVirtualInterfaceName pVlan pAsn =
    NewPrivateVirtualInterfaceAllocation'
    { _npviaCustomerAddress = Nothing
    , _npviaAmazonAddress = Nothing
    , _npviaAuthKey = Nothing
    , _npviaVirtualInterfaceName = pVirtualInterfaceName
    , _npviaVlan = pVlan
    , _npviaAsn = pAsn
    }

-- | FIXME: Undocumented member.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress = lens _npviaCustomerAddress (\ s a -> s{_npviaCustomerAddress = a});

-- | FIXME: Undocumented member.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress = lens _npviaAmazonAddress (\ s a -> s{_npviaAmazonAddress = a});

-- | FIXME: Undocumented member.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\ s a -> s{_npviaAuthKey = a});

-- | FIXME: Undocumented member.
npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation Text
npviaVirtualInterfaceName = lens _npviaVirtualInterfaceName (\ s a -> s{_npviaVirtualInterfaceName = a});

-- | FIXME: Undocumented member.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation Int
npviaVlan = lens _npviaVlan (\ s a -> s{_npviaVlan = a});

-- | FIXME: Undocumented member.
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
    { _npviAuthKey              :: !(Maybe Text)
    , _npviVirtualInterfaceName :: !Text
    , _npviVlan                 :: !Int
    , _npviAsn                  :: !Int
    , _npviAmazonAddress        :: !Text
    , _npviCustomerAddress      :: !Text
    , _npviRouteFilterPrefixes  :: ![RouteFilterPrefix]
    } deriving (Eq,Read,Show)

-- | 'NewPublicVirtualInterface' smart constructor.
newPublicVirtualInterface :: Text -> Int -> Int -> Text -> Text -> NewPublicVirtualInterface
newPublicVirtualInterface pVirtualInterfaceName pVlan pAsn pAmazonAddress pCustomerAddress =
    NewPublicVirtualInterface'
    { _npviAuthKey = Nothing
    , _npviVirtualInterfaceName = pVirtualInterfaceName
    , _npviVlan = pVlan
    , _npviAsn = pAsn
    , _npviAmazonAddress = pAmazonAddress
    , _npviCustomerAddress = pCustomerAddress
    , _npviRouteFilterPrefixes = mempty
    }

-- | FIXME: Undocumented member.
npviAuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npviAuthKey = lens _npviAuthKey (\ s a -> s{_npviAuthKey = a});

-- | FIXME: Undocumented member.
npviVirtualInterfaceName :: Lens' NewPublicVirtualInterface Text
npviVirtualInterfaceName = lens _npviVirtualInterfaceName (\ s a -> s{_npviVirtualInterfaceName = a});

-- | FIXME: Undocumented member.
npviVlan :: Lens' NewPublicVirtualInterface Int
npviVlan = lens _npviVlan (\ s a -> s{_npviVlan = a});

-- | FIXME: Undocumented member.
npviAsn :: Lens' NewPublicVirtualInterface Int
npviAsn = lens _npviAsn (\ s a -> s{_npviAsn = a});

-- | FIXME: Undocumented member.
npviAmazonAddress :: Lens' NewPublicVirtualInterface Text
npviAmazonAddress = lens _npviAmazonAddress (\ s a -> s{_npviAmazonAddress = a});

-- | FIXME: Undocumented member.
npviCustomerAddress :: Lens' NewPublicVirtualInterface Text
npviCustomerAddress = lens _npviCustomerAddress (\ s a -> s{_npviCustomerAddress = a});

-- | FIXME: Undocumented member.
npviRouteFilterPrefixes :: Lens' NewPublicVirtualInterface [RouteFilterPrefix]
npviRouteFilterPrefixes = lens _npviRouteFilterPrefixes (\ s a -> s{_npviRouteFilterPrefixes = a});

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
-- * 'nAuthKey'
--
-- * 'nVirtualInterfaceName'
--
-- * 'nVlan'
--
-- * 'nAsn'
--
-- * 'nAmazonAddress'
--
-- * 'nCustomerAddress'
--
-- * 'nRouteFilterPrefixes'
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
    { _nAuthKey              :: !(Maybe Text)
    , _nVirtualInterfaceName :: !Text
    , _nVlan                 :: !Int
    , _nAsn                  :: !Int
    , _nAmazonAddress        :: !Text
    , _nCustomerAddress      :: !Text
    , _nRouteFilterPrefixes  :: ![RouteFilterPrefix]
    } deriving (Eq,Read,Show)

-- | 'NewPublicVirtualInterfaceAllocation' smart constructor.
newPublicVirtualInterfaceAllocation :: Text -> Int -> Int -> Text -> Text -> NewPublicVirtualInterfaceAllocation
newPublicVirtualInterfaceAllocation pVirtualInterfaceName pVlan pAsn pAmazonAddress pCustomerAddress =
    NewPublicVirtualInterfaceAllocation'
    { _nAuthKey = Nothing
    , _nVirtualInterfaceName = pVirtualInterfaceName
    , _nVlan = pVlan
    , _nAsn = pAsn
    , _nAmazonAddress = pAmazonAddress
    , _nCustomerAddress = pCustomerAddress
    , _nRouteFilterPrefixes = mempty
    }

-- | FIXME: Undocumented member.
nAuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
nAuthKey = lens _nAuthKey (\ s a -> s{_nAuthKey = a});

-- | FIXME: Undocumented member.
nVirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation Text
nVirtualInterfaceName = lens _nVirtualInterfaceName (\ s a -> s{_nVirtualInterfaceName = a});

-- | FIXME: Undocumented member.
nVlan :: Lens' NewPublicVirtualInterfaceAllocation Int
nVlan = lens _nVlan (\ s a -> s{_nVlan = a});

-- | FIXME: Undocumented member.
nAsn :: Lens' NewPublicVirtualInterfaceAllocation Int
nAsn = lens _nAsn (\ s a -> s{_nAsn = a});

-- | FIXME: Undocumented member.
nAmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
nAmazonAddress = lens _nAmazonAddress (\ s a -> s{_nAmazonAddress = a});

-- | FIXME: Undocumented member.
nCustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
nCustomerAddress = lens _nCustomerAddress (\ s a -> s{_nCustomerAddress = a});

-- | FIXME: Undocumented member.
nRouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation [RouteFilterPrefix]
nRouteFilterPrefixes = lens _nRouteFilterPrefixes (\ s a -> s{_nRouteFilterPrefixes = a});

instance ToJSON NewPublicVirtualInterfaceAllocation
         where
        toJSON NewPublicVirtualInterfaceAllocation'{..}
          = object
              ["authKey" .= _nAuthKey,
               "virtualInterfaceName" .= _nVirtualInterfaceName,
               "vlan" .= _nVlan, "asn" .= _nAsn,
               "amazonAddress" .= _nAmazonAddress,
               "customerAddress" .= _nCustomerAddress,
               "routeFilterPrefixes" .= _nRouteFilterPrefixes]

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
    } deriving (Eq,Read,Show)

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
    { _vgVirtualGatewayId    :: !(Maybe Text)
    , _vgVirtualGatewayState :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'VirtualGateway' smart constructor.
virtualGateway :: VirtualGateway
virtualGateway =
    VirtualGateway'
    { _vgVirtualGatewayId = Nothing
    , _vgVirtualGatewayState = Nothing
    }

-- | FIXME: Undocumented member.
vgVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayId = lens _vgVirtualGatewayId (\ s a -> s{_vgVirtualGatewayId = a});

-- | FIXME: Undocumented member.
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
    { _viVirtualGatewayId      :: !(Maybe Text)
    , _viRouteFilterPrefixes   :: !(Maybe [RouteFilterPrefix])
    , _viCustomerAddress       :: !(Maybe Text)
    , _viVlan                  :: !(Maybe Int)
    , _viLocation              :: !(Maybe Text)
    , _viAmazonAddress         :: !(Maybe Text)
    , _viVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _viConnectionId          :: !(Maybe Text)
    , _viAsn                   :: !(Maybe Int)
    , _viVirtualInterfaceType  :: !(Maybe Text)
    , _viAuthKey               :: !(Maybe Text)
    , _viCustomerRouterConfig  :: !(Maybe Text)
    , _viOwnerAccount          :: !(Maybe Text)
    , _viVirtualInterfaceName  :: !(Maybe Text)
    , _viVirtualInterfaceId    :: !(Maybe Text)
    } deriving (Eq,Read,Show)

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

-- | FIXME: Undocumented member.
viVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
viVirtualGatewayId = lens _viVirtualGatewayId (\ s a -> s{_viVirtualGatewayId = a});

-- | FIXME: Undocumented member.
viRouteFilterPrefixes :: Lens' VirtualInterface [RouteFilterPrefix]
viRouteFilterPrefixes = lens _viRouteFilterPrefixes (\ s a -> s{_viRouteFilterPrefixes = a}) . _Default;

-- | FIXME: Undocumented member.
viCustomerAddress :: Lens' VirtualInterface (Maybe Text)
viCustomerAddress = lens _viCustomerAddress (\ s a -> s{_viCustomerAddress = a});

-- | FIXME: Undocumented member.
viVlan :: Lens' VirtualInterface (Maybe Int)
viVlan = lens _viVlan (\ s a -> s{_viVlan = a});

-- | FIXME: Undocumented member.
viLocation :: Lens' VirtualInterface (Maybe Text)
viLocation = lens _viLocation (\ s a -> s{_viLocation = a});

-- | FIXME: Undocumented member.
viAmazonAddress :: Lens' VirtualInterface (Maybe Text)
viAmazonAddress = lens _viAmazonAddress (\ s a -> s{_viAmazonAddress = a});

-- | FIXME: Undocumented member.
viVirtualInterfaceState :: Lens' VirtualInterface (Maybe VirtualInterfaceState)
viVirtualInterfaceState = lens _viVirtualInterfaceState (\ s a -> s{_viVirtualInterfaceState = a});

-- | FIXME: Undocumented member.
viConnectionId :: Lens' VirtualInterface (Maybe Text)
viConnectionId = lens _viConnectionId (\ s a -> s{_viConnectionId = a});

-- | FIXME: Undocumented member.
viAsn :: Lens' VirtualInterface (Maybe Int)
viAsn = lens _viAsn (\ s a -> s{_viAsn = a});

-- | FIXME: Undocumented member.
viVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceType = lens _viVirtualInterfaceType (\ s a -> s{_viVirtualInterfaceType = a});

-- | FIXME: Undocumented member.
viAuthKey :: Lens' VirtualInterface (Maybe Text)
viAuthKey = lens _viAuthKey (\ s a -> s{_viAuthKey = a});

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
viCustomerRouterConfig = lens _viCustomerRouterConfig (\ s a -> s{_viCustomerRouterConfig = a});

-- | FIXME: Undocumented member.
viOwnerAccount :: Lens' VirtualInterface (Maybe Text)
viOwnerAccount = lens _viOwnerAccount (\ s a -> s{_viOwnerAccount = a});

-- | FIXME: Undocumented member.
viVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceName = lens _viVirtualInterfaceName (\ s a -> s{_viVirtualInterfaceName = a});

-- | FIXME: Undocumented member.
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

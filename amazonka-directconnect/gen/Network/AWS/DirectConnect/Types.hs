{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Direct Connect links your internal network to an AWS Direct Connect
-- location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic
-- cable. One end of the cable is connected to your router, the other to an
-- AWS Direct Connect router. With this connection in place, you can create
-- virtual interfaces directly to the AWS cloud and Amazon Virtual Private
-- Cloud, bypassing Internet service providers in your network path.
module Network.AWS.DirectConnect.Types
    (
    -- * Service
      DirectConnect
    -- ** Errors
    , DirectConnectError (..)
    , _DirectConnectClient
    , _DirectConnectClientException
    , _DirectConnectSerializer
    , _DirectConnectServerException
    , _DirectConnectService

    -- * ConnectionState
    , ConnectionState (..)

    -- * InterconnectState
    , InterconnectState (..)

    -- * VirtualInterfaceState
    , VirtualInterfaceState (..)

    -- * RouteFilterPrefix
    , RouteFilterPrefix
    , routeFilterPrefix
    , rfpCidr

    -- * Connection
    , Connection
    , connection
    , cOwnerAccount
    , cConnectionId
    , cConnectionName
    , cConnectionState
    , cRegion
    , cLocation
    , cBandwidth
    , cVlan
    , cPartnerName

    -- * Interconnect
    , Interconnect
    , interconnect
    , iInterconnectId
    , iInterconnectName
    , iInterconnectState
    , iRegion
    , iLocation
    , iBandwidth

    -- * Location
    , Location
    , location
    , lLocationCode
    , lLocationName

    -- * NewPrivateVirtualInterface
    , NewPrivateVirtualInterface
    , newPrivateVirtualInterface
    , npviVirtualInterfaceName
    , npviVlan
    , npviAsn
    , npviAuthKey
    , npviAmazonAddress
    , npviCustomerAddress
    , npviVirtualGatewayId

    -- * NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation
    , newPrivateVirtualInterfaceAllocation
    , npviaVirtualInterfaceName
    , npviaVlan
    , npviaAsn
    , npviaAuthKey
    , npviaAmazonAddress
    , npviaCustomerAddress

    -- * NewPublicVirtualInterface
    , NewPublicVirtualInterface
    , newPublicVirtualInterface
    , npvi1VirtualInterfaceName
    , npvi1Vlan
    , npvi1Asn
    , npvi1AuthKey
    , npvi1AmazonAddress
    , npvi1CustomerAddress
    , npvi1RouteFilterPrefixes

    -- * NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation
    , newPublicVirtualInterfaceAllocation
    , npvia1VirtualInterfaceName
    , npvia1Vlan
    , npvia1Asn
    , npvia1AuthKey
    , npvia1AmazonAddress
    , npvia1CustomerAddress
    , npvia1RouteFilterPrefixes

    -- * VirtualGateway
    , VirtualGateway
    , virtualGateway
    , vgVirtualGatewayId
    , vgVirtualGatewayState

    -- * VirtualInterface
    , VirtualInterface
    , virtualInterface
    , viOwnerAccount
    , viVirtualInterfaceId
    , viLocation
    , viConnectionId
    , viVirtualInterfaceType
    , viVirtualInterfaceName
    , viVlan
    , viAsn
    , viAuthKey
    , viAmazonAddress
    , viCustomerAddress
    , viVirtualInterfaceState
    , viCustomerRouterConfig
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-10-25@) of the
-- @AWS Direct Connect@ service.
data DirectConnect deriving (Typeable)

instance AWSService DirectConnect where
    type Sg DirectConnect = V4
    type Er DirectConnect = DirectConnectError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "directconnect"
        , _svcVersion  = "2012-10-25"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'DirectConnect' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data DirectConnectError
    = DirectConnectClient HttpException
      -- | The API was called with invalid parameters. The error message
      -- will contain additional details about the cause.
    | DirectConnectClientException
        { _dcceMessage :: Maybe Text
        }
    | DirectConnectSerializer String
      -- | A server-side error occurred during the API call. The error
      -- message will contain additional details about the cause.
    | DirectConnectServerException
        { _dcseMessage :: Maybe Text
        }
    | DirectConnectService String
      deriving (Show, Typeable, Generic)

instance AWSError DirectConnectError where
    awsError = const "DirectConnectError"

instance AWSServiceError DirectConnectError where
    serviceError    = DirectConnectService
    clientError     = DirectConnectClient
    serializerError = DirectConnectSerializer

instance Exception DirectConnectError

-- | See: 'DirectConnectClient'
_DirectConnectClient :: Prism' DirectConnectError HttpException
_DirectConnectClient = prism
    DirectConnectClient
    (\case
        DirectConnectClient p1 -> Right p1
        x -> Left x)

-- | The API was called with invalid parameters. The error message will contain
-- additional details about the cause.
--
-- See: 'DirectConnectClientException'
_DirectConnectClientException :: Prism' DirectConnectError (Maybe Text)
_DirectConnectClientException = prism
    DirectConnectClientException
    (\case
        DirectConnectClientException p1 -> Right p1
        x -> Left x)

-- | See: 'DirectConnectSerializer'
_DirectConnectSerializer :: Prism' DirectConnectError String
_DirectConnectSerializer = prism
    DirectConnectSerializer
    (\case
        DirectConnectSerializer p1 -> Right p1
        x -> Left x)

-- | A server-side error occurred during the API call. The error message will
-- contain additional details about the cause.
--
-- See: 'DirectConnectServerException'
_DirectConnectServerException :: Prism' DirectConnectError (Maybe Text)
_DirectConnectServerException = prism
    DirectConnectServerException
    (\case
        DirectConnectServerException p1 -> Right p1
        x -> Left x)

-- | See: 'DirectConnectService'
_DirectConnectService :: Prism' DirectConnectError String
_DirectConnectService = prism
    DirectConnectService
    (\case
        DirectConnectService p1 -> Right p1
        x -> Left x)

data ConnectionState
    = ConnectionStateAvailable -- ^ available
    | ConnectionStateDeleted -- ^ deleted
    | ConnectionStateDeleting -- ^ deleting
    | ConnectionStateDown -- ^ down
    | ConnectionStateOrdering -- ^ ordering
    | ConnectionStatePending -- ^ pending
    | ConnectionStateRejected -- ^ rejected
    | ConnectionStateRequested -- ^ requested
      deriving (Eq, Show, Generic)

instance Hashable ConnectionState

instance FromText ConnectionState where
    parser = match "available" ConnectionStateAvailable
         <|> match "deleted" ConnectionStateDeleted
         <|> match "deleting" ConnectionStateDeleting
         <|> match "down" ConnectionStateDown
         <|> match "ordering" ConnectionStateOrdering
         <|> match "pending" ConnectionStatePending
         <|> match "rejected" ConnectionStateRejected
         <|> match "requested" ConnectionStateRequested

instance ToText ConnectionState where
    toText ConnectionStateAvailable = "available"
    toText ConnectionStateDeleted = "deleted"
    toText ConnectionStateDeleting = "deleting"
    toText ConnectionStateDown = "down"
    toText ConnectionStateOrdering = "ordering"
    toText ConnectionStatePending = "pending"
    toText ConnectionStateRejected = "rejected"
    toText ConnectionStateRequested = "requested"

instance ToByteString ConnectionState where
    toBS ConnectionStateAvailable = "available"
    toBS ConnectionStateDeleted = "deleted"
    toBS ConnectionStateDeleting = "deleting"
    toBS ConnectionStateDown = "down"
    toBS ConnectionStateOrdering = "ordering"
    toBS ConnectionStatePending = "pending"
    toBS ConnectionStateRejected = "rejected"
    toBS ConnectionStateRequested = "requested"

instance ToHeader ConnectionState where
    toHeader k = toHeader k . toBS

instance ToQuery ConnectionState where
    toQuery = toQuery . toBS

instance FromJSON ConnectionState

instance ToJSON ConnectionState

data InterconnectState
    = InterconnectStateAvailable -- ^ available
    | InterconnectStateDeleted -- ^ deleted
    | InterconnectStateDeleting -- ^ deleting
    | InterconnectStateDown -- ^ down
    | InterconnectStatePending -- ^ pending
    | InterconnectStateRequested -- ^ requested
      deriving (Eq, Show, Generic)

instance Hashable InterconnectState

instance FromText InterconnectState where
    parser = match "available" InterconnectStateAvailable
         <|> match "deleted" InterconnectStateDeleted
         <|> match "deleting" InterconnectStateDeleting
         <|> match "down" InterconnectStateDown
         <|> match "pending" InterconnectStatePending
         <|> match "requested" InterconnectStateRequested

instance ToText InterconnectState where
    toText InterconnectStateAvailable = "available"
    toText InterconnectStateDeleted = "deleted"
    toText InterconnectStateDeleting = "deleting"
    toText InterconnectStateDown = "down"
    toText InterconnectStatePending = "pending"
    toText InterconnectStateRequested = "requested"

instance ToByteString InterconnectState where
    toBS InterconnectStateAvailable = "available"
    toBS InterconnectStateDeleted = "deleted"
    toBS InterconnectStateDeleting = "deleting"
    toBS InterconnectStateDown = "down"
    toBS InterconnectStatePending = "pending"
    toBS InterconnectStateRequested = "requested"

instance ToHeader InterconnectState where
    toHeader k = toHeader k . toBS

instance ToQuery InterconnectState where
    toQuery = toQuery . toBS

instance FromJSON InterconnectState

instance ToJSON InterconnectState

data VirtualInterfaceState
    = VirtualInterfaceStateAvailable -- ^ available
    | VirtualInterfaceStateConfirming -- ^ confirming
    | VirtualInterfaceStateDeleted -- ^ deleted
    | VirtualInterfaceStateDeleting -- ^ deleting
    | VirtualInterfaceStatePending -- ^ pending
    | VirtualInterfaceStateRejected -- ^ rejected
    | VirtualInterfaceStateVerifying -- ^ verifying
      deriving (Eq, Show, Generic)

instance Hashable VirtualInterfaceState

instance FromText VirtualInterfaceState where
    parser = match "available" VirtualInterfaceStateAvailable
         <|> match "confirming" VirtualInterfaceStateConfirming
         <|> match "deleted" VirtualInterfaceStateDeleted
         <|> match "deleting" VirtualInterfaceStateDeleting
         <|> match "pending" VirtualInterfaceStatePending
         <|> match "rejected" VirtualInterfaceStateRejected
         <|> match "verifying" VirtualInterfaceStateVerifying

instance ToText VirtualInterfaceState where
    toText VirtualInterfaceStateAvailable = "available"
    toText VirtualInterfaceStateConfirming = "confirming"
    toText VirtualInterfaceStateDeleted = "deleted"
    toText VirtualInterfaceStateDeleting = "deleting"
    toText VirtualInterfaceStatePending = "pending"
    toText VirtualInterfaceStateRejected = "rejected"
    toText VirtualInterfaceStateVerifying = "verifying"

instance ToByteString VirtualInterfaceState where
    toBS VirtualInterfaceStateAvailable = "available"
    toBS VirtualInterfaceStateConfirming = "confirming"
    toBS VirtualInterfaceStateDeleted = "deleted"
    toBS VirtualInterfaceStateDeleting = "deleting"
    toBS VirtualInterfaceStatePending = "pending"
    toBS VirtualInterfaceStateRejected = "rejected"
    toBS VirtualInterfaceStateVerifying = "verifying"

instance ToHeader VirtualInterfaceState where
    toHeader k = toHeader k . toBS

instance ToQuery VirtualInterfaceState where
    toQuery = toQuery . toBS

instance FromJSON VirtualInterfaceState

instance ToJSON VirtualInterfaceState

-- | A route filter prefix that the customer can advertise through Border
-- Gateway Protocol (BGP) over a public virtual interface.
newtype RouteFilterPrefix = RouteFilterPrefix
    { _rfpCidr :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RouteFilterPrefix' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Cidr ::@ @Maybe Text@
--
routeFilterPrefix :: RouteFilterPrefix
routeFilterPrefix = RouteFilterPrefix
    { _rfpCidr = Nothing
    }

-- | CIDR notation for the advertised route. Multiple routes are separated by
-- commas. Example: 10.10.10.0/24,10.10.11.0/24.
rfpCidr :: Lens' RouteFilterPrefix (Maybe Text)
rfpCidr = lens _rfpCidr (\s a -> s { _rfpCidr = a })

instance FromJSON RouteFilterPrefix

instance ToJSON RouteFilterPrefix

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data Connection = Connection
    { _cOwnerAccount :: Maybe Text
    , _cConnectionId :: Maybe Text
    , _cConnectionName :: Maybe Text
    , _cConnectionState :: Maybe ConnectionState
    , _cRegion :: Maybe Text
    , _cLocation :: Maybe Text
    , _cBandwidth :: Maybe Text
    , _cVlan :: Maybe Integer
    , _cPartnerName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Connection' data type.
--
-- 'Connection' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerAccount ::@ @Maybe Text@
--
-- * @ConnectionId ::@ @Maybe Text@
--
-- * @ConnectionName ::@ @Maybe Text@
--
-- * @ConnectionState ::@ @Maybe ConnectionState@
--
-- * @Region ::@ @Maybe Text@
--
-- * @Location ::@ @Maybe Text@
--
-- * @Bandwidth ::@ @Maybe Text@
--
-- * @Vlan ::@ @Maybe Integer@
--
-- * @PartnerName ::@ @Maybe Text@
--
connection :: Connection
connection = Connection
    { _cOwnerAccount = Nothing
    , _cConnectionId = Nothing
    , _cConnectionName = Nothing
    , _cConnectionState = Nothing
    , _cRegion = Nothing
    , _cLocation = Nothing
    , _cBandwidth = Nothing
    , _cVlan = Nothing
    , _cPartnerName = Nothing
    }

cOwnerAccount :: Lens' Connection (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\s a -> s { _cOwnerAccount = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cConnectionId :: Lens' Connection (Maybe Text)
cConnectionId = lens _cConnectionId (\s a -> s { _cConnectionId = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
cConnectionName :: Lens' Connection (Maybe Text)
cConnectionName = lens _cConnectionName (\s a -> s { _cConnectionName = a })

-- | State of the connection. Ordering: The initial state of a hosted connection
-- provisioned on an interconnect. The connection stays in the ordering state
-- until the owner of the hosted connection confirms or declines the
-- connection order. Requested: The initial state of a standard connection.
-- The connection stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The connection has
-- been approved, and is being initialized. Available: The network link is up,
-- and the connection is ready for use. Down: The network link is down.
-- Deleted: The connection has been deleted. Rejected: A hosted connection in
-- the 'Ordering' state will enter the 'Rejected' state if it is deleted by
-- the end customer.
cConnectionState :: Lens' Connection (Maybe ConnectionState)
cConnectionState =
    lens _cConnectionState (\s a -> s { _cConnectionState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
cRegion :: Lens' Connection (Maybe Text)
cRegion = lens _cRegion (\s a -> s { _cRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
cLocation :: Lens' Connection (Maybe Text)
cLocation = lens _cLocation (\s a -> s { _cLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
cBandwidth :: Lens' Connection (Maybe Text)
cBandwidth = lens _cBandwidth (\s a -> s { _cBandwidth = a })

-- | The VLAN ID. Example: 101.
cVlan :: Lens' Connection (Maybe Integer)
cVlan = lens _cVlan (\s a -> s { _cVlan = a })

cPartnerName :: Lens' Connection (Maybe Text)
cPartnerName = lens _cPartnerName (\s a -> s { _cPartnerName = a })

instance FromJSON Connection

-- | An interconnect is a connection that can host other connections. Like a
-- standard AWS Direct Connect connection, an interconnect represents the
-- physical connection between an AWS Direct Connect partner's network and a
-- specific Direct Connect location. An AWS Direct Connect partner who owns an
-- interconnect can provision hosted connections on the interconnect for their
-- end customers, thereby providing the end customers with connectivity to AWS
-- services. The resources of the interconnect, including bandwidth and VLAN
-- numbers, are shared by all of the hosted connections on the interconnect,
-- and the owner of the interconnect determines how these resources are
-- assigned.
data Interconnect = Interconnect
    { _iInterconnectId :: Maybe Text
    , _iInterconnectName :: Maybe Text
    , _iInterconnectState :: Maybe InterconnectState
    , _iRegion :: Maybe Text
    , _iLocation :: Maybe Text
    , _iBandwidth :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Interconnect' data type.
--
-- 'Interconnect' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InterconnectId ::@ @Maybe Text@
--
-- * @InterconnectName ::@ @Maybe Text@
--
-- * @InterconnectState ::@ @Maybe InterconnectState@
--
-- * @Region ::@ @Maybe Text@
--
-- * @Location ::@ @Maybe Text@
--
-- * @Bandwidth ::@ @Maybe Text@
--
interconnect :: Interconnect
interconnect = Interconnect
    { _iInterconnectId = Nothing
    , _iInterconnectName = Nothing
    , _iInterconnectState = Nothing
    , _iRegion = Nothing
    , _iLocation = Nothing
    , _iBandwidth = Nothing
    }

-- | The ID of the interconnect. Example: dxcon-abc123.
iInterconnectId :: Lens' Interconnect (Maybe Text)
iInterconnectId = lens _iInterconnectId (\s a -> s { _iInterconnectId = a })

-- | The name of the interconnect. Example: "1G Interconnect to AWS".
iInterconnectName :: Lens' Interconnect (Maybe Text)
iInterconnectName =
    lens _iInterconnectName (\s a -> s { _iInterconnectName = a })

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
iInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
iInterconnectState =
    lens _iInterconnectState (\s a -> s { _iInterconnectState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
iRegion :: Lens' Interconnect (Maybe Text)
iRegion = lens _iRegion (\s a -> s { _iRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
iLocation :: Lens' Interconnect (Maybe Text)
iLocation = lens _iLocation (\s a -> s { _iLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
iBandwidth :: Lens' Interconnect (Maybe Text)
iBandwidth = lens _iBandwidth (\s a -> s { _iBandwidth = a })

instance FromJSON Interconnect

-- | An AWS Direct Connect location where connections and interconnects can be
-- requested.
data Location = Location
    { _lLocationCode :: Maybe Text
    , _lLocationName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Location' data type.
--
-- 'Location' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LocationCode ::@ @Maybe Text@
--
-- * @LocationName ::@ @Maybe Text@
--
location :: Location
location = Location
    { _lLocationCode = Nothing
    , _lLocationName = Nothing
    }

-- | The code used to indicate the AWS Direct Connect location.
lLocationCode :: Lens' Location (Maybe Text)
lLocationCode = lens _lLocationCode (\s a -> s { _lLocationCode = a })

-- | The name of the AWS Direct Connect location. The name includes the
-- colocation partner name and the physical site of the lit building.
lLocationName :: Lens' Location (Maybe Text)
lLocationName = lens _lLocationName (\s a -> s { _lLocationName = a })

instance FromJSON Location

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface
    { _npviVirtualInterfaceName :: Text
    , _npviVlan :: !Integer
    , _npviAsn :: !Integer
    , _npviAuthKey :: Maybe Text
    , _npviAmazonAddress :: Maybe Text
    , _npviCustomerAddress :: Maybe Text
    , _npviVirtualGatewayId :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPrivateVirtualInterface' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualInterfaceName ::@ @Text@
--
-- * @Vlan ::@ @Integer@
--
-- * @Asn ::@ @Integer@
--
-- * @AuthKey ::@ @Maybe Text@
--
-- * @AmazonAddress ::@ @Maybe Text@
--
-- * @CustomerAddress ::@ @Maybe Text@
--
-- * @VirtualGatewayId ::@ @Text@
--
newPrivateVirtualInterface :: Text -- ^ 'npviVirtualInterfaceName'
                           -> Integer -- ^ 'npviVlan'
                           -> Integer -- ^ 'npviAsn'
                           -> Text -- ^ 'npviVirtualGatewayId'
                           -> NewPrivateVirtualInterface
newPrivateVirtualInterface p1 p2 p3 p7 = NewPrivateVirtualInterface
    { _npviVirtualInterfaceName = p1
    , _npviVlan = p2
    , _npviAsn = p3
    , _npviAuthKey = Nothing
    , _npviAmazonAddress = Nothing
    , _npviCustomerAddress = Nothing
    , _npviVirtualGatewayId = p7
    }

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npviVirtualInterfaceName :: Lens' NewPrivateVirtualInterface Text
npviVirtualInterfaceName =
    lens _npviVirtualInterfaceName
         (\s a -> s { _npviVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
npviVlan :: Lens' NewPrivateVirtualInterface Integer
npviVlan = lens _npviVlan (\s a -> s { _npviVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npviAsn :: Lens' NewPrivateVirtualInterface Integer
npviAsn = lens _npviAsn (\s a -> s { _npviAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
npviAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviAuthKey = lens _npviAuthKey (\s a -> s { _npviAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npviAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviAmazonAddress =
    lens _npviAmazonAddress (\s a -> s { _npviAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npviCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviCustomerAddress =
    lens _npviCustomerAddress (\s a -> s { _npviCustomerAddress = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
npviVirtualGatewayId :: Lens' NewPrivateVirtualInterface Text
npviVirtualGatewayId =
    lens _npviVirtualGatewayId (\s a -> s { _npviVirtualGatewayId = a })

instance ToJSON NewPrivateVirtualInterface

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation
    { _npviaVirtualInterfaceName :: Text
    , _npviaVlan :: !Integer
    , _npviaAsn :: !Integer
    , _npviaAuthKey :: Maybe Text
    , _npviaAmazonAddress :: Maybe Text
    , _npviaCustomerAddress :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPrivateVirtualInterfaceAllocation' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualInterfaceName ::@ @Text@
--
-- * @Vlan ::@ @Integer@
--
-- * @Asn ::@ @Integer@
--
-- * @AuthKey ::@ @Maybe Text@
--
-- * @AmazonAddress ::@ @Maybe Text@
--
-- * @CustomerAddress ::@ @Maybe Text@
--
newPrivateVirtualInterfaceAllocation :: Text -- ^ 'npviaVirtualInterfaceName'
                                     -> Integer -- ^ 'npviaVlan'
                                     -> Integer -- ^ 'npviaAsn'
                                     -> NewPrivateVirtualInterfaceAllocation
newPrivateVirtualInterfaceAllocation p1 p2 p3 = NewPrivateVirtualInterfaceAllocation
    { _npviaVirtualInterfaceName = p1
    , _npviaVlan = p2
    , _npviaAsn = p3
    , _npviaAuthKey = Nothing
    , _npviaAmazonAddress = Nothing
    , _npviaCustomerAddress = Nothing
    }

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation Text
npviaVirtualInterfaceName =
    lens _npviaVirtualInterfaceName
         (\s a -> s { _npviaVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation Integer
npviaVlan = lens _npviaVlan (\s a -> s { _npviaVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npviaAsn :: Lens' NewPrivateVirtualInterfaceAllocation Integer
npviaAsn = lens _npviaAsn (\s a -> s { _npviaAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\s a -> s { _npviaAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress =
    lens _npviaAmazonAddress (\s a -> s { _npviaAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress =
    lens _npviaCustomerAddress (\s a -> s { _npviaCustomerAddress = a })

instance ToJSON NewPrivateVirtualInterfaceAllocation

-- | Detailed information for the public virtual interface to be created.
-- Default: None.
data NewPublicVirtualInterface = NewPublicVirtualInterface
    { _npvi1VirtualInterfaceName :: Text
    , _npvi1Vlan :: !Integer
    , _npvi1Asn :: !Integer
    , _npvi1AuthKey :: Maybe Text
    , _npvi1AmazonAddress :: Text
    , _npvi1CustomerAddress :: Text
    , _npvi1RouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPublicVirtualInterface' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualInterfaceName ::@ @Text@
--
-- * @Vlan ::@ @Integer@
--
-- * @Asn ::@ @Integer@
--
-- * @AuthKey ::@ @Maybe Text@
--
-- * @AmazonAddress ::@ @Text@
--
-- * @CustomerAddress ::@ @Text@
--
-- * @RouteFilterPrefixes ::@ @[RouteFilterPrefix]@
--
newPublicVirtualInterface :: Text -- ^ 'npvi1VirtualInterfaceName'
                          -> Integer -- ^ 'npvi1Vlan'
                          -> Integer -- ^ 'npvi1Asn'
                          -> Text -- ^ 'npvi1AmazonAddress'
                          -> Text -- ^ 'npvi1CustomerAddress'
                          -> [RouteFilterPrefix] -- ^ 'npvi1RouteFilterPrefixes'
                          -> NewPublicVirtualInterface
newPublicVirtualInterface p1 p2 p3 p5 p6 p7 = NewPublicVirtualInterface
    { _npvi1VirtualInterfaceName = p1
    , _npvi1Vlan = p2
    , _npvi1Asn = p3
    , _npvi1AuthKey = Nothing
    , _npvi1AmazonAddress = p5
    , _npvi1CustomerAddress = p6
    , _npvi1RouteFilterPrefixes = p7
    }

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npvi1VirtualInterfaceName :: Lens' NewPublicVirtualInterface Text
npvi1VirtualInterfaceName =
    lens _npvi1VirtualInterfaceName
         (\s a -> s { _npvi1VirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
npvi1Vlan :: Lens' NewPublicVirtualInterface Integer
npvi1Vlan = lens _npvi1Vlan (\s a -> s { _npvi1Vlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npvi1Asn :: Lens' NewPublicVirtualInterface Integer
npvi1Asn = lens _npvi1Asn (\s a -> s { _npvi1Asn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
npvi1AuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npvi1AuthKey = lens _npvi1AuthKey (\s a -> s { _npvi1AuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npvi1AmazonAddress :: Lens' NewPublicVirtualInterface Text
npvi1AmazonAddress =
    lens _npvi1AmazonAddress (\s a -> s { _npvi1AmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npvi1CustomerAddress :: Lens' NewPublicVirtualInterface Text
npvi1CustomerAddress =
    lens _npvi1CustomerAddress (\s a -> s { _npvi1CustomerAddress = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
npvi1RouteFilterPrefixes :: Lens' NewPublicVirtualInterface [RouteFilterPrefix]
npvi1RouteFilterPrefixes =
    lens _npvi1RouteFilterPrefixes
         (\s a -> s { _npvi1RouteFilterPrefixes = a })

instance ToJSON NewPublicVirtualInterface

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation
    { _npvia1VirtualInterfaceName :: Text
    , _npvia1Vlan :: !Integer
    , _npvia1Asn :: !Integer
    , _npvia1AuthKey :: Maybe Text
    , _npvia1AmazonAddress :: Text
    , _npvia1CustomerAddress :: Text
    , _npvia1RouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPublicVirtualInterfaceAllocation' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualInterfaceName ::@ @Text@
--
-- * @Vlan ::@ @Integer@
--
-- * @Asn ::@ @Integer@
--
-- * @AuthKey ::@ @Maybe Text@
--
-- * @AmazonAddress ::@ @Text@
--
-- * @CustomerAddress ::@ @Text@
--
-- * @RouteFilterPrefixes ::@ @[RouteFilterPrefix]@
--
newPublicVirtualInterfaceAllocation :: Text -- ^ 'npvia1VirtualInterfaceName'
                                    -> Integer -- ^ 'npvia1Vlan'
                                    -> Integer -- ^ 'npvia1Asn'
                                    -> Text -- ^ 'npvia1AmazonAddress'
                                    -> Text -- ^ 'npvia1CustomerAddress'
                                    -> [RouteFilterPrefix] -- ^ 'npvia1RouteFilterPrefixes'
                                    -> NewPublicVirtualInterfaceAllocation
newPublicVirtualInterfaceAllocation p1 p2 p3 p5 p6 p7 = NewPublicVirtualInterfaceAllocation
    { _npvia1VirtualInterfaceName = p1
    , _npvia1Vlan = p2
    , _npvia1Asn = p3
    , _npvia1AuthKey = Nothing
    , _npvia1AmazonAddress = p5
    , _npvia1CustomerAddress = p6
    , _npvia1RouteFilterPrefixes = p7
    }

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npvia1VirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation Text
npvia1VirtualInterfaceName =
    lens _npvia1VirtualInterfaceName
         (\s a -> s { _npvia1VirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
npvia1Vlan :: Lens' NewPublicVirtualInterfaceAllocation Integer
npvia1Vlan = lens _npvia1Vlan (\s a -> s { _npvia1Vlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npvia1Asn :: Lens' NewPublicVirtualInterfaceAllocation Integer
npvia1Asn = lens _npvia1Asn (\s a -> s { _npvia1Asn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
npvia1AuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
npvia1AuthKey = lens _npvia1AuthKey (\s a -> s { _npvia1AuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npvia1AmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
npvia1AmazonAddress =
    lens _npvia1AmazonAddress (\s a -> s { _npvia1AmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npvia1CustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation Text
npvia1CustomerAddress =
    lens _npvia1CustomerAddress (\s a -> s { _npvia1CustomerAddress = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
npvia1RouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation [RouteFilterPrefix]
npvia1RouteFilterPrefixes =
    lens _npvia1RouteFilterPrefixes
         (\s a -> s { _npvia1RouteFilterPrefixes = a })

instance ToJSON NewPublicVirtualInterfaceAllocation

-- | You can create one or more AWS Direct Connect private virtual interfaces
-- linking to your virtual private gateway. Virtual private gateways can be
-- managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the
-- Amazon EC2 CreateVpnGateway action.
data VirtualGateway = VirtualGateway
    { _vgVirtualGatewayId :: Maybe Text
    , _vgVirtualGatewayState :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VirtualGateway' data type.
--
-- 'VirtualGateway' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualGatewayId ::@ @Maybe Text@
--
-- * @VirtualGatewayState ::@ @Maybe Text@
--
virtualGateway :: VirtualGateway
virtualGateway = VirtualGateway
    { _vgVirtualGatewayId = Nothing
    , _vgVirtualGatewayState = Nothing
    }

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vgVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayId =
    lens _vgVirtualGatewayId (\s a -> s { _vgVirtualGatewayId = a })

-- | State of the virtual private gateway. Pending: This is the initial state
-- after calling CreateVpnGateway. Available: Ready for use by a private
-- virtual interface. Deleting: This is the initial state after calling
-- DeleteVpnGateway. Deleted: In this state, a private virtual interface is
-- unable to send traffic over this gateway.
vgVirtualGatewayState :: Lens' VirtualGateway (Maybe Text)
vgVirtualGatewayState =
    lens _vgVirtualGatewayState (\s a -> s { _vgVirtualGatewayState = a })

instance FromJSON VirtualGateway

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data VirtualInterface = VirtualInterface
    { _viOwnerAccount :: Maybe Text
    , _viVirtualInterfaceId :: Maybe Text
    , _viLocation :: Maybe Text
    , _viConnectionId :: Maybe Text
    , _viVirtualInterfaceType :: Maybe Text
    , _viVirtualInterfaceName :: Maybe Text
    , _viVlan :: Maybe Integer
    , _viAsn :: Maybe Integer
    , _viAuthKey :: Maybe Text
    , _viAmazonAddress :: Maybe Text
    , _viCustomerAddress :: Maybe Text
    , _viVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _viCustomerRouterConfig :: Maybe Text
    , _viVirtualGatewayId :: Maybe Text
    , _viRouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VirtualInterface' data type.
--
-- 'VirtualInterface' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerAccount ::@ @Maybe Text@
--
-- * @VirtualInterfaceId ::@ @Maybe Text@
--
-- * @Location ::@ @Maybe Text@
--
-- * @ConnectionId ::@ @Maybe Text@
--
-- * @VirtualInterfaceType ::@ @Maybe Text@
--
-- * @VirtualInterfaceName ::@ @Maybe Text@
--
-- * @Vlan ::@ @Maybe Integer@
--
-- * @Asn ::@ @Maybe Integer@
--
-- * @AuthKey ::@ @Maybe Text@
--
-- * @AmazonAddress ::@ @Maybe Text@
--
-- * @CustomerAddress ::@ @Maybe Text@
--
-- * @VirtualInterfaceState ::@ @Maybe VirtualInterfaceState@
--
-- * @CustomerRouterConfig ::@ @Maybe Text@
--
-- * @VirtualGatewayId ::@ @Maybe Text@
--
-- * @RouteFilterPrefixes ::@ @[RouteFilterPrefix]@
--
virtualInterface :: VirtualInterface
virtualInterface = VirtualInterface
    { _viOwnerAccount = Nothing
    , _viVirtualInterfaceId = Nothing
    , _viLocation = Nothing
    , _viConnectionId = Nothing
    , _viVirtualInterfaceType = Nothing
    , _viVirtualInterfaceName = Nothing
    , _viVlan = Nothing
    , _viAsn = Nothing
    , _viAuthKey = Nothing
    , _viAmazonAddress = Nothing
    , _viCustomerAddress = Nothing
    , _viVirtualInterfaceState = Nothing
    , _viCustomerRouterConfig = Nothing
    , _viVirtualGatewayId = Nothing
    , _viRouteFilterPrefixes = mempty
    }

viOwnerAccount :: Lens' VirtualInterface (Maybe Text)
viOwnerAccount = lens _viOwnerAccount (\s a -> s { _viOwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
viVirtualInterfaceId :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceId =
    lens _viVirtualInterfaceId (\s a -> s { _viVirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
viLocation :: Lens' VirtualInterface (Maybe Text)
viLocation = lens _viLocation (\s a -> s { _viLocation = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
viConnectionId :: Lens' VirtualInterface (Maybe Text)
viConnectionId = lens _viConnectionId (\s a -> s { _viConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
viVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceType =
    lens _viVirtualInterfaceType (\s a -> s { _viVirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
viVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
viVirtualInterfaceName =
    lens _viVirtualInterfaceName (\s a -> s { _viVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
viVlan :: Lens' VirtualInterface (Maybe Integer)
viVlan = lens _viVlan (\s a -> s { _viVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
viAsn :: Lens' VirtualInterface (Maybe Integer)
viAsn = lens _viAsn (\s a -> s { _viAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
viAuthKey :: Lens' VirtualInterface (Maybe Text)
viAuthKey = lens _viAuthKey (\s a -> s { _viAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
viAmazonAddress :: Lens' VirtualInterface (Maybe Text)
viAmazonAddress = lens _viAmazonAddress (\s a -> s { _viAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
viCustomerAddress :: Lens' VirtualInterface (Maybe Text)
viCustomerAddress =
    lens _viCustomerAddress (\s a -> s { _viCustomerAddress = a })

-- | State of the virtual interface. Confirming: The creation of the virtual
-- interface is pending confirmation from the virtual interface owner. If the
-- owner of the virtual interface is different from the owner of the
-- connection on which it is provisioned, then the virtual interface will
-- remain in this state until it is confirmed by the virtual interface owner.
-- Verifying: This state only applies to public virtual interfaces. Each
-- public virtual interface needs validation before the virtual interface can
-- be created. Pending: A virtual interface is in this state from the time
-- that it is created until the virtual interface is ready to forward traffic.
-- Available: A virtual interface that is able to forward traffic. Deleting: A
-- virtual interface is in this state immediately after calling
-- DeleteVirtualInterface until it can no longer forward traffic. Deleted: A
-- virtual interface that cannot forward traffic. Rejected: The virtual
-- interface owner has declined creation of the virtual interface. If a
-- virtual interface in the 'Confirming' state is deleted by the virtual
-- interface owner, the virtual interface will enter the 'Rejected' state.
viVirtualInterfaceState :: Lens' VirtualInterface (Maybe VirtualInterfaceState)
viVirtualInterfaceState =
    lens _viVirtualInterfaceState
         (\s a -> s { _viVirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
viCustomerRouterConfig =
    lens _viCustomerRouterConfig (\s a -> s { _viCustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
viVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
viVirtualGatewayId =
    lens _viVirtualGatewayId (\s a -> s { _viVirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
viRouteFilterPrefixes :: Lens' VirtualInterface [RouteFilterPrefix]
viRouteFilterPrefixes =
    lens _viRouteFilterPrefixes (\s a -> s { _viRouteFilterPrefixes = a })

instance FromJSON VirtualInterface

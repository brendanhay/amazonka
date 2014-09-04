{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.Types
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
module Network.AWS.DirectConnect.V2012_10_25.Types
    (
    -- * Service
      DirectConnect
    -- ** Errors
    , Er (..)
    -- * ConnectionState
    , ConnectionState (..)

    -- * InterconnectState
    , InterconnectState (..)

    -- * VirtualInterfaceState
    , VirtualInterfaceState (..)

    -- * RouteFilterPrefix
    , RouteFilterPrefix
    , mkRouteFilterPrefix
    , rfpCidr

    -- * Connection
    , Connection
    , nOwnerAccount
    , nConnectionId
    , nConnectionName
    , nConnectionState
    , nRegion
    , nLocation
    , nBandwidth
    , nVlan
    , nPartnerName

    -- * Interconnect
    , Interconnect
    , zInterconnectId
    , zInterconnectName
    , zInterconnectState
    , zRegion
    , zLocation
    , zBandwidth

    -- * Location
    , Location
    , lqLocationCode
    , lqLocationName

    -- * NewPrivateVirtualInterface
    , NewPrivateVirtualInterface
    , mkNewPrivateVirtualInterface
    , npviVirtualInterfaceName
    , npviVlan
    , npviAsn
    , npviAuthKey
    , npviAmazonAddress
    , npviCustomerAddress
    , npviVirtualGatewayId

    -- * NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation
    , mkNewPrivateVirtualInterfaceAllocation
    , npviaVirtualInterfaceName
    , npviaVlan
    , npviaAsn
    , npviaAuthKey
    , npviaAmazonAddress
    , npviaCustomerAddress

    -- * NewPublicVirtualInterface
    , NewPublicVirtualInterface
    , mkNewPublicVirtualInterface
    , npvjVirtualInterfaceName
    , npvjVlan
    , npvjAsn
    , npvjAuthKey
    , npvjAmazonAddress
    , npvjCustomerAddress
    , npvjRouteFilterPrefixes

    -- * NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation
    , mkNewPublicVirtualInterfaceAllocation
    , npvibVirtualInterfaceName
    , npvibVlan
    , npvibAsn
    , npvibAuthKey
    , npvibAmazonAddress
    , npvibCustomerAddress
    , npvibRouteFilterPrefixes

    -- * VirtualGateway
    , VirtualGateway
    , vhVirtualGatewayId
    , vhVirtualGatewayState

    -- * VirtualInterface
    , VirtualInterface
    , vnOwnerAccount
    , vnVirtualInterfaceId
    , vnLocation
    , vnConnectionId
    , vnVirtualInterfaceType
    , vnVirtualInterfaceName
    , vnVlan
    , vnAsn
    , vnAuthKey
    , vnAmazonAddress
    , vnCustomerAddress
    , vnVirtualInterfaceState
    , vnCustomerRouterConfig
    , vnVirtualGatewayId
    , vnRouteFilterPrefixes
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-10-25@) of the
-- @AWS Direct Connect@ service.
data DirectConnect deriving (Typeable)

instance AWSService DirectConnect where
    type Sg DirectConnect = V4
    data Er DirectConnect
        = DirectConnectClient HttpException
        | DirectConnectClientException
            { _dcceMessage :: Maybe Text
            }
        | DirectConnectSerializer String
        | DirectConnectServerException
            { _dcseMessage :: Maybe Text
            }
        | DirectConnectService String

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "directconnect"
        , _svcVersion  = "2012-10-25"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er DirectConnect)
deriving instance Generic (Er DirectConnect)

instance AWSError (Er DirectConnect) where
    awsError = const "DirectConnectError"

instance AWSServiceError (Er DirectConnect) where
    serviceError    = DirectConnectService
    clientError     = DirectConnectClient
    serializerError = DirectConnectSerializer

instance Exception (Er DirectConnect)

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

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
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
      -- ^ CIDR notation for the advertised route. Multiple routes are
      -- separated by commas. Example: 10.10.10.0/24,10.10.11.0/24.
    } deriving (Show, Generic)

-- | CIDR notation for the advertised route. Multiple routes are separated by
-- commas. Example: 10.10.10.0/24,10.10.11.0/24.
rfpCidr :: Lens' RouteFilterPrefix (Maybe Text)
rfpCidr = lens _rfpCidr (\s a -> s { _rfpCidr = a })
{-# INLINE rfpCidr #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RouteFilterPrefix' data type to populate a request.
mkRouteFilterPrefix :: RouteFilterPrefix
mkRouteFilterPrefix = RouteFilterPrefix
    { _rfpCidr = Nothing
    }
{-# INLINE mkRouteFilterPrefix #-}

instance FromJSON RouteFilterPrefix

instance ToJSON RouteFilterPrefix

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data Connection = Connection
    { _nOwnerAccount :: Maybe Text
    , _nConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _nConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    , _nConnectionState :: Maybe ConnectionState
      -- ^ State of the connection. Ordering: The initial state of a hosted
      -- connection provisioned on an interconnect. The connection stays
      -- in the ordering state until the owner of the hosted connection
      -- confirms or declines the connection order. Requested: The initial
      -- state of a standard connection. The connection stays in the
      -- requested state until the Letter of Authorization (LOA) is sent
      -- to the customer. Pending: The connection has been approved, and
      -- is being initialized. Available: The network link is up, and the
      -- connection is ready for use. Down: The network link is down.
      -- Deleted: The connection has been deleted. Rejected: A hosted
      -- connection in the 'Ordering' state will enter the 'Rejected'
      -- state if it is deleted by the end customer.
    , _nRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _nLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _nBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _nVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _nPartnerName :: Maybe Text
    } deriving (Show, Generic)

nOwnerAccount :: Lens' Connection (Maybe Text)
nOwnerAccount = lens _nOwnerAccount (\s a -> s { _nOwnerAccount = a })
{-# INLINE nOwnerAccount #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
nConnectionId :: Lens' Connection (Maybe Text)
nConnectionId = lens _nConnectionId (\s a -> s { _nConnectionId = a })
{-# INLINE nConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
nConnectionName :: Lens' Connection (Maybe Text)
nConnectionName = lens _nConnectionName (\s a -> s { _nConnectionName = a })
{-# INLINE nConnectionName #-}

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
nConnectionState :: Lens' Connection (Maybe ConnectionState)
nConnectionState = lens _nConnectionState (\s a -> s { _nConnectionState = a })
{-# INLINE nConnectionState #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
nRegion :: Lens' Connection (Maybe Text)
nRegion = lens _nRegion (\s a -> s { _nRegion = a })
{-# INLINE nRegion #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
nLocation :: Lens' Connection (Maybe Text)
nLocation = lens _nLocation (\s a -> s { _nLocation = a })
{-# INLINE nLocation #-}

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
nBandwidth :: Lens' Connection (Maybe Text)
nBandwidth = lens _nBandwidth (\s a -> s { _nBandwidth = a })
{-# INLINE nBandwidth #-}

-- | The VLAN ID. Example: 101.
nVlan :: Lens' Connection (Maybe Integer)
nVlan = lens _nVlan (\s a -> s { _nVlan = a })
{-# INLINE nVlan #-}

nPartnerName :: Lens' Connection (Maybe Text)
nPartnerName = lens _nPartnerName (\s a -> s { _nPartnerName = a })
{-# INLINE nPartnerName #-}

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
    { _zInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    , _zInterconnectName :: Maybe Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS".
    , _zInterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an
      -- interconnect. The interconnect stays in the requested state until
      -- the Letter of Authorization (LOA) is sent to the customer.
      -- Pending: The interconnect has been approved, and is being
      -- initialized. Available: The network link is up, and the
      -- interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    , _zRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _zLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _zBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    } deriving (Show, Generic)

-- | The ID of the interconnect. Example: dxcon-abc123.
zInterconnectId :: Lens' Interconnect (Maybe Text)
zInterconnectId = lens _zInterconnectId (\s a -> s { _zInterconnectId = a })
{-# INLINE zInterconnectId #-}

-- | The name of the interconnect. Example: "1G Interconnect to AWS".
zInterconnectName :: Lens' Interconnect (Maybe Text)
zInterconnectName = lens _zInterconnectName (\s a -> s { _zInterconnectName = a })
{-# INLINE zInterconnectName #-}

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
zInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
zInterconnectState = lens _zInterconnectState (\s a -> s { _zInterconnectState = a })
{-# INLINE zInterconnectState #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
zRegion :: Lens' Interconnect (Maybe Text)
zRegion = lens _zRegion (\s a -> s { _zRegion = a })
{-# INLINE zRegion #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
zLocation :: Lens' Interconnect (Maybe Text)
zLocation = lens _zLocation (\s a -> s { _zLocation = a })
{-# INLINE zLocation #-}

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
zBandwidth :: Lens' Interconnect (Maybe Text)
zBandwidth = lens _zBandwidth (\s a -> s { _zBandwidth = a })
{-# INLINE zBandwidth #-}

instance FromJSON Interconnect

-- | An AWS Direct Connect location where connections and interconnects can be
-- requested.
data Location = Location
    { _lqLocationCode :: Maybe Text
      -- ^ The code used to indicate the AWS Direct Connect location.
    , _lqLocationName :: Maybe Text
      -- ^ The name of the AWS Direct Connect location. The name includes
      -- the colocation partner name and the physical site of the lit
      -- building.
    } deriving (Show, Generic)

-- | The code used to indicate the AWS Direct Connect location.
lqLocationCode :: Lens' Location (Maybe Text)
lqLocationCode = lens _lqLocationCode (\s a -> s { _lqLocationCode = a })
{-# INLINE lqLocationCode #-}

-- | The name of the AWS Direct Connect location. The name includes the
-- colocation partner name and the physical site of the lit building.
lqLocationName :: Lens' Location (Maybe Text)
lqLocationName = lens _lqLocationName (\s a -> s { _lqLocationName = a })
{-# INLINE lqLocationName #-}

instance FromJSON Location

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface
    { _npviVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _npviVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npviAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npviAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npviAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npviCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _npviVirtualGatewayId :: Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    } deriving (Show, Generic)

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npviVirtualInterfaceName :: Lens' NewPrivateVirtualInterface (Text)
npviVirtualInterfaceName = lens _npviVirtualInterfaceName (\s a -> s { _npviVirtualInterfaceName = a })
{-# INLINE npviVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npviVlan :: Lens' NewPrivateVirtualInterface (Integer)
npviVlan = lens _npviVlan (\s a -> s { _npviVlan = a })
{-# INLINE npviVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npviAsn :: Lens' NewPrivateVirtualInterface (Integer)
npviAsn = lens _npviAsn (\s a -> s { _npviAsn = a })
{-# INLINE npviAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npviAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviAuthKey = lens _npviAuthKey (\s a -> s { _npviAuthKey = a })
{-# INLINE npviAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npviAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviAmazonAddress = lens _npviAmazonAddress (\s a -> s { _npviAmazonAddress = a })
{-# INLINE npviAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npviCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviCustomerAddress = lens _npviCustomerAddress (\s a -> s { _npviCustomerAddress = a })
{-# INLINE npviCustomerAddress #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
npviVirtualGatewayId :: Lens' NewPrivateVirtualInterface (Text)
npviVirtualGatewayId = lens _npviVirtualGatewayId (\s a -> s { _npviVirtualGatewayId = a })
{-# INLINE npviVirtualGatewayId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPrivateVirtualInterface' data type to populate a request.
mkNewPrivateVirtualInterface :: Text -- ^ 'npviVirtualInterfaceName'
                             -> Integer -- ^ 'npviVlan'
                             -> Integer -- ^ 'npviAsn'
                             -> Text -- ^ 'npviVirtualGatewayId'
                             -> NewPrivateVirtualInterface
mkNewPrivateVirtualInterface p1 p2 p3 p4 = NewPrivateVirtualInterface
    { _npviVirtualInterfaceName = p1
    , _npviVlan = p2
    , _npviAsn = p3
    , _npviAuthKey = Nothing
    , _npviAmazonAddress = Nothing
    , _npviCustomerAddress = Nothing
    , _npviVirtualGatewayId = p7
    }
{-# INLINE mkNewPrivateVirtualInterface #-}

instance ToJSON NewPrivateVirtualInterface

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation
    { _npviaVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _npviaVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npviaAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npviaAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npviaAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npviaCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    } deriving (Show, Generic)

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npviaVirtualInterfaceName :: Lens' NewPrivateVirtualInterfaceAllocation (Text)
npviaVirtualInterfaceName = lens _npviaVirtualInterfaceName (\s a -> s { _npviaVirtualInterfaceName = a })
{-# INLINE npviaVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation (Integer)
npviaVlan = lens _npviaVlan (\s a -> s { _npviaVlan = a })
{-# INLINE npviaVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npviaAsn :: Lens' NewPrivateVirtualInterfaceAllocation (Integer)
npviaAsn = lens _npviaAsn (\s a -> s { _npviaAsn = a })
{-# INLINE npviaAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey = lens _npviaAuthKey (\s a -> s { _npviaAuthKey = a })
{-# INLINE npviaAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress = lens _npviaAmazonAddress (\s a -> s { _npviaAmazonAddress = a })
{-# INLINE npviaAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress = lens _npviaCustomerAddress (\s a -> s { _npviaCustomerAddress = a })
{-# INLINE npviaCustomerAddress #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPrivateVirtualInterfaceAllocation' data type to populate a request.
mkNewPrivateVirtualInterfaceAllocation :: Text -- ^ 'npviaVirtualInterfaceName'
                                       -> Integer -- ^ 'npviaVlan'
                                       -> Integer -- ^ 'npviaAsn'
                                       -> NewPrivateVirtualInterfaceAllocation
mkNewPrivateVirtualInterfaceAllocation p1 p2 p3 = NewPrivateVirtualInterfaceAllocation
    { _npviaVirtualInterfaceName = p1
    , _npviaVlan = p2
    , _npviaAsn = p3
    , _npviaAuthKey = Nothing
    , _npviaAmazonAddress = Nothing
    , _npviaCustomerAddress = Nothing
    }
{-# INLINE mkNewPrivateVirtualInterfaceAllocation #-}

instance ToJSON NewPrivateVirtualInterfaceAllocation

-- | Detailed information for the public virtual interface to be created.
-- Default: None.
data NewPublicVirtualInterface = NewPublicVirtualInterface
    { _npvjVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _npvjVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npvjAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npvjAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npvjAmazonAddress :: Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npvjCustomerAddress :: Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _npvjRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    } deriving (Show, Generic)

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npvjVirtualInterfaceName :: Lens' NewPublicVirtualInterface (Text)
npvjVirtualInterfaceName = lens _npvjVirtualInterfaceName (\s a -> s { _npvjVirtualInterfaceName = a })
{-# INLINE npvjVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npvjVlan :: Lens' NewPublicVirtualInterface (Integer)
npvjVlan = lens _npvjVlan (\s a -> s { _npvjVlan = a })
{-# INLINE npvjVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npvjAsn :: Lens' NewPublicVirtualInterface (Integer)
npvjAsn = lens _npvjAsn (\s a -> s { _npvjAsn = a })
{-# INLINE npvjAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npvjAuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npvjAuthKey = lens _npvjAuthKey (\s a -> s { _npvjAuthKey = a })
{-# INLINE npvjAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npvjAmazonAddress :: Lens' NewPublicVirtualInterface (Text)
npvjAmazonAddress = lens _npvjAmazonAddress (\s a -> s { _npvjAmazonAddress = a })
{-# INLINE npvjAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npvjCustomerAddress :: Lens' NewPublicVirtualInterface (Text)
npvjCustomerAddress = lens _npvjCustomerAddress (\s a -> s { _npvjCustomerAddress = a })
{-# INLINE npvjCustomerAddress #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
npvjRouteFilterPrefixes :: Lens' NewPublicVirtualInterface ([RouteFilterPrefix])
npvjRouteFilterPrefixes = lens _npvjRouteFilterPrefixes (\s a -> s { _npvjRouteFilterPrefixes = a })
{-# INLINE npvjRouteFilterPrefixes #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPublicVirtualInterface' data type to populate a request.
mkNewPublicVirtualInterface :: Text -- ^ 'npvjVirtualInterfaceName'
                            -> Integer -- ^ 'npvjVlan'
                            -> Integer -- ^ 'npvjAsn'
                            -> Text -- ^ 'npvjAmazonAddress'
                            -> Text -- ^ 'npvjCustomerAddress'
                            -> [RouteFilterPrefix] -- ^ 'npvjRouteFilterPrefixes'
                            -> NewPublicVirtualInterface
mkNewPublicVirtualInterface p1 p2 p3 p4 p5 p6 = NewPublicVirtualInterface
    { _npvjVirtualInterfaceName = p1
    , _npvjVlan = p2
    , _npvjAsn = p3
    , _npvjAuthKey = Nothing
    , _npvjAmazonAddress = p5
    , _npvjCustomerAddress = p6
    , _npvjRouteFilterPrefixes = p7
    }
{-# INLINE mkNewPublicVirtualInterface #-}

instance ToJSON NewPublicVirtualInterface

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation
    { _npvibVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _npvibVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npvibAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npvibAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npvibAmazonAddress :: Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npvibCustomerAddress :: Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _npvibRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    } deriving (Show, Generic)

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
npvibVirtualInterfaceName :: Lens' NewPublicVirtualInterfaceAllocation (Text)
npvibVirtualInterfaceName = lens _npvibVirtualInterfaceName (\s a -> s { _npvibVirtualInterfaceName = a })
{-# INLINE npvibVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npvibVlan :: Lens' NewPublicVirtualInterfaceAllocation (Integer)
npvibVlan = lens _npvibVlan (\s a -> s { _npvibVlan = a })
{-# INLINE npvibVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npvibAsn :: Lens' NewPublicVirtualInterfaceAllocation (Integer)
npvibAsn = lens _npvibAsn (\s a -> s { _npvibAsn = a })
{-# INLINE npvibAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npvibAuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
npvibAuthKey = lens _npvibAuthKey (\s a -> s { _npvibAuthKey = a })
{-# INLINE npvibAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npvibAmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation (Text)
npvibAmazonAddress = lens _npvibAmazonAddress (\s a -> s { _npvibAmazonAddress = a })
{-# INLINE npvibAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npvibCustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation (Text)
npvibCustomerAddress = lens _npvibCustomerAddress (\s a -> s { _npvibCustomerAddress = a })
{-# INLINE npvibCustomerAddress #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
npvibRouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation ([RouteFilterPrefix])
npvibRouteFilterPrefixes = lens _npvibRouteFilterPrefixes (\s a -> s { _npvibRouteFilterPrefixes = a })
{-# INLINE npvibRouteFilterPrefixes #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NewPublicVirtualInterfaceAllocation' data type to populate a request.
mkNewPublicVirtualInterfaceAllocation :: Text -- ^ 'npvibVirtualInterfaceName'
                                      -> Integer -- ^ 'npvibVlan'
                                      -> Integer -- ^ 'npvibAsn'
                                      -> Text -- ^ 'npvibAmazonAddress'
                                      -> Text -- ^ 'npvibCustomerAddress'
                                      -> [RouteFilterPrefix] -- ^ 'npvibRouteFilterPrefixes'
                                      -> NewPublicVirtualInterfaceAllocation
mkNewPublicVirtualInterfaceAllocation p1 p2 p3 p4 p5 p6 = NewPublicVirtualInterfaceAllocation
    { _npvibVirtualInterfaceName = p1
    , _npvibVlan = p2
    , _npvibAsn = p3
    , _npvibAuthKey = Nothing
    , _npvibAmazonAddress = p5
    , _npvibCustomerAddress = p6
    , _npvibRouteFilterPrefixes = p7
    }
{-# INLINE mkNewPublicVirtualInterfaceAllocation #-}

instance ToJSON NewPublicVirtualInterfaceAllocation

-- | You can create one or more AWS Direct Connect private virtual interfaces
-- linking to your virtual private gateway. Virtual private gateways can be
-- managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the
-- Amazon EC2 CreateVpnGateway action.
data VirtualGateway = VirtualGateway
    { _vhVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vhVirtualGatewayState :: Maybe Text
      -- ^ State of the virtual private gateway. Pending: This is the
      -- initial state after calling CreateVpnGateway. Available: Ready
      -- for use by a private virtual interface. Deleting: This is the
      -- initial state after calling DeleteVpnGateway. Deleted: In this
      -- state, a private virtual interface is unable to send traffic over
      -- this gateway.
    } deriving (Show, Generic)

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vhVirtualGatewayId :: Lens' VirtualGateway (Maybe Text)
vhVirtualGatewayId = lens _vhVirtualGatewayId (\s a -> s { _vhVirtualGatewayId = a })
{-# INLINE vhVirtualGatewayId #-}

-- | State of the virtual private gateway. Pending: This is the initial state
-- after calling CreateVpnGateway. Available: Ready for use by a private
-- virtual interface. Deleting: This is the initial state after calling
-- DeleteVpnGateway. Deleted: In this state, a private virtual interface is
-- unable to send traffic over this gateway.
vhVirtualGatewayState :: Lens' VirtualGateway (Maybe Text)
vhVirtualGatewayState = lens _vhVirtualGatewayState (\s a -> s { _vhVirtualGatewayState = a })
{-# INLINE vhVirtualGatewayState #-}

instance FromJSON VirtualGateway

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data VirtualInterface = VirtualInterface
    { _vnOwnerAccount :: Maybe Text
    , _vnVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _vnLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vnConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vnVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    , _vnVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _vnVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vnAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vnAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vnAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vnCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vnVirtualInterfaceState :: Maybe VirtualInterfaceState
      -- ^ State of the virtual interface. Confirming: The creation of the
      -- virtual interface is pending confirmation from the virtual
      -- interface owner. If the owner of the virtual interface is
      -- different from the owner of the connection on which it is
      -- provisioned, then the virtual interface will remain in this state
      -- until it is confirmed by the virtual interface owner. Verifying:
      -- This state only applies to public virtual interfaces. Each public
      -- virtual interface needs validation before the virtual interface
      -- can be created. Pending: A virtual interface is in this state
      -- from the time that it is created until the virtual interface is
      -- ready to forward traffic. Available: A virtual interface that is
      -- able to forward traffic. Deleting: A virtual interface is in this
      -- state immediately after calling DeleteVirtualInterface until it
      -- can no longer forward traffic. Deleted: A virtual interface that
      -- cannot forward traffic. Rejected: The virtual interface owner has
      -- declined creation of the virtual interface. If a virtual
      -- interface in the 'Confirming' state is deleted by the virtual
      -- interface owner, the virtual interface will enter the 'Rejected'
      -- state.
    , _vnCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vnVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vnRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    } deriving (Show, Generic)

vnOwnerAccount :: Lens' VirtualInterface (Maybe Text)
vnOwnerAccount = lens _vnOwnerAccount (\s a -> s { _vnOwnerAccount = a })
{-# INLINE vnOwnerAccount #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
vnVirtualInterfaceId :: Lens' VirtualInterface (Maybe Text)
vnVirtualInterfaceId = lens _vnVirtualInterfaceId (\s a -> s { _vnVirtualInterfaceId = a })
{-# INLINE vnVirtualInterfaceId #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
vnLocation :: Lens' VirtualInterface (Maybe Text)
vnLocation = lens _vnLocation (\s a -> s { _vnLocation = a })
{-# INLINE vnLocation #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
vnConnectionId :: Lens' VirtualInterface (Maybe Text)
vnConnectionId = lens _vnConnectionId (\s a -> s { _vnConnectionId = a })
{-# INLINE vnConnectionId #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
vnVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
vnVirtualInterfaceType = lens _vnVirtualInterfaceType (\s a -> s { _vnVirtualInterfaceType = a })
{-# INLINE vnVirtualInterfaceType #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
vnVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
vnVirtualInterfaceName = lens _vnVirtualInterfaceName (\s a -> s { _vnVirtualInterfaceName = a })
{-# INLINE vnVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
vnVlan :: Lens' VirtualInterface (Maybe Integer)
vnVlan = lens _vnVlan (\s a -> s { _vnVlan = a })
{-# INLINE vnVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
vnAsn :: Lens' VirtualInterface (Maybe Integer)
vnAsn = lens _vnAsn (\s a -> s { _vnAsn = a })
{-# INLINE vnAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
vnAuthKey :: Lens' VirtualInterface (Maybe Text)
vnAuthKey = lens _vnAuthKey (\s a -> s { _vnAuthKey = a })
{-# INLINE vnAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
vnAmazonAddress :: Lens' VirtualInterface (Maybe Text)
vnAmazonAddress = lens _vnAmazonAddress (\s a -> s { _vnAmazonAddress = a })
{-# INLINE vnAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
vnCustomerAddress :: Lens' VirtualInterface (Maybe Text)
vnCustomerAddress = lens _vnCustomerAddress (\s a -> s { _vnCustomerAddress = a })
{-# INLINE vnCustomerAddress #-}

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
vnVirtualInterfaceState :: Lens' VirtualInterface (Maybe VirtualInterfaceState)
vnVirtualInterfaceState = lens _vnVirtualInterfaceState (\s a -> s { _vnVirtualInterfaceState = a })
{-# INLINE vnVirtualInterfaceState #-}

-- | Information for generating the customer router configuration.
vnCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
vnCustomerRouterConfig = lens _vnCustomerRouterConfig (\s a -> s { _vnCustomerRouterConfig = a })
{-# INLINE vnCustomerRouterConfig #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vnVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
vnVirtualGatewayId = lens _vnVirtualGatewayId (\s a -> s { _vnVirtualGatewayId = a })
{-# INLINE vnVirtualGatewayId #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
vnRouteFilterPrefixes :: Lens' VirtualInterface ([RouteFilterPrefix])
vnRouteFilterPrefixes = lens _vnRouteFilterPrefixes (\s a -> s { _vnRouteFilterPrefixes = a })
{-# INLINE vnRouteFilterPrefixes #-}

instance FromJSON VirtualInterface

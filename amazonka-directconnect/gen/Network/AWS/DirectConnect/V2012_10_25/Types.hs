{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , RouteFilterPrefix (..)
    , rfpCidr

    -- * Connection
    , Connection (..)
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
    , Interconnect (..)
    , zInterconnectId
    , zInterconnectName
    , zInterconnectState
    , zRegion
    , zLocation
    , zBandwidth

    -- * Location
    , Location (..)
    , lqLocationCode
    , lqLocationName

    -- * NewPrivateVirtualInterface
    , NewPrivateVirtualInterface (..)
    , npviVirtualInterfaceName
    , npviVlan
    , npviAsn
    , npviAuthKey
    , npviAmazonAddress
    , npviCustomerAddress
    , npviVirtualGatewayId

    -- * NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation (..)
    , npviaVirtualInterfaceName
    , npviaVlan
    , npviaAsn
    , npviaAuthKey
    , npviaAmazonAddress
    , npviaCustomerAddress

    -- * NewPublicVirtualInterface
    , NewPublicVirtualInterface (..)
    , npvjVirtualInterfaceName
    , npvjVlan
    , npvjAsn
    , npvjAuthKey
    , npvjAmazonAddress
    , npvjCustomerAddress
    , npvjRouteFilterPrefixes

    -- * NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation (..)
    , npvibVirtualInterfaceName
    , npvibVlan
    , npvibAsn
    , npvibAuthKey
    , npvibAmazonAddress
    , npvibCustomerAddress
    , npvibRouteFilterPrefixes

    -- * VirtualGateway
    , VirtualGateway (..)
    , vhVirtualGatewayId
    , vhVirtualGatewayState

    -- * VirtualInterface
    , VirtualInterface (..)
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
rfpCidr f x =
    f (_rfpCidr x)
        <&> \y -> x { _rfpCidr = y }
{-# INLINE rfpCidr #-}

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
nOwnerAccount f x =
    f (_nOwnerAccount x)
        <&> \y -> x { _nOwnerAccount = y }
{-# INLINE nOwnerAccount #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
nConnectionId :: Lens' Connection (Maybe Text)
nConnectionId f x =
    f (_nConnectionId x)
        <&> \y -> x { _nConnectionId = y }
{-# INLINE nConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
nConnectionName :: Lens' Connection (Maybe Text)
nConnectionName f x =
    f (_nConnectionName x)
        <&> \y -> x { _nConnectionName = y }
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
nConnectionState f x =
    f (_nConnectionState x)
        <&> \y -> x { _nConnectionState = y }
{-# INLINE nConnectionState #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
nRegion :: Lens' Connection (Maybe Text)
nRegion f x =
    f (_nRegion x)
        <&> \y -> x { _nRegion = y }
{-# INLINE nRegion #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
nLocation :: Lens' Connection (Maybe Text)
nLocation f x =
    f (_nLocation x)
        <&> \y -> x { _nLocation = y }
{-# INLINE nLocation #-}

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
nBandwidth :: Lens' Connection (Maybe Text)
nBandwidth f x =
    f (_nBandwidth x)
        <&> \y -> x { _nBandwidth = y }
{-# INLINE nBandwidth #-}

-- | The VLAN ID. Example: 101.
nVlan :: Lens' Connection (Maybe Integer)
nVlan f x =
    f (_nVlan x)
        <&> \y -> x { _nVlan = y }
{-# INLINE nVlan #-}

nPartnerName :: Lens' Connection (Maybe Text)
nPartnerName f x =
    f (_nPartnerName x)
        <&> \y -> x { _nPartnerName = y }
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
zInterconnectId f x =
    f (_zInterconnectId x)
        <&> \y -> x { _zInterconnectId = y }
{-# INLINE zInterconnectId #-}

-- | The name of the interconnect. Example: "1G Interconnect to AWS".
zInterconnectName :: Lens' Interconnect (Maybe Text)
zInterconnectName f x =
    f (_zInterconnectName x)
        <&> \y -> x { _zInterconnectName = y }
{-# INLINE zInterconnectName #-}

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
zInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
zInterconnectState f x =
    f (_zInterconnectState x)
        <&> \y -> x { _zInterconnectState = y }
{-# INLINE zInterconnectState #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
zRegion :: Lens' Interconnect (Maybe Text)
zRegion f x =
    f (_zRegion x)
        <&> \y -> x { _zRegion = y }
{-# INLINE zRegion #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
zLocation :: Lens' Interconnect (Maybe Text)
zLocation f x =
    f (_zLocation x)
        <&> \y -> x { _zLocation = y }
{-# INLINE zLocation #-}

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
zBandwidth :: Lens' Interconnect (Maybe Text)
zBandwidth f x =
    f (_zBandwidth x)
        <&> \y -> x { _zBandwidth = y }
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
lqLocationCode f x =
    f (_lqLocationCode x)
        <&> \y -> x { _lqLocationCode = y }
{-# INLINE lqLocationCode #-}

-- | The name of the AWS Direct Connect location. The name includes the
-- colocation partner name and the physical site of the lit building.
lqLocationName :: Lens' Location (Maybe Text)
lqLocationName f x =
    f (_lqLocationName x)
        <&> \y -> x { _lqLocationName = y }
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
npviVirtualInterfaceName f x =
    f (_npviVirtualInterfaceName x)
        <&> \y -> x { _npviVirtualInterfaceName = y }
{-# INLINE npviVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npviVlan :: Lens' NewPrivateVirtualInterface (Integer)
npviVlan f x =
    f (_npviVlan x)
        <&> \y -> x { _npviVlan = y }
{-# INLINE npviVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npviAsn :: Lens' NewPrivateVirtualInterface (Integer)
npviAsn f x =
    f (_npviAsn x)
        <&> \y -> x { _npviAsn = y }
{-# INLINE npviAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npviAuthKey :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviAuthKey f x =
    f (_npviAuthKey x)
        <&> \y -> x { _npviAuthKey = y }
{-# INLINE npviAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npviAmazonAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviAmazonAddress f x =
    f (_npviAmazonAddress x)
        <&> \y -> x { _npviAmazonAddress = y }
{-# INLINE npviAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npviCustomerAddress :: Lens' NewPrivateVirtualInterface (Maybe Text)
npviCustomerAddress f x =
    f (_npviCustomerAddress x)
        <&> \y -> x { _npviCustomerAddress = y }
{-# INLINE npviCustomerAddress #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
npviVirtualGatewayId :: Lens' NewPrivateVirtualInterface (Text)
npviVirtualGatewayId f x =
    f (_npviVirtualGatewayId x)
        <&> \y -> x { _npviVirtualGatewayId = y }
{-# INLINE npviVirtualGatewayId #-}

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
npviaVirtualInterfaceName f x =
    f (_npviaVirtualInterfaceName x)
        <&> \y -> x { _npviaVirtualInterfaceName = y }
{-# INLINE npviaVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npviaVlan :: Lens' NewPrivateVirtualInterfaceAllocation (Integer)
npviaVlan f x =
    f (_npviaVlan x)
        <&> \y -> x { _npviaVlan = y }
{-# INLINE npviaVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npviaAsn :: Lens' NewPrivateVirtualInterfaceAllocation (Integer)
npviaAsn f x =
    f (_npviaAsn x)
        <&> \y -> x { _npviaAsn = y }
{-# INLINE npviaAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npviaAuthKey :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAuthKey f x =
    f (_npviaAuthKey x)
        <&> \y -> x { _npviaAuthKey = y }
{-# INLINE npviaAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npviaAmazonAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaAmazonAddress f x =
    f (_npviaAmazonAddress x)
        <&> \y -> x { _npviaAmazonAddress = y }
{-# INLINE npviaAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npviaCustomerAddress :: Lens' NewPrivateVirtualInterfaceAllocation (Maybe Text)
npviaCustomerAddress f x =
    f (_npviaCustomerAddress x)
        <&> \y -> x { _npviaCustomerAddress = y }
{-# INLINE npviaCustomerAddress #-}

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
npvjVirtualInterfaceName f x =
    f (_npvjVirtualInterfaceName x)
        <&> \y -> x { _npvjVirtualInterfaceName = y }
{-# INLINE npvjVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npvjVlan :: Lens' NewPublicVirtualInterface (Integer)
npvjVlan f x =
    f (_npvjVlan x)
        <&> \y -> x { _npvjVlan = y }
{-# INLINE npvjVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npvjAsn :: Lens' NewPublicVirtualInterface (Integer)
npvjAsn f x =
    f (_npvjAsn x)
        <&> \y -> x { _npvjAsn = y }
{-# INLINE npvjAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npvjAuthKey :: Lens' NewPublicVirtualInterface (Maybe Text)
npvjAuthKey f x =
    f (_npvjAuthKey x)
        <&> \y -> x { _npvjAuthKey = y }
{-# INLINE npvjAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npvjAmazonAddress :: Lens' NewPublicVirtualInterface (Text)
npvjAmazonAddress f x =
    f (_npvjAmazonAddress x)
        <&> \y -> x { _npvjAmazonAddress = y }
{-# INLINE npvjAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npvjCustomerAddress :: Lens' NewPublicVirtualInterface (Text)
npvjCustomerAddress f x =
    f (_npvjCustomerAddress x)
        <&> \y -> x { _npvjCustomerAddress = y }
{-# INLINE npvjCustomerAddress #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
npvjRouteFilterPrefixes :: Lens' NewPublicVirtualInterface ([RouteFilterPrefix])
npvjRouteFilterPrefixes f x =
    f (_npvjRouteFilterPrefixes x)
        <&> \y -> x { _npvjRouteFilterPrefixes = y }
{-# INLINE npvjRouteFilterPrefixes #-}

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
npvibVirtualInterfaceName f x =
    f (_npvibVirtualInterfaceName x)
        <&> \y -> x { _npvibVirtualInterfaceName = y }
{-# INLINE npvibVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
npvibVlan :: Lens' NewPublicVirtualInterfaceAllocation (Integer)
npvibVlan f x =
    f (_npvibVlan x)
        <&> \y -> x { _npvibVlan = y }
{-# INLINE npvibVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
npvibAsn :: Lens' NewPublicVirtualInterfaceAllocation (Integer)
npvibAsn f x =
    f (_npvibAsn x)
        <&> \y -> x { _npvibAsn = y }
{-# INLINE npvibAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
npvibAuthKey :: Lens' NewPublicVirtualInterfaceAllocation (Maybe Text)
npvibAuthKey f x =
    f (_npvibAuthKey x)
        <&> \y -> x { _npvibAuthKey = y }
{-# INLINE npvibAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
npvibAmazonAddress :: Lens' NewPublicVirtualInterfaceAllocation (Text)
npvibAmazonAddress f x =
    f (_npvibAmazonAddress x)
        <&> \y -> x { _npvibAmazonAddress = y }
{-# INLINE npvibAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
npvibCustomerAddress :: Lens' NewPublicVirtualInterfaceAllocation (Text)
npvibCustomerAddress f x =
    f (_npvibCustomerAddress x)
        <&> \y -> x { _npvibCustomerAddress = y }
{-# INLINE npvibCustomerAddress #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
npvibRouteFilterPrefixes :: Lens' NewPublicVirtualInterfaceAllocation ([RouteFilterPrefix])
npvibRouteFilterPrefixes f x =
    f (_npvibRouteFilterPrefixes x)
        <&> \y -> x { _npvibRouteFilterPrefixes = y }
{-# INLINE npvibRouteFilterPrefixes #-}

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
vhVirtualGatewayId f x =
    f (_vhVirtualGatewayId x)
        <&> \y -> x { _vhVirtualGatewayId = y }
{-# INLINE vhVirtualGatewayId #-}

-- | State of the virtual private gateway. Pending: This is the initial state
-- after calling CreateVpnGateway. Available: Ready for use by a private
-- virtual interface. Deleting: This is the initial state after calling
-- DeleteVpnGateway. Deleted: In this state, a private virtual interface is
-- unable to send traffic over this gateway.
vhVirtualGatewayState :: Lens' VirtualGateway (Maybe Text)
vhVirtualGatewayState f x =
    f (_vhVirtualGatewayState x)
        <&> \y -> x { _vhVirtualGatewayState = y }
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
vnOwnerAccount f x =
    f (_vnOwnerAccount x)
        <&> \y -> x { _vnOwnerAccount = y }
{-# INLINE vnOwnerAccount #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
vnVirtualInterfaceId :: Lens' VirtualInterface (Maybe Text)
vnVirtualInterfaceId f x =
    f (_vnVirtualInterfaceId x)
        <&> \y -> x { _vnVirtualInterfaceId = y }
{-# INLINE vnVirtualInterfaceId #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
vnLocation :: Lens' VirtualInterface (Maybe Text)
vnLocation f x =
    f (_vnLocation x)
        <&> \y -> x { _vnLocation = y }
{-# INLINE vnLocation #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
vnConnectionId :: Lens' VirtualInterface (Maybe Text)
vnConnectionId f x =
    f (_vnConnectionId x)
        <&> \y -> x { _vnConnectionId = y }
{-# INLINE vnConnectionId #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
vnVirtualInterfaceType :: Lens' VirtualInterface (Maybe Text)
vnVirtualInterfaceType f x =
    f (_vnVirtualInterfaceType x)
        <&> \y -> x { _vnVirtualInterfaceType = y }
{-# INLINE vnVirtualInterfaceType #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
vnVirtualInterfaceName :: Lens' VirtualInterface (Maybe Text)
vnVirtualInterfaceName f x =
    f (_vnVirtualInterfaceName x)
        <&> \y -> x { _vnVirtualInterfaceName = y }
{-# INLINE vnVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
vnVlan :: Lens' VirtualInterface (Maybe Integer)
vnVlan f x =
    f (_vnVlan x)
        <&> \y -> x { _vnVlan = y }
{-# INLINE vnVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
vnAsn :: Lens' VirtualInterface (Maybe Integer)
vnAsn f x =
    f (_vnAsn x)
        <&> \y -> x { _vnAsn = y }
{-# INLINE vnAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
vnAuthKey :: Lens' VirtualInterface (Maybe Text)
vnAuthKey f x =
    f (_vnAuthKey x)
        <&> \y -> x { _vnAuthKey = y }
{-# INLINE vnAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
vnAmazonAddress :: Lens' VirtualInterface (Maybe Text)
vnAmazonAddress f x =
    f (_vnAmazonAddress x)
        <&> \y -> x { _vnAmazonAddress = y }
{-# INLINE vnAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
vnCustomerAddress :: Lens' VirtualInterface (Maybe Text)
vnCustomerAddress f x =
    f (_vnCustomerAddress x)
        <&> \y -> x { _vnCustomerAddress = y }
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
vnVirtualInterfaceState f x =
    f (_vnVirtualInterfaceState x)
        <&> \y -> x { _vnVirtualInterfaceState = y }
{-# INLINE vnVirtualInterfaceState #-}

-- | Information for generating the customer router configuration.
vnCustomerRouterConfig :: Lens' VirtualInterface (Maybe Text)
vnCustomerRouterConfig f x =
    f (_vnCustomerRouterConfig x)
        <&> \y -> x { _vnCustomerRouterConfig = y }
{-# INLINE vnCustomerRouterConfig #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vnVirtualGatewayId :: Lens' VirtualInterface (Maybe Text)
vnVirtualGatewayId f x =
    f (_vnVirtualGatewayId x)
        <&> \y -> x { _vnVirtualGatewayId = y }
{-# INLINE vnVirtualGatewayId #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
vnRouteFilterPrefixes :: Lens' VirtualInterface ([RouteFilterPrefix])
vnRouteFilterPrefixes f x =
    f (_vnRouteFilterPrefixes x)
        <&> \y -> x { _vnRouteFilterPrefixes = y }
{-# INLINE vnRouteFilterPrefixes #-}

instance FromJSON VirtualInterface

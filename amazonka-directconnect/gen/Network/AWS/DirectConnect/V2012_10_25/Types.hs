{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.DirectConnect.V2012_10_25.Types where

import Control.Lens.TH (makeLenses)
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

instance FromJSON RouteFilterPrefix

instance ToJSON RouteFilterPrefix

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data Connection = Connection
    { _kVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _kLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _kConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _kPartnerName :: Maybe Text
    , _kConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    , _kBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _kOwnerAccount :: Maybe Text
    , _kRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _kConnectionState :: Maybe ConnectionState
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
    } deriving (Show, Generic)

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
    { _jInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    , _jLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _jInterconnectName :: Maybe Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS".
    , _jBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _jInterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an
      -- interconnect. The interconnect stays in the requested state until
      -- the Letter of Authorization (LOA) is sent to the customer.
      -- Pending: The interconnect has been approved, and is being
      -- initialized. Available: The network link is up, and the
      -- interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    , _jRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    } deriving (Show, Generic)

instance FromJSON Interconnect

-- | An AWS Direct Connect location where connections and interconnects can be
-- requested.
data Location = Location
    { _qLocationName :: Maybe Text
      -- ^ The name of the AWS Direct Connect location. The name includes
      -- the colocation partner name and the physical site of the lit
      -- building.
    , _qLocationCode :: Maybe Text
      -- ^ The code used to indicate the AWS Direct Connect location.
    } deriving (Show, Generic)

instance FromJSON Location

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface
    { _npvjVirtualGatewayId :: Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _npvjCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _npvjVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npvjAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npvjAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npvjAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npvjVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    } deriving (Show, Generic)

instance ToJSON NewPrivateVirtualInterface

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation
    { _npviaCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _npviaVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npviaAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npviaAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npviaAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npviaVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    } deriving (Show, Generic)

instance ToJSON NewPrivateVirtualInterfaceAllocation

-- | Detailed information for the public virtual interface to be created.
-- Default: None.
data NewPublicVirtualInterface = NewPublicVirtualInterface
    { _npviRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    , _npviCustomerAddress :: Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _npviVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npviAmazonAddress :: Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npviAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npviAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npviVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    } deriving (Show, Generic)

instance ToJSON NewPublicVirtualInterface

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation
    { _npvibRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    , _npvibCustomerAddress :: Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _npvibVlan :: Integer
      -- ^ The VLAN ID. Example: 101.
    , _npvibAmazonAddress :: Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _npvibAsn :: Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _npvibAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _npvibVirtualInterfaceName :: Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    } deriving (Show, Generic)

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

instance FromJSON VirtualGateway

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data VirtualInterface = VirtualInterface
    { _vlVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vlRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    , _vlCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vlVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vlLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vlAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vlVirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , _vlConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vlVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    , _vlAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vlAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vlCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vlOwnerAccount :: Maybe Text
    , _vlVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _vlVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    } deriving (Show, Generic)

instance FromJSON VirtualInterface

-- Newtypes
makeLenses ''RouteFilterPrefix

-- Products
makeLenses ''Connection
makeLenses ''Interconnect
makeLenses ''Location
makeLenses ''NewPrivateVirtualInterface
makeLenses ''NewPrivateVirtualInterfaceAllocation
makeLenses ''NewPublicVirtualInterface
makeLenses ''NewPublicVirtualInterfaceAllocation
makeLenses ''VirtualGateway
makeLenses ''VirtualInterface

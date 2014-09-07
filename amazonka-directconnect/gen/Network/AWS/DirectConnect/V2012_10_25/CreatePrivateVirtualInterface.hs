{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new private virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A private virtual interface
-- supports sending traffic to a single virtual private cloud (VPC).
module Network.AWS.DirectConnect.V2012_10_25.CreatePrivateVirtualInterface
    (
    -- * Request
      CreatePrivateVirtualInterface
    -- ** Request constructor
    , mkCreatePrivateVirtualInterface
    -- ** Request lenses
    , cpvi2ConnectionId
    , cpvi2NewPrivateVirtualInterface

    -- * Response
    , CreatePrivateVirtualInterfaceResponse
    -- ** Response lenses
    , cpvirs1OwnerAccount
    , cpvirs1VirtualInterfaceId
    , cpvirs1Location
    , cpvirs1ConnectionId
    , cpvirs1VirtualInterfaceType
    , cpvirs1VirtualInterfaceName
    , cpvirs1Vlan
    , cpvirs1Asn
    , cpvirs1AuthKey
    , cpvirs1AmazonAddress
    , cpvirs1CustomerAddress
    , cpvirs1VirtualInterfaceState
    , cpvirs1CustomerRouterConfig
    , cpvirs1VirtualGatewayId
    , cpvirs1RouteFilterPrefixes
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Container for the parameters to the CreatePrivateVirtualInterface
-- operation.
data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface
    { _cpvi2ConnectionId :: Text
    , _cpvi2NewPrivateVirtualInterface :: NewPrivateVirtualInterface
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePrivateVirtualInterface' request.
mkCreatePrivateVirtualInterface :: Text -- ^ 'cpvi2ConnectionId'
                                -> NewPrivateVirtualInterface -- ^ 'cpvi2NewPrivateVirtualInterface'
                                -> CreatePrivateVirtualInterface
mkCreatePrivateVirtualInterface p1 p2 = CreatePrivateVirtualInterface
    { _cpvi2ConnectionId = p1
    , _cpvi2NewPrivateVirtualInterface = p2
    }

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvi2ConnectionId :: Lens' CreatePrivateVirtualInterface Text
cpvi2ConnectionId =
    lens _cpvi2ConnectionId (\s a -> s { _cpvi2ConnectionId = a })

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
cpvi2NewPrivateVirtualInterface :: Lens' CreatePrivateVirtualInterface NewPrivateVirtualInterface
cpvi2NewPrivateVirtualInterface =
    lens _cpvi2NewPrivateVirtualInterface
         (\s a -> s { _cpvi2NewPrivateVirtualInterface = a })

instance ToPath CreatePrivateVirtualInterface

instance ToQuery CreatePrivateVirtualInterface

instance ToHeaders CreatePrivateVirtualInterface

instance ToJSON CreatePrivateVirtualInterface

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data CreatePrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { _cpvirs1OwnerAccount :: Maybe Text
    , _cpvirs1VirtualInterfaceId :: Maybe Text
    , _cpvirs1Location :: Maybe Text
    , _cpvirs1ConnectionId :: Maybe Text
    , _cpvirs1VirtualInterfaceType :: Maybe Text
    , _cpvirs1VirtualInterfaceName :: Maybe Text
    , _cpvirs1Vlan :: Maybe Integer
    , _cpvirs1Asn :: Maybe Integer
    , _cpvirs1AuthKey :: Maybe Text
    , _cpvirs1AmazonAddress :: Maybe Text
    , _cpvirs1CustomerAddress :: Maybe Text
    , _cpvirs1VirtualInterfaceState :: Maybe VirtualInterfaceState
    , _cpvirs1CustomerRouterConfig :: Maybe Text
    , _cpvirs1VirtualGatewayId :: Maybe Text
    , _cpvirs1RouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

cpvirs1OwnerAccount :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1OwnerAccount =
    lens _cpvirs1OwnerAccount (\s a -> s { _cpvirs1OwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
cpvirs1VirtualInterfaceId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1VirtualInterfaceId =
    lens _cpvirs1VirtualInterfaceId
         (\s a -> s { _cpvirs1VirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
cpvirs1Location :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1Location = lens _cpvirs1Location (\s a -> s { _cpvirs1Location = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvirs1ConnectionId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1ConnectionId =
    lens _cpvirs1ConnectionId (\s a -> s { _cpvirs1ConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
cpvirs1VirtualInterfaceType :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1VirtualInterfaceType =
    lens _cpvirs1VirtualInterfaceType
         (\s a -> s { _cpvirs1VirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
cpvirs1VirtualInterfaceName :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1VirtualInterfaceName =
    lens _cpvirs1VirtualInterfaceName
         (\s a -> s { _cpvirs1VirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
cpvirs1Vlan :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Integer)
cpvirs1Vlan = lens _cpvirs1Vlan (\s a -> s { _cpvirs1Vlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
cpvirs1Asn :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Integer)
cpvirs1Asn = lens _cpvirs1Asn (\s a -> s { _cpvirs1Asn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
cpvirs1AuthKey :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1AuthKey = lens _cpvirs1AuthKey (\s a -> s { _cpvirs1AuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
cpvirs1AmazonAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1AmazonAddress =
    lens _cpvirs1AmazonAddress (\s a -> s { _cpvirs1AmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
cpvirs1CustomerAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1CustomerAddress =
    lens _cpvirs1CustomerAddress (\s a -> s { _cpvirs1CustomerAddress = a })

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
cpvirs1VirtualInterfaceState :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirs1VirtualInterfaceState =
    lens _cpvirs1VirtualInterfaceState
         (\s a -> s { _cpvirs1VirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
cpvirs1CustomerRouterConfig :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1CustomerRouterConfig =
    lens _cpvirs1CustomerRouterConfig
         (\s a -> s { _cpvirs1CustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
cpvirs1VirtualGatewayId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvirs1VirtualGatewayId =
    lens _cpvirs1VirtualGatewayId
         (\s a -> s { _cpvirs1VirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
cpvirs1RouteFilterPrefixes :: Lens' CreatePrivateVirtualInterfaceResponse [RouteFilterPrefix]
cpvirs1RouteFilterPrefixes =
    lens _cpvirs1RouteFilterPrefixes
         (\s a -> s { _cpvirs1RouteFilterPrefixes = a })

instance FromJSON CreatePrivateVirtualInterfaceResponse

instance AWSRequest CreatePrivateVirtualInterface where
    type Sv CreatePrivateVirtualInterface = DirectConnect
    type Rs CreatePrivateVirtualInterface = CreatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new public virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A public virtual interface
-- supports sending traffic to public services of AWS such as Amazon Simple
-- Storage Service (Amazon S3).
module Network.AWS.DirectConnect.V2012_10_25.CreatePublicVirtualInterface
    (
    -- * Request
      CreatePublicVirtualInterface
    -- ** Request constructor
    , mkCreatePublicVirtualInterface
    -- ** Request lenses
    , cpvi3ConnectionId
    , cpvi3NewPublicVirtualInterface

    -- * Response
    , CreatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , cpvirs1rsOwnerAccount
    , cpvirs1rsVirtualInterfaceId
    , cpvirs1rsLocation
    , cpvirs1rsConnectionId
    , cpvirs1rsVirtualInterfaceType
    , cpvirs1rsVirtualInterfaceName
    , cpvirs1rsVlan
    , cpvirs1rsAsn
    , cpvirs1rsAuthKey
    , cpvirs1rsAmazonAddress
    , cpvirs1rsCustomerAddress
    , cpvirs1rsVirtualInterfaceState
    , cpvirs1rsCustomerRouterConfig
    , cpvirs1rsVirtualGatewayId
    , cpvirs1rsRouteFilterPrefixes
    ) where

import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the CreatePublicVirtualInterface operation.
data CreatePublicVirtualInterface = CreatePublicVirtualInterface
    { _cpvi3ConnectionId :: Text
    , _cpvi3NewPublicVirtualInterface :: NewPublicVirtualInterface
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePublicVirtualInterface' request.
mkCreatePublicVirtualInterface :: Text -- ^ 'cpvi3ConnectionId'
                               -> NewPublicVirtualInterface -- ^ 'cpvi3NewPublicVirtualInterface'
                               -> CreatePublicVirtualInterface
mkCreatePublicVirtualInterface p1 p2 = CreatePublicVirtualInterface
    { _cpvi3ConnectionId = p1
    , _cpvi3NewPublicVirtualInterface = p2
    }

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvi3ConnectionId :: Lens' CreatePublicVirtualInterface Text
cpvi3ConnectionId =
    lens _cpvi3ConnectionId (\s a -> s { _cpvi3ConnectionId = a })

-- | Detailed information for the public virtual interface to be created.
-- Default: None.
cpvi3NewPublicVirtualInterface :: Lens' CreatePublicVirtualInterface NewPublicVirtualInterface
cpvi3NewPublicVirtualInterface =
    lens _cpvi3NewPublicVirtualInterface
         (\s a -> s { _cpvi3NewPublicVirtualInterface = a })

instance ToPath CreatePublicVirtualInterface

instance ToQuery CreatePublicVirtualInterface

instance ToHeaders CreatePublicVirtualInterface

instance ToJSON CreatePublicVirtualInterface

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data CreatePublicVirtualInterfaceResponse = CreatePublicVirtualInterfaceResponse
    { _cpvirs1rsOwnerAccount :: Maybe Text
    , _cpvirs1rsVirtualInterfaceId :: Maybe Text
    , _cpvirs1rsLocation :: Maybe Text
    , _cpvirs1rsConnectionId :: Maybe Text
    , _cpvirs1rsVirtualInterfaceType :: Maybe Text
    , _cpvirs1rsVirtualInterfaceName :: Maybe Text
    , _cpvirs1rsVlan :: Maybe Integer
    , _cpvirs1rsAsn :: Maybe Integer
    , _cpvirs1rsAuthKey :: Maybe Text
    , _cpvirs1rsAmazonAddress :: Maybe Text
    , _cpvirs1rsCustomerAddress :: Maybe Text
    , _cpvirs1rsVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _cpvirs1rsCustomerRouterConfig :: Maybe Text
    , _cpvirs1rsVirtualGatewayId :: Maybe Text
    , _cpvirs1rsRouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

cpvirs1rsOwnerAccount :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsOwnerAccount =
    lens _cpvirs1rsOwnerAccount (\s a -> s { _cpvirs1rsOwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
cpvirs1rsVirtualInterfaceId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsVirtualInterfaceId =
    lens _cpvirs1rsVirtualInterfaceId
         (\s a -> s { _cpvirs1rsVirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
cpvirs1rsLocation :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsLocation =
    lens _cpvirs1rsLocation (\s a -> s { _cpvirs1rsLocation = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvirs1rsConnectionId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsConnectionId =
    lens _cpvirs1rsConnectionId (\s a -> s { _cpvirs1rsConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
cpvirs1rsVirtualInterfaceType :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsVirtualInterfaceType =
    lens _cpvirs1rsVirtualInterfaceType
         (\s a -> s { _cpvirs1rsVirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
cpvirs1rsVirtualInterfaceName :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsVirtualInterfaceName =
    lens _cpvirs1rsVirtualInterfaceName
         (\s a -> s { _cpvirs1rsVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
cpvirs1rsVlan :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
cpvirs1rsVlan = lens _cpvirs1rsVlan (\s a -> s { _cpvirs1rsVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
cpvirs1rsAsn :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
cpvirs1rsAsn = lens _cpvirs1rsAsn (\s a -> s { _cpvirs1rsAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
cpvirs1rsAuthKey :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsAuthKey =
    lens _cpvirs1rsAuthKey (\s a -> s { _cpvirs1rsAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
cpvirs1rsAmazonAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsAmazonAddress =
    lens _cpvirs1rsAmazonAddress (\s a -> s { _cpvirs1rsAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
cpvirs1rsCustomerAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsCustomerAddress =
    lens _cpvirs1rsCustomerAddress
         (\s a -> s { _cpvirs1rsCustomerAddress = a })

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
cpvirs1rsVirtualInterfaceState :: Lens' CreatePublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirs1rsVirtualInterfaceState =
    lens _cpvirs1rsVirtualInterfaceState
         (\s a -> s { _cpvirs1rsVirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
cpvirs1rsCustomerRouterConfig :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsCustomerRouterConfig =
    lens _cpvirs1rsCustomerRouterConfig
         (\s a -> s { _cpvirs1rsCustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
cpvirs1rsVirtualGatewayId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvirs1rsVirtualGatewayId =
    lens _cpvirs1rsVirtualGatewayId
         (\s a -> s { _cpvirs1rsVirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
cpvirs1rsRouteFilterPrefixes :: Lens' CreatePublicVirtualInterfaceResponse [RouteFilterPrefix]
cpvirs1rsRouteFilterPrefixes =
    lens _cpvirs1rsRouteFilterPrefixes
         (\s a -> s { _cpvirs1rsRouteFilterPrefixes = a })

instance FromJSON CreatePublicVirtualInterfaceResponse

instance AWSRequest CreatePublicVirtualInterface where
    type Sv CreatePublicVirtualInterface = DirectConnect
    type Rs CreatePublicVirtualInterface = CreatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

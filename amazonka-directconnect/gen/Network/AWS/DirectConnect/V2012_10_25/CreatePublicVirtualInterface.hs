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
    -- ** Response constructor
    , mkCreatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , cpvir1rOwnerAccount
    , cpvir1rVirtualInterfaceId
    , cpvir1rLocation
    , cpvir1rConnectionId
    , cpvir1rVirtualInterfaceType
    , cpvir1rVirtualInterfaceName
    , cpvir1rVlan
    , cpvir1rAsn
    , cpvir1rAuthKey
    , cpvir1rAmazonAddress
    , cpvir1rCustomerAddress
    , cpvir1rVirtualInterfaceState
    , cpvir1rCustomerRouterConfig
    , cpvir1rVirtualGatewayId
    , cpvir1rRouteFilterPrefixes
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
    { _cpvir1rOwnerAccount :: Maybe Text
    , _cpvir1rVirtualInterfaceId :: Maybe Text
    , _cpvir1rLocation :: Maybe Text
    , _cpvir1rConnectionId :: Maybe Text
    , _cpvir1rVirtualInterfaceType :: Maybe Text
    , _cpvir1rVirtualInterfaceName :: Maybe Text
    , _cpvir1rVlan :: Maybe Integer
    , _cpvir1rAsn :: Maybe Integer
    , _cpvir1rAuthKey :: Maybe Text
    , _cpvir1rAmazonAddress :: Maybe Text
    , _cpvir1rCustomerAddress :: Maybe Text
    , _cpvir1rVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _cpvir1rCustomerRouterConfig :: Maybe Text
    , _cpvir1rVirtualGatewayId :: Maybe Text
    , _cpvir1rRouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePublicVirtualInterfaceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreatePublicVirtualInterfaceResponse :: CreatePublicVirtualInterfaceResponse
mkCreatePublicVirtualInterfaceResponse = CreatePublicVirtualInterfaceResponse
    { _cpvir1rOwnerAccount = Nothing
    , _cpvir1rVirtualInterfaceId = Nothing
    , _cpvir1rLocation = Nothing
    , _cpvir1rConnectionId = Nothing
    , _cpvir1rVirtualInterfaceType = Nothing
    , _cpvir1rVirtualInterfaceName = Nothing
    , _cpvir1rVlan = Nothing
    , _cpvir1rAsn = Nothing
    , _cpvir1rAuthKey = Nothing
    , _cpvir1rAmazonAddress = Nothing
    , _cpvir1rCustomerAddress = Nothing
    , _cpvir1rVirtualInterfaceState = Nothing
    , _cpvir1rCustomerRouterConfig = Nothing
    , _cpvir1rVirtualGatewayId = Nothing
    , _cpvir1rRouteFilterPrefixes = mempty
    }

cpvir1rOwnerAccount :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rOwnerAccount =
    lens _cpvir1rOwnerAccount (\s a -> s { _cpvir1rOwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
cpvir1rVirtualInterfaceId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rVirtualInterfaceId =
    lens _cpvir1rVirtualInterfaceId
         (\s a -> s { _cpvir1rVirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
cpvir1rLocation :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rLocation = lens _cpvir1rLocation (\s a -> s { _cpvir1rLocation = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvir1rConnectionId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rConnectionId =
    lens _cpvir1rConnectionId (\s a -> s { _cpvir1rConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
cpvir1rVirtualInterfaceType :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rVirtualInterfaceType =
    lens _cpvir1rVirtualInterfaceType
         (\s a -> s { _cpvir1rVirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
cpvir1rVirtualInterfaceName :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rVirtualInterfaceName =
    lens _cpvir1rVirtualInterfaceName
         (\s a -> s { _cpvir1rVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
cpvir1rVlan :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
cpvir1rVlan = lens _cpvir1rVlan (\s a -> s { _cpvir1rVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
cpvir1rAsn :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
cpvir1rAsn = lens _cpvir1rAsn (\s a -> s { _cpvir1rAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
cpvir1rAuthKey :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rAuthKey = lens _cpvir1rAuthKey (\s a -> s { _cpvir1rAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
cpvir1rAmazonAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rAmazonAddress =
    lens _cpvir1rAmazonAddress (\s a -> s { _cpvir1rAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
cpvir1rCustomerAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rCustomerAddress =
    lens _cpvir1rCustomerAddress (\s a -> s { _cpvir1rCustomerAddress = a })

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
cpvir1rVirtualInterfaceState :: Lens' CreatePublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvir1rVirtualInterfaceState =
    lens _cpvir1rVirtualInterfaceState
         (\s a -> s { _cpvir1rVirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
cpvir1rCustomerRouterConfig :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rCustomerRouterConfig =
    lens _cpvir1rCustomerRouterConfig
         (\s a -> s { _cpvir1rCustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
cpvir1rVirtualGatewayId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
cpvir1rVirtualGatewayId =
    lens _cpvir1rVirtualGatewayId
         (\s a -> s { _cpvir1rVirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
cpvir1rRouteFilterPrefixes :: Lens' CreatePublicVirtualInterfaceResponse [RouteFilterPrefix]
cpvir1rRouteFilterPrefixes =
    lens _cpvir1rRouteFilterPrefixes
         (\s a -> s { _cpvir1rRouteFilterPrefixes = a })

instance FromJSON CreatePublicVirtualInterfaceResponse

instance AWSRequest CreatePublicVirtualInterface where
    type Sv CreatePublicVirtualInterface = DirectConnect
    type Rs CreatePublicVirtualInterface = CreatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

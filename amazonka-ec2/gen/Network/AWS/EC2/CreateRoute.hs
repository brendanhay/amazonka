{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a route in a route table within a VPC. You must specify one of the
-- following targets: Internet gateway or virtual private gateway, NAT
-- instance, VPC peering connection, or network interface. When determining
-- how to route traffic, we use the route with the most specific match. For
-- example, let's say the traffic is destined for 192.0.2.3, and the route
-- table includes the following two routes: 192.0.2.0/24 (goes to some target
-- A) 192.0.2.0/28 (goes to some target B) Both routes apply to the traffic
-- destined for 192.0.2.3. However, the second route in the list covers a
-- smaller number of IP addresses and is therefore more specific, so we use
-- that route to determine where to target the traffic. For more information
-- about route tables, see Route Tables in the Amazon Virtual Private Cloud
-- User Guide.
module Network.AWS.EC2.CreateRoute
    (
    -- * Request
      CreateRoute
    -- ** Request constructor
    , createRoute
    -- ** Request lenses
    , crDestinationCidrBlock
    , crDryRun
    , crGatewayId
    , crInstanceId
    , crNetworkInterfaceId
    , crRouteTableId
    , crVpcPeeringConnectionId

    -- * Response
    , CreateRouteResponse
    -- ** Response constructor
    , createRouteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateRoute = CreateRoute
    { _crDestinationCidrBlock   :: Text
    , _crDryRun                 :: Maybe Bool
    , _crGatewayId              :: Maybe Text
    , _crInstanceId             :: Maybe Text
    , _crNetworkInterfaceId     :: Maybe Text
    , _crRouteTableId           :: Text
    , _crVpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crDestinationCidrBlock' @::@ 'Text'
--
-- * 'crDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'crGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'crInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'crNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'crRouteTableId' @::@ 'Text'
--
-- * 'crVpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
createRoute :: Text -- ^ 'crRouteTableId'
            -> Text -- ^ 'crDestinationCidrBlock'
            -> CreateRoute
createRoute p1 p2 = CreateRoute
    { _crRouteTableId           = p1
    , _crDestinationCidrBlock   = p2
    , _crDryRun                 = Nothing
    , _crGatewayId              = Nothing
    , _crInstanceId             = Nothing
    , _crNetworkInterfaceId     = Nothing
    , _crVpcPeeringConnectionId = Nothing
    }

-- | The CIDR address block used for the destination match. Routing decisions
-- are based on the most specific match.
crDestinationCidrBlock :: Lens' CreateRoute Text
crDestinationCidrBlock =
    lens _crDestinationCidrBlock (\s a -> s { _crDestinationCidrBlock = a })

crDryRun :: Lens' CreateRoute (Maybe Bool)
crDryRun = lens _crDryRun (\s a -> s { _crDryRun = a })

-- | The ID of an Internet gateway or virtual private gateway attached to your
-- VPC.
crGatewayId :: Lens' CreateRoute (Maybe Text)
crGatewayId = lens _crGatewayId (\s a -> s { _crGatewayId = a })

-- | The ID of a NAT instance in your VPC. The operation fails if you specify
-- an instance ID unless exactly one network interface is attached.
crInstanceId :: Lens' CreateRoute (Maybe Text)
crInstanceId = lens _crInstanceId (\s a -> s { _crInstanceId = a })

-- | The ID of a network interface.
crNetworkInterfaceId :: Lens' CreateRoute (Maybe Text)
crNetworkInterfaceId =
    lens _crNetworkInterfaceId (\s a -> s { _crNetworkInterfaceId = a })

-- | The ID of the route table for the route.
crRouteTableId :: Lens' CreateRoute Text
crRouteTableId = lens _crRouteTableId (\s a -> s { _crRouteTableId = a })

-- | The ID of a VPC peering connection.
crVpcPeeringConnectionId :: Lens' CreateRoute (Maybe Text)
crVpcPeeringConnectionId =
    lens _crVpcPeeringConnectionId
        (\s a -> s { _crVpcPeeringConnectionId = a })

data CreateRouteResponse = CreateRouteResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateRouteResponse' constructor.
createRouteResponse :: CreateRouteResponse
createRouteResponse = CreateRouteResponse

instance AWSRequest CreateRoute where
    type Sv CreateRoute = EC2
    type Rs CreateRoute = CreateRouteResponse

    request  = post "CreateRoute"
    response = nullResponse CreateRouteResponse

instance ToPath CreateRoute where
    toPath = const "/"

instance ToHeaders CreateRoute

instance ToQuery CreateRoute

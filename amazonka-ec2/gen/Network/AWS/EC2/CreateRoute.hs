{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a route in a route table within a VPC.
--
-- You must specify one of the following targets: Internet gateway or virtual
-- private gateway, NAT instance, VPC peering connection, or network interface.
--
-- When determining how to route traffic, we use the route with the most
-- specific match. For example, let's say the traffic is destined for '192.0.2.3',
-- and the route table includes the following two routes:
--
-- '192.0.2.0/24' (goes to some target A)
--
-- '192.0.2.0/28' (goes to some target B)
--
-- Both routes apply to the traffic destined for '192.0.2.3'. However, the
-- second route in the list covers a smaller number of IP addresses and is
-- therefore more specific, so we use that route to determine where to target
-- the traffic.
--
-- For more information about route tables, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables> in the /AmazonVirtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRoute.html>
module Network.AWS.EC2.CreateRoute
    (
    -- * Request
      CreateRoute
    -- ** Request constructor
    , createRoute
    -- ** Request lenses
    , crClientToken
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
    -- ** Response lenses
    , crrClientToken
    , crrReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateRoute = CreateRoute
    { _crClientToken            :: Maybe Text
    , _crDestinationCidrBlock   :: Text
    , _crDryRun                 :: Maybe Bool
    , _crGatewayId              :: Maybe Text
    , _crInstanceId             :: Maybe Text
    , _crNetworkInterfaceId     :: Maybe Text
    , _crRouteTableId           :: Text
    , _crVpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crClientToken' @::@ 'Maybe' 'Text'
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
    , _crClientToken            = Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
crClientToken :: Lens' CreateRoute (Maybe Text)
crClientToken = lens _crClientToken (\s a -> s { _crClientToken = a })

-- | The CIDR address block used for the destination match. Routing decisions are
-- based on the most specific match.
crDestinationCidrBlock :: Lens' CreateRoute Text
crDestinationCidrBlock =
    lens _crDestinationCidrBlock (\s a -> s { _crDestinationCidrBlock = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
crDryRun :: Lens' CreateRoute (Maybe Bool)
crDryRun = lens _crDryRun (\s a -> s { _crDryRun = a })

-- | The ID of an Internet gateway or virtual private gateway attached to your VPC.
crGatewayId :: Lens' CreateRoute (Maybe Text)
crGatewayId = lens _crGatewayId (\s a -> s { _crGatewayId = a })

-- | The ID of a NAT instance in your VPC. The operation fails if you specify an
-- instance ID unless exactly one network interface is attached.
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
    { _crrClientToken :: Maybe Text
    , _crrReturn      :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateRouteResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrClientToken' @::@ 'Maybe' 'Text'
--
-- * 'crrReturn' @::@ 'Maybe' 'Bool'
--
createRouteResponse :: CreateRouteResponse
createRouteResponse = CreateRouteResponse
    { _crrReturn      = Nothing
    , _crrClientToken = Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request.
crrClientToken :: Lens' CreateRouteResponse (Maybe Text)
crrClientToken = lens _crrClientToken (\s a -> s { _crrClientToken = a })

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
crrReturn :: Lens' CreateRouteResponse (Maybe Bool)
crrReturn = lens _crrReturn (\s a -> s { _crrReturn = a })

instance ToPath CreateRoute where
    toPath = const "/"

instance ToQuery CreateRoute where
    toQuery CreateRoute{..} = mconcat
        [ "ClientToken"            =? _crClientToken
        , "DestinationCidrBlock"   =? _crDestinationCidrBlock
        , "DryRun"                 =? _crDryRun
        , "GatewayId"              =? _crGatewayId
        , "InstanceId"             =? _crInstanceId
        , "NetworkInterfaceId"     =? _crNetworkInterfaceId
        , "RouteTableId"           =? _crRouteTableId
        , "VpcPeeringConnectionId" =? _crVpcPeeringConnectionId
        ]

instance ToHeaders CreateRoute

instance AWSRequest CreateRoute where
    type Sv CreateRoute = EC2
    type Rs CreateRoute = CreateRouteResponse

    request  = post "CreateRoute"
    response = xmlResponse

instance FromXML CreateRouteResponse where
    parseXML x = CreateRouteResponse
        <$> x .@? "clientToken"
        <*> x .@? "return"

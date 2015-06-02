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

-- Module      : Network.AWS.EC2.ReplaceRoute
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

-- | Replaces an existing route within a route table in a VPC. You must provide
-- only one of the following: Internet gateway or virtual private gateway, NAT
-- instance, VPC peering connection, or network interface.
--
-- For more information about route tables, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables> in the /AmazonVirtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRoute.html>
module Network.AWS.EC2.ReplaceRoute
    (
    -- * Request
      ReplaceRoute
    -- ** Request constructor
    , replaceRoute
    -- ** Request lenses
    , rrDestinationCidrBlock
    , rrDryRun
    , rrGatewayId
    , rrInstanceId
    , rrNetworkInterfaceId
    , rrRouteTableId
    , rrVpcPeeringConnectionId

    -- * Response
    , ReplaceRouteResponse
    -- ** Response constructor
    , replaceRouteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ReplaceRoute = ReplaceRoute
    { _rrDestinationCidrBlock   :: Text
    , _rrDryRun                 :: Maybe Bool
    , _rrGatewayId              :: Maybe Text
    , _rrInstanceId             :: Maybe Text
    , _rrNetworkInterfaceId     :: Maybe Text
    , _rrRouteTableId           :: Text
    , _rrVpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ReplaceRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrDestinationCidrBlock' @::@ 'Text'
--
-- * 'rrDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rrGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'rrInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'rrNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'rrRouteTableId' @::@ 'Text'
--
-- * 'rrVpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
replaceRoute :: Text -- ^ 'rrRouteTableId'
             -> Text -- ^ 'rrDestinationCidrBlock'
             -> ReplaceRoute
replaceRoute p1 p2 = ReplaceRoute
    { _rrRouteTableId           = p1
    , _rrDestinationCidrBlock   = p2
    , _rrDryRun                 = Nothing
    , _rrGatewayId              = Nothing
    , _rrInstanceId             = Nothing
    , _rrNetworkInterfaceId     = Nothing
    , _rrVpcPeeringConnectionId = Nothing
    }

-- | The CIDR address block used for the destination match. The value you provide
-- must match the CIDR of an existing route in the table.
rrDestinationCidrBlock :: Lens' ReplaceRoute Text
rrDestinationCidrBlock =
    lens _rrDestinationCidrBlock (\s a -> s { _rrDestinationCidrBlock = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
rrDryRun :: Lens' ReplaceRoute (Maybe Bool)
rrDryRun = lens _rrDryRun (\s a -> s { _rrDryRun = a })

-- | The ID of an Internet gateway or virtual private gateway.
rrGatewayId :: Lens' ReplaceRoute (Maybe Text)
rrGatewayId = lens _rrGatewayId (\s a -> s { _rrGatewayId = a })

-- | The ID of a NAT instance in your VPC.
rrInstanceId :: Lens' ReplaceRoute (Maybe Text)
rrInstanceId = lens _rrInstanceId (\s a -> s { _rrInstanceId = a })

-- | The ID of a network interface.
rrNetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)
rrNetworkInterfaceId =
    lens _rrNetworkInterfaceId (\s a -> s { _rrNetworkInterfaceId = a })

-- | The ID of the route table.
rrRouteTableId :: Lens' ReplaceRoute Text
rrRouteTableId = lens _rrRouteTableId (\s a -> s { _rrRouteTableId = a })

-- | The ID of a VPC peering connection.
rrVpcPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)
rrVpcPeeringConnectionId =
    lens _rrVpcPeeringConnectionId
        (\s a -> s { _rrVpcPeeringConnectionId = a })

data ReplaceRouteResponse = ReplaceRouteResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ReplaceRouteResponse' constructor.
replaceRouteResponse :: ReplaceRouteResponse
replaceRouteResponse = ReplaceRouteResponse

instance ToPath ReplaceRoute where
    toPath = const "/"

instance ToQuery ReplaceRoute where
    toQuery ReplaceRoute{..} = mconcat
        [ "DestinationCidrBlock"   =? _rrDestinationCidrBlock
        , "DryRun"                 =? _rrDryRun
        , "GatewayId"              =? _rrGatewayId
        , "InstanceId"             =? _rrInstanceId
        , "NetworkInterfaceId"     =? _rrNetworkInterfaceId
        , "RouteTableId"           =? _rrRouteTableId
        , "VpcPeeringConnectionId" =? _rrVpcPeeringConnectionId
        ]

instance ToHeaders ReplaceRoute

instance AWSRequest ReplaceRoute where
    type Sv ReplaceRoute = EC2
    type Rs ReplaceRoute = ReplaceRouteResponse

    request  = post "ReplaceRoute"
    response = nullResponse ReplaceRouteResponse

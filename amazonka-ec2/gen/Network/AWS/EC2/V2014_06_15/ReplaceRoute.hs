{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ReplaceRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces an existing route within a route table in a VPC. You must provide
-- only one of the following: Internet gateway, NAT instance, VPC peering
-- connection, or network interface. For more information about route tables,
-- see Route Tables in the Amazon Virtual Private Cloud User Guide. Example
-- This example replaces a route in the specified route table. The new route
-- matches the CIDR 10.0.0.0/8 and sends the traffic to the virtual private
-- gateway with the ID vgw-1d00376e.
-- https://ec2.amazonaws.com/?Action=ReplaceRoute
-- &amp;RouteTableId=rtb-e4ad488d &amp;DestinationCidrBlock=10.0.0.0/8
-- &amp;GatewayId=vgw-1d00376e &amp;AUTHPARAMS &lt;ReplaceRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ReplaceRouteResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ReplaceRoute
    (
    -- * Request
      ReplaceRoute
    -- ** Request constructor
    , mkReplaceRoute
    -- ** Request lenses
    , rrRouteTableId
    , rrDestinationCidrBlock
    , rrGatewayId
    , rrInstanceId
    , rrNetworkInterfaceId
    , rrVpcPeeringConnectionId

    -- * Response
    , ReplaceRouteResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data ReplaceRoute = ReplaceRoute
    { _rrRouteTableId :: Text
    , _rrDestinationCidrBlock :: Text
    , _rrGatewayId :: Maybe Text
    , _rrInstanceId :: Maybe Text
    , _rrNetworkInterfaceId :: Maybe Text
    , _rrVpcPeeringConnectionId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceRoute' request.
mkReplaceRoute :: Text -- ^ 'rrRouteTableId'
               -> Text -- ^ 'rrDestinationCidrBlock'
               -> ReplaceRoute
mkReplaceRoute p1 p2 = ReplaceRoute
    { _rrRouteTableId = p1
    , _rrDestinationCidrBlock = p2
    , _rrGatewayId = Nothing
    , _rrInstanceId = Nothing
    , _rrNetworkInterfaceId = Nothing
    , _rrVpcPeeringConnectionId = Nothing
    }

-- | The ID of the route table.
rrRouteTableId :: Lens' ReplaceRoute Text
rrRouteTableId = lens _rrRouteTableId (\s a -> s { _rrRouteTableId = a })

-- | The CIDR address block used for the destination match. The value you
-- provide must match the CIDR of an existing route in the table.
rrDestinationCidrBlock :: Lens' ReplaceRoute Text
rrDestinationCidrBlock =
    lens _rrDestinationCidrBlock (\s a -> s { _rrDestinationCidrBlock = a })

-- | The ID of an Internet gateway attached to your VPC.
rrGatewayId :: Lens' ReplaceRoute (Maybe Text)
rrGatewayId = lens _rrGatewayId (\s a -> s { _rrGatewayId = a })

-- | The ID of a NAT instance in your VPC.
rrInstanceId :: Lens' ReplaceRoute (Maybe Text)
rrInstanceId = lens _rrInstanceId (\s a -> s { _rrInstanceId = a })

-- | The ID of a network interface.
rrNetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)
rrNetworkInterfaceId =
    lens _rrNetworkInterfaceId (\s a -> s { _rrNetworkInterfaceId = a })

-- | The ID of a VPC peering connection.
rrVpcPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)
rrVpcPeeringConnectionId =
    lens _rrVpcPeeringConnectionId
         (\s a -> s { _rrVpcPeeringConnectionId = a })

instance ToQuery ReplaceRoute where
    toQuery = genericQuery def

data ReplaceRouteResponse = ReplaceRouteResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ReplaceRoute where
    type Sv ReplaceRoute = EC2
    type Rs ReplaceRoute = ReplaceRouteResponse

    request = post "ReplaceRoute"
    response _ = nullaryResponse ReplaceRouteResponse

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
    , mkReplaceRouteRequest
    -- ** Request lenses
    , rrrRouteTableId
    , rrrDestinationCidrBlock
    , rrrGatewayId
    , rrrInstanceId
    , rrrNetworkInterfaceId
    , rrrVpcPeeringConnectionId

    -- * Response
    , ReplaceRouteResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceRoute' request.
mkReplaceRouteRequest :: Text -- ^ 'rrrRouteTableId'
                      -> Text -- ^ 'rrrDestinationCidrBlock'
                      -> ReplaceRoute
mkReplaceRouteRequest p1 p2 = ReplaceRoute
    { _rrrRouteTableId = p1
    , _rrrDestinationCidrBlock = p2
    , _rrrGatewayId = Nothing
    , _rrrInstanceId = Nothing
    , _rrrNetworkInterfaceId = Nothing
    , _rrrVpcPeeringConnectionId = Nothing
    }
{-# INLINE mkReplaceRouteRequest #-}

data ReplaceRoute = ReplaceRoute
    { _rrrRouteTableId :: Text
      -- ^ The ID of the route table.
    , _rrrDestinationCidrBlock :: Text
      -- ^ The CIDR address block used for the destination match. The value
      -- you provide must match the CIDR of an existing route in the
      -- table.
    , _rrrGatewayId :: Maybe Text
      -- ^ The ID of an Internet gateway attached to your VPC.
    , _rrrInstanceId :: Maybe Text
      -- ^ The ID of a NAT instance in your VPC.
    , _rrrNetworkInterfaceId :: Maybe Text
      -- ^ The ID of a network interface.
    , _rrrVpcPeeringConnectionId :: Maybe Text
      -- ^ The ID of a VPC peering connection.
    } deriving (Show, Generic)

-- | The ID of the route table.
rrrRouteTableId :: Lens' ReplaceRoute (Text)
rrrRouteTableId = lens _rrrRouteTableId (\s a -> s { _rrrRouteTableId = a })
{-# INLINE rrrRouteTableId #-}

-- | The CIDR address block used for the destination match. The value you
-- provide must match the CIDR of an existing route in the table.
rrrDestinationCidrBlock :: Lens' ReplaceRoute (Text)
rrrDestinationCidrBlock = lens _rrrDestinationCidrBlock (\s a -> s { _rrrDestinationCidrBlock = a })
{-# INLINE rrrDestinationCidrBlock #-}

-- | The ID of an Internet gateway attached to your VPC.
rrrGatewayId :: Lens' ReplaceRoute (Maybe Text)
rrrGatewayId = lens _rrrGatewayId (\s a -> s { _rrrGatewayId = a })
{-# INLINE rrrGatewayId #-}

-- | The ID of a NAT instance in your VPC.
rrrInstanceId :: Lens' ReplaceRoute (Maybe Text)
rrrInstanceId = lens _rrrInstanceId (\s a -> s { _rrrInstanceId = a })
{-# INLINE rrrInstanceId #-}

-- | The ID of a network interface.
rrrNetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)
rrrNetworkInterfaceId = lens _rrrNetworkInterfaceId (\s a -> s { _rrrNetworkInterfaceId = a })
{-# INLINE rrrNetworkInterfaceId #-}

-- | The ID of a VPC peering connection.
rrrVpcPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)
rrrVpcPeeringConnectionId = lens _rrrVpcPeeringConnectionId (\s a -> s { _rrrVpcPeeringConnectionId = a })
{-# INLINE rrrVpcPeeringConnectionId #-}

instance ToQuery ReplaceRoute where
    toQuery = genericQuery def

data ReplaceRouteResponse = ReplaceRouteResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ReplaceRoute where
    type Sv ReplaceRoute = EC2
    type Rs ReplaceRoute = ReplaceRouteResponse

    request = post "ReplaceRoute"
    response _ = nullaryResponse ReplaceRouteResponse

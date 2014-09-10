{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
module Network.AWS.EC2
    (
    -- * Request
      ReplaceRoute
    -- ** Request constructor
    , mkReplaceRoute
    -- ** Request lenses
    , rr1RouteTableId
    , rr1DestinationCidrBlock
    , rr1GatewayId
    , rr1InstanceId
    , rr1NetworkInterfaceId
    , rr1VpcPeeringConnectionId

    -- * Response
    , ReplaceRouteResponse
    -- ** Response constructor
    , mkReplaceRouteResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ReplaceRoute = ReplaceRoute
    { _rr1RouteTableId :: !Text
    , _rr1DestinationCidrBlock :: !Text
    , _rr1GatewayId :: !(Maybe Text)
    , _rr1InstanceId :: !(Maybe Text)
    , _rr1NetworkInterfaceId :: !(Maybe Text)
    , _rr1VpcPeeringConnectionId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceRoute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RouteTableId ::@ @Text@
--
-- * @DestinationCidrBlock ::@ @Text@
--
-- * @GatewayId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @VpcPeeringConnectionId ::@ @Maybe Text@
--
mkReplaceRoute :: Text -- ^ 'rr1RouteTableId'
               -> Text -- ^ 'rr1DestinationCidrBlock'
               -> ReplaceRoute
mkReplaceRoute p1 p2 = ReplaceRoute
    { _rr1RouteTableId = p1
    , _rr1DestinationCidrBlock = p2
    , _rr1GatewayId = Nothing
    , _rr1InstanceId = Nothing
    , _rr1NetworkInterfaceId = Nothing
    , _rr1VpcPeeringConnectionId = Nothing
    }

-- | The ID of the route table.
rr1RouteTableId :: Lens' ReplaceRoute Text
rr1RouteTableId = lens _rr1RouteTableId (\s a -> s { _rr1RouteTableId = a })

-- | The CIDR address block used for the destination match. The value you
-- provide must match the CIDR of an existing route in the table.
rr1DestinationCidrBlock :: Lens' ReplaceRoute Text
rr1DestinationCidrBlock =
    lens _rr1DestinationCidrBlock
         (\s a -> s { _rr1DestinationCidrBlock = a })

-- | The ID of an Internet gateway attached to your VPC.
rr1GatewayId :: Lens' ReplaceRoute (Maybe Text)
rr1GatewayId = lens _rr1GatewayId (\s a -> s { _rr1GatewayId = a })

-- | The ID of a NAT instance in your VPC.
rr1InstanceId :: Lens' ReplaceRoute (Maybe Text)
rr1InstanceId = lens _rr1InstanceId (\s a -> s { _rr1InstanceId = a })

-- | The ID of a network interface.
rr1NetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)
rr1NetworkInterfaceId =
    lens _rr1NetworkInterfaceId (\s a -> s { _rr1NetworkInterfaceId = a })

-- | The ID of a VPC peering connection.
rr1VpcPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)
rr1VpcPeeringConnectionId =
    lens _rr1VpcPeeringConnectionId
         (\s a -> s { _rr1VpcPeeringConnectionId = a })

instance ToQuery ReplaceRoute where
    toQuery = genericQuery def

data ReplaceRouteResponse = ReplaceRouteResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceRouteResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkReplaceRouteResponse :: ReplaceRouteResponse
mkReplaceRouteResponse = ReplaceRouteResponse

instance AWSRequest ReplaceRoute where
    type Sv ReplaceRoute = EC2
    type Rs ReplaceRoute = ReplaceRouteResponse

    request = post "ReplaceRoute"
    response _ = nullaryResponse ReplaceRouteResponse

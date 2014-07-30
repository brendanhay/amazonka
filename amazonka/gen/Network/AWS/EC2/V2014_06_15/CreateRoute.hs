{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a route in a route table within a VPC. You must specify one of the
-- following targets: Internet gateway, NAT instance, VPC peering connection,
-- or network interface. When determining how to route traffic, we use the
-- route with the most specific match. For example, let's say the traffic is
-- destined for 192.0.2.3, and the route table includes the following two
-- routes: 192.0.2.0/24 (goes to some target A) 192.0.2.0/28 (goes to some
-- target B) Both routes apply to the traffic destined for 192.0.2.3. However,
-- the second route in the list covers a smaller number of IP addresses and is
-- therefore more specific, so we use that route to determine where to target
-- the traffic. For more information about route tables, see Route Tables in
-- the Amazon Virtual Private Cloud User Guide. Example 1 This example creates
-- a route in the route table with the ID rtb-e4ad488d. The route matches all
-- traffic (0.0.0.0/0) and routes it to the Internet gateway with the ID
-- igw-eaad4883. https://ec2.amazonaws.com/?Action=CreateRoute
-- &amp;RouteTableId=rtb-e4ad488d &amp;DestinationCidrBlock=0.0.0.0/0
-- &amp;GatewayId=igw-eaad4883 &amp;AUTHPARAMS Example 2 This example creates
-- a route in the route table with the ID rtb-g8ff4ea2. The route sends all
-- traffic (0.0.0.0/0) to the NAT instance with the ID i-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=CreateRoute
-- &amp;RouteTableId=rtb-g8ff4ea2 &amp;DestinationCidrBlock=0.0.0.0/0
-- &amp;InstanceId=i-1a2b3c4d &amp;AUTHPARAMS Example 3 This example command
-- creates a route in route table rtb-g8ff4ea2. The route matches traffic for
-- the CIDR block 10.0.0.0/16 and routes it to VPC peering connection,
-- pcx-111aaa22. This route enables traffic to be directed to the other peered
-- VPC in the VPC peering connection.
-- https://ec2.amazonaws.com/?Action=CreateRoute
-- &amp;RouteTableId=rtb-g8ff4ea2 &amp;DestinationCidrBlock=10.0.0.0/16
-- &amp;vpcPeeringConnectionId=pcx-111aaa22 &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.CreateRoute where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'CreateRoute' request.
createRoute :: Text -- ^ '_crrRouteTableId'
            -> Text -- ^ '_crrDestinationCidrBlock'
            -> CreateRoute
createRoute p1 p2 = CreateRoute
    { _crrRouteTableId = p1
    , _crrDestinationCidrBlock = p2
    , _crrDryRun = Nothing
    , _crrVpcPeeringConnectionId = Nothing
    , _crrInstanceId = Nothing
    , _crrNetworkInterfaceId = Nothing
    , _crrGatewayId = Nothing
    }

data CreateRoute = CreateRoute
    { _crrRouteTableId :: Text
      -- ^ The ID of the route table for the route.
    , _crrDestinationCidrBlock :: Text
      -- ^ The CIDR address block used for the destination match. Routing
      -- decisions are based on the most specific match.
    , _crrDryRun :: Maybe Bool
      -- ^ 
    , _crrVpcPeeringConnectionId :: Maybe Text
      -- ^ The ID of a VPC peering connection.
    , _crrInstanceId :: Maybe Text
      -- ^ The ID of a NAT instance in your VPC. The operation fails if you
      -- specify an instance ID unless exactly one network interface is
      -- attached.
    , _crrNetworkInterfaceId :: Maybe Text
      -- ^ The ID of a network interface.
    , _crrGatewayId :: Maybe Text
      -- ^ The ID of an Internet gateway attached to your VPC.
    } deriving (Generic)

instance ToQuery CreateRoute where
    toQuery = genericToQuery def

instance AWSRequest CreateRoute where
    type Sv CreateRoute = EC2
    type Rs CreateRoute = CreateRouteResponse

    request = post "CreateRoute"
    response _ _ = return (Right CreateRouteResponse)

data CreateRouteResponse = CreateRouteResponse
    deriving (Eq, Show, Generic)

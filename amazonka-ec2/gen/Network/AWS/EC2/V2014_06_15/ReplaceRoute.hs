{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    -- ** Default constructor
    , replaceRoute
    -- ** Accessors and lenses
    , _rrrRouteTableId
    , rrrRouteTableId
    , _rrrDestinationCidrBlock
    , rrrDestinationCidrBlock
    , _rrrGatewayId
    , rrrGatewayId
    , _rrrInstanceId
    , rrrInstanceId
    , _rrrNetworkInterfaceId
    , rrrNetworkInterfaceId
    , _rrrVpcPeeringConnectionId
    , rrrVpcPeeringConnectionId

    -- * Response
    , ReplaceRouteResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ReplaceRoute' request.
replaceRoute :: Text -- ^ 'rrrRouteTableId'
             -> Text -- ^ 'rrrDestinationCidrBlock'
             -> ReplaceRoute
replaceRoute p1 p2 = ReplaceRoute
    { _rrrRouteTableId = p1
    , _rrrDestinationCidrBlock = p2
    , _rrrGatewayId = Nothing
    , _rrrInstanceId = Nothing
    , _rrrNetworkInterfaceId = Nothing
    , _rrrVpcPeeringConnectionId = Nothing
    }

data ReplaceRoute = ReplaceRoute

makeSiglessLenses ''ReplaceRoute

instance ToQuery ReplaceRoute where
    toQuery = genericQuery def

data ReplaceRouteResponse = ReplaceRouteResponse
    deriving (Eq, Show, Generic)

makeSiglessLenses ''ReplaceRouteResponse

instance AWSRequest ReplaceRoute where
    type Sv ReplaceRoute = EC2
    type Rs ReplaceRoute = ReplaceRouteResponse

    request = post "ReplaceRoute"
    response _ = nullaryResponse ReplaceRouteResponse

-- | The ID of the route table.
rrrRouteTableId :: Lens' ReplaceRoute (Text)

-- | The CIDR address block used for the destination match. The value you
-- provide must match the CIDR of an existing route in the table.
rrrDestinationCidrBlock :: Lens' ReplaceRoute (Text)

-- | The ID of an Internet gateway attached to your VPC.
rrrGatewayId :: Lens' ReplaceRoute (Maybe Text)

-- | The ID of a NAT instance in your VPC.
rrrInstanceId :: Lens' ReplaceRoute (Maybe Text)

-- | The ID of a network interface.
rrrNetworkInterfaceId :: Lens' ReplaceRoute (Maybe Text)

-- | The ID of a VPC peering connection.
rrrVpcPeeringConnectionId :: Lens' ReplaceRoute (Maybe Text)

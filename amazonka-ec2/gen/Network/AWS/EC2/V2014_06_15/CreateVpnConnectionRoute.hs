{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateVpnConnectionRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a static route associated with a VPN connection between an existing
-- virtual private gateway and a VPN customer gateway. The static route allows
-- traffic to be routed from the virtual private gateway to the VPN customer
-- gateway. For more information about VPN connections, see Adding a Hardware
-- Virtual Private Gateway to Your VPC in the Amazon Virtual Private Cloud
-- User Guide. Example This example creates a static route to the VPN
-- connection for the VPN connection with the ID vpn-83ad48ea to the
-- destination CIDR block 11.12.0.0/16. Note that when using the Query API the
-- "/" is denoted as "%2F".
-- https://ec2.amazonaws.com/?Action=CreateVpnConnectionRoute
-- &amp;DestinationCidrBlock=11.12.0.0%2F16 &amp;VpnConnectionId=vpn-83ad48ea
-- &amp;AUTHPARAMS &lt;CreateVpnConnectionRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/CreateVpnConnectionRouteResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateVpnConnectionRoute where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { _cvcrrDestinationCidrBlock :: Text
      -- ^ The CIDR block associated with the local subnet of the customer
      -- network.
    , _cvcrrVpnConnectionId :: Text
      -- ^ The ID of the VPN connection.
    } deriving (Show, Generic)

makeLenses ''CreateVpnConnectionRoute

instance ToQuery CreateVpnConnectionRoute where
    toQuery = genericQuery def

data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
    deriving (Eq, Show, Generic)

makeLenses ''CreateVpnConnectionRouteResponse

instance AWSRequest CreateVpnConnectionRoute where
    type Sv CreateVpnConnectionRoute = EC2
    type Rs CreateVpnConnectionRoute = CreateVpnConnectionRouteResponse

    request = post "CreateVpnConnectionRoute"
    response _ = nullaryResponse CreateVpnConnectionRouteResponse

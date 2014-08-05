{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeVpnConnections
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your VPN connections. For more information about
-- VPN connections, see Adding a Hardware Virtual Private Gateway to Your VPC
-- in the Amazon Virtual Private Cloud User Guide. Example 1 This example
-- describes the specified VPN connection. The response includes the customer
-- gateway configuration information. Because it's a long set of information,
-- we haven't displayed it here. To see an example of the configuration
-- information, see the Amazon Virtual Private Cloud Network Administrator
-- Guide. https://ec2.amazonaws.com/?Action=DescribeVpnConnections
-- &amp;VpnConnectionId.1=vpn-44a8938f &amp;AUTHPARAMS
-- &lt;DescribeVpnConnectionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnConnectionSet&gt; &lt;item&gt;
-- &lt;vpnConnectionId&gt;vpn-44a8938f&lt;/vpnConnectionId&gt;
-- &lt;state&gt;available&lt;/state&gt; &lt;customerGatewayConfiguration&gt;
-- ...Customer gateway configuration data in escaped XML format...
-- &lt;/customerGatewayConfiguration&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt; &lt;tagSet/&gt;
-- &lt;/item&gt; &lt;/vpnConnectionSet&gt;
-- &lt;/DescribeVpnConnectionsResponse&gt; Example 2 This example describes
-- any VPN connection you own that is associated with the customer gateway
-- with ID cgw-b4dc3961, and whose state is either pending or available.
-- https://ec2.amazonaws.com/?Action=DescribeVpnConnections
-- &amp;Filter.1.Name=customer-gateway-id &amp;Filter.1.Value.1=cgw-b4dc3961
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=pending
-- &amp;Filter.2.Value.2=available &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeVpnConnections where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeVpnConnections' request.
describeVpnConnections :: DescribeVpnConnections
describeVpnConnections = DescribeVpnConnections
    { _dvcsDryRun = Nothing
    , _dvcsFilters = mempty
    , _dvcsVpnConnectionIds = mempty
    }

data DescribeVpnConnections = DescribeVpnConnections
    { _dvcsDryRun :: Maybe Bool
      -- ^ 
    , _dvcsFilters :: [Filter]
      -- ^ One or more filters. customer-gateway-configuration - The
      -- configuration information for the customer gateway.
      -- customer-gateway-id - The ID of a customer gateway associated
      -- with the VPN connection. state - The state of the VPN connection
      -- (pending | available | deleting | deleted).
      -- option.static-routes-only - Indicates whether the connection has
      -- static routes only. Used for devices that do not support Border
      -- Gateway Protocol (BGP). route.destination-cidr-block - The
      -- destination CIDR block. This corresponds to the subnet used in a
      -- customer data center. bgp-asn - The BGP Autonomous System Number
      -- (ASN) associated with a BGP device. tag:key=value - The key/value
      -- combination of a tag assigned to the resource. tag-key - The key
      -- of a tag assigned to the resource. This filter is independent of
      -- the tag-value filter. For example, if you use both the filter
      -- "tag-key=Purpose" and the filter "tag-value=X", you get any
      -- resources assigned both the tag key Purpose (regardless of what
      -- the tag's value is), and the tag value X (regardless of what the
      -- tag's key is). If you want to list only resources where Purpose
      -- is X, see the tag:key=value filter. tag-value - The value of a
      -- tag assigned to the resource. This filter is independent of the
      -- tag-key filter. type - The type of VPN connection. Currently the
      -- only supported type is ipsec.1. vpn-connection-id - The ID of the
      -- VPN connection. vpn-gateway-id - The ID of a virtual private
      -- gateway associated with the VPN connection.
    , _dvcsVpnConnectionIds :: [Text]
      -- ^ One or more VPN connection IDs. Default: Describes your VPN
      -- connections.
    } deriving (Show, Generic)

makeLenses ''DescribeVpnConnections

instance ToQuery DescribeVpnConnections where
    toQuery = genericToQuery def

data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { _dvctVpnConnections :: [VpnConnection]
      -- ^ Information about one or more VPN connections.
    } deriving (Show, Generic)

makeLenses ''DescribeVpnConnectionsResponse

instance AWSRequest DescribeVpnConnections where
    type Sv DescribeVpnConnections = EC2
    type Rs DescribeVpnConnections = DescribeVpnConnectionsResponse

    request = post "DescribeVpnConnections"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeVpnConnectionsResponse
            <*> xml %| "VpnConnectionList"

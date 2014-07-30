{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeVpnGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your virtual private gateways. For more
-- information about virtual private gateways, see Adding an IPsec Hardware
-- VPN to Your VPC in the Amazon Virtual Private Cloud User Guide. Example 1
-- This example describes the specified virtual private gateway.
-- https://ec2.amazonaws.com/?Action=DescribeVpnGateways
-- &amp;VpnGatewayId.1=vgw-8db04f81 &amp;AUTHPARAMS
-- &lt;DescribeVpnGatewaysResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnGatewaySet&gt; &lt;item&gt;
-- &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt;
-- &lt;state&gt;available&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;attachments&gt; &lt;item&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;vpcId&gt;
-- &lt;state&gt;attached&lt;/state&gt; &lt;/item&gt; &lt;/attachments&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;/vpnGatewaySet&gt;
-- &lt;/DescribeVpnGatewaysResponse&gt; Example 2 This example uses filters to
-- describe any virtual private gateway you own that is in the us-east-1a
-- Availability Zone, and whose state is either pending or available.
-- https://ec2.amazonaws.com/?Action=DescribeVpnGateways
-- &amp;Filter.1.Name=availability-zone &amp;Filter.1.Value.1=us-east-1a
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=pending
-- &amp;Filter.2.Value.2=available &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeVpnGateways where

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
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeVpnGateways' request.
describeVpnGateways :: DescribeVpnGateways
describeVpnGateways = DescribeVpnGateways
    { _dvgtDryRun = Nothing
    , _dvgtFilters = mempty
    , _dvgtVpnGatewayIds = mempty
    }

data DescribeVpnGateways = DescribeVpnGateways
    { _dvgtDryRun :: Maybe Bool
      -- ^ 
    , _dvgtFilters :: [Filter]
      -- ^ One or more filters. attachment.state - The current state of the
      -- attachment between the gateway and the VPC (attaching | attached
      -- | detaching | detached). attachment.vpc-id - The ID of an
      -- attached VPC. availability-zone - The Availability Zone for the
      -- virtual private gateway. state - The state of the virtual private
      -- gateway (pending | available | deleting | deleted). tag:key=value
      -- - The key/value combination of a tag assigned to the resource.
      -- tag-key - The key of a tag assigned to the resource. This filter
      -- is independent of the tag-value filter. For example, if you use
      -- both the filter "tag-key=Purpose" and the filter "tag-value=X",
      -- you get any resources assigned both the tag key Purpose
      -- (regardless of what the tag's value is), and the tag value X
      -- (regardless of what the tag's key is). If you want to list only
      -- resources where Purpose is X, see the tag:key=value filter.
      -- tag-value - The value of a tag assigned to the resource. This
      -- filter is independent of the tag-key filter. type - The type of
      -- virtual private gateway. Currently the only supported type is
      -- ipsec.1. vpn-gateway-id - The ID of the virtual private gateway.
    , _dvgtVpnGatewayIds :: [Text]
      -- ^ One or more virtual private gateway IDs. Default: Describes all
      -- your virtual private gateways.
    } deriving (Generic)

instance ToQuery DescribeVpnGateways where
    toQuery = genericToQuery def

instance AWSRequest DescribeVpnGateways where
    type Sv DescribeVpnGateways = EC2
    type Rs DescribeVpnGateways = DescribeVpnGatewaysResponse

    request = post "DescribeVpnGateways"
    response _ = xmlResponse

data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { _dvguVpnGateways :: [VpnGateway]
      -- ^ Information about one or more virtual private gateways.
    } deriving (Generic)

instance FromXML DescribeVpnGatewaysResponse where
    fromXMLOptions = xmlOptions

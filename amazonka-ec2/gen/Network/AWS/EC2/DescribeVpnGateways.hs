{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVpnGateways
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
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
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
module Network.AWS.EC2.DescribeVpnGateways
    (
    -- * Request
      DescribeVpnGateways
    -- ** Request constructor
    , describeVpnGateways
    -- ** Request lenses
    , dvg1VpnGatewayIds
    , dvg1Filters

    -- * Response
    , DescribeVpnGatewaysResponse
    -- ** Response constructor
    , describeVpnGatewaysResponse
    -- ** Response lenses
    , dvgrVpnGateways
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeVpnGateways = DescribeVpnGateways
    { _dvg1VpnGatewayIds :: [Text]
    , _dvg1Filters :: [Filter]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVpnGateways' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnGatewayIds ::@ @[Text]@
--
-- * @Filters ::@ @[Filter]@
--
describeVpnGateways :: DescribeVpnGateways
describeVpnGateways = DescribeVpnGateways
    { _dvg1VpnGatewayIds = mempty
    , _dvg1Filters = mempty
    }

-- | One or more virtual private gateway IDs. Default: Describes all your
-- virtual private gateways.
dvg1VpnGatewayIds :: Lens' DescribeVpnGateways [Text]
dvg1VpnGatewayIds =
    lens _dvg1VpnGatewayIds (\s a -> s { _dvg1VpnGatewayIds = a })

-- | One or more filters. attachment.state - The current state of the attachment
-- between the gateway and the VPC (attaching | attached | detaching |
-- detached). attachment.vpc-id - The ID of an attached VPC. availability-zone
-- - The Availability Zone for the virtual private gateway. state - The state
-- of the virtual private gateway (pending | available | deleting | deleted).
-- tag:key=value - The key/value combination of a tag assigned to the
-- resource. tag-key - The key of a tag assigned to the resource. This filter
-- is independent of the tag-value filter. For example, if you use both the
-- filter "tag-key=Purpose" and the filter "tag-value=X", you get any
-- resources assigned both the tag key Purpose (regardless of what the tag's
-- value is), and the tag value X (regardless of what the tag's key is). If
-- you want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter. type - The type of virtual
-- private gateway. Currently the only supported type is ipsec.1.
-- vpn-gateway-id - The ID of the virtual private gateway.
dvg1Filters :: Lens' DescribeVpnGateways [Filter]
dvg1Filters = lens _dvg1Filters (\s a -> s { _dvg1Filters = a })

instance ToQuery DescribeVpnGateways where
    toQuery = genericQuery def

newtype DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { _dvgrVpnGateways :: [VpnGateway]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVpnGatewaysResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnGateways ::@ @[VpnGateway]@
--
describeVpnGatewaysResponse :: DescribeVpnGatewaysResponse
describeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { _dvgrVpnGateways = mempty
    }

-- | Information about one or more virtual private gateways.
dvgrVpnGateways :: Lens' DescribeVpnGatewaysResponse [VpnGateway]
dvgrVpnGateways = lens _dvgrVpnGateways (\s a -> s { _dvgrVpnGateways = a })

instance FromXML DescribeVpnGatewaysResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVpnGateways where
    type Sv DescribeVpnGateways = EC2
    type Rs DescribeVpnGateways = DescribeVpnGatewaysResponse

    request = post "DescribeVpnGateways"
    response _ = xmlResponse

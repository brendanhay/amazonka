{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeCustomerGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your VPN customer gateways. For more information
-- about VPN customer gateways, see Adding a Hardware Virtual Private Gateway
-- to Your VPC in the Amazon Virtual Private Cloud User Guide. Example 1 This
-- example request describes the specified customer gateway.
-- https://ec2.amazonaws.com/?Action=DescribeCustomerGateways
-- &amp;CustomerGatewayId.1=cgw-b4dc3961 &amp;AUTHPARAMS
-- &lt;DescribeCustomerGatewaysResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;customerGatewaySet&gt; &lt;item&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;state&gt;available&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;ipAddress&gt;12.1.2.3&lt;/ipAddress&gt;
-- &lt;bgpAsn&gt;65534&lt;/bgpasn&gt; &lt;tagSet/&gt; &lt;/item&gt;
-- &lt;/customerGatewaySet&gt; &lt;/DescribeCustomerGatewaysResponse&gt;
-- Example 2 This example request uses filters to describe any customer
-- gateway you own whose IP address is 12.1.2.3, and whose state is either
-- pending or available.
-- https://ec2.amazonaws.com/?Action=DescribeCustomerGateways
-- &amp;Filter.1.Name=ip-address &amp;Filter.1.Value.1=12.1.2.3
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=pending
-- &amp;Filter.2.Value.2=available &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeCustomerGateways
    (
    -- * Request
      DescribeCustomerGateways
    -- ** Default constructor
    , describeCustomerGateways
    -- ** Accessors and lenses
    , _dcgsCustomerGatewayIds
    , dcgsCustomerGatewayIds
    , _dcgsFilters
    , dcgsFilters

    -- * Response
    , DescribeCustomerGatewaysResponse
    -- ** Accessors and lenses
    , _dcgtCustomerGateways
    , dcgtCustomerGateways
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeCustomerGateways' request.
describeCustomerGateways :: DescribeCustomerGateways
describeCustomerGateways = DescribeCustomerGateways
    { _dcgsCustomerGatewayIds = mempty
    , _dcgsFilters = mempty
    }

data DescribeCustomerGateways = DescribeCustomerGateways

makeSiglessLenses ''DescribeCustomerGateways

instance ToQuery DescribeCustomerGateways where
    toQuery = genericQuery def

data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { _dcgtCustomerGateways :: [CustomerGateway]
      -- ^ Information about one or more customer gateways.
    } deriving (Show, Generic)

makeSiglessLenses ''DescribeCustomerGatewaysResponse

instance FromXML DescribeCustomerGatewaysResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeCustomerGateways where
    type Sv DescribeCustomerGateways = EC2
    type Rs DescribeCustomerGateways = DescribeCustomerGatewaysResponse

    request = post "DescribeCustomerGateways"
    response _ = xmlResponse

-- | One or more customer gateway IDs. Default: Describes all your customer
-- gateways.
dcgsCustomerGatewayIds :: Lens' DescribeCustomerGateways ([Text])

-- | One or more filters. bgp-asn - The customer gateway's Border Gateway
-- Protocol (BGP) Autonomous System Number (ASN). customer-gateway-id - The ID
-- of the customer gateway. ip-address - The IP address of the customer
-- gateway's Internet-routable external interface. state - The state of the
-- customer gateway (pending | available | deleting | deleted). type - The
-- type of customer gateway. Currently, the only supported type is ipsec.1.
-- tag:key=value - The key/value combination of a tag assigned to the
-- resource. tag-key - The key of a tag assigned to the resource. This filter
-- is independent of the tag-value filter. For example, if you use both the
-- filter "tag-key=Purpose" and the filter "tag-value=X", you get any
-- resources assigned both the tag key Purpose (regardless of what the tag's
-- value is), and the tag value X (regardless of what the tag's key is). If
-- you want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter.
dcgsFilters :: Lens' DescribeCustomerGateways ([Filter])

-- | Information about one or more customer gateways.
dcgtCustomerGateways :: Lens' DescribeCustomerGatewaysResponse ([CustomerGateway])

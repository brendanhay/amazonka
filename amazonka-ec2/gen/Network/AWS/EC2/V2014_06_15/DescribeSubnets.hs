{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your subnets. For more information about subnets,
-- see Your VPC and Subnets in the Amazon Virtual Private Cloud User Guide.
-- Example 1 This example describes the subnets with the IDs subnet-9d4a7b6c
-- and subnet-6e7f829e. https://ec2.amazonaws.com/?Action=DescribeSubnets
-- &amp;SubnetId.1=subnet-9d4a7b6c &amp;SubnetId.2=subnet-6e7f829e
-- &amp;AUTHPARAMS &lt;DescribeSubnetsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;subnetSet&gt; &lt;item&gt;
-- &lt;subnetId&gt;subnet-9d4a7b6c&lt;/subnetId&gt;
-- &lt;state&gt;available&lt;/state&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.1.0/24&lt;/cidrBlock&gt;
-- &lt;availableIpAddressCount&gt;251&lt;/availableIpAddressCount&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;defaultForAz&gt;false&lt;/defaultForAz&gt;
-- &lt;mapPublicIpOnLaunch&gt;false&lt;/mapPublicIpOnLaunch&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;subnetId&gt;subnet-6e7f829e&lt;/subnetId&gt;
-- &lt;state&gt;available&lt;/state&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&gt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.0.0/24&lt;/cidrBlock&gt;
-- &lt;availableIpAddressCount&gt;251&lt;/availableIpAddressCount&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;defaultForAz&gt;false&lt;/defaultForAz&gt;
-- &lt;mapPublicIpOnLaunch&gt;false&lt;/mapPublicIpOnLaunch&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;subnetSet/&gt;
-- &lt;/DescribeSubnetsResponse&gt; Example 2 This example uses filters to
-- describe any subnet you own that is in the VPC with the ID vpc-1a2b3c4d or
-- vpc-6e7f8a92, and whose state is available.
-- https://ec2.amazonaws.com/?Action=DescribeSubnets &amp;Filter.1.Name=vpc-id
-- &amp;Filter.1.Value.1=vpc-1a2b3c4d &amp;Filter.1.Value.2=vpc-6e7f8a92
-- &amp;Filter.2.Name=state &amp;Filter.2.Value.1=available &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeSubnets
    (
    -- * Request
      DescribeSubnets
    -- ** Request constructor
    , mkDescribeSubnets
    -- ** Request lenses
    , ds3SubnetIds
    , ds3Filters

    -- * Response
    , DescribeSubnetsResponse
    -- ** Response lenses
    , dsrsrsSubnets
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeSubnets = DescribeSubnets
    { _ds3SubnetIds :: [Text]
    , _ds3Filters :: [Filter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSubnets' request.
mkDescribeSubnets :: DescribeSubnets
mkDescribeSubnets = DescribeSubnets
    { _ds3SubnetIds = mempty
    , _ds3Filters = mempty
    }
{-# INLINE mkDescribeSubnets #-}

-- | One or more subnet IDs. Default: Describes all your subnets.
ds3SubnetIds :: Lens' DescribeSubnets [Text]
ds3SubnetIds = lens _ds3SubnetIds (\s a -> s { _ds3SubnetIds = a })
{-# INLINE ds3SubnetIds #-}

-- | One or more filters. availabilityZone - The Availability Zone for the
-- subnet. You can also use availability-zone as the filter name.
-- available-ip-address-count - The number of IP addresses in the subnet that
-- are available. cidrBlock - The CIDR block of the subnet. The CIDR block you
-- specify must exactly match the subnet's CIDR block for information to be
-- returned for the subnet. You can also use cidr or cidr-block as the filter
-- names. defaultForAz - Indicates whether this is the default subnet for the
-- Availability Zone. You can also use default-for-az as the filter name.
-- state - The state of the subnet (pending | available). subnet-id - The ID
-- of the subnet. tag:key=value - The key/value combination of a tag assigned
-- to the resource. tag-key - The key of a tag assigned to the resource. This
-- filter is independent of the tag-value filter. For example, if you use both
-- the filter "tag-key=Purpose" and the filter "tag-value=X", you get any
-- resources assigned both the tag key Purpose (regardless of what the tag's
-- value is), and the tag value X (regardless of what the tag's key is). If
-- you want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter. vpc-id - The ID of the VPC for
-- the subnet.
ds3Filters :: Lens' DescribeSubnets [Filter]
ds3Filters = lens _ds3Filters (\s a -> s { _ds3Filters = a })
{-# INLINE ds3Filters #-}

instance ToQuery DescribeSubnets where
    toQuery = genericQuery def

-- | 
newtype DescribeSubnetsResponse = DescribeSubnetsResponse
    { _dsrsrsSubnets :: [Subnet]
    } deriving (Show, Generic)

-- | Information about one or more subnets.
dsrsrsSubnets :: Lens' DescribeSubnetsResponse [Subnet]
dsrsrsSubnets = lens _dsrsrsSubnets (\s a -> s { _dsrsrsSubnets = a })
{-# INLINE dsrsrsSubnets #-}

instance FromXML DescribeSubnetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSubnets where
    type Sv DescribeSubnets = EC2
    type Rs DescribeSubnets = DescribeSubnetsResponse

    request = post "DescribeSubnets"
    response _ = xmlResponse

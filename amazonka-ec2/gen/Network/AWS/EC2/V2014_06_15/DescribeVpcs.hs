{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeVpcs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your VPCs. Example 1 This example describes the
-- specified VPC. https://ec2.amazonaws.com/?Action=DescribeVpcs
-- &amp;VpcId.1=vpc-1a2b3c4d &amp;AUTHPARAMS &lt;DescribeVpcsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcSet&gt; &lt;item&gt; &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;state&gt;available&lt;/state&gt;
-- &lt;cidrBlock&gt;10.0.0.0/23&lt;/cidrBlock&gt;
-- &lt;dhcpOptionsId&gt;dopt-7a8b9c2d&lt;/dhcpOptionsId&gt;
-- &lt;instanceTenancy&gt;default&lt;/instanceTenancy&gt;
-- &lt;isDefault&gt;false&lt;/isDefault&gt; &lt;tagSet/&gt; &lt;/item&gt;
-- &lt;/vpcSet&gt; &lt;/DescribeVpcsResponse&gt; Example 2 This example uses
-- filters to describe any VPC you own that uses the set of DHCP options with
-- the ID dopt-7a8b9c2d or dopt-2b2a3d3c and whose state is available.
-- https://ec2.amazonaws.com/?Action=DescribeVpcs
-- &amp;Filter.1.Name=dhcp-options-id &amp;Filter.1.Value.1=dopt-7a8b9c2d
-- &amp;Filter.1.Value.2=dopt-2b2a3d3c &amp;Filter.2.Name=state
-- &amp;Filter.2.Value.1=available &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeVpcs where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeVpcs' request.
describeVpcs :: DescribeVpcs
describeVpcs = DescribeVpcs
    { _dvvDryRun = Nothing
    , _dvvFilters = mempty
    , _dvvVpcIds = mempty
    }

data DescribeVpcs = DescribeVpcs
    { _dvvDryRun :: Maybe Bool
      -- ^ 
    , _dvvFilters :: [Filter]
      -- ^ One or more filters. cidr - The CIDR block of the VPC. The CIDR
      -- block you specify must exactly match the VPC's CIDR block for
      -- information to be returned for the VPC. dhcp-options-id - The ID
      -- of a set of DHCP options. isDefault - Indicates whether the VPC
      -- is the default VPC. state - The state of the VPC (pending |
      -- available). tag:key=value - The key/value combination of a tag
      -- assigned to the resource. tag-key - The key of a tag assigned to
      -- the resource. This filter is independent of the tag-value filter.
      -- For example, if you use both the filter "tag-key=Purpose" and the
      -- filter "tag-value=X", you get any resources assigned both the tag
      -- key Purpose (regardless of what the tag's value is), and the tag
      -- value X (regardless of what the tag's key is). If you want to
      -- list only resources where Purpose is X, see the tag:key=value
      -- filter. tag-value - The value of a tag assigned to the resource.
      -- This filter is independent of the tag-key filter. vpc-id - The ID
      -- of the VPC.
    , _dvvVpcIds :: [Text]
      -- ^ One or more VPC IDs. Default: Describes all your VPCs.
    } deriving (Show, Generic)

makeLenses ''DescribeVpcs

instance ToQuery DescribeVpcs where
    toQuery = genericQuery def

data DescribeVpcsResponse = DescribeVpcsResponse
    { _dvwVpcs :: [Vpc]
      -- ^ Information about one or more VPCs.
    } deriving (Show, Generic)

makeLenses ''DescribeVpcsResponse

instance FromXML DescribeVpcsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVpcs where
    type Sv DescribeVpcs = EC2
    type Rs DescribeVpcs = DescribeVpcsResponse

    request = post "DescribeVpcs"
    response _ = xmlResponse

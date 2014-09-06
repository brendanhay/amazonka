{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeVpcAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified VPC. You can specify
-- only one attribute at a time. Example 1 This example describes the
-- enableDnsSupport attribute of the specified VPC. The sample response
-- indicates that DNS resolution is supported.
-- https://ec2.amazonaws.com/?Action=DescribeVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;Attribute=enableDnsSupport &amp;AUTHPARAMS
-- &lt;DescribeVpcAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt; &lt;enableDnsSupport&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/enableDnsSupport&gt;
-- &lt;/DescribeVpcAttributeResponse&gt; Example 2 This request describes the
-- enableDnsHostnames attribute of the specified VPC. The sample response
-- indicates that DNS hostnames are supported.
-- https://ec2.amazonaws.com/?Action=DescribeVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;Attribute=enableDnsHostnames &amp;AUTHPARAMS
-- &lt;DescribeVpcAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt; &lt;enableDnsHostnames&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/enableDnsHostnames&gt;
-- &lt;/DescribeVpcAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DescribeVpcAttribute
    (
    -- * Request
      DescribeVpcAttribute
    -- ** Request constructor
    , mkDescribeVpcAttribute
    -- ** Request lenses
    , dva1VpcId
    , dva1Attribute

    -- * Response
    , DescribeVpcAttributeResponse
    -- ** Response lenses
    , dvarsrsVpcId
    , dvarsrsEnableDnsSupport
    , dvarsrsEnableDnsHostnames
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeVpcAttribute = DescribeVpcAttribute
    { _dva1VpcId :: Text
    , _dva1Attribute :: Maybe VpcAttributeName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVpcAttribute' request.
mkDescribeVpcAttribute :: Text -- ^ 'dva1VpcId'
                       -> DescribeVpcAttribute
mkDescribeVpcAttribute p1 = DescribeVpcAttribute
    { _dva1VpcId = p1
    , _dva1Attribute = Nothing
    }
{-# INLINE mkDescribeVpcAttribute #-}

-- | The ID of the VPC.
dva1VpcId :: Lens' DescribeVpcAttribute Text
dva1VpcId = lens _dva1VpcId (\s a -> s { _dva1VpcId = a })
{-# INLINE dva1VpcId #-}

-- | The VPC attribute.
dva1Attribute :: Lens' DescribeVpcAttribute (Maybe VpcAttributeName)
dva1Attribute = lens _dva1Attribute (\s a -> s { _dva1Attribute = a })
{-# INLINE dva1Attribute #-}

instance ToQuery DescribeVpcAttribute where
    toQuery = genericQuery def

-- | 
data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { _dvarsrsVpcId :: Maybe Text
    , _dvarsrsEnableDnsSupport :: Maybe AttributeBooleanValue
    , _dvarsrsEnableDnsHostnames :: Maybe AttributeBooleanValue
    } deriving (Show, Generic)

-- | The ID of the VPC.
dvarsrsVpcId :: Lens' DescribeVpcAttributeResponse (Maybe Text)
dvarsrsVpcId = lens _dvarsrsVpcId (\s a -> s { _dvarsrsVpcId = a })
{-# INLINE dvarsrsVpcId #-}

-- | Indicates whether DNS resolution is enabled for the VPC. If this attribute
-- is true, the Amazon DNS server resolves DNS hostnames for your instances to
-- their corresponding IP addresses; otherwise, it does not.
dvarsrsEnableDnsSupport :: Lens' DescribeVpcAttributeResponse (Maybe AttributeBooleanValue)
dvarsrsEnableDnsSupport =
    lens _dvarsrsEnableDnsSupport
         (\s a -> s { _dvarsrsEnableDnsSupport = a })
{-# INLINE dvarsrsEnableDnsSupport #-}

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- this attribute is true, instances in the VPC get DNS hostnames; otherwise,
-- they do not.
dvarsrsEnableDnsHostnames :: Lens' DescribeVpcAttributeResponse (Maybe AttributeBooleanValue)
dvarsrsEnableDnsHostnames =
    lens _dvarsrsEnableDnsHostnames
         (\s a -> s { _dvarsrsEnableDnsHostnames = a })
{-# INLINE dvarsrsEnableDnsHostnames #-}

instance FromXML DescribeVpcAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVpcAttribute where
    type Sv DescribeVpcAttribute = EC2
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse

    request = post "DescribeVpcAttribute"
    response _ = xmlResponse

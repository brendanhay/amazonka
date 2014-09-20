{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVpcAttribute
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
module Network.AWS.EC2.DescribeVpcAttribute
    (
    -- * Request
      DescribeVpcAttribute
    -- ** Request constructor
    , describeVpcAttribute
    -- ** Request lenses
    , dva1VpcId
    , dva1Attribute

    -- * Response
    , DescribeVpcAttributeResponse
    -- ** Response constructor
    , describeVpcAttributeResponse
    -- ** Response lenses
    , dvarrVpcId
    , dvarrEnableDnsSupport
    , dvarrEnableDnsHostnames
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeVpcAttribute = DescribeVpcAttribute
    { _dva1VpcId :: Text
    , _dva1Attribute :: Maybe VpcAttributeName
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVpcAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Text@
--
-- * @Attribute ::@ @Maybe VpcAttributeName@
--
describeVpcAttribute :: Text -- ^ 'dva1VpcId'
                     -> DescribeVpcAttribute
describeVpcAttribute p1 = DescribeVpcAttribute
    { _dva1VpcId = p1
    , _dva1Attribute = Nothing
    }

-- | The ID of the VPC.
dva1VpcId :: Lens' DescribeVpcAttribute Text
dva1VpcId = lens _dva1VpcId (\s a -> s { _dva1VpcId = a })

-- | The VPC attribute.
dva1Attribute :: Lens' DescribeVpcAttribute (Maybe VpcAttributeName)
dva1Attribute = lens _dva1Attribute (\s a -> s { _dva1Attribute = a })

instance ToQuery DescribeVpcAttribute where
    toQuery = genericQuery def

data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { _dvarrVpcId :: Maybe Text
    , _dvarrEnableDnsSupport :: Maybe AttributeBooleanValue
    , _dvarrEnableDnsHostnames :: Maybe AttributeBooleanValue
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVpcAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @EnableDnsSupport ::@ @Maybe AttributeBooleanValue@
--
-- * @EnableDnsHostnames ::@ @Maybe AttributeBooleanValue@
--
describeVpcAttributeResponse :: DescribeVpcAttributeResponse
describeVpcAttributeResponse = DescribeVpcAttributeResponse
    { _dvarrVpcId = Nothing
    , _dvarrEnableDnsSupport = Nothing
    , _dvarrEnableDnsHostnames = Nothing
    }

-- | The ID of the VPC.
dvarrVpcId :: Lens' DescribeVpcAttributeResponse (Maybe Text)
dvarrVpcId = lens _dvarrVpcId (\s a -> s { _dvarrVpcId = a })

-- | Indicates whether DNS resolution is enabled for the VPC. If this attribute
-- is true, the Amazon DNS server resolves DNS hostnames for your instances to
-- their corresponding IP addresses; otherwise, it does not.
dvarrEnableDnsSupport :: Lens' DescribeVpcAttributeResponse (Maybe AttributeBooleanValue)
dvarrEnableDnsSupport =
    lens _dvarrEnableDnsSupport (\s a -> s { _dvarrEnableDnsSupport = a })

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- this attribute is true, instances in the VPC get DNS hostnames; otherwise,
-- they do not.
dvarrEnableDnsHostnames :: Lens' DescribeVpcAttributeResponse (Maybe AttributeBooleanValue)
dvarrEnableDnsHostnames =
    lens _dvarrEnableDnsHostnames
         (\s a -> s { _dvarrEnableDnsHostnames = a })

instance FromXML DescribeVpcAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVpcAttribute where
    type Sv DescribeVpcAttribute = EC2
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse

    request = post "DescribeVpcAttribute"
    response _ = xmlResponse

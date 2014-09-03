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
    , describeVpcAttribute
    -- ** Request lenses
    , dvatVpcId
    , dvatAttribute

    -- * Response
    , DescribeVpcAttributeResponse
    -- ** Response lenses
    , dvauEnableDnsSupport
    , dvauEnableDnsHostnames
    , dvauVpcId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeVpcAttribute' request.
describeVpcAttribute :: Text -- ^ 'dvatVpcId'
                     -> DescribeVpcAttribute
describeVpcAttribute p1 = DescribeVpcAttribute
    { _dvatVpcId = p1
    , _dvatAttribute = Nothing
    }

data DescribeVpcAttribute = DescribeVpcAttribute
    { _dvatVpcId :: Text
      -- ^ The ID of the VPC.
    , _dvatAttribute :: Maybe VpcAttributeName
      -- ^ The VPC attribute.
    } deriving (Show, Generic)

-- | The ID of the VPC.
dvatVpcId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeVpcAttribute
    -> f DescribeVpcAttribute
dvatVpcId f x =
    (\y -> x { _dvatVpcId = y })
       <$> f (_dvatVpcId x)
{-# INLINE dvatVpcId #-}

-- | The VPC attribute.
dvatAttribute
    :: Functor f
    => (Maybe VpcAttributeName
    -> f (Maybe VpcAttributeName))
    -> DescribeVpcAttribute
    -> f DescribeVpcAttribute
dvatAttribute f x =
    (\y -> x { _dvatAttribute = y })
       <$> f (_dvatAttribute x)
{-# INLINE dvatAttribute #-}

instance ToQuery DescribeVpcAttribute where
    toQuery = genericQuery def

data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { _dvauEnableDnsSupport :: Maybe AttributeBooleanValue
      -- ^ Indicates whether DNS resolution is enabled for the VPC. If this
      -- attribute is true, the Amazon DNS server resolves DNS hostnames
      -- for your instances to their corresponding IP addresses;
      -- otherwise, it does not.
    , _dvauEnableDnsHostnames :: Maybe AttributeBooleanValue
      -- ^ Indicates whether the instances launched in the VPC get DNS
      -- hostnames. If this attribute is true, instances in the VPC get
      -- DNS hostnames; otherwise, they do not.
    , _dvauVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

-- | Indicates whether DNS resolution is enabled for the VPC. If this attribute
-- is true, the Amazon DNS server resolves DNS hostnames for your instances to
-- their corresponding IP addresses; otherwise, it does not.
dvauEnableDnsSupport
    :: Functor f
    => (Maybe AttributeBooleanValue
    -> f (Maybe AttributeBooleanValue))
    -> DescribeVpcAttributeResponse
    -> f DescribeVpcAttributeResponse
dvauEnableDnsSupport f x =
    (\y -> x { _dvauEnableDnsSupport = y })
       <$> f (_dvauEnableDnsSupport x)
{-# INLINE dvauEnableDnsSupport #-}

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- this attribute is true, instances in the VPC get DNS hostnames; otherwise,
-- they do not.
dvauEnableDnsHostnames
    :: Functor f
    => (Maybe AttributeBooleanValue
    -> f (Maybe AttributeBooleanValue))
    -> DescribeVpcAttributeResponse
    -> f DescribeVpcAttributeResponse
dvauEnableDnsHostnames f x =
    (\y -> x { _dvauEnableDnsHostnames = y })
       <$> f (_dvauEnableDnsHostnames x)
{-# INLINE dvauEnableDnsHostnames #-}

-- | The ID of the VPC.
dvauVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeVpcAttributeResponse
    -> f DescribeVpcAttributeResponse
dvauVpcId f x =
    (\y -> x { _dvauVpcId = y })
       <$> f (_dvauVpcId x)
{-# INLINE dvauVpcId #-}

instance FromXML DescribeVpcAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVpcAttribute where
    type Sv DescribeVpcAttribute = EC2
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse

    request = post "DescribeVpcAttribute"
    response _ = xmlResponse

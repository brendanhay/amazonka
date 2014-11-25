{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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

-- | Describes the specified attribute of the specified VPC. You can specify only
-- one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcAttribute.html>
module Network.AWS.EC2.DescribeVpcAttribute
    (
    -- * Request
      DescribeVpcAttribute
    -- ** Request constructor
    , describeVpcAttribute
    -- ** Request lenses
    , dva1Attribute
    , dva1DryRun
    , dva1VpcId

    -- * Response
    , DescribeVpcAttributeResponse
    -- ** Response constructor
    , describeVpcAttributeResponse
    -- ** Response lenses
    , dvarEnableDnsHostnames
    , dvarEnableDnsSupport
    , dvarVpcId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpcAttribute = DescribeVpcAttribute
    { _dva1Attribute :: Maybe VpcAttributeName
    , _dva1DryRun    :: Maybe Bool
    , _dva1VpcId     :: Text
    } deriving (Eq, Show)

-- | 'DescribeVpcAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dva1Attribute' @::@ 'Maybe' 'VpcAttributeName'
--
-- * 'dva1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dva1VpcId' @::@ 'Text'
--
describeVpcAttribute :: Text -- ^ 'dva1VpcId'
                     -> DescribeVpcAttribute
describeVpcAttribute p1 = DescribeVpcAttribute
    { _dva1VpcId     = p1
    , _dva1DryRun    = Nothing
    , _dva1Attribute = Nothing
    }

-- | The VPC attribute.
--
dva1Attribute :: Lens' DescribeVpcAttribute (Maybe VpcAttributeName)
dva1Attribute = lens _dva1Attribute (\s a -> s { _dva1Attribute = a })

dva1DryRun :: Lens' DescribeVpcAttribute (Maybe Bool)
dva1DryRun = lens _dva1DryRun (\s a -> s { _dva1DryRun = a })

-- | The ID of the VPC.
--
dva1VpcId :: Lens' DescribeVpcAttribute Text
dva1VpcId = lens _dva1VpcId (\s a -> s { _dva1VpcId = a })

data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { _dvarEnableDnsHostnames :: Maybe AttributeBooleanValue
    , _dvarEnableDnsSupport   :: Maybe AttributeBooleanValue
    , _dvarVpcId              :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeVpcAttributeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvarEnableDnsHostnames' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'dvarEnableDnsSupport' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'dvarVpcId' @::@ 'Maybe' 'Text'
--
describeVpcAttributeResponse :: DescribeVpcAttributeResponse
describeVpcAttributeResponse = DescribeVpcAttributeResponse
    { _dvarVpcId              = Nothing
    , _dvarEnableDnsSupport   = Nothing
    , _dvarEnableDnsHostnames = Nothing
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- this attribute is 'true', instances in the VPC get DNS hostnames; otherwise,
-- they do not.
--
dvarEnableDnsHostnames :: Lens' DescribeVpcAttributeResponse (Maybe AttributeBooleanValue)
dvarEnableDnsHostnames =
    lens _dvarEnableDnsHostnames (\s a -> s { _dvarEnableDnsHostnames = a })

-- | Indicates whether DNS resolution is enabled for the VPC. If this attribute is 'true', the Amazon DNS server resolves DNS hostnames for your instances to
-- their corresponding IP addresses; otherwise, it does not.
--
dvarEnableDnsSupport :: Lens' DescribeVpcAttributeResponse (Maybe AttributeBooleanValue)
dvarEnableDnsSupport =
    lens _dvarEnableDnsSupport (\s a -> s { _dvarEnableDnsSupport = a })

-- | The ID of the VPC.
--
dvarVpcId :: Lens' DescribeVpcAttributeResponse (Maybe Text)
dvarVpcId = lens _dvarVpcId (\s a -> s { _dvarVpcId = a })

instance ToPath DescribeVpcAttribute where
    toPath = const "/"

instance ToQuery DescribeVpcAttribute where
    toQuery DescribeVpcAttribute{..} = mconcat
        [ "Attribute" =? _dva1Attribute
        , "dryRun"    =? _dva1DryRun
        , "VpcId"     =? _dva1VpcId
        ]

instance ToHeaders DescribeVpcAttribute

instance AWSRequest DescribeVpcAttribute where
    type Sv DescribeVpcAttribute = EC2
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse

    request  = post "DescribeVpcAttribute"
    response = xmlResponse

instance FromXML DescribeVpcAttributeResponse where
    parseXML x = DescribeVpcAttributeResponse
        <$> x .@? "enableDnsHostnames"
        <*> x .@? "enableDnsSupport"
        <*> x .@? "vpcId"

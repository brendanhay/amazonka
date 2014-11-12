{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- only one attribute at a time.
module Network.AWS.EC2.DescribeVpcAttribute
    (
    -- * Request
      DescribeVpcAttribute
    -- ** Request constructor
    , describeVpcAttribute
    -- ** Request lenses
    , dvaAttribute
    , dvaDryRun
    , dvaVpcId

    -- * Response
    , DescribeVpcAttributeResult
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

data DescribeVpcAttribute = DescribeVpcAttribute
    { _dvaAttribute :: Maybe Text
    , _dvaDryRun    :: Maybe Bool
    , _dvaVpcId     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeVpcAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvaAttribute' @::@ 'Maybe' 'Text'
--
-- * 'dvaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvaVpcId' @::@ 'Text'
--
describeVpcAttribute :: Text -- ^ 'dvaVpcId'
                     -> DescribeVpcAttribute
describeVpcAttribute p1 = DescribeVpcAttribute
    { _dvaVpcId     = p1
    , _dvaDryRun    = Nothing
    , _dvaAttribute = Nothing
    }

-- | The VPC attribute.
dvaAttribute :: Lens' DescribeVpcAttribute (Maybe Text)
dvaAttribute = lens _dvaAttribute (\s a -> s { _dvaAttribute = a })

dvaDryRun :: Lens' DescribeVpcAttribute (Maybe Bool)
dvaDryRun = lens _dvaDryRun (\s a -> s { _dvaDryRun = a })

-- | The ID of the VPC.
dvaVpcId :: Lens' DescribeVpcAttribute Text
dvaVpcId = lens _dvaVpcId (\s a -> s { _dvaVpcId = a })

instance ToQuery DescribeVpcAttribute

instance ToPath DescribeVpcAttribute where
    toPath = const "/"

data DescribeVpcAttributeResult = DescribeVpcAttributeResult
    { _dvarEnableDnsHostnames :: Maybe AttributeBooleanValue
    , _dvarEnableDnsSupport   :: Maybe AttributeBooleanValue
    , _dvarVpcId              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeVpcAttributeResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvarEnableDnsHostnames' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'dvarEnableDnsSupport' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'dvarVpcId' @::@ 'Maybe' 'Text'
--
describeVpcAttributeResponse :: DescribeVpcAttributeResult
describeVpcAttributeResponse = DescribeVpcAttributeResult
    { _dvarVpcId              = Nothing
    , _dvarEnableDnsSupport   = Nothing
    , _dvarEnableDnsHostnames = Nothing
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- this attribute is true, instances in the VPC get DNS hostnames;
-- otherwise, they do not.
dvarEnableDnsHostnames :: Lens' DescribeVpcAttributeResult (Maybe AttributeBooleanValue)
dvarEnableDnsHostnames =
    lens _dvarEnableDnsHostnames (\s a -> s { _dvarEnableDnsHostnames = a })

-- | Indicates whether DNS resolution is enabled for the VPC. If this
-- attribute is true, the Amazon DNS server resolves DNS hostnames for your
-- instances to their corresponding IP addresses; otherwise, it does not.
dvarEnableDnsSupport :: Lens' DescribeVpcAttributeResult (Maybe AttributeBooleanValue)
dvarEnableDnsSupport =
    lens _dvarEnableDnsSupport (\s a -> s { _dvarEnableDnsSupport = a })

-- | The ID of the VPC.
dvarVpcId :: Lens' DescribeVpcAttributeResult (Maybe Text)
dvarVpcId = lens _dvarVpcId (\s a -> s { _dvarVpcId = a })

instance FromXML DescribeVpcAttributeResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeVpcAttributeResult"

instance AWSRequest DescribeVpcAttribute where
    type Sv DescribeVpcAttribute = EC2
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResult

    request  = post "DescribeVpcAttribute"
    response = xmlResponse $ \h x -> DescribeVpcAttributeResult
        <$> x %| "enableDnsHostnames"
        <*> x %| "enableDnsSupport"
        <*> x %| "vpcId"

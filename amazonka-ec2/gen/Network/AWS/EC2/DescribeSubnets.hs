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

-- Module      : Network.AWS.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your subnets.
--
-- For more information about subnets, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html Your VPC and Subnets> in the /AmazonVirtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSubnets.html>
module Network.AWS.EC2.DescribeSubnets
    (
    -- * Request
      DescribeSubnets
    -- ** Request constructor
    , describeSubnets
    -- ** Request lenses
    , dsDryRun
    , dsFilters
    , dsSubnetIds

    -- * Response
    , DescribeSubnetsResponse
    -- ** Response constructor
    , describeSubnetsResponse
    -- ** Response lenses
    , dsrSubnets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeSubnets = DescribeSubnets
    { _dsDryRun    :: Maybe Bool
    , _dsFilters   :: List "Filter" Filter
    , _dsSubnetIds :: List "SubnetId" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeSubnets' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsFilters' @::@ ['Filter']
--
-- * 'dsSubnetIds' @::@ ['Text']
--
describeSubnets :: DescribeSubnets
describeSubnets = DescribeSubnets
    { _dsDryRun    = Nothing
    , _dsSubnetIds = mempty
    , _dsFilters   = mempty
    }

dsDryRun :: Lens' DescribeSubnets (Maybe Bool)
dsDryRun = lens _dsDryRun (\s a -> s { _dsDryRun = a })

-- | One or more filters.
--
-- 'availabilityZone' - The Availability Zone for the subnet. You can also use 'availability-zone' as the filter name.
--
-- 'available-ip-address-count' - The number of IP addresses in the subnet that
-- are available.
--
-- 'cidrBlock' - The CIDR block of the subnet. The CIDR block you specify must
-- exactly match the subnet's CIDR block for information to be returned for the
-- subnet. You can also use 'cidr' or 'cidr-block' as the filter names.
--
-- 'defaultForAz' - Indicates whether this is the default subnet for the
-- Availability Zone. You can also use 'default-for-az' as the filter name.
--
-- 'state' - The state of the subnet ('pending' | 'available').
--
-- 'subnet-id' - The ID of the subnet.
--
-- 'tag':/key/=/value/ - The key/value combination of a tag assigned to the
-- resource.
--
-- 'tag-key' - The key of a tag assigned to the resource. This filter is
-- independent of the 'tag-value' filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value is),
-- and the tag value X (regardless of what the tag's key is). If you want to
-- list only resources where Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- 'tag-value' - The value of a tag assigned to the resource. This filter is
-- independent of the 'tag-key' filter.
--
-- 'vpc-id' - The ID of the VPC for the subnet.
--
--
dsFilters :: Lens' DescribeSubnets [Filter]
dsFilters = lens _dsFilters (\s a -> s { _dsFilters = a }) . _List

-- | One or more subnet IDs.
--
-- Default: Describes all your subnets.
dsSubnetIds :: Lens' DescribeSubnets [Text]
dsSubnetIds = lens _dsSubnetIds (\s a -> s { _dsSubnetIds = a }) . _List

newtype DescribeSubnetsResponse = DescribeSubnetsResponse
    { _dsrSubnets :: List "item" Subnet
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeSubnetsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSubnets' @::@ ['Subnet']
--
describeSubnetsResponse :: DescribeSubnetsResponse
describeSubnetsResponse = DescribeSubnetsResponse
    { _dsrSubnets = mempty
    }

-- | Information about one or more subnets.
dsrSubnets :: Lens' DescribeSubnetsResponse [Subnet]
dsrSubnets = lens _dsrSubnets (\s a -> s { _dsrSubnets = a }) . _List

instance ToPath DescribeSubnets where
    toPath = const "/"

instance ToQuery DescribeSubnets where
    toQuery DescribeSubnets{..} = mconcat
        [ "DryRun"   =? _dsDryRun
        , "Filter"   `toQueryList` _dsFilters
        , "SubnetId" `toQueryList` _dsSubnetIds
        ]

instance ToHeaders DescribeSubnets

instance AWSRequest DescribeSubnets where
    type Sv DescribeSubnets = EC2
    type Rs DescribeSubnets = DescribeSubnetsResponse

    request  = post "DescribeSubnets"
    response = xmlResponse

instance FromXML DescribeSubnetsResponse where
    parseXML x = DescribeSubnetsResponse
        <$> x .@? "subnetSet" .!@ mempty

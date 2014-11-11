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

-- Module      : Network.AWS.EC2.DescribeSubnets
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
    , DescribeSubnetsResult
    -- ** Response constructor
    , describeSubnetsResult
    -- ** Response lenses
    , dsrSubnets
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeSubnets = DescribeSubnets
    { _dsDryRun    :: Maybe Bool
    , _dsFilters   :: [Filter]
    , _dsSubnetIds :: [Text]
    } deriving (Eq, Show, Generic)

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

-- | One or more filters. availabilityZone - The Availability Zone for the
-- subnet. You can also use availability-zone as the filter name.
-- available-ip-address-count - The number of IP addresses in the subnet
-- that are available. cidrBlock - The CIDR block of the subnet. The CIDR
-- block you specify must exactly match the subnet's CIDR block for
-- information to be returned for the subnet. You can also use cidr or
-- cidr-block as the filter names. defaultForAz - Indicates whether this is
-- the default subnet for the Availability Zone. You can also use
-- default-for-az as the filter name. state - The state of the subnet
-- (pending | available). subnet-id - The ID of the subnet. tag:key=value -
-- The key/value combination of a tag assigned to the resource. tag-key -
-- The key of a tag assigned to the resource. This filter is independent of
-- the tag-value filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value
-- is), and the tag value X (regardless of what the tag's key is). If you
-- want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter. vpc-id - The ID of the VPC
-- for the subnet.
dsFilters :: Lens' DescribeSubnets [Filter]
dsFilters = lens _dsFilters (\s a -> s { _dsFilters = a })

-- | One or more subnet IDs. Default: Describes all your subnets.
dsSubnetIds :: Lens' DescribeSubnets [Text]
dsSubnetIds = lens _dsSubnetIds (\s a -> s { _dsSubnetIds = a })
instance ToQuery DescribeSubnets

instance ToPath DescribeSubnets where
    toPath = const "/"

newtype DescribeSubnetsResult = DescribeSubnetsResult
    { _dsrSubnets :: [Subnet]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeSubnetsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSubnets' @::@ ['Subnet']
--
describeSubnetsResult :: DescribeSubnetsResult
describeSubnetsResult = DescribeSubnetsResult
    { _dsrSubnets = mempty
    }

-- | Information about one or more subnets.
dsrSubnets :: Lens' DescribeSubnetsResult [Subnet]
dsrSubnets = lens _dsrSubnets (\s a -> s { _dsrSubnets = a })
instance FromXML DescribeSubnetsResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeSubnetsResult"

instance AWSRequest DescribeSubnets where
    type Sv DescribeSubnets = EC2
    type Rs DescribeSubnets = DescribeSubnetsResult

    request  = post "DescribeSubnets"
    response = xmlResponse $ \h x -> DescribeSubnetsResult
        <$> x %| "subnetSet"

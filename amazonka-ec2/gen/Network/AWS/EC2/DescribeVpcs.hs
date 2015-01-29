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

-- Module      : Network.AWS.EC2.DescribeVpcs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your VPCs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcs.html>
module Network.AWS.EC2.DescribeVpcs
    (
    -- * Request
      DescribeVpcs
    -- ** Request constructor
    , describeVpcs
    -- ** Request lenses
    , dv1DryRun
    , dv1Filters
    , dv1VpcIds

    -- * Response
    , DescribeVpcsResponse
    -- ** Response constructor
    , describeVpcsResponse
    -- ** Response lenses
    , dvrVpcs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpcs = DescribeVpcs
    { _dv1DryRun  :: Maybe Bool
    , _dv1Filters :: List "Filter" Filter
    , _dv1VpcIds  :: List "VpcId" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeVpcs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv1Filters' @::@ ['Filter']
--
-- * 'dv1VpcIds' @::@ ['Text']
--
describeVpcs :: DescribeVpcs
describeVpcs = DescribeVpcs
    { _dv1DryRun  = Nothing
    , _dv1VpcIds  = mempty
    , _dv1Filters = mempty
    }

dv1DryRun :: Lens' DescribeVpcs (Maybe Bool)
dv1DryRun = lens _dv1DryRun (\s a -> s { _dv1DryRun = a })

-- | One or more filters.
--
-- 'cidr' - The CIDR block of the VPC. The CIDR block you specify must exactly
-- match the VPC's CIDR block for information to be returned for the VPC. Must
-- contain the slash followed by one or two digits (for example, '/28').
--
-- 'dhcp-options-id' - The ID of a set of DHCP options.
--
-- 'isDefault' - Indicates whether the VPC is the default VPC.
--
-- 'state' - The state of the VPC ('pending' | 'available').
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
-- 'vpc-id' - The ID of the VPC.
--
--
dv1Filters :: Lens' DescribeVpcs [Filter]
dv1Filters = lens _dv1Filters (\s a -> s { _dv1Filters = a }) . _List

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
dv1VpcIds :: Lens' DescribeVpcs [Text]
dv1VpcIds = lens _dv1VpcIds (\s a -> s { _dv1VpcIds = a }) . _List

newtype DescribeVpcsResponse = DescribeVpcsResponse
    { _dvrVpcs :: List "item" Vpc
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeVpcsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrVpcs' @::@ ['Vpc']
--
describeVpcsResponse :: DescribeVpcsResponse
describeVpcsResponse = DescribeVpcsResponse
    { _dvrVpcs = mempty
    }

-- | Information about one or more VPCs.
dvrVpcs :: Lens' DescribeVpcsResponse [Vpc]
dvrVpcs = lens _dvrVpcs (\s a -> s { _dvrVpcs = a }) . _List

instance ToPath DescribeVpcs where
    toPath = const "/"

instance ToQuery DescribeVpcs where
    toQuery DescribeVpcs{..} = mconcat
        [ "DryRun" =? _dv1DryRun
        , "Filter" `toQueryList` _dv1Filters
        , "VpcId"  `toQueryList` _dv1VpcIds
        ]

instance ToHeaders DescribeVpcs

instance AWSRequest DescribeVpcs where
    type Sv DescribeVpcs = EC2
    type Rs DescribeVpcs = DescribeVpcsResponse

    request  = post "DescribeVpcs"
    response = xmlResponse

instance FromXML DescribeVpcsResponse where
    parseXML x = DescribeVpcsResponse
        <$> x .@? "vpcSet" .!@ mempty

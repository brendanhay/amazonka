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

-- Module      : Network.AWS.EC2.DescribeVpcClassicLink
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

-- | Describes the ClassicLink status of one or more VPCs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcClassicLink.html>
module Network.AWS.EC2.DescribeVpcClassicLink
    (
    -- * Request
      DescribeVpcClassicLink
    -- ** Request constructor
    , describeVpcClassicLink
    -- ** Request lenses
    , dvclDryRun
    , dvclFilters
    , dvclVpcIds

    -- * Response
    , DescribeVpcClassicLinkResponse
    -- ** Response constructor
    , describeVpcClassicLinkResponse
    -- ** Response lenses
    , dvclrVpcs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpcClassicLink = DescribeVpcClassicLink
    { _dvclDryRun  :: Maybe Bool
    , _dvclFilters :: List "Filter" Filter
    , _dvclVpcIds  :: List "VpcId" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeVpcClassicLink' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvclDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvclFilters' @::@ ['Filter']
--
-- * 'dvclVpcIds' @::@ ['Text']
--
describeVpcClassicLink :: DescribeVpcClassicLink
describeVpcClassicLink = DescribeVpcClassicLink
    { _dvclDryRun  = Nothing
    , _dvclVpcIds  = mempty
    , _dvclFilters = mempty
    }

dvclDryRun :: Lens' DescribeVpcClassicLink (Maybe Bool)
dvclDryRun = lens _dvclDryRun (\s a -> s { _dvclDryRun = a })

-- | One or more filters.
--
-- 'is-classic-link-enabled' - Whether the VPC is enabled for ClassicLink ('true'
-- | 'false').
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
--
dvclFilters :: Lens' DescribeVpcClassicLink [Filter]
dvclFilters = lens _dvclFilters (\s a -> s { _dvclFilters = a }) . _List

-- | One or more VPCs for which you want to describe the ClassicLink status.
dvclVpcIds :: Lens' DescribeVpcClassicLink [Text]
dvclVpcIds = lens _dvclVpcIds (\s a -> s { _dvclVpcIds = a }) . _List

newtype DescribeVpcClassicLinkResponse = DescribeVpcClassicLinkResponse
    { _dvclrVpcs :: List "item" VpcClassicLink
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeVpcClassicLinkResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvclrVpcs' @::@ ['VpcClassicLink']
--
describeVpcClassicLinkResponse :: DescribeVpcClassicLinkResponse
describeVpcClassicLinkResponse = DescribeVpcClassicLinkResponse
    { _dvclrVpcs = mempty
    }

-- | The ClassicLink status of one or more VPCs.
dvclrVpcs :: Lens' DescribeVpcClassicLinkResponse [VpcClassicLink]
dvclrVpcs = lens _dvclrVpcs (\s a -> s { _dvclrVpcs = a }) . _List

instance ToPath DescribeVpcClassicLink where
    toPath = const "/"

instance ToQuery DescribeVpcClassicLink where
    toQuery DescribeVpcClassicLink{..} = mconcat
        [ "DryRun" =? _dvclDryRun
        , "Filter" `toQueryList` _dvclFilters
        , "VpcId"  `toQueryList` _dvclVpcIds
        ]

instance ToHeaders DescribeVpcClassicLink

instance AWSRequest DescribeVpcClassicLink where
    type Sv DescribeVpcClassicLink = EC2
    type Rs DescribeVpcClassicLink = DescribeVpcClassicLinkResponse

    request  = post "DescribeVpcClassicLink"
    response = xmlResponse

instance FromXML DescribeVpcClassicLinkResponse where
    parseXML x = DescribeVpcClassicLinkResponse
        <$> x .@? "vpcSet" .!@ mempty

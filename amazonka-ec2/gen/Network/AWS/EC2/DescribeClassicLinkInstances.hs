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

-- Module      : Network.AWS.EC2.DescribeClassicLinkInstances
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

-- | Describes one or more of your linked EC2-Classic instances. This request only
-- returns information about EC2-Classic instances linked to a VPC through
-- ClassicLink; you cannot use this request to return information about other
-- instances.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeClassicLinkInstances.html>
module Network.AWS.EC2.DescribeClassicLinkInstances
    (
    -- * Request
      DescribeClassicLinkInstances
    -- ** Request constructor
    , describeClassicLinkInstances
    -- ** Request lenses
    , dcliDryRun
    , dcliFilters
    , dcliInstanceIds
    , dcliMaxResults
    , dcliNextToken

    -- * Response
    , DescribeClassicLinkInstancesResponse
    -- ** Response constructor
    , describeClassicLinkInstancesResponse
    -- ** Response lenses
    , dclirInstances
    , dclirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeClassicLinkInstances = DescribeClassicLinkInstances
    { _dcliDryRun      :: Maybe Bool
    , _dcliFilters     :: List "Filter" Filter
    , _dcliInstanceIds :: List "InstanceId" Text
    , _dcliMaxResults  :: Maybe Int
    , _dcliNextToken   :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeClassicLinkInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcliDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dcliFilters' @::@ ['Filter']
--
-- * 'dcliInstanceIds' @::@ ['Text']
--
-- * 'dcliMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dcliNextToken' @::@ 'Maybe' 'Text'
--
describeClassicLinkInstances :: DescribeClassicLinkInstances
describeClassicLinkInstances = DescribeClassicLinkInstances
    { _dcliDryRun      = Nothing
    , _dcliInstanceIds = mempty
    , _dcliFilters     = mempty
    , _dcliNextToken   = Nothing
    , _dcliMaxResults  = Nothing
    }

dcliDryRun :: Lens' DescribeClassicLinkInstances (Maybe Bool)
dcliDryRun = lens _dcliDryRun (\s a -> s { _dcliDryRun = a })

-- | One or more filters.
--
-- 'group-id' - The ID of a VPC security group that's associated with the
-- instance.
--
-- 'instance-id' - The ID of the instance.
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
-- 'vpc-id' - The ID of the VPC that the instance is linked to.
--
--
dcliFilters :: Lens' DescribeClassicLinkInstances [Filter]
dcliFilters = lens _dcliFilters (\s a -> s { _dcliFilters = a }) . _List

-- | One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
dcliInstanceIds :: Lens' DescribeClassicLinkInstances [Text]
dcliInstanceIds = lens _dcliInstanceIds (\s a -> s { _dcliInstanceIds = a }) . _List

-- | The maximum number of results to return for the request in a single page. The
-- remaining results of the initial request can be seen by sending another
-- request with the returned 'NextToken' value. This value can be between 5 and
-- 1000; if 'MaxResults' is given a value larger than 1000, only 1000 results are
-- returned. You cannot specify this parameter and the instance IDs parameter in
-- the same request.
--
-- Constraint: If the value is greater than 1000, we return only 1000 items.
dcliMaxResults :: Lens' DescribeClassicLinkInstances (Maybe Int)
dcliMaxResults = lens _dcliMaxResults (\s a -> s { _dcliMaxResults = a })

-- | The token to retrieve the next page of results.
dcliNextToken :: Lens' DescribeClassicLinkInstances (Maybe Text)
dcliNextToken = lens _dcliNextToken (\s a -> s { _dcliNextToken = a })

data DescribeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse
    { _dclirInstances :: List "item" ClassicLinkInstance
    , _dclirNextToken :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeClassicLinkInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dclirInstances' @::@ ['ClassicLinkInstance']
--
-- * 'dclirNextToken' @::@ 'Maybe' 'Text'
--
describeClassicLinkInstancesResponse :: DescribeClassicLinkInstancesResponse
describeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse
    { _dclirInstances = mempty
    , _dclirNextToken = Nothing
    }

-- | Information about one or more linked EC2-Classic instances.
dclirInstances :: Lens' DescribeClassicLinkInstancesResponse [ClassicLinkInstance]
dclirInstances = lens _dclirInstances (\s a -> s { _dclirInstances = a }) . _List

-- | The token to use to retrieve the next page of results. This value is 'null'
-- when there are no more results to return.
dclirNextToken :: Lens' DescribeClassicLinkInstancesResponse (Maybe Text)
dclirNextToken = lens _dclirNextToken (\s a -> s { _dclirNextToken = a })

instance ToPath DescribeClassicLinkInstances where
    toPath = const "/"

instance ToQuery DescribeClassicLinkInstances where
    toQuery DescribeClassicLinkInstances{..} = mconcat
        [ "DryRun"     =? _dcliDryRun
        , "Filter"     `toQueryList` _dcliFilters
        , "InstanceId" `toQueryList` _dcliInstanceIds
        , "MaxResults" =? _dcliMaxResults
        , "NextToken"  =? _dcliNextToken
        ]

instance ToHeaders DescribeClassicLinkInstances

instance AWSRequest DescribeClassicLinkInstances where
    type Sv DescribeClassicLinkInstances = EC2
    type Rs DescribeClassicLinkInstances = DescribeClassicLinkInstancesResponse

    request  = post "DescribeClassicLinkInstances"
    response = xmlResponse

instance FromXML DescribeClassicLinkInstancesResponse where
    parseXML x = DescribeClassicLinkInstancesResponse
        <$> x .@? "instancesSet" .!@ mempty
        <*> x .@? "nextToken"

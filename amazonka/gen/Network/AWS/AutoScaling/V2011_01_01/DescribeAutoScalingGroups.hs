{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a full description of each Auto Scaling group in the given list.
-- This includes all Amazon EC2 instances that are members of the group. If a
-- list of names is not provided, the service returns the full details of all
-- Auto Scaling groups. This action supports pagination by returning a token
-- if there are more pages to retrieve. To get the next page, call this action
-- again with the returned token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupNames.member.1=my-test-asg-lbs
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeAutoScalingGroups
-- &AUTHPARAMS my-test-asg-lbs ELB 2013-05-06T17:47:15.107Z my-test-lc 2
-- us-east-1b us-east-1a my-test-asg-loadbalancer 2 120 300
-- arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb
-- :autoScalingGroupName/my-test-asg-lbs Default 10
-- 0f02a07d-b677-11e2-9eb0-dd50EXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingGroups where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAutoScalingGroups' request.
describeAutoScalingGroups :: DescribeAutoScalingGroups
describeAutoScalingGroups = DescribeAutoScalingGroups
    { _asgntAutoScalingGroupNames = mempty
    , _asgntMaxRecords = Nothing
    , _asgntNextToken = Nothing
    }

data DescribeAutoScalingGroups = DescribeAutoScalingGroups
    { _asgntAutoScalingGroupNames :: [Text]
      -- ^ A list of Auto Scaling group names.
    , _asgntMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to return.
    , _asgntNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Generic)

makeLenses ''DescribeAutoScalingGroups

instance ToQuery DescribeAutoScalingGroups where
    toQuery = genericToQuery def

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { _asgtAutoScalingGroups :: [AutoScalingGroup]
      -- ^ A list of Auto Scaling groups.
    , _asgtNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Generic)

makeLenses ''DescribeAutoScalingGroupsResponse

instance FromXML DescribeAutoScalingGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAutoScalingGroups where
    type Sv DescribeAutoScalingGroups = AutoScaling
    type Rs DescribeAutoScalingGroups = DescribeAutoScalingGroupsResponse

    request = post "DescribeAutoScalingGroups"
    response _ = xmlResponse

instance AWSPager DescribeAutoScalingGroups where
    next rq rs = (\x -> rq { Keyed "_asgntNextToken" = Just x })
        <$> (Keyed "_asgtNextToken" rs)

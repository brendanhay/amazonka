{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingGroups
    (
    -- * Request
      DescribeAutoScalingGroups
    -- ** Request constructor
    , describeAutoScalingGroups
    -- ** Request lenses
    , asgntAutoScalingGroupNames
    , asgntMaxRecords
    , asgntNextToken

    -- * Response
    , DescribeAutoScalingGroupsResponse
    -- ** Response lenses
    , asgtAutoScalingGroups
    , asgtNextToken
    ) where

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
{-# INLINE describeAutoScalingGroups #-}

data DescribeAutoScalingGroups = DescribeAutoScalingGroups
    { _asgntAutoScalingGroupNames :: [Text]
      -- ^ A list of Auto Scaling group names.
    , _asgntMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to return.
    , _asgntNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of Auto Scaling group names.
asgntAutoScalingGroupNames :: Lens' DescribeAutoScalingGroups ([Text])
asgntAutoScalingGroupNames f x =
    f (_asgntAutoScalingGroupNames x)
        <&> \y -> x { _asgntAutoScalingGroupNames = y }
{-# INLINE asgntAutoScalingGroupNames #-}

-- | The maximum number of records to return.
asgntMaxRecords :: Lens' DescribeAutoScalingGroups (Maybe Integer)
asgntMaxRecords f x =
    f (_asgntMaxRecords x)
        <&> \y -> x { _asgntMaxRecords = y }
{-# INLINE asgntMaxRecords #-}

-- | A string that marks the start of the next batch of returned results.
asgntNextToken :: Lens' DescribeAutoScalingGroups (Maybe Text)
asgntNextToken f x =
    f (_asgntNextToken x)
        <&> \y -> x { _asgntNextToken = y }
{-# INLINE asgntNextToken #-}

instance ToQuery DescribeAutoScalingGroups where
    toQuery = genericQuery def

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { _asgtAutoScalingGroups :: [AutoScalingGroup]
      -- ^ A list of Auto Scaling groups.
    , _asgtNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of Auto Scaling groups.
asgtAutoScalingGroups :: Lens' DescribeAutoScalingGroupsResponse ([AutoScalingGroup])
asgtAutoScalingGroups f x =
    f (_asgtAutoScalingGroups x)
        <&> \y -> x { _asgtAutoScalingGroups = y }
{-# INLINE asgtAutoScalingGroups #-}

-- | A string that marks the start of the next batch of returned results.
asgtNextToken :: Lens' DescribeAutoScalingGroupsResponse (Maybe Text)
asgtNextToken f x =
    f (_asgtNextToken x)
        <&> \y -> x { _asgtNextToken = y }
{-# INLINE asgtNextToken #-}

instance FromXML DescribeAutoScalingGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAutoScalingGroups where
    type Sv DescribeAutoScalingGroups = AutoScaling
    type Rs DescribeAutoScalingGroups = DescribeAutoScalingGroupsResponse

    request = post "DescribeAutoScalingGroups"
    response _ = xmlResponse

instance AWSPager DescribeAutoScalingGroups where
    next rq rs = (\x -> rq { _asgntNextToken = Just x })
        <$> (_asgtNextToken rs)

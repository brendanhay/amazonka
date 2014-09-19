{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
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
module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    (
    -- * Request
      DescribeAutoScalingGroups
    -- ** Request constructor
    , describeAutoScalingGroups
    -- ** Request lenses
    , dasg1AutoScalingGroupNames
    , dasg1NextToken
    , dasg1MaxRecords

    -- * Response
    , DescribeAutoScalingGroupsResponse
    -- ** Response constructor
    , describeAutoScalingGroupsResponse
    -- ** Response lenses
    , dasgrAutoScalingGroups
    , dasgrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

-- | The AutoScalingGroupNamesType data type.
data DescribeAutoScalingGroups = DescribeAutoScalingGroups
    { _dasg1AutoScalingGroupNames :: [Text]
    , _dasg1NextToken :: Maybe Text
    , _dasg1MaxRecords :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAutoScalingGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupNames ::@ @[Text]@
--
-- * @NextToken ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
describeAutoScalingGroups :: DescribeAutoScalingGroups
describeAutoScalingGroups = DescribeAutoScalingGroups
    { _dasg1AutoScalingGroupNames = mempty
    , _dasg1NextToken = Nothing
    , _dasg1MaxRecords = Nothing
    }

-- | A list of Auto Scaling group names.
dasg1AutoScalingGroupNames :: Lens' DescribeAutoScalingGroups [Text]
dasg1AutoScalingGroupNames =
    lens _dasg1AutoScalingGroupNames
         (\s a -> s { _dasg1AutoScalingGroupNames = a })

-- | A string that marks the start of the next batch of returned results.
dasg1NextToken :: Lens' DescribeAutoScalingGroups (Maybe Text)
dasg1NextToken = lens _dasg1NextToken (\s a -> s { _dasg1NextToken = a })

-- | The maximum number of records to return.
dasg1MaxRecords :: Lens' DescribeAutoScalingGroups (Maybe Integer)
dasg1MaxRecords = lens _dasg1MaxRecords (\s a -> s { _dasg1MaxRecords = a })

instance ToQuery DescribeAutoScalingGroups where
    toQuery = genericQuery def

-- | The AutoScalingGroupsType data type.
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { _dasgrAutoScalingGroups :: [AutoScalingGroup]
    , _dasgrNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAutoScalingGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroups ::@ @[AutoScalingGroup]@
--
-- * @NextToken ::@ @Maybe Text@
--
describeAutoScalingGroupsResponse :: [AutoScalingGroup] -- ^ 'dasgrAutoScalingGroups'
                                  -> DescribeAutoScalingGroupsResponse
describeAutoScalingGroupsResponse p1 = DescribeAutoScalingGroupsResponse
    { _dasgrAutoScalingGroups = p1
    , _dasgrNextToken = Nothing
    }

-- | A list of Auto Scaling groups.
dasgrAutoScalingGroups :: Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
dasgrAutoScalingGroups =
    lens _dasgrAutoScalingGroups (\s a -> s { _dasgrAutoScalingGroups = a })

-- | A string that marks the start of the next batch of returned results.
dasgrNextToken :: Lens' DescribeAutoScalingGroupsResponse (Maybe Text)
dasgrNextToken = lens _dasgrNextToken (\s a -> s { _dasgrNextToken = a })

instance FromXML DescribeAutoScalingGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAutoScalingGroups where
    type Sv DescribeAutoScalingGroups = AutoScaling
    type Rs DescribeAutoScalingGroups = DescribeAutoScalingGroupsResponse

    request = post "DescribeAutoScalingGroups"
    response _ = xmlResponse

instance AWSPager DescribeAutoScalingGroups where
    next rq rs = (\x -> rq & dasg1NextToken ?~ x)
        <$> (rs ^. dasgrNextToken)

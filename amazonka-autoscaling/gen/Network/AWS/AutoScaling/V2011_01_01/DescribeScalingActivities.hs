{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeScalingActivities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the scaling activities for the specified Auto Scaling group. If the
-- specified ActivityIds list is empty, all the activities from the past six
-- weeks are returned. Activities are sorted by the start time. Activities
-- still in progress appear first on the list. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeScalingActivities
-- &AUTHPARAMS Failed 0 063308ae-aa22-4a9b-94f4-9faeEXAMPLE
-- 2012-04-12T17:32:07.882Z my-test-asg At 2012-04-12T17:31:30Z a user request
-- created an AutoScalingGroup changing the desired capacity from 0 to 1. At
-- 2012-04-12T17:32:07Z an instance was started in response to a difference
-- between desired and actual capacity, increasing the capacity from 0 to 1.
-- {} Launching a new EC2 instance. Status Reason: The image id 'ami-4edb0327'
-- does not exist. Launching EC2 instance failed. 2012-04-12T17:32:08Z The
-- image id 'ami-4edb0327' does not exist. Launching EC2 instance failed.
-- 7a641adc-84c5-11e1-a8a5-217ebEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeScalingActivities
    (
    -- * Request
      DescribeScalingActivities
    -- ** Request constructor
    , mkDescribeScalingActivities
    -- ** Request lenses
    , dsa1ActivityIds
    , dsa1AutoScalingGroupName
    , dsa1MaxRecords
    , dsa1NextToken

    -- * Response
    , DescribeScalingActivitiesResponse
    -- ** Response lenses
    , dsarsActivities
    , dsarsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
data DescribeScalingActivities = DescribeScalingActivities
    { _dsa1ActivityIds :: [Text]
    , _dsa1AutoScalingGroupName :: Maybe Text
    , _dsa1MaxRecords :: Maybe Integer
    , _dsa1NextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeScalingActivities' request.
mkDescribeScalingActivities :: DescribeScalingActivities
mkDescribeScalingActivities = DescribeScalingActivities
    { _dsa1ActivityIds = mempty
    , _dsa1AutoScalingGroupName = Nothing
    , _dsa1MaxRecords = Nothing
    , _dsa1NextToken = Nothing
    }

-- | A list containing the activity IDs of the desired scaling activities. If
-- this list is omitted, all activities are described. If an
-- AutoScalingGroupName is provided, the results are limited to that group.
-- The list of requested activities cannot contain more than 50 items. If
-- unknown activities are requested, they are ignored with no error.
dsa1ActivityIds :: Lens' DescribeScalingActivities [Text]
dsa1ActivityIds = lens _dsa1ActivityIds (\s a -> s { _dsa1ActivityIds = a })

-- | The name of the AutoScalingGroup.
dsa1AutoScalingGroupName :: Lens' DescribeScalingActivities (Maybe Text)
dsa1AutoScalingGroupName =
    lens _dsa1AutoScalingGroupName
         (\s a -> s { _dsa1AutoScalingGroupName = a })

-- | The maximum number of scaling activities to return.
dsa1MaxRecords :: Lens' DescribeScalingActivities (Maybe Integer)
dsa1MaxRecords = lens _dsa1MaxRecords (\s a -> s { _dsa1MaxRecords = a })

-- | A string that marks the start of the next batch of returned results for
-- pagination.
dsa1NextToken :: Lens' DescribeScalingActivities (Maybe Text)
dsa1NextToken = lens _dsa1NextToken (\s a -> s { _dsa1NextToken = a })

instance ToQuery DescribeScalingActivities where
    toQuery = genericQuery def

-- | The output for the DescribeScalingActivities action.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { _dsarsActivities :: [Activity]
    , _dsarsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | A list of the requested scaling activities.
dsarsActivities :: Lens' DescribeScalingActivitiesResponse [Activity]
dsarsActivities = lens _dsarsActivities (\s a -> s { _dsarsActivities = a })

-- | Acts as a paging mechanism for large result sets. Set to a non-empty string
-- if there are additional results waiting to be returned. Pass this in to
-- subsequent calls to return additional results.
dsarsNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dsarsNextToken = lens _dsarsNextToken (\s a -> s { _dsarsNextToken = a })

instance FromXML DescribeScalingActivitiesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScalingActivities where
    type Sv DescribeScalingActivities = AutoScaling
    type Rs DescribeScalingActivities = DescribeScalingActivitiesResponse

    request = post "DescribeScalingActivities"
    response _ = xmlResponse

instance AWSPager DescribeScalingActivities where
    next rq rs = (\x -> rq & dsa1NextToken ?~ x) <$> (rs ^. dsarsNextToken)


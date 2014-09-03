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
    , describeScalingActivities
    -- ** Request lenses
    , dsauActivityIds
    , dsauMaxRecords
    , dsauAutoScalingGroupName
    , dsauNextToken

    -- * Response
    , DescribeScalingActivitiesResponse
    -- ** Response lenses
    , avActivities
    , avNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeScalingActivities' request.
describeScalingActivities :: DescribeScalingActivities
describeScalingActivities = DescribeScalingActivities
    { _dsauActivityIds = mempty
    , _dsauMaxRecords = Nothing
    , _dsauAutoScalingGroupName = Nothing
    , _dsauNextToken = Nothing
    }

data DescribeScalingActivities = DescribeScalingActivities
    { _dsauActivityIds :: [Text]
      -- ^ A list containing the activity IDs of the desired scaling
      -- activities. If this list is omitted, all activities are
      -- described. If an AutoScalingGroupName is provided, the results
      -- are limited to that group. The list of requested activities
      -- cannot contain more than 50 items. If unknown activities are
      -- requested, they are ignored with no error.
    , _dsauMaxRecords :: Maybe Integer
      -- ^ The maximum number of scaling activities to return.
    , _dsauAutoScalingGroupName :: Maybe Text
      -- ^ The name of the AutoScalingGroup.
    , _dsauNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results for pagination.
    } deriving (Show, Generic)

-- | A list containing the activity IDs of the desired scaling activities. If
-- this list is omitted, all activities are described. If an
-- AutoScalingGroupName is provided, the results are limited to that group.
-- The list of requested activities cannot contain more than 50 items. If
-- unknown activities are requested, they are ignored with no error.
dsauActivityIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeScalingActivities
    -> f DescribeScalingActivities
dsauActivityIds f x =
    (\y -> x { _dsauActivityIds = y })
       <$> f (_dsauActivityIds x)
{-# INLINE dsauActivityIds #-}

-- | The maximum number of scaling activities to return.
dsauMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeScalingActivities
    -> f DescribeScalingActivities
dsauMaxRecords f x =
    (\y -> x { _dsauMaxRecords = y })
       <$> f (_dsauMaxRecords x)
{-# INLINE dsauMaxRecords #-}

-- | The name of the AutoScalingGroup.
dsauAutoScalingGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeScalingActivities
    -> f DescribeScalingActivities
dsauAutoScalingGroupName f x =
    (\y -> x { _dsauAutoScalingGroupName = y })
       <$> f (_dsauAutoScalingGroupName x)
{-# INLINE dsauAutoScalingGroupName #-}

-- | A string that marks the start of the next batch of returned results for
-- pagination.
dsauNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeScalingActivities
    -> f DescribeScalingActivities
dsauNextToken f x =
    (\y -> x { _dsauNextToken = y })
       <$> f (_dsauNextToken x)
{-# INLINE dsauNextToken #-}

instance ToQuery DescribeScalingActivities where
    toQuery = genericQuery def

data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { _avActivities :: [Activity]
      -- ^ A list of the requested scaling activities.
    , _avNextToken :: Maybe Text
      -- ^ Acts as a paging mechanism for large result sets. Set to a
      -- non-empty string if there are additional results waiting to be
      -- returned. Pass this in to subsequent calls to return additional
      -- results.
    } deriving (Show, Generic)

-- | A list of the requested scaling activities.
avActivities
    :: Functor f
    => ([Activity]
    -> f ([Activity]))
    -> DescribeScalingActivitiesResponse
    -> f DescribeScalingActivitiesResponse
avActivities f x =
    (\y -> x { _avActivities = y })
       <$> f (_avActivities x)
{-# INLINE avActivities #-}

-- | Acts as a paging mechanism for large result sets. Set to a non-empty string
-- if there are additional results waiting to be returned. Pass this in to
-- subsequent calls to return additional results.
avNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeScalingActivitiesResponse
    -> f DescribeScalingActivitiesResponse
avNextToken f x =
    (\y -> x { _avNextToken = y })
       <$> f (_avNextToken x)
{-# INLINE avNextToken #-}

instance FromXML DescribeScalingActivitiesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScalingActivities where
    type Sv DescribeScalingActivities = AutoScaling
    type Rs DescribeScalingActivities = DescribeScalingActivitiesResponse

    request = post "DescribeScalingActivities"
    response _ = xmlResponse

instance AWSPager DescribeScalingActivities where
    next rq rs = (\x -> rq { _dsauNextToken = Just x })
        <$> (_avNextToken rs)

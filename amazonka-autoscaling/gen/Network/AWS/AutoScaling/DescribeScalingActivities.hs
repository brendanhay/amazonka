{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
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

-- | Describes one or more scaling activities for the specified Auto Scaling
-- group. If you omit the @ActivityIds@, the call returns all activities
-- from the past six weeks. Activities are sorted by the start time.
-- Activities still in progress appear first on the list.
--
-- You can specify a maximum number of items to be returned with a single
-- call. If there are more items to return, the call returns a token. To
-- get the next set of items, repeat the call with the returned token in
-- the @NextToken@ parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingActivities.html>
module Network.AWS.AutoScaling.DescribeScalingActivities
    (
    -- * Request
      DescribeScalingActivities
    -- ** Request constructor
    , describeScalingActivities
    -- ** Request lenses
    , dsa1NextToken
    , dsa1MaxRecords
    , dsa1AutoScalingGroupName
    , dsa1ActivityIds

    -- * Response
    , DescribeScalingActivitiesResponse
    -- ** Response constructor
    , describeScalingActivitiesResponse
    -- ** Response lenses
    , dNextToken
    , dActivities
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'describeScalingActivities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsa1NextToken'
--
-- * 'dsa1MaxRecords'
--
-- * 'dsa1AutoScalingGroupName'
--
-- * 'dsa1ActivityIds'
data DescribeScalingActivities = DescribeScalingActivities'{_dsa1NextToken :: Maybe Text, _dsa1MaxRecords :: Maybe Int, _dsa1AutoScalingGroupName :: Maybe Text, _dsa1ActivityIds :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeScalingActivities' smart constructor.
describeScalingActivities :: DescribeScalingActivities
describeScalingActivities = DescribeScalingActivities'{_dsa1NextToken = Nothing, _dsa1MaxRecords = Nothing, _dsa1AutoScalingGroupName = Nothing, _dsa1ActivityIds = Nothing};

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dsa1NextToken :: Lens' DescribeScalingActivities (Maybe Text)
dsa1NextToken = lens _dsa1NextToken (\ s a -> s{_dsa1NextToken = a});

-- | The maximum number of items to return with this call.
dsa1MaxRecords :: Lens' DescribeScalingActivities (Maybe Int)
dsa1MaxRecords = lens _dsa1MaxRecords (\ s a -> s{_dsa1MaxRecords = a});

-- | The name of the group.
dsa1AutoScalingGroupName :: Lens' DescribeScalingActivities (Maybe Text)
dsa1AutoScalingGroupName = lens _dsa1AutoScalingGroupName (\ s a -> s{_dsa1AutoScalingGroupName = a});

-- | A list containing the activity IDs of the desired scaling activities. If
-- this list is omitted, all activities are described. If an
-- @AutoScalingGroupName@ is provided, the results are limited to that
-- group. The list of requested activities cannot contain more than 50
-- items. If unknown activities are requested, they are ignored with no
-- error.
dsa1ActivityIds :: Lens' DescribeScalingActivities (Maybe [Text])
dsa1ActivityIds = lens _dsa1ActivityIds (\ s a -> s{_dsa1ActivityIds = a});

instance AWSRequest DescribeScalingActivities where
        type Sv DescribeScalingActivities = AutoScaling
        type Rs DescribeScalingActivities =
             DescribeScalingActivitiesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeScalingActivitiesResult"
              (\ s h x ->
                 DescribeScalingActivitiesResponse' <$>
                   x .@? "NextToken" <*>
                     (x .@? "Activities" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders DescribeScalingActivities where
        toHeaders = const mempty

instance ToPath DescribeScalingActivities where
        toPath = const "/"

instance ToQuery DescribeScalingActivities where
        toQuery DescribeScalingActivities'{..}
          = mconcat
              ["Action" =:
                 ("DescribeScalingActivities" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dsa1NextToken,
               "MaxRecords" =: _dsa1MaxRecords,
               "AutoScalingGroupName" =: _dsa1AutoScalingGroupName,
               "ActivityIds" =: "member" =: _dsa1ActivityIds]

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dNextToken'
--
-- * 'dActivities'
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'{_dNextToken :: Maybe Text, _dActivities :: [Activity]} deriving (Eq, Read, Show)

-- | 'DescribeScalingActivitiesResponse' smart constructor.
describeScalingActivitiesResponse :: DescribeScalingActivitiesResponse
describeScalingActivitiesResponse = DescribeScalingActivitiesResponse'{_dNextToken = Nothing, _dActivities = mempty};

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | The scaling activities.
dActivities :: Lens' DescribeScalingActivitiesResponse [Activity]
dActivities = lens _dActivities (\ s a -> s{_dActivities = a});

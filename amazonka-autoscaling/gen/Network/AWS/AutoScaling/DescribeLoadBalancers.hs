{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancers
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

-- | Describes the load balancers for the specified Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLoadBalancers.html>
module Network.AWS.AutoScaling.DescribeLoadBalancers
    (
    -- * Request
      DescribeLoadBalancers
    -- ** Request constructor
    , describeLoadBalancers
    -- ** Request lenses
    , dlbNextToken
    , dlbMaxRecords
    , dlbAutoScalingGroupName

    -- * Response
    , DescribeLoadBalancersResponse
    -- ** Response constructor
    , describeLoadBalancersResponse
    -- ** Response lenses
    , dlbrLoadBalancers
    , dlbrNextToken
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbNextToken'
--
-- * 'dlbMaxRecords'
--
-- * 'dlbAutoScalingGroupName'
data DescribeLoadBalancers = DescribeLoadBalancers'{_dlbNextToken :: Maybe Text, _dlbMaxRecords :: Maybe Int, _dlbAutoScalingGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancers' smart constructor.
describeLoadBalancers :: Text -> DescribeLoadBalancers
describeLoadBalancers pAutoScalingGroupName = DescribeLoadBalancers'{_dlbNextToken = Nothing, _dlbMaxRecords = Nothing, _dlbAutoScalingGroupName = pAutoScalingGroupName};

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dlbNextToken :: Lens' DescribeLoadBalancers (Maybe Text)
dlbNextToken = lens _dlbNextToken (\ s a -> s{_dlbNextToken = a});

-- | The maximum number of items to return with this call.
dlbMaxRecords :: Lens' DescribeLoadBalancers (Maybe Int)
dlbMaxRecords = lens _dlbMaxRecords (\ s a -> s{_dlbMaxRecords = a});

-- | The name of the group.
dlbAutoScalingGroupName :: Lens' DescribeLoadBalancers Text
dlbAutoScalingGroupName = lens _dlbAutoScalingGroupName (\ s a -> s{_dlbAutoScalingGroupName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeLoadBalancers where
        type Sv DescribeLoadBalancers = AutoScaling
        type Rs DescribeLoadBalancers =
             DescribeLoadBalancersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeLoadBalancersResult"
              (\ s h x ->
                 DescribeLoadBalancersResponse' <$>
                   (x .@? "LoadBalancers" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken"))

instance ToHeaders DescribeLoadBalancers where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancers where
        toPath = const "/"

instance ToQuery DescribeLoadBalancers where
        toQuery DescribeLoadBalancers'{..}
          = mconcat
              ["Action" =: ("DescribeLoadBalancers" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dlbNextToken,
               "MaxRecords" =: _dlbMaxRecords,
               "AutoScalingGroupName" =: _dlbAutoScalingGroupName]

-- | /See:/ 'describeLoadBalancersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbrLoadBalancers'
--
-- * 'dlbrNextToken'
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'{_dlbrLoadBalancers :: Maybe [LoadBalancerState], _dlbrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancersResponse' smart constructor.
describeLoadBalancersResponse :: DescribeLoadBalancersResponse
describeLoadBalancersResponse = DescribeLoadBalancersResponse'{_dlbrLoadBalancers = Nothing, _dlbrNextToken = Nothing};

-- | The load balancers.
dlbrLoadBalancers :: Lens' DescribeLoadBalancersResponse [LoadBalancerState]
dlbrLoadBalancers = lens _dlbrLoadBalancers (\ s a -> s{_dlbrLoadBalancers = a}) . _Default;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dlbrNextToken :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrNextToken = lens _dlbrNextToken (\ s a -> s{_dlbrNextToken = a});

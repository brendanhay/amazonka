{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.DescribeLoadBalancers
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

-- | Describes the specified the load balancers. If no load balancers are
-- specified, the call describes all of your load balancers.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancers.html>
module Network.AWS.ELB.DescribeLoadBalancers
    (
    -- * Request
      DescribeLoadBalancers
    -- ** Request constructor
    , describeLoadBalancers
    -- ** Request lenses
    , dlbMarker
    , dlbPageSize
    , dlbLoadBalancerNames

    -- * Response
    , DescribeLoadBalancersResponse
    -- ** Response constructor
    , describeLoadBalancersResponse
    -- ** Response lenses
    , dlbrLoadBalancerDescriptions
    , dlbrNextMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ELB.Types

-- | /See:/ 'describeLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbMarker'
--
-- * 'dlbPageSize'
--
-- * 'dlbLoadBalancerNames'
data DescribeLoadBalancers = DescribeLoadBalancers'{_dlbMarker :: Maybe Text, _dlbPageSize :: Maybe Nat, _dlbLoadBalancerNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancers' smart constructor.
describeLoadBalancers :: DescribeLoadBalancers
describeLoadBalancers = DescribeLoadBalancers'{_dlbMarker = Nothing, _dlbPageSize = Nothing, _dlbLoadBalancerNames = Nothing};

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
dlbMarker :: Lens' DescribeLoadBalancers (Maybe Text)
dlbMarker = lens _dlbMarker (\ s a -> s{_dlbMarker = a});

-- | The maximum number of results to return with this call (a number from 1
-- to 400). The default is 400.
dlbPageSize :: Lens' DescribeLoadBalancers (Maybe Natural)
dlbPageSize = lens _dlbPageSize (\ s a -> s{_dlbPageSize = a}) . mapping _Nat;

-- | The names of the load balancers.
dlbLoadBalancerNames :: Lens' DescribeLoadBalancers [Text]
dlbLoadBalancerNames = lens _dlbLoadBalancerNames (\ s a -> s{_dlbLoadBalancerNames = a}) . _Default;

instance AWSRequest DescribeLoadBalancers where
        type Sv DescribeLoadBalancers = ELB
        type Rs DescribeLoadBalancers =
             DescribeLoadBalancersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeLoadBalancersResult"
              (\ s h x ->
                 DescribeLoadBalancersResponse' <$>
                   (x .@? "LoadBalancerDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextMarker"))

instance ToHeaders DescribeLoadBalancers where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancers where
        toPath = const "/"

instance ToQuery DescribeLoadBalancers where
        toQuery DescribeLoadBalancers'{..}
          = mconcat
              ["Action" =: ("DescribeLoadBalancers" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "Marker" =: _dlbMarker, "PageSize" =: _dlbPageSize,
               "LoadBalancerNames" =:
                 toQuery
                   (toQueryList "member" <$> _dlbLoadBalancerNames)]

-- | /See:/ 'describeLoadBalancersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbrLoadBalancerDescriptions'
--
-- * 'dlbrNextMarker'
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'{_dlbrLoadBalancerDescriptions :: Maybe [LoadBalancerDescription], _dlbrNextMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancersResponse' smart constructor.
describeLoadBalancersResponse :: DescribeLoadBalancersResponse
describeLoadBalancersResponse = DescribeLoadBalancersResponse'{_dlbrLoadBalancerDescriptions = Nothing, _dlbrNextMarker = Nothing};

-- | Information about the load balancers.
dlbrLoadBalancerDescriptions :: Lens' DescribeLoadBalancersResponse [LoadBalancerDescription]
dlbrLoadBalancerDescriptions = lens _dlbrLoadBalancerDescriptions (\ s a -> s{_dlbrLoadBalancerDescriptions = a}) . _Default;

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
dlbrNextMarker :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrNextMarker = lens _dlbrNextMarker (\ s a -> s{_dlbrNextMarker = a});

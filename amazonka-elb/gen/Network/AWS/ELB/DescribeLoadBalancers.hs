{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified the load balancers. If no load balancers are
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
    , dlbrsLoadBalancerDescriptions
    , dlbrsNextMarker
    , dlbrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbMarker'
--
-- * 'dlbPageSize'
--
-- * 'dlbLoadBalancerNames'
data DescribeLoadBalancers = DescribeLoadBalancers'
    { _dlbMarker            :: !(Maybe Text)
    , _dlbPageSize          :: !(Maybe Nat)
    , _dlbLoadBalancerNames :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancers' smart constructor.
describeLoadBalancers :: DescribeLoadBalancers
describeLoadBalancers =
    DescribeLoadBalancers'
    { _dlbMarker = Nothing
    , _dlbPageSize = Nothing
    , _dlbLoadBalancerNames = Nothing
    }

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

instance AWSPager DescribeLoadBalancers where
        page rq rs
          | stop (rs ^. dlbrsNextMarker) = Nothing
          | stop (rs ^. dlbrsLoadBalancerDescriptions) =
            Nothing
          | otherwise =
            Just $ rq & dlbMarker .~ rs ^. dlbrsNextMarker

instance AWSRequest DescribeLoadBalancers where
        type Sv DescribeLoadBalancers = ELB
        type Rs DescribeLoadBalancers =
             DescribeLoadBalancersResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeLoadBalancersResult"
              (\ s h x ->
                 DescribeLoadBalancersResponse' <$>
                   (x .@? "LoadBalancerDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextMarker")
                     <*> (pure (fromEnum s)))

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
-- * 'dlbrsLoadBalancerDescriptions'
--
-- * 'dlbrsNextMarker'
--
-- * 'dlbrsStatus'
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
    { _dlbrsLoadBalancerDescriptions :: !(Maybe [LoadBalancerDescription])
    , _dlbrsNextMarker               :: !(Maybe Text)
    , _dlbrsStatus                   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancersResponse' smart constructor.
describeLoadBalancersResponse :: Int -> DescribeLoadBalancersResponse
describeLoadBalancersResponse pStatus_ =
    DescribeLoadBalancersResponse'
    { _dlbrsLoadBalancerDescriptions = Nothing
    , _dlbrsNextMarker = Nothing
    , _dlbrsStatus = pStatus_
    }

-- | Information about the load balancers.
dlbrsLoadBalancerDescriptions :: Lens' DescribeLoadBalancersResponse [LoadBalancerDescription]
dlbrsLoadBalancerDescriptions = lens _dlbrsLoadBalancerDescriptions (\ s a -> s{_dlbrsLoadBalancerDescriptions = a}) . _Default;

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
dlbrsNextMarker :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrsNextMarker = lens _dlbrsNextMarker (\ s a -> s{_dlbrsNextMarker = a});

-- | FIXME: Undocumented member.
dlbrsStatus :: Lens' DescribeLoadBalancersResponse Int
dlbrsStatus = lens _dlbrsStatus (\ s a -> s{_dlbrsStatus = a});

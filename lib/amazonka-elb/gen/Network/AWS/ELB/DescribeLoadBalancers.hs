{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified the load balancers. If no load balancers are specified, the call describes all of your load balancers.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ELB.DescribeLoadBalancers
    (
    -- * Creating a Request
      describeLoadBalancers
    , DescribeLoadBalancers
    -- * Request Lenses
    , dlbMarker
    , dlbPageSize
    , dlbLoadBalancerNames

    -- * Destructuring the Response
    , describeLoadBalancersResponse
    , DescribeLoadBalancersResponse
    -- * Response Lenses
    , dlbrsLoadBalancerDescriptions
    , dlbrsNextMarker
    , dlbrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeLoadBalancers.
--
--
--
-- /See:/ 'describeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { _dlbMarker            :: !(Maybe Text)
  , _dlbPageSize          :: !(Maybe Nat)
  , _dlbLoadBalancerNames :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dlbPageSize' - The maximum number of results to return with this call (a number from 1 to 400). The default is 400.
--
-- * 'dlbLoadBalancerNames' - The names of the load balancers.
describeLoadBalancers
    :: DescribeLoadBalancers
describeLoadBalancers =
  DescribeLoadBalancers'
    { _dlbMarker = Nothing
    , _dlbPageSize = Nothing
    , _dlbLoadBalancerNames = Nothing
    }


-- | The marker for the next set of results. (You received this marker from a previous call.)
dlbMarker :: Lens' DescribeLoadBalancers (Maybe Text)
dlbMarker = lens _dlbMarker (\ s a -> s{_dlbMarker = a})

-- | The maximum number of results to return with this call (a number from 1 to 400). The default is 400.
dlbPageSize :: Lens' DescribeLoadBalancers (Maybe Natural)
dlbPageSize = lens _dlbPageSize (\ s a -> s{_dlbPageSize = a}) . mapping _Nat

-- | The names of the load balancers.
dlbLoadBalancerNames :: Lens' DescribeLoadBalancers [Text]
dlbLoadBalancerNames = lens _dlbLoadBalancerNames (\ s a -> s{_dlbLoadBalancerNames = a}) . _Default . _Coerce

instance AWSPager DescribeLoadBalancers where
        page rq rs
          | stop (rs ^. dlbrsNextMarker) = Nothing
          | stop (rs ^. dlbrsLoadBalancerDescriptions) =
            Nothing
          | otherwise =
            Just $ rq & dlbMarker .~ rs ^. dlbrsNextMarker

instance AWSRequest DescribeLoadBalancers where
        type Rs DescribeLoadBalancers =
             DescribeLoadBalancersResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "DescribeLoadBalancersResult"
              (\ s h x ->
                 DescribeLoadBalancersResponse' <$>
                   (x .@? "LoadBalancerDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLoadBalancers where

instance NFData DescribeLoadBalancers where

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

-- | Contains the parameters for DescribeLoadBalancers.
--
--
--
-- /See:/ 'describeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { _dlbrsLoadBalancerDescriptions :: !(Maybe [LoadBalancerDescription])
  , _dlbrsNextMarker               :: !(Maybe Text)
  , _dlbrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBalancersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbrsLoadBalancerDescriptions' - Information about the load balancers.
--
-- * 'dlbrsNextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dlbrsResponseStatus' - -- | The response status code.
describeLoadBalancersResponse
    :: Int -- ^ 'dlbrsResponseStatus'
    -> DescribeLoadBalancersResponse
describeLoadBalancersResponse pResponseStatus_ =
  DescribeLoadBalancersResponse'
    { _dlbrsLoadBalancerDescriptions = Nothing
    , _dlbrsNextMarker = Nothing
    , _dlbrsResponseStatus = pResponseStatus_
    }


-- | Information about the load balancers.
dlbrsLoadBalancerDescriptions :: Lens' DescribeLoadBalancersResponse [LoadBalancerDescription]
dlbrsLoadBalancerDescriptions = lens _dlbrsLoadBalancerDescriptions (\ s a -> s{_dlbrsLoadBalancerDescriptions = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dlbrsNextMarker :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrsNextMarker = lens _dlbrsNextMarker (\ s a -> s{_dlbrsNextMarker = a})

-- | -- | The response status code.
dlbrsResponseStatus :: Lens' DescribeLoadBalancersResponse Int
dlbrsResponseStatus = lens _dlbrsResponseStatus (\ s a -> s{_dlbrsResponseStatus = a})

instance NFData DescribeLoadBalancersResponse where

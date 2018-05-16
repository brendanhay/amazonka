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
-- Module      : Network.AWS.ELBv2.DescribeListeners
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified listeners or the listeners for the specified Application Load Balancer or Network Load Balancer. You must specify either a load balancer or one or more listeners.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeListeners
    (
    -- * Creating a Request
      describeListeners
    , DescribeListeners
    -- * Request Lenses
    , dlListenerARNs
    , dlLoadBalancerARN
    , dlMarker
    , dlPageSize

    -- * Destructuring the Response
    , describeListenersResponse
    , DescribeListenersResponse
    -- * Response Lenses
    , dlsrsNextMarker
    , dlsrsListeners
    , dlsrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeListeners' smart constructor.
data DescribeListeners = DescribeListeners'
  { _dlListenerARNs    :: !(Maybe [Text])
  , _dlLoadBalancerARN :: !(Maybe Text)
  , _dlMarker          :: !(Maybe Text)
  , _dlPageSize        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeListeners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlListenerARNs' - The Amazon Resource Names (ARN) of the listeners.
--
-- * 'dlLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'dlMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dlPageSize' - The maximum number of results to return with this call.
describeListeners
    :: DescribeListeners
describeListeners =
  DescribeListeners'
    { _dlListenerARNs = Nothing
    , _dlLoadBalancerARN = Nothing
    , _dlMarker = Nothing
    , _dlPageSize = Nothing
    }


-- | The Amazon Resource Names (ARN) of the listeners.
dlListenerARNs :: Lens' DescribeListeners [Text]
dlListenerARNs = lens _dlListenerARNs (\ s a -> s{_dlListenerARNs = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
dlLoadBalancerARN :: Lens' DescribeListeners (Maybe Text)
dlLoadBalancerARN = lens _dlLoadBalancerARN (\ s a -> s{_dlLoadBalancerARN = a})

-- | The marker for the next set of results. (You received this marker from a previous call.)
dlMarker :: Lens' DescribeListeners (Maybe Text)
dlMarker = lens _dlMarker (\ s a -> s{_dlMarker = a})

-- | The maximum number of results to return with this call.
dlPageSize :: Lens' DescribeListeners (Maybe Natural)
dlPageSize = lens _dlPageSize (\ s a -> s{_dlPageSize = a}) . mapping _Nat

instance AWSPager DescribeListeners where
        page rq rs
          | stop (rs ^. dlsrsNextMarker) = Nothing
          | stop (rs ^. dlsrsListeners) = Nothing
          | otherwise =
            Just $ rq & dlMarker .~ rs ^. dlsrsNextMarker

instance AWSRequest DescribeListeners where
        type Rs DescribeListeners = DescribeListenersResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DescribeListenersResult"
              (\ s h x ->
                 DescribeListenersResponse' <$>
                   (x .@? "NextMarker") <*>
                     (x .@? "Listeners" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeListeners where

instance NFData DescribeListeners where

instance ToHeaders DescribeListeners where
        toHeaders = const mempty

instance ToPath DescribeListeners where
        toPath = const "/"

instance ToQuery DescribeListeners where
        toQuery DescribeListeners'{..}
          = mconcat
              ["Action" =: ("DescribeListeners" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "ListenerArns" =:
                 toQuery (toQueryList "member" <$> _dlListenerARNs),
               "LoadBalancerArn" =: _dlLoadBalancerARN,
               "Marker" =: _dlMarker, "PageSize" =: _dlPageSize]

-- | /See:/ 'describeListenersResponse' smart constructor.
data DescribeListenersResponse = DescribeListenersResponse'
  { _dlsrsNextMarker     :: !(Maybe Text)
  , _dlsrsListeners      :: !(Maybe [Listener])
  , _dlsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeListenersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsrsNextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dlsrsListeners' - Information about the listeners.
--
-- * 'dlsrsResponseStatus' - -- | The response status code.
describeListenersResponse
    :: Int -- ^ 'dlsrsResponseStatus'
    -> DescribeListenersResponse
describeListenersResponse pResponseStatus_ =
  DescribeListenersResponse'
    { _dlsrsNextMarker = Nothing
    , _dlsrsListeners = Nothing
    , _dlsrsResponseStatus = pResponseStatus_
    }


-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dlsrsNextMarker :: Lens' DescribeListenersResponse (Maybe Text)
dlsrsNextMarker = lens _dlsrsNextMarker (\ s a -> s{_dlsrsNextMarker = a})

-- | Information about the listeners.
dlsrsListeners :: Lens' DescribeListenersResponse [Listener]
dlsrsListeners = lens _dlsrsListeners (\ s a -> s{_dlsrsListeners = a}) . _Default . _Coerce

-- | -- | The response status code.
dlsrsResponseStatus :: Lens' DescribeListenersResponse Int
dlsrsResponseStatus = lens _dlsrsResponseStatus (\ s a -> s{_dlsrsResponseStatus = a})

instance NFData DescribeListenersResponse where

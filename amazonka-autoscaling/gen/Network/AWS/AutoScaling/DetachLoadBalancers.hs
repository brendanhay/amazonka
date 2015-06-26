{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DetachLoadBalancers
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

-- | Removes one or more load balancers from the specified Auto Scaling
-- group.
--
-- When you detach a load balancer, it enters the @Removing@ state while
-- deregistering the instances in the group. When all instances are
-- deregistered, then you can no longer describe the load balancer using
-- DescribeLoadBalancers. Note that the instances remain running.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DetachLoadBalancers.html>
module Network.AWS.AutoScaling.DetachLoadBalancers
    (
    -- * Request
      DetachLoadBalancers
    -- ** Request constructor
    , detachLoadBalancers
    -- ** Request lenses
    , detAutoScalingGroupName
    , detLoadBalancerNames

    -- * Response
    , DetachLoadBalancersResponse
    -- ** Response constructor
    , detachLoadBalancersResponse
    -- ** Response lenses
    , detStatusCode
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detAutoScalingGroupName'
--
-- * 'detLoadBalancerNames'
data DetachLoadBalancers = DetachLoadBalancers'{_detAutoScalingGroupName :: Maybe Text, _detLoadBalancerNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DetachLoadBalancers' smart constructor.
detachLoadBalancers :: DetachLoadBalancers
detachLoadBalancers = DetachLoadBalancers'{_detAutoScalingGroupName = Nothing, _detLoadBalancerNames = Nothing};

-- | The name of the group.
detAutoScalingGroupName :: Lens' DetachLoadBalancers (Maybe Text)
detAutoScalingGroupName = lens _detAutoScalingGroupName (\ s a -> s{_detAutoScalingGroupName = a});

-- | One or more load balancer names.
detLoadBalancerNames :: Lens' DetachLoadBalancers [Text]
detLoadBalancerNames = lens _detLoadBalancerNames (\ s a -> s{_detLoadBalancerNames = a}) . _Default;

instance AWSRequest DetachLoadBalancers where
        type Sv DetachLoadBalancers = AutoScaling
        type Rs DetachLoadBalancers =
             DetachLoadBalancersResponse
        request = post
        response
          = receiveXMLWrapper "DetachLoadBalancersResult"
              (\ s h x ->
                 DetachLoadBalancersResponse' <$> (pure (fromEnum s)))

instance ToHeaders DetachLoadBalancers where
        toHeaders = const mempty

instance ToPath DetachLoadBalancers where
        toPath = const "/"

instance ToQuery DetachLoadBalancers where
        toQuery DetachLoadBalancers'{..}
          = mconcat
              ["Action" =: ("DetachLoadBalancers" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _detAutoScalingGroupName,
               "LoadBalancerNames" =:
                 toQuery
                   (toQueryList "member" <$> _detLoadBalancerNames)]

-- | /See:/ 'detachLoadBalancersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detStatusCode'
newtype DetachLoadBalancersResponse = DetachLoadBalancersResponse'{_detStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DetachLoadBalancersResponse' smart constructor.
detachLoadBalancersResponse :: Int -> DetachLoadBalancersResponse
detachLoadBalancersResponse pStatusCode = DetachLoadBalancersResponse'{_detStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
detStatusCode :: Lens' DetachLoadBalancersResponse Int
detStatusCode = lens _detStatusCode (\ s a -> s{_detStatusCode = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more load balancers from the specified Auto Scaling
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
    , detStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detAutoScalingGroupName'
--
-- * 'detLoadBalancerNames'
data DetachLoadBalancers = DetachLoadBalancers'
    { _detAutoScalingGroupName :: !(Maybe Text)
    , _detLoadBalancerNames    :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachLoadBalancers' smart constructor.
detachLoadBalancers :: DetachLoadBalancers
detachLoadBalancers =
    DetachLoadBalancers'
    { _detAutoScalingGroupName = Nothing
    , _detLoadBalancerNames = Nothing
    }

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
-- * 'detStatus'
newtype DetachLoadBalancersResponse = DetachLoadBalancersResponse'
    { _detStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachLoadBalancersResponse' smart constructor.
detachLoadBalancersResponse :: Int -> DetachLoadBalancersResponse
detachLoadBalancersResponse pStatus =
    DetachLoadBalancersResponse'
    { _detStatus = pStatus
    }

-- | FIXME: Undocumented member.
detStatus :: Lens' DetachLoadBalancersResponse Int
detStatus = lens _detStatus (\ s a -> s{_detStatus = a});

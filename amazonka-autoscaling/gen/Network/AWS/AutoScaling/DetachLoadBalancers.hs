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
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more load balancers from the specified Auto Scaling
-- group.
--
-- When you detach a load balancer, it enters the 'Removing' state while
-- deregistering the instances in the group. When all instances are
-- deregistered, then you can no longer describe the load balancer using
-- DescribeLoadBalancers. Note that the instances remain running.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DetachLoadBalancers.html AWS API Reference> for DetachLoadBalancers.
module Network.AWS.AutoScaling.DetachLoadBalancers
    (
    -- * Creating a Request
      detachLoadBalancers
    , DetachLoadBalancers
    -- * Request Lenses
    , dAutoScalingGroupName
    , dLoadBalancerNames

    -- * Destructuring the Response
    , detachLoadBalancersResponse
    , DetachLoadBalancersResponse
    -- * Response Lenses
    , dlbsrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachLoadBalancers' smart constructor.
data DetachLoadBalancers = DetachLoadBalancers'
    { _dAutoScalingGroupName :: !(Maybe Text)
    , _dLoadBalancerNames    :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachLoadBalancers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAutoScalingGroupName'
--
-- * 'dLoadBalancerNames'
detachLoadBalancers
    :: DetachLoadBalancers
detachLoadBalancers =
    DetachLoadBalancers'
    { _dAutoScalingGroupName = Nothing
    , _dLoadBalancerNames = Nothing
    }

-- | The name of the group.
dAutoScalingGroupName :: Lens' DetachLoadBalancers (Maybe Text)
dAutoScalingGroupName = lens _dAutoScalingGroupName (\ s a -> s{_dAutoScalingGroupName = a});

-- | One or more load balancer names.
dLoadBalancerNames :: Lens' DetachLoadBalancers [Text]
dLoadBalancerNames = lens _dLoadBalancerNames (\ s a -> s{_dLoadBalancerNames = a}) . _Default . _Coerce;

instance AWSRequest DetachLoadBalancers where
        type Sv DetachLoadBalancers = AutoScaling
        type Rs DetachLoadBalancers =
             DetachLoadBalancersResponse
        request = postQuery
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
               "AutoScalingGroupName" =: _dAutoScalingGroupName,
               "LoadBalancerNames" =:
                 toQuery
                   (toQueryList "member" <$> _dLoadBalancerNames)]

-- | /See:/ 'detachLoadBalancersResponse' smart constructor.
newtype DetachLoadBalancersResponse = DetachLoadBalancersResponse'
    { _dlbsrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachLoadBalancersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbsrsStatus'
detachLoadBalancersResponse
    :: Int -- ^ 'dlbsrsStatus'
    -> DetachLoadBalancersResponse
detachLoadBalancersResponse pStatus_ =
    DetachLoadBalancersResponse'
    { _dlbsrsStatus = pStatus_
    }

-- | The response status code.
dlbsrsStatus :: Lens' DetachLoadBalancersResponse Int
dlbsrsStatus = lens _dlbsrsStatus (\ s a -> s{_dlbsrsStatus = a});

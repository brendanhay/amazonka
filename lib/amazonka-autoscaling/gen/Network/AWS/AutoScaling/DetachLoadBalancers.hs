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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more Classic Load Balancers from the specified Auto Scaling group.
--
--
-- Note that this operation detaches only Classic Load Balancers. If you have Application Load Balancers, use 'DetachLoadBalancerTargetGroups' instead.
--
-- When you detach a load balancer, it enters the @Removing@ state while deregistering the instances in the group. When all instances are deregistered, then you can no longer describe the load balancer using 'DescribeLoadBalancers' . Note that the instances remain running.
--
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
    , dlbsrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachLoadBalancers' smart constructor.
data DetachLoadBalancers = DetachLoadBalancers'
  { _dAutoScalingGroupName :: !Text
  , _dLoadBalancerNames    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachLoadBalancers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'dLoadBalancerNames' - The names of the load balancers. You can specify up to 10 load balancers.
detachLoadBalancers
    :: Text -- ^ 'dAutoScalingGroupName'
    -> DetachLoadBalancers
detachLoadBalancers pAutoScalingGroupName_ =
  DetachLoadBalancers'
    { _dAutoScalingGroupName = pAutoScalingGroupName_
    , _dLoadBalancerNames = mempty
    }


-- | The name of the Auto Scaling group.
dAutoScalingGroupName :: Lens' DetachLoadBalancers Text
dAutoScalingGroupName = lens _dAutoScalingGroupName (\ s a -> s{_dAutoScalingGroupName = a})

-- | The names of the load balancers. You can specify up to 10 load balancers.
dLoadBalancerNames :: Lens' DetachLoadBalancers [Text]
dLoadBalancerNames = lens _dLoadBalancerNames (\ s a -> s{_dLoadBalancerNames = a}) . _Coerce

instance AWSRequest DetachLoadBalancers where
        type Rs DetachLoadBalancers =
             DetachLoadBalancersResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DetachLoadBalancersResult"
              (\ s h x ->
                 DetachLoadBalancersResponse' <$> (pure (fromEnum s)))

instance Hashable DetachLoadBalancers where

instance NFData DetachLoadBalancers where

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
                 toQueryList "member" _dLoadBalancerNames]

-- | /See:/ 'detachLoadBalancersResponse' smart constructor.
newtype DetachLoadBalancersResponse = DetachLoadBalancersResponse'
  { _dlbsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachLoadBalancersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbsrsResponseStatus' - -- | The response status code.
detachLoadBalancersResponse
    :: Int -- ^ 'dlbsrsResponseStatus'
    -> DetachLoadBalancersResponse
detachLoadBalancersResponse pResponseStatus_ =
  DetachLoadBalancersResponse' {_dlbsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dlbsrsResponseStatus :: Lens' DetachLoadBalancersResponse Int
dlbsrsResponseStatus = lens _dlbsrsResponseStatus (\ s a -> s{_dlbsrsResponseStatus = a})

instance NFData DetachLoadBalancersResponse where

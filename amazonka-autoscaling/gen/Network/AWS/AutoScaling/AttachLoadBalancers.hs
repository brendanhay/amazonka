{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.AttachLoadBalancers
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

-- | Attaches one or more load balancers to the specified Auto Scaling group.
--
-- To describe the load balancers for an Auto Scaling group, use
-- DescribeLoadBalancers. To detach the load balancer from the Auto Scaling
-- group, use DetachLoadBalancers.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/attach-load-balancer-asg.html Attach a Load Balancer to Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AttachLoadBalancers.html>
module Network.AWS.AutoScaling.AttachLoadBalancers
    (
    -- * Request
      AttachLoadBalancers
    -- ** Request constructor
    , attachLoadBalancers
    -- ** Request lenses
    , albLoadBalancerNames
    , albAutoScalingGroupName

    -- * Response
    , AttachLoadBalancersResponse
    -- ** Response constructor
    , attachLoadBalancersResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'attachLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'albLoadBalancerNames'
--
-- * 'albAutoScalingGroupName'
data AttachLoadBalancers = AttachLoadBalancers'{_albLoadBalancerNames :: [Text], _albAutoScalingGroupName :: Text} deriving (Eq, Read, Show)

-- | 'AttachLoadBalancers' smart constructor.
attachLoadBalancers :: Text -> AttachLoadBalancers
attachLoadBalancers pAutoScalingGroupName = AttachLoadBalancers'{_albLoadBalancerNames = mempty, _albAutoScalingGroupName = pAutoScalingGroupName};

-- | One or more load balancer names.
albLoadBalancerNames :: Lens' AttachLoadBalancers [Text]
albLoadBalancerNames = lens _albLoadBalancerNames (\ s a -> s{_albLoadBalancerNames = a});

-- | The name of the group.
albAutoScalingGroupName :: Lens' AttachLoadBalancers Text
albAutoScalingGroupName = lens _albAutoScalingGroupName (\ s a -> s{_albAutoScalingGroupName = a});

instance AWSRequest AttachLoadBalancers where
        type Sv AttachLoadBalancers = AutoScaling
        type Rs AttachLoadBalancers =
             AttachLoadBalancersResponse
        request = post
        response
          = receiveNullWrapper "AttachLoadBalancersResult"
              AttachLoadBalancersResponse'

instance ToHeaders AttachLoadBalancers where
        toHeaders = const mempty

instance ToPath AttachLoadBalancers where
        toPath = const "/"

instance ToQuery AttachLoadBalancers where
        toQuery AttachLoadBalancers'{..}
          = mconcat
              ["Action" =: ("AttachLoadBalancers" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "LoadBalancerNames" =:
                 "member" =: _albLoadBalancerNames,
               "AutoScalingGroupName" =: _albAutoScalingGroupName]

-- | /See:/ 'attachLoadBalancersResponse' smart constructor.
data AttachLoadBalancersResponse = AttachLoadBalancersResponse' deriving (Eq, Read, Show)

-- | 'AttachLoadBalancersResponse' smart constructor.
attachLoadBalancersResponse :: AttachLoadBalancersResponse
attachLoadBalancersResponse = AttachLoadBalancersResponse';

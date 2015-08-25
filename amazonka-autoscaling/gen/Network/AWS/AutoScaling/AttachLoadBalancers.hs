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
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more load balancers to the specified Auto Scaling group.
--
-- To describe the load balancers for an Auto Scaling group, use
-- DescribeLoadBalancers. To detach the load balancer from the Auto Scaling
-- group, use DetachLoadBalancers.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/attach-load-balancer-asg.html Attach a Load Balancer to Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AttachLoadBalancers.html AWS API Reference> for AttachLoadBalancers.
module Network.AWS.AutoScaling.AttachLoadBalancers
    (
    -- * Creating a Request
      attachLoadBalancers
    , AttachLoadBalancers
    -- * Request Lenses
    , albAutoScalingGroupName
    , albLoadBalancerNames

    -- * Destructuring the Response
    , attachLoadBalancersResponse
    , AttachLoadBalancersResponse
    -- * Response Lenses
    , albrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachLoadBalancers' smart constructor.
data AttachLoadBalancers = AttachLoadBalancers'
    { _albAutoScalingGroupName :: !(Maybe Text)
    , _albLoadBalancerNames    :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachLoadBalancers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albAutoScalingGroupName'
--
-- * 'albLoadBalancerNames'
attachLoadBalancers
    :: AttachLoadBalancers
attachLoadBalancers =
    AttachLoadBalancers'
    { _albAutoScalingGroupName = Nothing
    , _albLoadBalancerNames = Nothing
    }

-- | The name of the group.
albAutoScalingGroupName :: Lens' AttachLoadBalancers (Maybe Text)
albAutoScalingGroupName = lens _albAutoScalingGroupName (\ s a -> s{_albAutoScalingGroupName = a});

-- | One or more load balancer names.
albLoadBalancerNames :: Lens' AttachLoadBalancers [Text]
albLoadBalancerNames = lens _albLoadBalancerNames (\ s a -> s{_albLoadBalancerNames = a}) . _Default . _Coerce;

instance AWSRequest AttachLoadBalancers where
        type Rs AttachLoadBalancers =
             AttachLoadBalancersResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "AttachLoadBalancersResult"
              (\ s h x ->
                 AttachLoadBalancersResponse' <$> (pure (fromEnum s)))

instance ToHeaders AttachLoadBalancers where
        toHeaders = const mempty

instance ToPath AttachLoadBalancers where
        toPath = const "/"

instance ToQuery AttachLoadBalancers where
        toQuery AttachLoadBalancers'{..}
          = mconcat
              ["Action" =: ("AttachLoadBalancers" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _albAutoScalingGroupName,
               "LoadBalancerNames" =:
                 toQuery
                   (toQueryList "member" <$> _albLoadBalancerNames)]

-- | /See:/ 'attachLoadBalancersResponse' smart constructor.
newtype AttachLoadBalancersResponse = AttachLoadBalancersResponse'
    { _albrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachLoadBalancersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albrsStatus'
attachLoadBalancersResponse
    :: Int -- ^ 'albrsStatus'
    -> AttachLoadBalancersResponse
attachLoadBalancersResponse pStatus_ =
    AttachLoadBalancersResponse'
    { _albrsStatus = pStatus_
    }

-- | The response status code.
albrsStatus :: Lens' AttachLoadBalancersResponse Int
albrsStatus = lens _albrsStatus (\ s a -> s{_albrsStatus = a});

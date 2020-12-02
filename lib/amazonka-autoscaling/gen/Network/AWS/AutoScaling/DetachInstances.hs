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
-- Module      : Network.AWS.AutoScaling.DetachInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more instances from the specified Auto Scaling group.
--
--
-- After the instances are detached, you can manage them independent of the Auto Scaling group.
--
-- If you do not specify the option to decrement the desired capacity, Auto Scaling launches instances to replace the ones that are detached.
--
-- If there is a Classic Load Balancer attached to the Auto Scaling group, the instances are deregistered from the load balancer. If there are target groups attached to the Auto Scaling group, the instances are deregistered from the target groups.
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/detach-instance-asg.html Detach EC2 Instances from Your Auto Scaling Group> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.DetachInstances
    (
    -- * Creating a Request
      detachInstances
    , DetachInstances
    -- * Request Lenses
    , diInstanceIds
    , diAutoScalingGroupName
    , diShouldDecrementDesiredCapacity

    -- * Destructuring the Response
    , detachInstancesResponse
    , DetachInstancesResponse
    -- * Response Lenses
    , dirsActivities
    , dirsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachInstances' smart constructor.
data DetachInstances = DetachInstances'
  { _diInstanceIds                    :: !(Maybe [Text])
  , _diAutoScalingGroupName           :: !Text
  , _diShouldDecrementDesiredCapacity :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diInstanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- * 'diAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'diShouldDecrementDesiredCapacity' - Indicates whether the Auto Scaling group decrements the desired capacity value by the number of instances detached.
detachInstances
    :: Text -- ^ 'diAutoScalingGroupName'
    -> Bool -- ^ 'diShouldDecrementDesiredCapacity'
    -> DetachInstances
detachInstances pAutoScalingGroupName_ pShouldDecrementDesiredCapacity_ =
  DetachInstances'
    { _diInstanceIds = Nothing
    , _diAutoScalingGroupName = pAutoScalingGroupName_
    , _diShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
    }


-- | The IDs of the instances. You can specify up to 20 instances.
diInstanceIds :: Lens' DetachInstances [Text]
diInstanceIds = lens _diInstanceIds (\ s a -> s{_diInstanceIds = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
diAutoScalingGroupName :: Lens' DetachInstances Text
diAutoScalingGroupName = lens _diAutoScalingGroupName (\ s a -> s{_diAutoScalingGroupName = a})

-- | Indicates whether the Auto Scaling group decrements the desired capacity value by the number of instances detached.
diShouldDecrementDesiredCapacity :: Lens' DetachInstances Bool
diShouldDecrementDesiredCapacity = lens _diShouldDecrementDesiredCapacity (\ s a -> s{_diShouldDecrementDesiredCapacity = a})

instance AWSRequest DetachInstances where
        type Rs DetachInstances = DetachInstancesResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DetachInstancesResult"
              (\ s h x ->
                 DetachInstancesResponse' <$>
                   (x .@? "Activities" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DetachInstances where

instance NFData DetachInstances where

instance ToHeaders DetachInstances where
        toHeaders = const mempty

instance ToPath DetachInstances where
        toPath = const "/"

instance ToQuery DetachInstances where
        toQuery DetachInstances'{..}
          = mconcat
              ["Action" =: ("DetachInstances" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _diInstanceIds),
               "AutoScalingGroupName" =: _diAutoScalingGroupName,
               "ShouldDecrementDesiredCapacity" =:
                 _diShouldDecrementDesiredCapacity]

-- | /See:/ 'detachInstancesResponse' smart constructor.
data DetachInstancesResponse = DetachInstancesResponse'
  { _dirsActivities     :: !(Maybe [Activity])
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsActivities' - The activities related to detaching the instances from the Auto Scaling group.
--
-- * 'dirsResponseStatus' - -- | The response status code.
detachInstancesResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DetachInstancesResponse
detachInstancesResponse pResponseStatus_ =
  DetachInstancesResponse'
    {_dirsActivities = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | The activities related to detaching the instances from the Auto Scaling group.
dirsActivities :: Lens' DetachInstancesResponse [Activity]
dirsActivities = lens _dirsActivities (\ s a -> s{_dirsActivities = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DetachInstancesResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DetachInstancesResponse where

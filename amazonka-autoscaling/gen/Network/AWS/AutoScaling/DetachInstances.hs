{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more instances from the specified Auto Scaling group.
-- After the instances are detached, you can manage them independently from
-- the rest of the Auto Scaling group.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/detach-instance-asg.html Detach EC2 Instances from Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DetachInstances.html>
module Network.AWS.AutoScaling.DetachInstances
    (
    -- * Request
      DetachInstances
    -- ** Request constructor
    , detachInstances
    -- ** Request lenses
    , diInstanceIds
    , diAutoScalingGroupName
    , diShouldDecrementDesiredCapacity

    -- * Response
    , DetachInstancesResponse
    -- ** Response constructor
    , detachInstancesResponse
    -- ** Response lenses
    , dirsActivities
    , dirsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diInstanceIds'
--
-- * 'diAutoScalingGroupName'
--
-- * 'diShouldDecrementDesiredCapacity'
data DetachInstances = DetachInstances'
    { _diInstanceIds                    :: !(Maybe [Text])
    , _diAutoScalingGroupName           :: !Text
    , _diShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachInstances' smart constructor.
detachInstances :: Text -> Bool -> DetachInstances
detachInstances pAutoScalingGroupName_ pShouldDecrementDesiredCapacity_ =
    DetachInstances'
    { _diInstanceIds = Nothing
    , _diAutoScalingGroupName = pAutoScalingGroupName_
    , _diShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
    }

-- | One or more instance IDs.
diInstanceIds :: Lens' DetachInstances [Text]
diInstanceIds = lens _diInstanceIds (\ s a -> s{_diInstanceIds = a}) . _Default . _Coerce;

-- | The name of the group.
diAutoScalingGroupName :: Lens' DetachInstances Text
diAutoScalingGroupName = lens _diAutoScalingGroupName (\ s a -> s{_diAutoScalingGroupName = a});

-- | If @True@, the Auto Scaling group decrements the desired capacity value
-- by the number of instances detached.
diShouldDecrementDesiredCapacity :: Lens' DetachInstances Bool
diShouldDecrementDesiredCapacity = lens _diShouldDecrementDesiredCapacity (\ s a -> s{_diShouldDecrementDesiredCapacity = a});

instance AWSRequest DetachInstances where
        type Sv DetachInstances = AutoScaling
        type Rs DetachInstances = DetachInstancesResponse
        request = post
        response
          = receiveXMLWrapper "DetachInstancesResult"
              (\ s h x ->
                 DetachInstancesResponse' <$>
                   (x .@? "Activities" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirsActivities'
--
-- * 'dirsStatus'
data DetachInstancesResponse = DetachInstancesResponse'
    { _dirsActivities :: !(Maybe [Activity])
    , _dirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachInstancesResponse' smart constructor.
detachInstancesResponse :: Int -> DetachInstancesResponse
detachInstancesResponse pStatus_ =
    DetachInstancesResponse'
    { _dirsActivities = Nothing
    , _dirsStatus = pStatus_
    }

-- | The activities related to detaching the instances from the Auto Scaling
-- group.
dirsActivities :: Lens' DetachInstancesResponse [Activity]
dirsActivities = lens _dirsActivities (\ s a -> s{_dirsActivities = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
dirsStatus :: Lens' DetachInstancesResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});

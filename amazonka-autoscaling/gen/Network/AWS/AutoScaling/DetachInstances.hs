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
    , dirqInstanceIds
    , dirqAutoScalingGroupName
    , dirqShouldDecrementDesiredCapacity

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
-- * 'dirqInstanceIds'
--
-- * 'dirqAutoScalingGroupName'
--
-- * 'dirqShouldDecrementDesiredCapacity'
data DetachInstances = DetachInstances'
    { _dirqInstanceIds                    :: !(Maybe [Text])
    , _dirqAutoScalingGroupName           :: !Text
    , _dirqShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachInstances' smart constructor.
detachInstances :: Text -> Bool -> DetachInstances
detachInstances pAutoScalingGroupName pShouldDecrementDesiredCapacity =
    DetachInstances'
    { _dirqInstanceIds = Nothing
    , _dirqAutoScalingGroupName = pAutoScalingGroupName
    , _dirqShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity
    }

-- | One or more instance IDs.
dirqInstanceIds :: Lens' DetachInstances [Text]
dirqInstanceIds = lens _dirqInstanceIds (\ s a -> s{_dirqInstanceIds = a}) . _Default;

-- | The name of the group.
dirqAutoScalingGroupName :: Lens' DetachInstances Text
dirqAutoScalingGroupName = lens _dirqAutoScalingGroupName (\ s a -> s{_dirqAutoScalingGroupName = a});

-- | If @True@, the Auto Scaling group decrements the desired capacity value
-- by the number of instances detached.
dirqShouldDecrementDesiredCapacity :: Lens' DetachInstances Bool
dirqShouldDecrementDesiredCapacity = lens _dirqShouldDecrementDesiredCapacity (\ s a -> s{_dirqShouldDecrementDesiredCapacity = a});

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
                 toQuery (toQueryList "member" <$> _dirqInstanceIds),
               "AutoScalingGroupName" =: _dirqAutoScalingGroupName,
               "ShouldDecrementDesiredCapacity" =:
                 _dirqShouldDecrementDesiredCapacity]

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
detachInstancesResponse pStatus =
    DetachInstancesResponse'
    { _dirsActivities = Nothing
    , _dirsStatus = pStatus
    }

-- | The activities related to detaching the instances from the Auto Scaling
-- group.
dirsActivities :: Lens' DetachInstancesResponse [Activity]
dirsActivities = lens _dirsActivities (\ s a -> s{_dirsActivities = a}) . _Default;

-- | FIXME: Undocumented member.
dirsStatus :: Lens' DetachInstancesResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});

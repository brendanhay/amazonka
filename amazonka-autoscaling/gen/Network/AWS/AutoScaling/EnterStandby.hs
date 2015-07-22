{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.EnterStandby
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances into @Standby@ mode.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingInServiceState.html Auto Scaling InService State>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnterStandby.html>
module Network.AWS.AutoScaling.EnterStandby
    (
    -- * Request
      EnterStandby
    -- ** Request constructor
    , enterStandby
    -- ** Request lenses
    , esrqInstanceIds
    , esrqAutoScalingGroupName
    , esrqShouldDecrementDesiredCapacity

    -- * Response
    , EnterStandbyResponse
    -- ** Response constructor
    , enterStandbyResponse
    -- ** Response lenses
    , esrsActivities
    , esrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enterStandby' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esrqInstanceIds'
--
-- * 'esrqAutoScalingGroupName'
--
-- * 'esrqShouldDecrementDesiredCapacity'
data EnterStandby = EnterStandby'
    { _esrqInstanceIds                    :: !(Maybe [Text])
    , _esrqAutoScalingGroupName           :: !Text
    , _esrqShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnterStandby' smart constructor.
enterStandby :: Text -> Bool -> EnterStandby
enterStandby pAutoScalingGroupName_ pShouldDecrementDesiredCapacity_ =
    EnterStandby'
    { _esrqInstanceIds = Nothing
    , _esrqAutoScalingGroupName = pAutoScalingGroupName_
    , _esrqShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
    }

-- | One or more instances to move into @Standby@ mode. You must specify at
-- least one instance ID.
esrqInstanceIds :: Lens' EnterStandby [Text]
esrqInstanceIds = lens _esrqInstanceIds (\ s a -> s{_esrqInstanceIds = a}) . _Default;

-- | The name of the Auto Scaling group.
esrqAutoScalingGroupName :: Lens' EnterStandby Text
esrqAutoScalingGroupName = lens _esrqAutoScalingGroupName (\ s a -> s{_esrqAutoScalingGroupName = a});

-- | Specifies whether the instances moved to @Standby@ mode count as part of
-- the Auto Scaling group\'s desired capacity. If set, the desired capacity
-- for the Auto Scaling group decrements by the number of instances moved
-- to @Standby@ mode.
esrqShouldDecrementDesiredCapacity :: Lens' EnterStandby Bool
esrqShouldDecrementDesiredCapacity = lens _esrqShouldDecrementDesiredCapacity (\ s a -> s{_esrqShouldDecrementDesiredCapacity = a});

instance AWSRequest EnterStandby where
        type Sv EnterStandby = AutoScaling
        type Rs EnterStandby = EnterStandbyResponse
        request = post
        response
          = receiveXMLWrapper "EnterStandbyResult"
              (\ s h x ->
                 EnterStandbyResponse' <$>
                   (x .@? "Activities" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders EnterStandby where
        toHeaders = const mempty

instance ToPath EnterStandby where
        toPath = const "/"

instance ToQuery EnterStandby where
        toQuery EnterStandby'{..}
          = mconcat
              ["Action" =: ("EnterStandby" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _esrqInstanceIds),
               "AutoScalingGroupName" =: _esrqAutoScalingGroupName,
               "ShouldDecrementDesiredCapacity" =:
                 _esrqShouldDecrementDesiredCapacity]

-- | /See:/ 'enterStandbyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esrsActivities'
--
-- * 'esrsStatus'
data EnterStandbyResponse = EnterStandbyResponse'
    { _esrsActivities :: !(Maybe [Activity])
    , _esrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnterStandbyResponse' smart constructor.
enterStandbyResponse :: Int -> EnterStandbyResponse
enterStandbyResponse pStatus_ =
    EnterStandbyResponse'
    { _esrsActivities = Nothing
    , _esrsStatus = pStatus_
    }

-- | The activities related to moving instances into @Standby@ mode.
esrsActivities :: Lens' EnterStandbyResponse [Activity]
esrsActivities = lens _esrsActivities (\ s a -> s{_esrsActivities = a}) . _Default;

-- | FIXME: Undocumented member.
esrsStatus :: Lens' EnterStandbyResponse Int
esrsStatus = lens _esrsStatus (\ s a -> s{_esrsStatus = a});

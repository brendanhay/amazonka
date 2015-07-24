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
    , esInstanceIds
    , esAutoScalingGroupName
    , esShouldDecrementDesiredCapacity

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
-- * 'esInstanceIds'
--
-- * 'esAutoScalingGroupName'
--
-- * 'esShouldDecrementDesiredCapacity'
data EnterStandby = EnterStandby'
    { _esInstanceIds                    :: !(Maybe [Text])
    , _esAutoScalingGroupName           :: !Text
    , _esShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnterStandby' smart constructor.
enterStandby :: Text -> Bool -> EnterStandby
enterStandby pAutoScalingGroupName_ pShouldDecrementDesiredCapacity_ =
    EnterStandby'
    { _esInstanceIds = Nothing
    , _esAutoScalingGroupName = pAutoScalingGroupName_
    , _esShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
    }

-- | One or more instances to move into @Standby@ mode. You must specify at
-- least one instance ID.
esInstanceIds :: Lens' EnterStandby [Text]
esInstanceIds = lens _esInstanceIds (\ s a -> s{_esInstanceIds = a}) . _Default;

-- | The name of the Auto Scaling group.
esAutoScalingGroupName :: Lens' EnterStandby Text
esAutoScalingGroupName = lens _esAutoScalingGroupName (\ s a -> s{_esAutoScalingGroupName = a});

-- | Specifies whether the instances moved to @Standby@ mode count as part of
-- the Auto Scaling group\'s desired capacity. If set, the desired capacity
-- for the Auto Scaling group decrements by the number of instances moved
-- to @Standby@ mode.
esShouldDecrementDesiredCapacity :: Lens' EnterStandby Bool
esShouldDecrementDesiredCapacity = lens _esShouldDecrementDesiredCapacity (\ s a -> s{_esShouldDecrementDesiredCapacity = a});

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
                 toQuery (toQueryList "member" <$> _esInstanceIds),
               "AutoScalingGroupName" =: _esAutoScalingGroupName,
               "ShouldDecrementDesiredCapacity" =:
                 _esShouldDecrementDesiredCapacity]

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

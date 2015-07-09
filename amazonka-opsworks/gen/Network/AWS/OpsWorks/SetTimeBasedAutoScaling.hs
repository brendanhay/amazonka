{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetTimeBasedAutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Specify the time-based auto scaling configuration for a specified
-- instance. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_SetTimeBasedAutoScaling.html>
module Network.AWS.OpsWorks.SetTimeBasedAutoScaling
    (
    -- * Request
      SetTimeBasedAutoScaling
    -- ** Request constructor
    , setTimeBasedAutoScaling
    -- ** Request lenses
    , stbasAutoScalingSchedule
    , stbasInstanceId

    -- * Response
    , SetTimeBasedAutoScalingResponse
    -- ** Response constructor
    , setTimeBasedAutoScalingResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setTimeBasedAutoScaling' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stbasAutoScalingSchedule'
--
-- * 'stbasInstanceId'
data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling'
    { _stbasAutoScalingSchedule :: !(Maybe WeeklyAutoScalingSchedule)
    , _stbasInstanceId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTimeBasedAutoScaling' smart constructor.
setTimeBasedAutoScaling :: Text -> SetTimeBasedAutoScaling
setTimeBasedAutoScaling pInstanceId =
    SetTimeBasedAutoScaling'
    { _stbasAutoScalingSchedule = Nothing
    , _stbasInstanceId = pInstanceId
    }

-- | An @AutoScalingSchedule@ with the instance schedule.
stbasAutoScalingSchedule :: Lens' SetTimeBasedAutoScaling (Maybe WeeklyAutoScalingSchedule)
stbasAutoScalingSchedule = lens _stbasAutoScalingSchedule (\ s a -> s{_stbasAutoScalingSchedule = a});

-- | The instance ID.
stbasInstanceId :: Lens' SetTimeBasedAutoScaling Text
stbasInstanceId = lens _stbasInstanceId (\ s a -> s{_stbasInstanceId = a});

instance AWSRequest SetTimeBasedAutoScaling where
        type Sv SetTimeBasedAutoScaling = OpsWorks
        type Rs SetTimeBasedAutoScaling =
             SetTimeBasedAutoScalingResponse
        request = postJSON
        response
          = receiveNull SetTimeBasedAutoScalingResponse'

instance ToHeaders SetTimeBasedAutoScaling where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.SetTimeBasedAutoScaling" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetTimeBasedAutoScaling where
        toJSON SetTimeBasedAutoScaling'{..}
          = object
              ["AutoScalingSchedule" .= _stbasAutoScalingSchedule,
               "InstanceId" .= _stbasInstanceId]

instance ToPath SetTimeBasedAutoScaling where
        toPath = const "/"

instance ToQuery SetTimeBasedAutoScaling where
        toQuery = const mempty

-- | /See:/ 'setTimeBasedAutoScalingResponse' smart constructor.
data SetTimeBasedAutoScalingResponse =
    SetTimeBasedAutoScalingResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTimeBasedAutoScalingResponse' smart constructor.
setTimeBasedAutoScalingResponse :: SetTimeBasedAutoScalingResponse
setTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse'

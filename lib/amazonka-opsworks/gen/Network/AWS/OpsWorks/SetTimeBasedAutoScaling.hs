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
-- Module      : Network.AWS.OpsWorks.SetTimeBasedAutoScaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the time-based auto scaling configuration for a specified instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.SetTimeBasedAutoScaling
    (
    -- * Creating a Request
      setTimeBasedAutoScaling
    , SetTimeBasedAutoScaling
    -- * Request Lenses
    , stbasAutoScalingSchedule
    , stbasInstanceId

    -- * Destructuring the Response
    , setTimeBasedAutoScalingResponse
    , SetTimeBasedAutoScalingResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setTimeBasedAutoScaling' smart constructor.
data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling'
  { _stbasAutoScalingSchedule :: !(Maybe WeeklyAutoScalingSchedule)
  , _stbasInstanceId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetTimeBasedAutoScaling' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stbasAutoScalingSchedule' - An @AutoScalingSchedule@ with the instance schedule.
--
-- * 'stbasInstanceId' - The instance ID.
setTimeBasedAutoScaling
    :: Text -- ^ 'stbasInstanceId'
    -> SetTimeBasedAutoScaling
setTimeBasedAutoScaling pInstanceId_ =
  SetTimeBasedAutoScaling'
    {_stbasAutoScalingSchedule = Nothing, _stbasInstanceId = pInstanceId_}


-- | An @AutoScalingSchedule@ with the instance schedule.
stbasAutoScalingSchedule :: Lens' SetTimeBasedAutoScaling (Maybe WeeklyAutoScalingSchedule)
stbasAutoScalingSchedule = lens _stbasAutoScalingSchedule (\ s a -> s{_stbasAutoScalingSchedule = a})

-- | The instance ID.
stbasInstanceId :: Lens' SetTimeBasedAutoScaling Text
stbasInstanceId = lens _stbasInstanceId (\ s a -> s{_stbasInstanceId = a})

instance AWSRequest SetTimeBasedAutoScaling where
        type Rs SetTimeBasedAutoScaling =
             SetTimeBasedAutoScalingResponse
        request = postJSON opsWorks
        response
          = receiveNull SetTimeBasedAutoScalingResponse'

instance Hashable SetTimeBasedAutoScaling where

instance NFData SetTimeBasedAutoScaling where

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
              (catMaybes
                 [("AutoScalingSchedule" .=) <$>
                    _stbasAutoScalingSchedule,
                  Just ("InstanceId" .= _stbasInstanceId)])

instance ToPath SetTimeBasedAutoScaling where
        toPath = const "/"

instance ToQuery SetTimeBasedAutoScaling where
        toQuery = const mempty

-- | /See:/ 'setTimeBasedAutoScalingResponse' smart constructor.
data SetTimeBasedAutoScalingResponse =
  SetTimeBasedAutoScalingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetTimeBasedAutoScalingResponse' with the minimum fields required to make a request.
--
setTimeBasedAutoScalingResponse
    :: SetTimeBasedAutoScalingResponse
setTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse'


instance NFData SetTimeBasedAutoScalingResponse where

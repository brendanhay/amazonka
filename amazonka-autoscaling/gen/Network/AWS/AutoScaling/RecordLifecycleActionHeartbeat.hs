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
-- Module      : Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a heartbeat for the lifecycle action associated with the specified token or instance. This extends the timeout by the length of time defined using 'PutLifecycleHook' .
--
--
-- This step is a part of the procedure for adding a lifecycle hook to an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Auto Scaling launches or terminates instances.
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Auto Scaling to publish lifecycle notifications to the target.
--
--     * Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.
--
--     * __If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.__
--
--     * If you finish before the timeout period ends, complete the lifecycle action.
--
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/AutoScalingGroupLifecycle.html Auto Scaling Lifecycle> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
    (
    -- * Creating a Request
      recordLifecycleActionHeartbeat
    , RecordLifecycleActionHeartbeat
    -- * Request Lenses
    , rlahInstanceId
    , rlahLifecycleActionToken
    , rlahLifecycleHookName
    , rlahAutoScalingGroupName

    -- * Destructuring the Response
    , recordLifecycleActionHeartbeatResponse
    , RecordLifecycleActionHeartbeatResponse
    -- * Response Lenses
    , rlahrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'recordLifecycleActionHeartbeat' smart constructor.
data RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeat'
  { _rlahInstanceId           :: !(Maybe Text)
  , _rlahLifecycleActionToken :: !(Maybe Text)
  , _rlahLifecycleHookName    :: !Text
  , _rlahAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordLifecycleActionHeartbeat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlahInstanceId' - The ID of the instance.
--
-- * 'rlahLifecycleActionToken' - A token that uniquely identifies a specific lifecycle action associated with an instance. Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
--
-- * 'rlahLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'rlahAutoScalingGroupName' - The name of the Auto Scaling group.
recordLifecycleActionHeartbeat
    :: Text -- ^ 'rlahLifecycleHookName'
    -> Text -- ^ 'rlahAutoScalingGroupName'
    -> RecordLifecycleActionHeartbeat
recordLifecycleActionHeartbeat pLifecycleHookName_ pAutoScalingGroupName_ =
  RecordLifecycleActionHeartbeat'
    { _rlahInstanceId = Nothing
    , _rlahLifecycleActionToken = Nothing
    , _rlahLifecycleHookName = pLifecycleHookName_
    , _rlahAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | The ID of the instance.
rlahInstanceId :: Lens' RecordLifecycleActionHeartbeat (Maybe Text)
rlahInstanceId = lens _rlahInstanceId (\ s a -> s{_rlahInstanceId = a})

-- | A token that uniquely identifies a specific lifecycle action associated with an instance. Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
rlahLifecycleActionToken :: Lens' RecordLifecycleActionHeartbeat (Maybe Text)
rlahLifecycleActionToken = lens _rlahLifecycleActionToken (\ s a -> s{_rlahLifecycleActionToken = a})

-- | The name of the lifecycle hook.
rlahLifecycleHookName :: Lens' RecordLifecycleActionHeartbeat Text
rlahLifecycleHookName = lens _rlahLifecycleHookName (\ s a -> s{_rlahLifecycleHookName = a})

-- | The name of the Auto Scaling group.
rlahAutoScalingGroupName :: Lens' RecordLifecycleActionHeartbeat Text
rlahAutoScalingGroupName = lens _rlahAutoScalingGroupName (\ s a -> s{_rlahAutoScalingGroupName = a})

instance AWSRequest RecordLifecycleActionHeartbeat
         where
        type Rs RecordLifecycleActionHeartbeat =
             RecordLifecycleActionHeartbeatResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "RecordLifecycleActionHeartbeatResult"
              (\ s h x ->
                 RecordLifecycleActionHeartbeatResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RecordLifecycleActionHeartbeat
         where

instance NFData RecordLifecycleActionHeartbeat where

instance ToHeaders RecordLifecycleActionHeartbeat
         where
        toHeaders = const mempty

instance ToPath RecordLifecycleActionHeartbeat where
        toPath = const "/"

instance ToQuery RecordLifecycleActionHeartbeat where
        toQuery RecordLifecycleActionHeartbeat'{..}
          = mconcat
              ["Action" =:
                 ("RecordLifecycleActionHeartbeat" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceId" =: _rlahInstanceId,
               "LifecycleActionToken" =: _rlahLifecycleActionToken,
               "LifecycleHookName" =: _rlahLifecycleHookName,
               "AutoScalingGroupName" =: _rlahAutoScalingGroupName]

-- | /See:/ 'recordLifecycleActionHeartbeatResponse' smart constructor.
newtype RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse'
  { _rlahrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordLifecycleActionHeartbeatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlahrsResponseStatus' - -- | The response status code.
recordLifecycleActionHeartbeatResponse
    :: Int -- ^ 'rlahrsResponseStatus'
    -> RecordLifecycleActionHeartbeatResponse
recordLifecycleActionHeartbeatResponse pResponseStatus_ =
  RecordLifecycleActionHeartbeatResponse'
    {_rlahrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rlahrsResponseStatus :: Lens' RecordLifecycleActionHeartbeatResponse Int
rlahrsResponseStatus = lens _rlahrsResponseStatus (\ s a -> s{_rlahrsResponseStatus = a})

instance NFData
           RecordLifecycleActionHeartbeatResponse
         where

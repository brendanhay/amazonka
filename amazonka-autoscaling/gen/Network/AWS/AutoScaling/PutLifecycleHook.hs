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
-- Module      : Network.AWS.AutoScaling.PutLifecycleHook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a lifecycle hook for the specified Auto Scaling Group.
--
--
-- A lifecycle hook tells Auto Scaling that you want to perform an action on an instance that is not actively in service; for example, either when the instance launches or before the instance terminates.
--
-- This step is a part of the procedure for adding a lifecycle hook to an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Auto Scaling launches or terminates instances.
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Auto Scaling to publish lifecycle notifications to the target.
--
--     * __Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.__
--
--     * If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.
--
--     * If you finish before the timeout period ends, complete the lifecycle action.
--
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/lifecycle-hooks.html Auto Scaling Lifecycle Hooks> in the /Auto Scaling User Guide/ .
--
-- If you exceed your maximum limit of lifecycle hooks, which by default is 50 per Auto Scaling group, the call fails. For information about updating this limit, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html AWS Service Limits> in the /Amazon Web Services General Reference/ .
--
module Network.AWS.AutoScaling.PutLifecycleHook
    (
    -- * Creating a Request
      putLifecycleHook
    , PutLifecycleHook
    -- * Request Lenses
    , plhDefaultResult
    , plhHeartbeatTimeout
    , plhNotificationMetadata
    , plhNotificationTargetARN
    , plhLifecycleTransition
    , plhRoleARN
    , plhLifecycleHookName
    , plhAutoScalingGroupName

    -- * Destructuring the Response
    , putLifecycleHookResponse
    , PutLifecycleHookResponse
    -- * Response Lenses
    , plhrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putLifecycleHook' smart constructor.
data PutLifecycleHook = PutLifecycleHook'
  { _plhDefaultResult         :: !(Maybe Text)
  , _plhHeartbeatTimeout      :: !(Maybe Int)
  , _plhNotificationMetadata  :: !(Maybe Text)
  , _plhNotificationTargetARN :: !(Maybe Text)
  , _plhLifecycleTransition   :: !(Maybe Text)
  , _plhRoleARN               :: !(Maybe Text)
  , _plhLifecycleHookName     :: !Text
  , _plhAutoScalingGroupName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecycleHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plhDefaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. This parameter can be either @CONTINUE@ or @ABANDON@ . The default value is @ABANDON@ .
--
-- * 'plhHeartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out. The range is from 30 to 7200 seconds. The default is 3600 seconds (1 hour). If the lifecycle hook times out, Auto Scaling performs the default action. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
--
-- * 'plhNotificationMetadata' - Contains additional information that you want to include any time Auto Scaling sends a message to the notification target.
--
-- * 'plhNotificationTargetARN' - The ARN of the notification target that Auto Scaling will use to notify you when an instance is in the transition state for the lifecycle hook. This target can be either an SQS queue or an SNS topic. If you specify an empty string, this overrides the current ARN. This operation uses the JSON format when sending notifications to an Amazon SQS queue, and an email key/value pair format when sending notifications to an Amazon SNS topic. When you specify a notification target, Auto Scaling sends it a test message. Test messages contains the following additional key/value pair: @"Event": "autoscaling:TEST_NOTIFICATION"@ .
--
-- * 'plhLifecycleTransition' - The instance state to which you want to attach the lifecycle hook. For a list of lifecycle hook types, see 'DescribeLifecycleHookTypes' . This parameter is required for new lifecycle hooks, but optional when updating existing hooks.
--
-- * 'plhRoleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target. This parameter is required for new lifecycle hooks, but optional when updating existing hooks.
--
-- * 'plhLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'plhAutoScalingGroupName' - The name of the Auto Scaling group.
putLifecycleHook
    :: Text -- ^ 'plhLifecycleHookName'
    -> Text -- ^ 'plhAutoScalingGroupName'
    -> PutLifecycleHook
putLifecycleHook pLifecycleHookName_ pAutoScalingGroupName_ =
  PutLifecycleHook'
    { _plhDefaultResult = Nothing
    , _plhHeartbeatTimeout = Nothing
    , _plhNotificationMetadata = Nothing
    , _plhNotificationTargetARN = Nothing
    , _plhLifecycleTransition = Nothing
    , _plhRoleARN = Nothing
    , _plhLifecycleHookName = pLifecycleHookName_
    , _plhAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. This parameter can be either @CONTINUE@ or @ABANDON@ . The default value is @ABANDON@ .
plhDefaultResult :: Lens' PutLifecycleHook (Maybe Text)
plhDefaultResult = lens _plhDefaultResult (\ s a -> s{_plhDefaultResult = a})

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. The range is from 30 to 7200 seconds. The default is 3600 seconds (1 hour). If the lifecycle hook times out, Auto Scaling performs the default action. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
plhHeartbeatTimeout :: Lens' PutLifecycleHook (Maybe Int)
plhHeartbeatTimeout = lens _plhHeartbeatTimeout (\ s a -> s{_plhHeartbeatTimeout = a})

-- | Contains additional information that you want to include any time Auto Scaling sends a message to the notification target.
plhNotificationMetadata :: Lens' PutLifecycleHook (Maybe Text)
plhNotificationMetadata = lens _plhNotificationMetadata (\ s a -> s{_plhNotificationMetadata = a})

-- | The ARN of the notification target that Auto Scaling will use to notify you when an instance is in the transition state for the lifecycle hook. This target can be either an SQS queue or an SNS topic. If you specify an empty string, this overrides the current ARN. This operation uses the JSON format when sending notifications to an Amazon SQS queue, and an email key/value pair format when sending notifications to an Amazon SNS topic. When you specify a notification target, Auto Scaling sends it a test message. Test messages contains the following additional key/value pair: @"Event": "autoscaling:TEST_NOTIFICATION"@ .
plhNotificationTargetARN :: Lens' PutLifecycleHook (Maybe Text)
plhNotificationTargetARN = lens _plhNotificationTargetARN (\ s a -> s{_plhNotificationTargetARN = a})

-- | The instance state to which you want to attach the lifecycle hook. For a list of lifecycle hook types, see 'DescribeLifecycleHookTypes' . This parameter is required for new lifecycle hooks, but optional when updating existing hooks.
plhLifecycleTransition :: Lens' PutLifecycleHook (Maybe Text)
plhLifecycleTransition = lens _plhLifecycleTransition (\ s a -> s{_plhLifecycleTransition = a})

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target. This parameter is required for new lifecycle hooks, but optional when updating existing hooks.
plhRoleARN :: Lens' PutLifecycleHook (Maybe Text)
plhRoleARN = lens _plhRoleARN (\ s a -> s{_plhRoleARN = a})

-- | The name of the lifecycle hook.
plhLifecycleHookName :: Lens' PutLifecycleHook Text
plhLifecycleHookName = lens _plhLifecycleHookName (\ s a -> s{_plhLifecycleHookName = a})

-- | The name of the Auto Scaling group.
plhAutoScalingGroupName :: Lens' PutLifecycleHook Text
plhAutoScalingGroupName = lens _plhAutoScalingGroupName (\ s a -> s{_plhAutoScalingGroupName = a})

instance AWSRequest PutLifecycleHook where
        type Rs PutLifecycleHook = PutLifecycleHookResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "PutLifecycleHookResult"
              (\ s h x ->
                 PutLifecycleHookResponse' <$> (pure (fromEnum s)))

instance Hashable PutLifecycleHook where

instance NFData PutLifecycleHook where

instance ToHeaders PutLifecycleHook where
        toHeaders = const mempty

instance ToPath PutLifecycleHook where
        toPath = const "/"

instance ToQuery PutLifecycleHook where
        toQuery PutLifecycleHook'{..}
          = mconcat
              ["Action" =: ("PutLifecycleHook" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "DefaultResult" =: _plhDefaultResult,
               "HeartbeatTimeout" =: _plhHeartbeatTimeout,
               "NotificationMetadata" =: _plhNotificationMetadata,
               "NotificationTargetARN" =: _plhNotificationTargetARN,
               "LifecycleTransition" =: _plhLifecycleTransition,
               "RoleARN" =: _plhRoleARN,
               "LifecycleHookName" =: _plhLifecycleHookName,
               "AutoScalingGroupName" =: _plhAutoScalingGroupName]

-- | /See:/ 'putLifecycleHookResponse' smart constructor.
newtype PutLifecycleHookResponse = PutLifecycleHookResponse'
  { _plhrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecycleHookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plhrsResponseStatus' - -- | The response status code.
putLifecycleHookResponse
    :: Int -- ^ 'plhrsResponseStatus'
    -> PutLifecycleHookResponse
putLifecycleHookResponse pResponseStatus_ =
  PutLifecycleHookResponse' {_plhrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
plhrsResponseStatus :: Lens' PutLifecycleHookResponse Int
plhrsResponseStatus = lens _plhrsResponseStatus (\ s a -> s{_plhrsResponseStatus = a})

instance NFData PutLifecycleHookResponse where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutLifecycleHook
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a lifecycle hook for the specified Auto Scaling
-- Group.
--
-- A lifecycle hook tells Auto Scaling that you want to perform an action
-- on an instance that is not actively in service; for example, either when
-- the instance launches or before the instance terminates.
--
-- This operation is a part of the basic sequence for adding a lifecycle
-- hook to an Auto Scaling group:
--
-- 1.  Create a notification target. A target can be either an Amazon SQS
--     queue or an Amazon SNS topic.
-- 2.  Create an IAM role. This role allows Auto Scaling to publish
--     lifecycle notifications to the designated SQS queue or SNS topic.
-- 3.  __Create the lifecycle hook. You can create a hook that acts when
--     instances launch or when instances terminate.__
-- 4.  If necessary, record the lifecycle action heartbeat to keep the
--     instance in a pending state.
-- 5.  Complete the lifecycle action.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingPendingState.html Auto Scaling Pending State>
-- and
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingTerminatingState.html Auto Scaling Terminating State>
-- in the /Auto Scaling Developer Guide/.
--
-- If you exceed your maximum limit of lifecycle hooks, which by default is
-- 50 per region, the call fails. For information about updating this
-- limit, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html AWS Service Limits>
-- in the /Amazon Web Services General Reference/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutLifecycleHook.html>
module Network.AWS.AutoScaling.PutLifecycleHook
    (
    -- * Request
      PutLifecycleHook
    -- ** Request constructor
    , putLifecycleHook
    -- ** Request lenses
    , plhDefaultResult
    , plhHeartbeatTimeout
    , plhNotificationMetadata
    , plhRoleARN
    , plhLifecycleTransition
    , plhNotificationTargetARN
    , plhLifecycleHookName
    , plhAutoScalingGroupName

    -- * Response
    , PutLifecycleHookResponse
    -- ** Response constructor
    , putLifecycleHookResponse
    -- ** Response lenses
    , plhrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putLifecycleHook' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plhDefaultResult'
--
-- * 'plhHeartbeatTimeout'
--
-- * 'plhNotificationMetadata'
--
-- * 'plhRoleARN'
--
-- * 'plhLifecycleTransition'
--
-- * 'plhNotificationTargetARN'
--
-- * 'plhLifecycleHookName'
--
-- * 'plhAutoScalingGroupName'
data PutLifecycleHook = PutLifecycleHook'
    { _plhDefaultResult         :: !(Maybe Text)
    , _plhHeartbeatTimeout      :: !(Maybe Int)
    , _plhNotificationMetadata  :: !(Maybe Text)
    , _plhRoleARN               :: !(Maybe Text)
    , _plhLifecycleTransition   :: !(Maybe Text)
    , _plhNotificationTargetARN :: !(Maybe Text)
    , _plhLifecycleHookName     :: !Text
    , _plhAutoScalingGroupName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutLifecycleHook' smart constructor.
putLifecycleHook :: Text -> Text -> PutLifecycleHook
putLifecycleHook pLifecycleHookName_ pAutoScalingGroupName_ =
    PutLifecycleHook'
    { _plhDefaultResult = Nothing
    , _plhHeartbeatTimeout = Nothing
    , _plhNotificationMetadata = Nothing
    , _plhRoleARN = Nothing
    , _plhLifecycleTransition = Nothing
    , _plhNotificationTargetARN = Nothing
    , _plhLifecycleHookName = pLifecycleHookName_
    , _plhAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for
-- this parameter can be either @CONTINUE@ or @ABANDON@. The default value
-- for this parameter is @ABANDON@.
plhDefaultResult :: Lens' PutLifecycleHook (Maybe Text)
plhDefaultResult = lens _plhDefaultResult (\ s a -> s{_plhDefaultResult = a});

-- | Defines the amount of time, in seconds, that can elapse before the
-- lifecycle hook times out. When the lifecycle hook times out, Auto
-- Scaling performs the action defined in the @DefaultResult@ parameter.
-- You can prevent the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat. The default value for this parameter is
-- 3600 seconds (1 hour).
plhHeartbeatTimeout :: Lens' PutLifecycleHook (Maybe Int)
plhHeartbeatTimeout = lens _plhHeartbeatTimeout (\ s a -> s{_plhHeartbeatTimeout = a});

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
plhNotificationMetadata :: Lens' PutLifecycleHook (Maybe Text)
plhNotificationMetadata = lens _plhNotificationMetadata (\ s a -> s{_plhNotificationMetadata = a});

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
--
-- This parameter is required for new lifecycle hooks, but optional when
-- updating existing hooks.
plhRoleARN :: Lens' PutLifecycleHook (Maybe Text)
plhRoleARN = lens _plhRoleARN (\ s a -> s{_plhRoleARN = a});

-- | The instance state to which you want to attach the lifecycle hook. For a
-- list of lifecycle hook types, see DescribeLifecycleHookTypes.
--
-- This parameter is required for new lifecycle hooks, but optional when
-- updating existing hooks.
plhLifecycleTransition :: Lens' PutLifecycleHook (Maybe Text)
plhLifecycleTransition = lens _plhLifecycleTransition (\ s a -> s{_plhLifecycleTransition = a});

-- | The ARN of the notification target that Auto Scaling will use to notify
-- you when an instance is in the transition state for the lifecycle hook.
-- This ARN target can be either an SQS queue or an SNS topic.
--
-- This parameter is required for new lifecycle hooks, but optional when
-- updating existing hooks.
--
-- The notification message sent to the target will include:
--
-- -   __LifecycleActionToken__. The Lifecycle action token.
-- -   __AccountId__. The user account ID.
-- -   __AutoScalingGroupName__. The name of the Auto Scaling group.
-- -   __LifecycleHookName__. The lifecycle hook name.
-- -   __EC2InstanceId__. The EC2 instance ID.
-- -   __LifecycleTransition__. The lifecycle transition.
-- -   __NotificationMetadata__. The notification metadata.
--
-- This operation uses the JSON format when sending notifications to an
-- Amazon SQS queue, and an email key\/value pair format when sending
-- notifications to an Amazon SNS topic.
--
-- When you call this operation, a test message is sent to the notification
-- target. This test message contains an additional key\/value pair:
-- @Event:autoscaling:TEST_NOTIFICATION@.
plhNotificationTargetARN :: Lens' PutLifecycleHook (Maybe Text)
plhNotificationTargetARN = lens _plhNotificationTargetARN (\ s a -> s{_plhNotificationTargetARN = a});

-- | The name of the lifecycle hook.
plhLifecycleHookName :: Lens' PutLifecycleHook Text
plhLifecycleHookName = lens _plhLifecycleHookName (\ s a -> s{_plhLifecycleHookName = a});

-- | The name of the Auto Scaling group to which you want to assign the
-- lifecycle hook.
plhAutoScalingGroupName :: Lens' PutLifecycleHook Text
plhAutoScalingGroupName = lens _plhAutoScalingGroupName (\ s a -> s{_plhAutoScalingGroupName = a});

instance AWSRequest PutLifecycleHook where
        type Sv PutLifecycleHook = AutoScaling
        type Rs PutLifecycleHook = PutLifecycleHookResponse
        request = postQuery
        response
          = receiveXMLWrapper "PutLifecycleHookResult"
              (\ s h x ->
                 PutLifecycleHookResponse' <$> (pure (fromEnum s)))

instance ToHeaders PutLifecycleHook where
        toHeaders = const mempty

instance ToPath PutLifecycleHook where
        toPath = const mempty

instance ToQuery PutLifecycleHook where
        toQuery PutLifecycleHook'{..}
          = mconcat
              ["Action" =: ("PutLifecycleHook" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "DefaultResult" =: _plhDefaultResult,
               "HeartbeatTimeout" =: _plhHeartbeatTimeout,
               "NotificationMetadata" =: _plhNotificationMetadata,
               "RoleARN" =: _plhRoleARN,
               "LifecycleTransition" =: _plhLifecycleTransition,
               "NotificationTargetARN" =: _plhNotificationTargetARN,
               "LifecycleHookName" =: _plhLifecycleHookName,
               "AutoScalingGroupName" =: _plhAutoScalingGroupName]

-- | /See:/ 'putLifecycleHookResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plhrsStatus'
newtype PutLifecycleHookResponse = PutLifecycleHookResponse'
    { _plhrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutLifecycleHookResponse' smart constructor.
putLifecycleHookResponse :: Int -> PutLifecycleHookResponse
putLifecycleHookResponse pStatus_ =
    PutLifecycleHookResponse'
    { _plhrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
plhrsStatus :: Lens' PutLifecycleHookResponse Int
plhrsStatus = lens _plhrsStatus (\ s a -> s{_plhrsStatus = a});

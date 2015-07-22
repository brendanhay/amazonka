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
    , plhrqDefaultResult
    , plhrqHeartbeatTimeout
    , plhrqNotificationMetadata
    , plhrqRoleARN
    , plhrqLifecycleTransition
    , plhrqNotificationTargetARN
    , plhrqLifecycleHookName
    , plhrqAutoScalingGroupName

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
-- * 'plhrqDefaultResult'
--
-- * 'plhrqHeartbeatTimeout'
--
-- * 'plhrqNotificationMetadata'
--
-- * 'plhrqRoleARN'
--
-- * 'plhrqLifecycleTransition'
--
-- * 'plhrqNotificationTargetARN'
--
-- * 'plhrqLifecycleHookName'
--
-- * 'plhrqAutoScalingGroupName'
data PutLifecycleHook = PutLifecycleHook'
    { _plhrqDefaultResult         :: !(Maybe Text)
    , _plhrqHeartbeatTimeout      :: !(Maybe Int)
    , _plhrqNotificationMetadata  :: !(Maybe Text)
    , _plhrqRoleARN               :: !(Maybe Text)
    , _plhrqLifecycleTransition   :: !(Maybe Text)
    , _plhrqNotificationTargetARN :: !(Maybe Text)
    , _plhrqLifecycleHookName     :: !Text
    , _plhrqAutoScalingGroupName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutLifecycleHook' smart constructor.
putLifecycleHook :: Text -> Text -> PutLifecycleHook
putLifecycleHook pLifecycleHookName pAutoScalingGroupName =
    PutLifecycleHook'
    { _plhrqDefaultResult = Nothing
    , _plhrqHeartbeatTimeout = Nothing
    , _plhrqNotificationMetadata = Nothing
    , _plhrqRoleARN = Nothing
    , _plhrqLifecycleTransition = Nothing
    , _plhrqNotificationTargetARN = Nothing
    , _plhrqLifecycleHookName = pLifecycleHookName
    , _plhrqAutoScalingGroupName = pAutoScalingGroupName
    }

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for
-- this parameter can be either @CONTINUE@ or @ABANDON@. The default value
-- for this parameter is @ABANDON@.
plhrqDefaultResult :: Lens' PutLifecycleHook (Maybe Text)
plhrqDefaultResult = lens _plhrqDefaultResult (\ s a -> s{_plhrqDefaultResult = a});

-- | Defines the amount of time, in seconds, that can elapse before the
-- lifecycle hook times out. When the lifecycle hook times out, Auto
-- Scaling performs the action defined in the @DefaultResult@ parameter.
-- You can prevent the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat. The default value for this parameter is
-- 3600 seconds (1 hour).
plhrqHeartbeatTimeout :: Lens' PutLifecycleHook (Maybe Int)
plhrqHeartbeatTimeout = lens _plhrqHeartbeatTimeout (\ s a -> s{_plhrqHeartbeatTimeout = a});

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
plhrqNotificationMetadata :: Lens' PutLifecycleHook (Maybe Text)
plhrqNotificationMetadata = lens _plhrqNotificationMetadata (\ s a -> s{_plhrqNotificationMetadata = a});

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
--
-- This parameter is required for new lifecycle hooks, but optional when
-- updating existing hooks.
plhrqRoleARN :: Lens' PutLifecycleHook (Maybe Text)
plhrqRoleARN = lens _plhrqRoleARN (\ s a -> s{_plhrqRoleARN = a});

-- | The instance state to which you want to attach the lifecycle hook. For a
-- list of lifecycle hook types, see DescribeLifecycleHookTypes.
--
-- This parameter is required for new lifecycle hooks, but optional when
-- updating existing hooks.
plhrqLifecycleTransition :: Lens' PutLifecycleHook (Maybe Text)
plhrqLifecycleTransition = lens _plhrqLifecycleTransition (\ s a -> s{_plhrqLifecycleTransition = a});

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
plhrqNotificationTargetARN :: Lens' PutLifecycleHook (Maybe Text)
plhrqNotificationTargetARN = lens _plhrqNotificationTargetARN (\ s a -> s{_plhrqNotificationTargetARN = a});

-- | The name of the lifecycle hook.
plhrqLifecycleHookName :: Lens' PutLifecycleHook Text
plhrqLifecycleHookName = lens _plhrqLifecycleHookName (\ s a -> s{_plhrqLifecycleHookName = a});

-- | The name of the Auto Scaling group to which you want to assign the
-- lifecycle hook.
plhrqAutoScalingGroupName :: Lens' PutLifecycleHook Text
plhrqAutoScalingGroupName = lens _plhrqAutoScalingGroupName (\ s a -> s{_plhrqAutoScalingGroupName = a});

instance AWSRequest PutLifecycleHook where
        type Sv PutLifecycleHook = AutoScaling
        type Rs PutLifecycleHook = PutLifecycleHookResponse
        request = post
        response
          = receiveXMLWrapper "PutLifecycleHookResult"
              (\ s h x ->
                 PutLifecycleHookResponse' <$> (pure (fromEnum s)))

instance ToHeaders PutLifecycleHook where
        toHeaders = const mempty

instance ToPath PutLifecycleHook where
        toPath = const "/"

instance ToQuery PutLifecycleHook where
        toQuery PutLifecycleHook'{..}
          = mconcat
              ["Action" =: ("PutLifecycleHook" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "DefaultResult" =: _plhrqDefaultResult,
               "HeartbeatTimeout" =: _plhrqHeartbeatTimeout,
               "NotificationMetadata" =: _plhrqNotificationMetadata,
               "RoleARN" =: _plhrqRoleARN,
               "LifecycleTransition" =: _plhrqLifecycleTransition,
               "NotificationTargetARN" =:
                 _plhrqNotificationTargetARN,
               "LifecycleHookName" =: _plhrqLifecycleHookName,
               "AutoScalingGroupName" =: _plhrqAutoScalingGroupName]

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
putLifecycleHookResponse pStatus =
    PutLifecycleHookResponse'
    { _plhrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
plhrsStatus :: Lens' PutLifecycleHookResponse Int
plhrsStatus = lens _plhrsStatus (\ s a -> s{_plhrsStatus = a});

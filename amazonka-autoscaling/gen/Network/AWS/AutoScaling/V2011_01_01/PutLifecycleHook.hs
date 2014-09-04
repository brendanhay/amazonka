{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.PutLifecycleHook
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a lifecycle hook for an Auto Scaling Group. A lifecycle
-- hook tells Auto Scaling that you want to perform an action on an instance
-- that is not actively in service; for example, either when the instance
-- launches or before the instance terminates. This operation is a part of the
-- basic sequence for adding a lifecycle hook to an Auto Scaling group: Create
-- a notification target. A target can be either an Amazon SQS queue or an
-- Amazon SNS topic. Create an IAM role. This role allows Auto Scaling to
-- publish lifecycle notifications to the designated SQS queue or SNS topic.
-- Create the lifecycle hook. You can create a hook that acts when instances
-- launch or when instances terminate. If necessary, record the lifecycle
-- action heartbeat to keep the instance in a pending state. Complete the
-- lifecycle action. To learn more, see Auto Scaling Pending State and Auto
-- Scaling Terminating State.
-- http://autoscaling.amazonaws.com/?RoleARN=arn%3Aaws%3Aiam%3A%3A896650972448%3Arole%2FAutoScaling&AutoScalingGroupName=my-asg&LifecycleHookName=ReadyForSoftwareInst
-- 
-- all&NotificationTargetARN=arn%3Aaws%3Asqs%3Aus-east-1%3A896650972448%3Alifecyclehookqueue&LifecycleTransition=autoscaling%3AEC2_INSTANCE_LAUNCHING&Version=2011-
-- 
-- 01-01&Action=PutLifecycleHook&SignatureVersion=2&SignatureMethod=HmacSHA256&Timestamp=2014-06-17T17%3A30%3A36.125Z&AUTHPARAMS
-- 1952f458-f645-11e3-bc51-b35178f0274f.
module Network.AWS.AutoScaling.V2011_01_01.PutLifecycleHook
    (
    -- * Request
      PutLifecycleHook
    -- ** Request constructor
    , mkPutLifecycleHookType
    -- ** Request lenses
    , plhtLifecycleHookName
    , plhtAutoScalingGroupName
    , plhtLifecycleTransition
    , plhtRoleARN
    , plhtNotificationTargetARN
    , plhtNotificationMetadata
    , plhtHeartbeatTimeout
    , plhtDefaultResult

    -- * Response
    , PutLifecycleHookResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutLifecycleHook' request.
mkPutLifecycleHookType :: Text -- ^ 'plhtLifecycleHookName'
                       -> Text -- ^ 'plhtAutoScalingGroupName'
                       -> PutLifecycleHook
mkPutLifecycleHookType p1 p2 = PutLifecycleHook
    { _plhtLifecycleHookName = p1
    , _plhtAutoScalingGroupName = p2
    , _plhtLifecycleTransition = Nothing
    , _plhtRoleARN = Nothing
    , _plhtNotificationTargetARN = Nothing
    , _plhtNotificationMetadata = Nothing
    , _plhtHeartbeatTimeout = Nothing
    , _plhtDefaultResult = Nothing
    }
{-# INLINE mkPutLifecycleHookType #-}

data PutLifecycleHook = PutLifecycleHook
    { _plhtLifecycleHookName :: Text
      -- ^ The name of the lifecycle hook.
    , _plhtAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which you want to assign
      -- the lifecycle hook.
    , _plhtLifecycleTransition :: Maybe Text
      -- ^ The Amazon EC2 instance state to which you want to attach the
      -- lifecycle hook. See DescribeLifecycleHookTypes for a list of
      -- available lifecycle hook types. This parameter is required for
      -- new lifecycle hooks, but optional when updating existing hooks.
    , _plhtRoleARN :: Maybe Text
      -- ^ The ARN of the Amazon IAM role that allows the Auto Scaling group
      -- to publish to the specified notification target. This parameter
      -- is required for new lifecycle hooks, but optional when updating
      -- existing hooks.
    , _plhtNotificationTargetARN :: Maybe Text
      -- ^ The ARN of the notification target that Auto Scaling will use to
      -- notify you when an instance is in the transition state for the
      -- lifecycle hook. This ARN target can be either an SQS queue or an
      -- SNS topic. This parameter is required for new lifecycle hooks,
      -- but optional when updating existing hooks. The notification
      -- message sent to the target will include: LifecycleActionToken.
      -- The Lifecycle action token. AccountId. The user account ID.
      -- AutoScalingGroupName. The name of the Auto Scaling group.
      -- LifecycleHookName. The lifecycle hook name. EC2InstanceId. The
      -- EC2 instance ID. LifecycleTransition. The lifecycle transition.
      -- NotificationMetadata. The notification metadata. This operation
      -- uses the JSON format when sending notifications to an Amazon SQS
      -- queue, and an email key/value pair format when sending
      -- notifications to an Amazon SNS topic. When you call this
      -- operation, a test message is sent to the notification target.
      -- This test message contains an additional key/value pair:
      -- Event:autoscaling:TEST_NOTIFICATION.
    , _plhtNotificationMetadata :: Maybe Text
      -- ^ Contains additional information that you want to include any time
      -- Auto Scaling sends a message to the notification target.
    , _plhtHeartbeatTimeout :: Maybe Integer
      -- ^ Defines the amount of time, in seconds, that can elapse before
      -- the lifecycle hook times out. When the lifecycle hook times out,
      -- Auto Scaling performs the action defined in the DefaultResult
      -- parameter. You can prevent the lifecycle hook from timing out by
      -- calling RecordLifecycleActionHeartbeat. The default value for
      -- this parameter is 3600 seconds (1 hour).
    , _plhtDefaultResult :: Maybe Text
      -- ^ Defines the action the Auto Scaling group should take when the
      -- lifecycle hook timeout elapses or if an unexpected failure
      -- occurs. The value for this parameter can be either CONTINUE or
      -- ABANDON. The default value for this parameter is ABANDON.
    } deriving (Show, Generic)

-- | The name of the lifecycle hook.
plhtLifecycleHookName :: Lens' PutLifecycleHook (Text)
plhtLifecycleHookName = lens _plhtLifecycleHookName (\s a -> s { _plhtLifecycleHookName = a })
{-# INLINE plhtLifecycleHookName #-}

-- | The name of the Auto Scaling group to which you want to assign the
-- lifecycle hook.
plhtAutoScalingGroupName :: Lens' PutLifecycleHook (Text)
plhtAutoScalingGroupName = lens _plhtAutoScalingGroupName (\s a -> s { _plhtAutoScalingGroupName = a })
{-# INLINE plhtAutoScalingGroupName #-}

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHookTypes for a list of available lifecycle hook
-- types. This parameter is required for new lifecycle hooks, but optional
-- when updating existing hooks.
plhtLifecycleTransition :: Lens' PutLifecycleHook (Maybe Text)
plhtLifecycleTransition = lens _plhtLifecycleTransition (\s a -> s { _plhtLifecycleTransition = a })
{-# INLINE plhtLifecycleTransition #-}

-- | The ARN of the Amazon IAM role that allows the Auto Scaling group to
-- publish to the specified notification target. This parameter is required
-- for new lifecycle hooks, but optional when updating existing hooks.
plhtRoleARN :: Lens' PutLifecycleHook (Maybe Text)
plhtRoleARN = lens _plhtRoleARN (\s a -> s { _plhtRoleARN = a })
{-# INLINE plhtRoleARN #-}

-- | The ARN of the notification target that Auto Scaling will use to notify you
-- when an instance is in the transition state for the lifecycle hook. This
-- ARN target can be either an SQS queue or an SNS topic. This parameter is
-- required for new lifecycle hooks, but optional when updating existing
-- hooks. The notification message sent to the target will include:
-- LifecycleActionToken. The Lifecycle action token. AccountId. The user
-- account ID. AutoScalingGroupName. The name of the Auto Scaling group.
-- LifecycleHookName. The lifecycle hook name. EC2InstanceId. The EC2 instance
-- ID. LifecycleTransition. The lifecycle transition. NotificationMetadata.
-- The notification metadata. This operation uses the JSON format when sending
-- notifications to an Amazon SQS queue, and an email key/value pair format
-- when sending notifications to an Amazon SNS topic. When you call this
-- operation, a test message is sent to the notification target. This test
-- message contains an additional key/value pair:
-- Event:autoscaling:TEST_NOTIFICATION.
plhtNotificationTargetARN :: Lens' PutLifecycleHook (Maybe Text)
plhtNotificationTargetARN = lens _plhtNotificationTargetARN (\s a -> s { _plhtNotificationTargetARN = a })
{-# INLINE plhtNotificationTargetARN #-}

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
plhtNotificationMetadata :: Lens' PutLifecycleHook (Maybe Text)
plhtNotificationMetadata = lens _plhtNotificationMetadata (\s a -> s { _plhtNotificationMetadata = a })
{-# INLINE plhtNotificationMetadata #-}

-- | Defines the amount of time, in seconds, that can elapse before the
-- lifecycle hook times out. When the lifecycle hook times out, Auto Scaling
-- performs the action defined in the DefaultResult parameter. You can prevent
-- the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat. The default value for this parameter is
-- 3600 seconds (1 hour).
plhtHeartbeatTimeout :: Lens' PutLifecycleHook (Maybe Integer)
plhtHeartbeatTimeout = lens _plhtHeartbeatTimeout (\s a -> s { _plhtHeartbeatTimeout = a })
{-# INLINE plhtHeartbeatTimeout #-}

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for this
-- parameter can be either CONTINUE or ABANDON. The default value for this
-- parameter is ABANDON.
plhtDefaultResult :: Lens' PutLifecycleHook (Maybe Text)
plhtDefaultResult = lens _plhtDefaultResult (\s a -> s { _plhtDefaultResult = a })
{-# INLINE plhtDefaultResult #-}

instance ToQuery PutLifecycleHook where
    toQuery = genericQuery def

    deriving (Eq, Show, Generic)

instance AWSRequest PutLifecycleHook where
    type Sv PutLifecycleHook = AutoScaling
    type Rs PutLifecycleHook = PutLifecycleHookResponse

    request = post "PutLifecycleHook"
    response _ = nullaryResponse PutLifecycleHookResponse

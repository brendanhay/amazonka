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
    , putLifecycleHook
    -- ** Request lenses
    , plhtLifecycleHookName
    , plhtAutoScalingGroupName
    , plhtHeartbeatTimeout
    , plhtDefaultResult
    , plhtLifecycleTransition
    , plhtRoleARN
    , plhtNotificationTargetARN
    , plhtNotificationMetadata

    -- * Response
    , PutLifecycleHookResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutLifecycleHook' request.
putLifecycleHook :: Text -- ^ 'plhtLifecycleHookName'
                 -> Text -- ^ 'plhtAutoScalingGroupName'
                 -> PutLifecycleHook
putLifecycleHook p1 p2 = PutLifecycleHook
    { _plhtLifecycleHookName = p1
    , _plhtAutoScalingGroupName = p2
    , _plhtHeartbeatTimeout = Nothing
    , _plhtDefaultResult = Nothing
    , _plhtLifecycleTransition = Nothing
    , _plhtRoleARN = Nothing
    , _plhtNotificationTargetARN = Nothing
    , _plhtNotificationMetadata = Nothing
    }

data PutLifecycleHook = PutLifecycleHook
    { _plhtLifecycleHookName :: Text
      -- ^ The name of the lifecycle hook.
    , _plhtAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which you want to assign
      -- the lifecycle hook.
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
    } deriving (Show, Generic)

-- | The name of the lifecycle hook.
plhtLifecycleHookName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtLifecycleHookName f x =
    (\y -> x { _plhtLifecycleHookName = y })
       <$> f (_plhtLifecycleHookName x)
{-# INLINE plhtLifecycleHookName #-}

-- | The name of the Auto Scaling group to which you want to assign the
-- lifecycle hook.
plhtAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtAutoScalingGroupName f x =
    (\y -> x { _plhtAutoScalingGroupName = y })
       <$> f (_plhtAutoScalingGroupName x)
{-# INLINE plhtAutoScalingGroupName #-}

-- | Defines the amount of time, in seconds, that can elapse before the
-- lifecycle hook times out. When the lifecycle hook times out, Auto Scaling
-- performs the action defined in the DefaultResult parameter. You can prevent
-- the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat. The default value for this parameter is
-- 3600 seconds (1 hour).
plhtHeartbeatTimeout
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtHeartbeatTimeout f x =
    (\y -> x { _plhtHeartbeatTimeout = y })
       <$> f (_plhtHeartbeatTimeout x)
{-# INLINE plhtHeartbeatTimeout #-}

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for this
-- parameter can be either CONTINUE or ABANDON. The default value for this
-- parameter is ABANDON.
plhtDefaultResult
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtDefaultResult f x =
    (\y -> x { _plhtDefaultResult = y })
       <$> f (_plhtDefaultResult x)
{-# INLINE plhtDefaultResult #-}

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHookTypes for a list of available lifecycle hook
-- types. This parameter is required for new lifecycle hooks, but optional
-- when updating existing hooks.
plhtLifecycleTransition
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtLifecycleTransition f x =
    (\y -> x { _plhtLifecycleTransition = y })
       <$> f (_plhtLifecycleTransition x)
{-# INLINE plhtLifecycleTransition #-}

-- | The ARN of the Amazon IAM role that allows the Auto Scaling group to
-- publish to the specified notification target. This parameter is required
-- for new lifecycle hooks, but optional when updating existing hooks.
plhtRoleARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtRoleARN f x =
    (\y -> x { _plhtRoleARN = y })
       <$> f (_plhtRoleARN x)
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
plhtNotificationTargetARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtNotificationTargetARN f x =
    (\y -> x { _plhtNotificationTargetARN = y })
       <$> f (_plhtNotificationTargetARN x)
{-# INLINE plhtNotificationTargetARN #-}

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
plhtNotificationMetadata
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutLifecycleHook
    -> f PutLifecycleHook
plhtNotificationMetadata f x =
    (\y -> x { _plhtNotificationMetadata = y })
       <$> f (_plhtNotificationMetadata x)
{-# INLINE plhtNotificationMetadata #-}

instance ToQuery PutLifecycleHook where
    toQuery = genericQuery def

data PutLifecycleHookResponse = PutLifecycleHookResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutLifecycleHook where
    type Sv PutLifecycleHook = AutoScaling
    type Rs PutLifecycleHook = PutLifecycleHookResponse

    request = post "PutLifecycleHook"
    response _ = nullaryResponse PutLifecycleHookResponse

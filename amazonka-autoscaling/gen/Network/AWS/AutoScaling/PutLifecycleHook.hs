{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.PutLifecycleHook
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a lifecycle hook for the specified Auto Scaling Group. A
-- lifecycle hook tells Auto Scaling that you want to perform an action on an
-- instance that is not actively in service; for example, either when the
-- instance launches or before the instance terminates. This operation is a
-- part of the basic sequence for adding a lifecycle hook to an Auto Scaling
-- group: Create a notification target. A target can be either an Amazon SQS
-- queue or an Amazon SNS topic. Create an IAM role. This role allows Auto
-- Scaling to publish lifecycle notifications to the designated SQS queue or
-- SNS topic. Create the lifecycle hook. You can create a hook that acts when
-- instances launch or when instances terminate. If necessary, record the
-- lifecycle action heartbeat to keep the instance in a pending state.
-- Complete the lifecycle action. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingPendingState.html
-- Auto Scaling Pending State> and
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingTerminatingState.html
-- Auto Scaling Terminating State> in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutLifecycleHook.html>
module Network.AWS.AutoScaling.PutLifecycleHook
    (
    -- * Request
      PutLifecycleHook
    -- ** Request constructor
    , putLifecycleHook
    -- ** Request lenses
    , plhAutoScalingGroupName
    , plhDefaultResult
    , plhHeartbeatTimeout
    , plhLifecycleHookName
    , plhLifecycleTransition
    , plhNotificationMetadata
    , plhNotificationTargetARN
    , plhRoleARN

    -- * Response
    , PutLifecycleHookResponse
    -- ** Response constructor
    , putLifecycleHookResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data PutLifecycleHook = PutLifecycleHook
    { _plhAutoScalingGroupName  :: Text
    , _plhDefaultResult         :: Maybe Text
    , _plhHeartbeatTimeout      :: Maybe Int
    , _plhLifecycleHookName     :: Text
    , _plhLifecycleTransition   :: Maybe Text
    , _plhNotificationMetadata  :: Maybe Text
    , _plhNotificationTargetARN :: Maybe Text
    , _plhRoleARN               :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'PutLifecycleHook' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plhAutoScalingGroupName' @::@ 'Text'
--
-- * 'plhDefaultResult' @::@ 'Maybe' 'Text'
--
-- * 'plhHeartbeatTimeout' @::@ 'Maybe' 'Int'
--
-- * 'plhLifecycleHookName' @::@ 'Text'
--
-- * 'plhLifecycleTransition' @::@ 'Maybe' 'Text'
--
-- * 'plhNotificationMetadata' @::@ 'Maybe' 'Text'
--
-- * 'plhNotificationTargetARN' @::@ 'Maybe' 'Text'
--
-- * 'plhRoleARN' @::@ 'Maybe' 'Text'
--
putLifecycleHook :: Text -- ^ 'plhLifecycleHookName'
                 -> Text -- ^ 'plhAutoScalingGroupName'
                 -> PutLifecycleHook
putLifecycleHook p1 p2 = PutLifecycleHook
    { _plhLifecycleHookName     = p1
    , _plhAutoScalingGroupName  = p2
    , _plhLifecycleTransition   = Nothing
    , _plhRoleARN               = Nothing
    , _plhNotificationTargetARN = Nothing
    , _plhNotificationMetadata  = Nothing
    , _plhHeartbeatTimeout      = Nothing
    , _plhDefaultResult         = Nothing
    }

-- | The name of the Auto Scaling group to which you want to assign the
-- lifecycle hook.
plhAutoScalingGroupName :: Lens' PutLifecycleHook Text
plhAutoScalingGroupName =
    lens _plhAutoScalingGroupName (\s a -> s { _plhAutoScalingGroupName = a })

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for
-- this parameter can be either @CONTINUE@ or @ABANDON@. The default value
-- for this parameter is @ABANDON@.
plhDefaultResult :: Lens' PutLifecycleHook (Maybe Text)
plhDefaultResult = lens _plhDefaultResult (\s a -> s { _plhDefaultResult = a })

-- | Defines the amount of time, in seconds, that can elapse before the
-- lifecycle hook times out. When the lifecycle hook times out, Auto Scaling
-- performs the action defined in the @DefaultResult@ parameter. You can
-- prevent the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat>. The default value for this parameter is
-- 3600 seconds (1 hour).
plhHeartbeatTimeout :: Lens' PutLifecycleHook (Maybe Int)
plhHeartbeatTimeout =
    lens _plhHeartbeatTimeout (\s a -> s { _plhHeartbeatTimeout = a })

-- | The name of the lifecycle hook.
plhLifecycleHookName :: Lens' PutLifecycleHook Text
plhLifecycleHookName =
    lens _plhLifecycleHookName (\s a -> s { _plhLifecycleHookName = a })

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHookTypes> for a list of available lifecycle
-- hook types. This parameter is required for new lifecycle hooks, but
-- optional when updating existing hooks.
plhLifecycleTransition :: Lens' PutLifecycleHook (Maybe Text)
plhLifecycleTransition =
    lens _plhLifecycleTransition (\s a -> s { _plhLifecycleTransition = a })

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
plhNotificationMetadata :: Lens' PutLifecycleHook (Maybe Text)
plhNotificationMetadata =
    lens _plhNotificationMetadata (\s a -> s { _plhNotificationMetadata = a })

-- | The ARN of the notification target that Auto Scaling will use to notify
-- you when an instance is in the transition state for the lifecycle hook.
-- This ARN target can be either an SQS queue or an SNS topic. This
-- parameter is required for new lifecycle hooks, but optional when updating
-- existing hooks. The notification message sent to the target will include:
-- LifecycleActionToken. The Lifecycle action token. AccountId. The user
-- account ID. AutoScalingGroupName. The name of the Auto Scaling group.
-- LifecycleHookName. The lifecycle hook name. EC2InstanceId. The EC2
-- instance ID. LifecycleTransition. The lifecycle transition.
-- NotificationMetadata. The notification metadata. This operation uses the
-- JSON format when sending notifications to an Amazon SQS queue, and an
-- email key/value pair format when sending notifications to an Amazon SNS
-- topic. When you call this operation, a test message is sent to the
-- notification target. This test message contains an additional key/value
-- pair: @Event:autoscaling:TEST_NOTIFICATION@.
plhNotificationTargetARN :: Lens' PutLifecycleHook (Maybe Text)
plhNotificationTargetARN =
    lens _plhNotificationTargetARN
        (\s a -> s { _plhNotificationTargetARN = a })

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target. This parameter is required for new
-- lifecycle hooks, but optional when updating existing hooks.
plhRoleARN :: Lens' PutLifecycleHook (Maybe Text)
plhRoleARN = lens _plhRoleARN (\s a -> s { _plhRoleARN = a })

data PutLifecycleHookResponse = PutLifecycleHookResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutLifecycleHookResponse' constructor.
putLifecycleHookResponse :: PutLifecycleHookResponse
putLifecycleHookResponse = PutLifecycleHookResponse

instance ToPath PutLifecycleHook where
    toPath = const "/"

instance ToQuery PutLifecycleHook where
    toQuery PutLifecycleHook{..} = mconcat
        [ "AutoScalingGroupName"  =? _plhAutoScalingGroupName
        , "DefaultResult"         =? _plhDefaultResult
        , "HeartbeatTimeout"      =? _plhHeartbeatTimeout
        , "LifecycleHookName"     =? _plhLifecycleHookName
        , "LifecycleTransition"   =? _plhLifecycleTransition
        , "NotificationMetadata"  =? _plhNotificationMetadata
        , "NotificationTargetARN" =? _plhNotificationTargetARN
        , "RoleARN"               =? _plhRoleARN
        ]

instance ToHeaders PutLifecycleHook

instance AWSRequest PutLifecycleHook where
    type Sv PutLifecycleHook = AutoScaling
    type Rs PutLifecycleHook = PutLifecycleHookResponse

    request  = post "PutLifecycleHook"
    response = nullResponse PutLifecycleHookResponse

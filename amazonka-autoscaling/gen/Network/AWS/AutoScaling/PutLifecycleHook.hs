{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.AutoScaling.PutLifecycleHook
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
module Network.AWS.AutoScaling.PutLifecycleHook
    (
    -- * Request
      PutLifecycleHookType
    -- ** Request constructor
    , putLifecycleHookType
    -- ** Request lenses
    , plhtAutoScalingGroupName
    , plhtDefaultResult
    , plhtHeartbeatTimeout
    , plhtLifecycleHookName
    , plhtLifecycleTransition
    , plhtNotificationMetadata
    , plhtNotificationTargetARN
    , plhtRoleARN

    -- * Response
    , PutLifecycleHookResponse
    -- ** Response constructor
    , putLifecycleHookResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data PutLifecycleHookType = PutLifecycleHookType
    { _plhtAutoScalingGroupName  :: Text
    , _plhtDefaultResult         :: Maybe Text
    , _plhtHeartbeatTimeout      :: Maybe Int
    , _plhtLifecycleHookName     :: Text
    , _plhtLifecycleTransition   :: Maybe Text
    , _plhtNotificationMetadata  :: Maybe Text
    , _plhtNotificationTargetARN :: Maybe Text
    , _plhtRoleARN               :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutLifecycleHookType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plhtAutoScalingGroupName' @::@ 'Text'
--
-- * 'plhtDefaultResult' @::@ 'Maybe' 'Text'
--
-- * 'plhtHeartbeatTimeout' @::@ 'Maybe' 'Int'
--
-- * 'plhtLifecycleHookName' @::@ 'Text'
--
-- * 'plhtLifecycleTransition' @::@ 'Maybe' 'Text'
--
-- * 'plhtNotificationMetadata' @::@ 'Maybe' 'Text'
--
-- * 'plhtNotificationTargetARN' @::@ 'Maybe' 'Text'
--
-- * 'plhtRoleARN' @::@ 'Maybe' 'Text'
--
putLifecycleHookType :: Text -- ^ 'plhtLifecycleHookName'
                     -> Text -- ^ 'plhtAutoScalingGroupName'
                     -> PutLifecycleHookType
putLifecycleHookType p1 p2 = PutLifecycleHookType
    { _plhtLifecycleHookName     = p1
    , _plhtAutoScalingGroupName  = p2
    , _plhtLifecycleTransition   = Nothing
    , _plhtRoleARN               = Nothing
    , _plhtNotificationTargetARN = Nothing
    , _plhtNotificationMetadata  = Nothing
    , _plhtHeartbeatTimeout      = Nothing
    , _plhtDefaultResult         = Nothing
    }

-- | The name of the Auto Scaling group to which you want to assign the
-- lifecycle hook.
plhtAutoScalingGroupName :: Lens' PutLifecycleHookType Text
plhtAutoScalingGroupName =
    lens _plhtAutoScalingGroupName
        (\s a -> s { _plhtAutoScalingGroupName = a })

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for
-- this parameter can be either CONTINUE or ABANDON. The default value for
-- this parameter is ABANDON.
plhtDefaultResult :: Lens' PutLifecycleHookType (Maybe Text)
plhtDefaultResult =
    lens _plhtDefaultResult (\s a -> s { _plhtDefaultResult = a })

-- | Defines the amount of time, in seconds, that can elapse before the
-- lifecycle hook times out. When the lifecycle hook times out, Auto Scaling
-- performs the action defined in the DefaultResult parameter. You can
-- prevent the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat. The default value for this parameter is
-- 3600 seconds (1 hour).
plhtHeartbeatTimeout :: Lens' PutLifecycleHookType (Maybe Int)
plhtHeartbeatTimeout =
    lens _plhtHeartbeatTimeout (\s a -> s { _plhtHeartbeatTimeout = a })

-- | The name of the lifecycle hook.
plhtLifecycleHookName :: Lens' PutLifecycleHookType Text
plhtLifecycleHookName =
    lens _plhtLifecycleHookName (\s a -> s { _plhtLifecycleHookName = a })

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHookTypes for a list of available lifecycle
-- hook types.
plhtLifecycleTransition :: Lens' PutLifecycleHookType (Maybe Text)
plhtLifecycleTransition =
    lens _plhtLifecycleTransition (\s a -> s { _plhtLifecycleTransition = a })

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
plhtNotificationMetadata :: Lens' PutLifecycleHookType (Maybe Text)
plhtNotificationMetadata =
    lens _plhtNotificationMetadata
        (\s a -> s { _plhtNotificationMetadata = a })

-- | The ARN of the notification target that Auto Scaling will use to notify
-- you when an instance is in the transition state for the lifecycle hook.
-- This ARN target can be either an SQS queue or an SNS topic. The
-- notification message sent to the target will include:
-- LifecycleActionToken. The Lifecycle action token. AccountId. The user
-- account ID. AutoScalingGroupName. The name of the Auto Scaling group.
-- LifecycleHookName. The lifecycle hook name. EC2InstanceId. The EC2
-- instance ID. LifecycleTransition. The lifecycle transition.
-- NotificationMetadata. The notification metadata. This operation uses the
-- JSON format when sending notifications to an Amazon SQS queue, and an
-- email key/value pair format when sending notifications to an Amazon SNS
-- topic. When you call this operation, a test message is sent to the
-- notification target. This test message contains an additional key/value
-- pair: Event:autoscaling:TEST_NOTIFICATION.
plhtNotificationTargetARN :: Lens' PutLifecycleHookType (Maybe Text)
plhtNotificationTargetARN =
    lens _plhtNotificationTargetARN
        (\s a -> s { _plhtNotificationTargetARN = a })

-- | The ARN of the Amazon IAM role that allows the Auto Scaling group to
-- publish to the specified notification target.
plhtRoleARN :: Lens' PutLifecycleHookType (Maybe Text)
plhtRoleARN = lens _plhtRoleARN (\s a -> s { _plhtRoleARN = a })

instance ToPath PutLifecycleHookType where
    toPath = const "/"

instance ToQuery PutLifecycleHookType

data PutLifecycleHookResponse = PutLifecycleHookResponse

-- | 'PutLifecycleHookResponse' constructor.
putLifecycleHookResponse :: PutLifecycleHookResponse
putLifecycleHookResponse = PutLifecycleHookResponse

instance AWSRequest PutLifecycleHookType where
    type Sv PutLifecycleHookType = AutoScaling
    type Rs PutLifecycleHookType = PutLifecycleHookResponse

    request  = post "PutLifecycleHook"
    response = const (nullaryResponse PutLifecycleHookResponse)

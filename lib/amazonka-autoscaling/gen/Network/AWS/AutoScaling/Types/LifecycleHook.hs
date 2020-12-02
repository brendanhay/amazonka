{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleHook where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a lifecycle hook, which tells Amazon EC2 Auto Scaling that you want to perform an action whenever it launches instances or terminates instances.
--
--
--
-- /See:/ 'lifecycleHook' smart constructor.
data LifecycleHook = LifecycleHook'
  { _lhDefaultResult ::
      !(Maybe Text),
    _lhLifecycleHookName :: !(Maybe Text),
    _lhHeartbeatTimeout :: !(Maybe Int),
    _lhAutoScalingGroupName :: !(Maybe Text),
    _lhNotificationMetadata :: !(Maybe Text),
    _lhGlobalTimeout :: !(Maybe Int),
    _lhNotificationTargetARN :: !(Maybe Text),
    _lhLifecycleTransition :: !(Maybe Text),
    _lhRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecycleHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhDefaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The possible values are @CONTINUE@ and @ABANDON@ .
--
-- * 'lhLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'lhHeartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter.
--
-- * 'lhAutoScalingGroupName' - The name of the Auto Scaling group for the lifecycle hook.
--
-- * 'lhNotificationMetadata' - Additional information that is included any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- * 'lhGlobalTimeout' - The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
--
-- * 'lhNotificationTargetARN' - The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- * 'lhLifecycleTransition' - The state of the EC2 instance to which to attach the lifecycle hook. The following are possible values:     * autoscaling:EC2_INSTANCE_LAUNCHING     * autoscaling:EC2_INSTANCE_TERMINATING
--
-- * 'lhRoleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
lifecycleHook ::
  LifecycleHook
lifecycleHook =
  LifecycleHook'
    { _lhDefaultResult = Nothing,
      _lhLifecycleHookName = Nothing,
      _lhHeartbeatTimeout = Nothing,
      _lhAutoScalingGroupName = Nothing,
      _lhNotificationMetadata = Nothing,
      _lhGlobalTimeout = Nothing,
      _lhNotificationTargetARN = Nothing,
      _lhLifecycleTransition = Nothing,
      _lhRoleARN = Nothing
    }

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The possible values are @CONTINUE@ and @ABANDON@ .
lhDefaultResult :: Lens' LifecycleHook (Maybe Text)
lhDefaultResult = lens _lhDefaultResult (\s a -> s {_lhDefaultResult = a})

-- | The name of the lifecycle hook.
lhLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
lhLifecycleHookName = lens _lhLifecycleHookName (\s a -> s {_lhLifecycleHookName = a})

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter.
lhHeartbeatTimeout :: Lens' LifecycleHook (Maybe Int)
lhHeartbeatTimeout = lens _lhHeartbeatTimeout (\s a -> s {_lhHeartbeatTimeout = a})

-- | The name of the Auto Scaling group for the lifecycle hook.
lhAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
lhAutoScalingGroupName = lens _lhAutoScalingGroupName (\s a -> s {_lhAutoScalingGroupName = a})

-- | Additional information that is included any time Amazon EC2 Auto Scaling sends a message to the notification target.
lhNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
lhNotificationMetadata = lens _lhNotificationMetadata (\s a -> s {_lhNotificationMetadata = a})

-- | The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
lhGlobalTimeout :: Lens' LifecycleHook (Maybe Int)
lhGlobalTimeout = lens _lhGlobalTimeout (\s a -> s {_lhGlobalTimeout = a})

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
lhNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
lhNotificationTargetARN = lens _lhNotificationTargetARN (\s a -> s {_lhNotificationTargetARN = a})

-- | The state of the EC2 instance to which to attach the lifecycle hook. The following are possible values:     * autoscaling:EC2_INSTANCE_LAUNCHING     * autoscaling:EC2_INSTANCE_TERMINATING
lhLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
lhLifecycleTransition = lens _lhLifecycleTransition (\s a -> s {_lhLifecycleTransition = a})

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
lhRoleARN :: Lens' LifecycleHook (Maybe Text)
lhRoleARN = lens _lhRoleARN (\s a -> s {_lhRoleARN = a})

instance FromXML LifecycleHook where
  parseXML x =
    LifecycleHook'
      <$> (x .@? "DefaultResult")
      <*> (x .@? "LifecycleHookName")
      <*> (x .@? "HeartbeatTimeout")
      <*> (x .@? "AutoScalingGroupName")
      <*> (x .@? "NotificationMetadata")
      <*> (x .@? "GlobalTimeout")
      <*> (x .@? "NotificationTargetARN")
      <*> (x .@? "LifecycleTransition")
      <*> (x .@? "RoleARN")

instance Hashable LifecycleHook

instance NFData LifecycleHook

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleHookSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleHookSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes information used to specify a lifecycle hook for an Auto Scaling group.
--
--
-- A lifecycle hook tells Amazon EC2 Auto Scaling to perform an action on an instance when the instance launches (before it is put into service) or as the instance terminates (before it is fully terminated).
--
-- This step is a part of the procedure for creating a lifecycle hook for an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Amazon EC2 Auto Scaling launches or terminates instances.
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Amazon EC2 Auto Scaling to publish lifecycle notifications to the target.
--
--     * __Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.__
--
--     * If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.
--
--     * If you finish before the timeout period ends, complete the lifecycle action.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
-- /See:/ 'lifecycleHookSpecification' smart constructor.
data LifecycleHookSpecification = LifecycleHookSpecification'
  { _lhsDefaultResult ::
      !(Maybe Text),
    _lhsHeartbeatTimeout :: !(Maybe Int),
    _lhsNotificationMetadata ::
      !(Maybe Text),
    _lhsNotificationTargetARN ::
      !(Maybe Text),
    _lhsRoleARN :: !(Maybe Text),
    _lhsLifecycleHookName :: !Text,
    _lhsLifecycleTransition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecycleHookSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhsDefaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @ABANDON@ .
--
-- * 'lhsHeartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
--
-- * 'lhsNotificationMetadata' - Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- * 'lhsNotificationTargetARN' - The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- * 'lhsRoleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
--
-- * 'lhsLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'lhsLifecycleTransition' - The state of the EC2 instance to which you want to attach the lifecycle hook. The valid values are:     * autoscaling:EC2_INSTANCE_LAUNCHING     * autoscaling:EC2_INSTANCE_TERMINATING
lifecycleHookSpecification ::
  -- | 'lhsLifecycleHookName'
  Text ->
  -- | 'lhsLifecycleTransition'
  Text ->
  LifecycleHookSpecification
lifecycleHookSpecification
  pLifecycleHookName_
  pLifecycleTransition_ =
    LifecycleHookSpecification'
      { _lhsDefaultResult = Nothing,
        _lhsHeartbeatTimeout = Nothing,
        _lhsNotificationMetadata = Nothing,
        _lhsNotificationTargetARN = Nothing,
        _lhsRoleARN = Nothing,
        _lhsLifecycleHookName = pLifecycleHookName_,
        _lhsLifecycleTransition = pLifecycleTransition_
      }

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @ABANDON@ .
lhsDefaultResult :: Lens' LifecycleHookSpecification (Maybe Text)
lhsDefaultResult = lens _lhsDefaultResult (\s a -> s {_lhsDefaultResult = a})

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
lhsHeartbeatTimeout :: Lens' LifecycleHookSpecification (Maybe Int)
lhsHeartbeatTimeout = lens _lhsHeartbeatTimeout (\s a -> s {_lhsHeartbeatTimeout = a})

-- | Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
lhsNotificationMetadata :: Lens' LifecycleHookSpecification (Maybe Text)
lhsNotificationMetadata = lens _lhsNotificationMetadata (\s a -> s {_lhsNotificationMetadata = a})

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
lhsNotificationTargetARN :: Lens' LifecycleHookSpecification (Maybe Text)
lhsNotificationTargetARN = lens _lhsNotificationTargetARN (\s a -> s {_lhsNotificationTargetARN = a})

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
lhsRoleARN :: Lens' LifecycleHookSpecification (Maybe Text)
lhsRoleARN = lens _lhsRoleARN (\s a -> s {_lhsRoleARN = a})

-- | The name of the lifecycle hook.
lhsLifecycleHookName :: Lens' LifecycleHookSpecification Text
lhsLifecycleHookName = lens _lhsLifecycleHookName (\s a -> s {_lhsLifecycleHookName = a})

-- | The state of the EC2 instance to which you want to attach the lifecycle hook. The valid values are:     * autoscaling:EC2_INSTANCE_LAUNCHING     * autoscaling:EC2_INSTANCE_TERMINATING
lhsLifecycleTransition :: Lens' LifecycleHookSpecification Text
lhsLifecycleTransition = lens _lhsLifecycleTransition (\s a -> s {_lhsLifecycleTransition = a})

instance Hashable LifecycleHookSpecification

instance NFData LifecycleHookSpecification

instance ToQuery LifecycleHookSpecification where
  toQuery LifecycleHookSpecification' {..} =
    mconcat
      [ "DefaultResult" =: _lhsDefaultResult,
        "HeartbeatTimeout" =: _lhsHeartbeatTimeout,
        "NotificationMetadata" =: _lhsNotificationMetadata,
        "NotificationTargetARN" =: _lhsNotificationTargetARN,
        "RoleARN" =: _lhsRoleARN,
        "LifecycleHookName" =: _lhsLifecycleHookName,
        "LifecycleTransition" =: _lhsLifecycleTransition
      ]

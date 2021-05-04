{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleHookSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleHookSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes information used to specify a lifecycle hook for an Auto
-- Scaling group.
--
-- A lifecycle hook tells Amazon EC2 Auto Scaling to perform an action on
-- an instance when the instance launches (before it is put into service)
-- or as the instance terminates (before it is fully terminated).
--
-- This step is a part of the procedure for creating a lifecycle hook for
-- an Auto Scaling group:
--
-- 1.  (Optional) Create a Lambda function and a rule that allows
--     CloudWatch Events to invoke your Lambda function when Amazon EC2
--     Auto Scaling launches or terminates instances.
--
-- 2.  (Optional) Create a notification target and an IAM role. The target
--     can be either an Amazon SQS queue or an Amazon SNS topic. The role
--     allows Amazon EC2 Auto Scaling to publish lifecycle notifications to
--     the target.
--
-- 3.  __Create the lifecycle hook. Specify whether the hook is used when
--     the instances launch or terminate.__
--
-- 4.  If you need more time, record the lifecycle action heartbeat to keep
--     the instance in a pending state.
--
-- 5.  If you finish before the timeout period ends, complete the lifecycle
--     action.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newLifecycleHookSpecification' smart constructor.
data LifecycleHookSpecification = LifecycleHookSpecification'
  { -- | The ARN of the IAM role that allows the Auto Scaling group to publish to
    -- the specified notification target, for example, an Amazon SNS topic or
    -- an Amazon SQS queue.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications
    -- to when an instance is in the transition state for the lifecycle hook.
    -- The notification target can be either an SQS queue or an SNS topic.
    notificationTargetARN :: Prelude.Maybe Prelude.Text,
    -- | The maximum time, in seconds, that can elapse before the lifecycle hook
    -- times out.
    --
    -- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the
    -- action that you specified in the @DefaultResult@ parameter. You can
    -- prevent the lifecycle hook from timing out by calling
    -- RecordLifecycleActionHeartbeat.
    heartbeatTimeout :: Prelude.Maybe Prelude.Int,
    -- | Additional information that you want to include any time Amazon EC2 Auto
    -- Scaling sends a message to the notification target.
    notificationMetadata :: Prelude.Maybe Prelude.Text,
    -- | Defines the action the Auto Scaling group should take when the lifecycle
    -- hook timeout elapses or if an unexpected failure occurs. The valid
    -- values are @CONTINUE@ and @ABANDON@. The default value is @ABANDON@.
    defaultResult :: Prelude.Maybe Prelude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Prelude.Text,
    -- | The state of the EC2 instance to which you want to attach the lifecycle
    -- hook. The valid values are:
    --
    -- -   autoscaling:EC2_INSTANCE_LAUNCHING
    --
    -- -   autoscaling:EC2_INSTANCE_TERMINATING
    lifecycleTransition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LifecycleHookSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'lifecycleHookSpecification_roleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target, for example, an Amazon SNS topic or
-- an Amazon SQS queue.
--
-- 'notificationTargetARN', 'lifecycleHookSpecification_notificationTargetARN' - The ARN of the target that Amazon EC2 Auto Scaling sends notifications
-- to when an instance is in the transition state for the lifecycle hook.
-- The notification target can be either an SQS queue or an SNS topic.
--
-- 'heartbeatTimeout', 'lifecycleHookSpecification_heartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out.
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the
-- action that you specified in the @DefaultResult@ parameter. You can
-- prevent the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat.
--
-- 'notificationMetadata', 'lifecycleHookSpecification_notificationMetadata' - Additional information that you want to include any time Amazon EC2 Auto
-- Scaling sends a message to the notification target.
--
-- 'defaultResult', 'lifecycleHookSpecification_defaultResult' - Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The valid
-- values are @CONTINUE@ and @ABANDON@. The default value is @ABANDON@.
--
-- 'lifecycleHookName', 'lifecycleHookSpecification_lifecycleHookName' - The name of the lifecycle hook.
--
-- 'lifecycleTransition', 'lifecycleHookSpecification_lifecycleTransition' - The state of the EC2 instance to which you want to attach the lifecycle
-- hook. The valid values are:
--
-- -   autoscaling:EC2_INSTANCE_LAUNCHING
--
-- -   autoscaling:EC2_INSTANCE_TERMINATING
newLifecycleHookSpecification ::
  -- | 'lifecycleHookName'
  Prelude.Text ->
  -- | 'lifecycleTransition'
  Prelude.Text ->
  LifecycleHookSpecification
newLifecycleHookSpecification
  pLifecycleHookName_
  pLifecycleTransition_ =
    LifecycleHookSpecification'
      { roleARN =
          Prelude.Nothing,
        notificationTargetARN = Prelude.Nothing,
        heartbeatTimeout = Prelude.Nothing,
        notificationMetadata = Prelude.Nothing,
        defaultResult = Prelude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        lifecycleTransition = pLifecycleTransition_
      }

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target, for example, an Amazon SNS topic or
-- an Amazon SQS queue.
lifecycleHookSpecification_roleARN :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_roleARN = Lens.lens (\LifecycleHookSpecification' {roleARN} -> roleARN) (\s@LifecycleHookSpecification' {} a -> s {roleARN = a} :: LifecycleHookSpecification)

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications
-- to when an instance is in the transition state for the lifecycle hook.
-- The notification target can be either an SQS queue or an SNS topic.
lifecycleHookSpecification_notificationTargetARN :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_notificationTargetARN = Lens.lens (\LifecycleHookSpecification' {notificationTargetARN} -> notificationTargetARN) (\s@LifecycleHookSpecification' {} a -> s {notificationTargetARN = a} :: LifecycleHookSpecification)

-- | The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out.
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the
-- action that you specified in the @DefaultResult@ parameter. You can
-- prevent the lifecycle hook from timing out by calling
-- RecordLifecycleActionHeartbeat.
lifecycleHookSpecification_heartbeatTimeout :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Int)
lifecycleHookSpecification_heartbeatTimeout = Lens.lens (\LifecycleHookSpecification' {heartbeatTimeout} -> heartbeatTimeout) (\s@LifecycleHookSpecification' {} a -> s {heartbeatTimeout = a} :: LifecycleHookSpecification)

-- | Additional information that you want to include any time Amazon EC2 Auto
-- Scaling sends a message to the notification target.
lifecycleHookSpecification_notificationMetadata :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_notificationMetadata = Lens.lens (\LifecycleHookSpecification' {notificationMetadata} -> notificationMetadata) (\s@LifecycleHookSpecification' {} a -> s {notificationMetadata = a} :: LifecycleHookSpecification)

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The valid
-- values are @CONTINUE@ and @ABANDON@. The default value is @ABANDON@.
lifecycleHookSpecification_defaultResult :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_defaultResult = Lens.lens (\LifecycleHookSpecification' {defaultResult} -> defaultResult) (\s@LifecycleHookSpecification' {} a -> s {defaultResult = a} :: LifecycleHookSpecification)

-- | The name of the lifecycle hook.
lifecycleHookSpecification_lifecycleHookName :: Lens.Lens' LifecycleHookSpecification Prelude.Text
lifecycleHookSpecification_lifecycleHookName = Lens.lens (\LifecycleHookSpecification' {lifecycleHookName} -> lifecycleHookName) (\s@LifecycleHookSpecification' {} a -> s {lifecycleHookName = a} :: LifecycleHookSpecification)

-- | The state of the EC2 instance to which you want to attach the lifecycle
-- hook. The valid values are:
--
-- -   autoscaling:EC2_INSTANCE_LAUNCHING
--
-- -   autoscaling:EC2_INSTANCE_TERMINATING
lifecycleHookSpecification_lifecycleTransition :: Lens.Lens' LifecycleHookSpecification Prelude.Text
lifecycleHookSpecification_lifecycleTransition = Lens.lens (\LifecycleHookSpecification' {lifecycleTransition} -> lifecycleTransition) (\s@LifecycleHookSpecification' {} a -> s {lifecycleTransition = a} :: LifecycleHookSpecification)

instance Prelude.Hashable LifecycleHookSpecification

instance Prelude.NFData LifecycleHookSpecification

instance Prelude.ToQuery LifecycleHookSpecification where
  toQuery LifecycleHookSpecification' {..} =
    Prelude.mconcat
      [ "RoleARN" Prelude.=: roleARN,
        "NotificationTargetARN"
          Prelude.=: notificationTargetARN,
        "HeartbeatTimeout" Prelude.=: heartbeatTimeout,
        "NotificationMetadata"
          Prelude.=: notificationMetadata,
        "DefaultResult" Prelude.=: defaultResult,
        "LifecycleHookName" Prelude.=: lifecycleHookName,
        "LifecycleTransition" Prelude.=: lifecycleTransition
      ]

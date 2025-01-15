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
-- Module      : Amazonka.AutoScaling.Types.LifecycleHookSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LifecycleHookSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes information used to specify a lifecycle hook for an Auto
-- Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newLifecycleHookSpecification' smart constructor.
data LifecycleHookSpecification = LifecycleHookSpecification'
  { -- | The action the Auto Scaling group takes when the lifecycle hook timeout
    -- elapses or if an unexpected failure occurs. The default value is
    -- @ABANDON@.
    --
    -- Valid values: @CONTINUE@ | @ABANDON@
    defaultResult :: Prelude.Maybe Prelude.Text,
    -- | The maximum time, in seconds, that can elapse before the lifecycle hook
    -- times out. The range is from @30@ to @7200@ seconds. The default value
    -- is @3600@ seconds (1 hour).
    heartbeatTimeout :: Prelude.Maybe Prelude.Int,
    -- | Additional information that you want to include any time Amazon EC2 Auto
    -- Scaling sends a message to the notification target.
    notificationMetadata :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the notification target that Amazon
    -- EC2 Auto Scaling sends notifications to when an instance is in a wait
    -- state for the lifecycle hook. You can specify an Amazon SNS topic or an
    -- Amazon SQS queue.
    notificationTargetARN :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that allows the Auto Scaling group to publish to
    -- the specified notification target. For information about creating this
    -- role, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/prepare-for-lifecycle-notifications.html#lifecycle-hook-notification-target Configure a notification target for a lifecycle hook>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Valid only if the notification target is an Amazon SNS topic or an
    -- Amazon SQS queue.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Prelude.Text,
    -- | The lifecycle transition. For Auto Scaling groups, there are two major
    -- lifecycle transitions.
    --
    -- -   To create a lifecycle hook for scale-out events, specify
    --     @autoscaling:EC2_INSTANCE_LAUNCHING@.
    --
    -- -   To create a lifecycle hook for scale-in events, specify
    --     @autoscaling:EC2_INSTANCE_TERMINATING@.
    lifecycleTransition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleHookSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultResult', 'lifecycleHookSpecification_defaultResult' - The action the Auto Scaling group takes when the lifecycle hook timeout
-- elapses or if an unexpected failure occurs. The default value is
-- @ABANDON@.
--
-- Valid values: @CONTINUE@ | @ABANDON@
--
-- 'heartbeatTimeout', 'lifecycleHookSpecification_heartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out. The range is from @30@ to @7200@ seconds. The default value
-- is @3600@ seconds (1 hour).
--
-- 'notificationMetadata', 'lifecycleHookSpecification_notificationMetadata' - Additional information that you want to include any time Amazon EC2 Auto
-- Scaling sends a message to the notification target.
--
-- 'notificationTargetARN', 'lifecycleHookSpecification_notificationTargetARN' - The Amazon Resource Name (ARN) of the notification target that Amazon
-- EC2 Auto Scaling sends notifications to when an instance is in a wait
-- state for the lifecycle hook. You can specify an Amazon SNS topic or an
-- Amazon SQS queue.
--
-- 'roleARN', 'lifecycleHookSpecification_roleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target. For information about creating this
-- role, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/prepare-for-lifecycle-notifications.html#lifecycle-hook-notification-target Configure a notification target for a lifecycle hook>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid only if the notification target is an Amazon SNS topic or an
-- Amazon SQS queue.
--
-- 'lifecycleHookName', 'lifecycleHookSpecification_lifecycleHookName' - The name of the lifecycle hook.
--
-- 'lifecycleTransition', 'lifecycleHookSpecification_lifecycleTransition' - The lifecycle transition. For Auto Scaling groups, there are two major
-- lifecycle transitions.
--
-- -   To create a lifecycle hook for scale-out events, specify
--     @autoscaling:EC2_INSTANCE_LAUNCHING@.
--
-- -   To create a lifecycle hook for scale-in events, specify
--     @autoscaling:EC2_INSTANCE_TERMINATING@.
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
      { defaultResult =
          Prelude.Nothing,
        heartbeatTimeout = Prelude.Nothing,
        notificationMetadata = Prelude.Nothing,
        notificationTargetARN = Prelude.Nothing,
        roleARN = Prelude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        lifecycleTransition = pLifecycleTransition_
      }

-- | The action the Auto Scaling group takes when the lifecycle hook timeout
-- elapses or if an unexpected failure occurs. The default value is
-- @ABANDON@.
--
-- Valid values: @CONTINUE@ | @ABANDON@
lifecycleHookSpecification_defaultResult :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_defaultResult = Lens.lens (\LifecycleHookSpecification' {defaultResult} -> defaultResult) (\s@LifecycleHookSpecification' {} a -> s {defaultResult = a} :: LifecycleHookSpecification)

-- | The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out. The range is from @30@ to @7200@ seconds. The default value
-- is @3600@ seconds (1 hour).
lifecycleHookSpecification_heartbeatTimeout :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Int)
lifecycleHookSpecification_heartbeatTimeout = Lens.lens (\LifecycleHookSpecification' {heartbeatTimeout} -> heartbeatTimeout) (\s@LifecycleHookSpecification' {} a -> s {heartbeatTimeout = a} :: LifecycleHookSpecification)

-- | Additional information that you want to include any time Amazon EC2 Auto
-- Scaling sends a message to the notification target.
lifecycleHookSpecification_notificationMetadata :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_notificationMetadata = Lens.lens (\LifecycleHookSpecification' {notificationMetadata} -> notificationMetadata) (\s@LifecycleHookSpecification' {} a -> s {notificationMetadata = a} :: LifecycleHookSpecification)

-- | The Amazon Resource Name (ARN) of the notification target that Amazon
-- EC2 Auto Scaling sends notifications to when an instance is in a wait
-- state for the lifecycle hook. You can specify an Amazon SNS topic or an
-- Amazon SQS queue.
lifecycleHookSpecification_notificationTargetARN :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_notificationTargetARN = Lens.lens (\LifecycleHookSpecification' {notificationTargetARN} -> notificationTargetARN) (\s@LifecycleHookSpecification' {} a -> s {notificationTargetARN = a} :: LifecycleHookSpecification)

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target. For information about creating this
-- role, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/prepare-for-lifecycle-notifications.html#lifecycle-hook-notification-target Configure a notification target for a lifecycle hook>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid only if the notification target is an Amazon SNS topic or an
-- Amazon SQS queue.
lifecycleHookSpecification_roleARN :: Lens.Lens' LifecycleHookSpecification (Prelude.Maybe Prelude.Text)
lifecycleHookSpecification_roleARN = Lens.lens (\LifecycleHookSpecification' {roleARN} -> roleARN) (\s@LifecycleHookSpecification' {} a -> s {roleARN = a} :: LifecycleHookSpecification)

-- | The name of the lifecycle hook.
lifecycleHookSpecification_lifecycleHookName :: Lens.Lens' LifecycleHookSpecification Prelude.Text
lifecycleHookSpecification_lifecycleHookName = Lens.lens (\LifecycleHookSpecification' {lifecycleHookName} -> lifecycleHookName) (\s@LifecycleHookSpecification' {} a -> s {lifecycleHookName = a} :: LifecycleHookSpecification)

-- | The lifecycle transition. For Auto Scaling groups, there are two major
-- lifecycle transitions.
--
-- -   To create a lifecycle hook for scale-out events, specify
--     @autoscaling:EC2_INSTANCE_LAUNCHING@.
--
-- -   To create a lifecycle hook for scale-in events, specify
--     @autoscaling:EC2_INSTANCE_TERMINATING@.
lifecycleHookSpecification_lifecycleTransition :: Lens.Lens' LifecycleHookSpecification Prelude.Text
lifecycleHookSpecification_lifecycleTransition = Lens.lens (\LifecycleHookSpecification' {lifecycleTransition} -> lifecycleTransition) (\s@LifecycleHookSpecification' {} a -> s {lifecycleTransition = a} :: LifecycleHookSpecification)

instance Prelude.Hashable LifecycleHookSpecification where
  hashWithSalt _salt LifecycleHookSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` defaultResult
      `Prelude.hashWithSalt` heartbeatTimeout
      `Prelude.hashWithSalt` notificationMetadata
      `Prelude.hashWithSalt` notificationTargetARN
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` lifecycleHookName
      `Prelude.hashWithSalt` lifecycleTransition

instance Prelude.NFData LifecycleHookSpecification where
  rnf LifecycleHookSpecification' {..} =
    Prelude.rnf defaultResult `Prelude.seq`
      Prelude.rnf heartbeatTimeout `Prelude.seq`
        Prelude.rnf notificationMetadata `Prelude.seq`
          Prelude.rnf notificationTargetARN `Prelude.seq`
            Prelude.rnf roleARN `Prelude.seq`
              Prelude.rnf lifecycleHookName `Prelude.seq`
                Prelude.rnf lifecycleTransition

instance Data.ToQuery LifecycleHookSpecification where
  toQuery LifecycleHookSpecification' {..} =
    Prelude.mconcat
      [ "DefaultResult" Data.=: defaultResult,
        "HeartbeatTimeout" Data.=: heartbeatTimeout,
        "NotificationMetadata" Data.=: notificationMetadata,
        "NotificationTargetARN"
          Data.=: notificationTargetARN,
        "RoleARN" Data.=: roleARN,
        "LifecycleHookName" Data.=: lifecycleHookName,
        "LifecycleTransition" Data.=: lifecycleTransition
      ]

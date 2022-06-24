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
-- Module      : Amazonka.AutoScaling.Types.LifecycleHook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LifecycleHook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a lifecycle hook, which tells Amazon EC2 Auto Scaling that you
-- want to perform an action whenever it launches instances or terminates
-- instances.
--
-- /See:/ 'newLifecycleHook' smart constructor.
data LifecycleHook = LifecycleHook'
  { -- | The name of the lifecycle hook.
    lifecycleHookName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that allows the Auto Scaling group to publish to
    -- the specified notification target.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Additional information that is included any time Amazon EC2 Auto Scaling
    -- sends a message to the notification target.
    notificationMetadata :: Prelude.Maybe Prelude.Text,
    -- | Defines the action the Auto Scaling group should take when the lifecycle
    -- hook timeout elapses or if an unexpected failure occurs. The possible
    -- values are @CONTINUE@ and @ABANDON@.
    defaultResult :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications
    -- to when an instance is in the transition state for the lifecycle hook.
    -- The notification target can be either an SQS queue or an SNS topic.
    notificationTargetARN :: Prelude.Maybe Prelude.Text,
    -- | The maximum time, in seconds, that an instance can remain in a
    -- @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800
    -- seconds (48 hours) or 100 times @HeartbeatTimeout@, whichever is
    -- smaller.
    globalTimeout :: Prelude.Maybe Prelude.Int,
    -- | The name of the Auto Scaling group for the lifecycle hook.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The state of the EC2 instance to which to attach the lifecycle hook. The
    -- following are possible values:
    --
    -- -   autoscaling:EC2_INSTANCE_LAUNCHING
    --
    -- -   autoscaling:EC2_INSTANCE_TERMINATING
    lifecycleTransition :: Prelude.Maybe Prelude.Text,
    -- | The maximum time, in seconds, that can elapse before the lifecycle hook
    -- times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling
    -- performs the action that you specified in the @DefaultResult@ parameter.
    heartbeatTimeout :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleHookName', 'lifecycleHook_lifecycleHookName' - The name of the lifecycle hook.
--
-- 'roleARN', 'lifecycleHook_roleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
--
-- 'notificationMetadata', 'lifecycleHook_notificationMetadata' - Additional information that is included any time Amazon EC2 Auto Scaling
-- sends a message to the notification target.
--
-- 'defaultResult', 'lifecycleHook_defaultResult' - Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The possible
-- values are @CONTINUE@ and @ABANDON@.
--
-- 'notificationTargetARN', 'lifecycleHook_notificationTargetARN' - The ARN of the target that Amazon EC2 Auto Scaling sends notifications
-- to when an instance is in the transition state for the lifecycle hook.
-- The notification target can be either an SQS queue or an SNS topic.
--
-- 'globalTimeout', 'lifecycleHook_globalTimeout' - The maximum time, in seconds, that an instance can remain in a
-- @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800
-- seconds (48 hours) or 100 times @HeartbeatTimeout@, whichever is
-- smaller.
--
-- 'autoScalingGroupName', 'lifecycleHook_autoScalingGroupName' - The name of the Auto Scaling group for the lifecycle hook.
--
-- 'lifecycleTransition', 'lifecycleHook_lifecycleTransition' - The state of the EC2 instance to which to attach the lifecycle hook. The
-- following are possible values:
--
-- -   autoscaling:EC2_INSTANCE_LAUNCHING
--
-- -   autoscaling:EC2_INSTANCE_TERMINATING
--
-- 'heartbeatTimeout', 'lifecycleHook_heartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling
-- performs the action that you specified in the @DefaultResult@ parameter.
newLifecycleHook ::
  LifecycleHook
newLifecycleHook =
  LifecycleHook'
    { lifecycleHookName = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      notificationMetadata = Prelude.Nothing,
      defaultResult = Prelude.Nothing,
      notificationTargetARN = Prelude.Nothing,
      globalTimeout = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      lifecycleTransition = Prelude.Nothing,
      heartbeatTimeout = Prelude.Nothing
    }

-- | The name of the lifecycle hook.
lifecycleHook_lifecycleHookName :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Text)
lifecycleHook_lifecycleHookName = Lens.lens (\LifecycleHook' {lifecycleHookName} -> lifecycleHookName) (\s@LifecycleHook' {} a -> s {lifecycleHookName = a} :: LifecycleHook)

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
lifecycleHook_roleARN :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Text)
lifecycleHook_roleARN = Lens.lens (\LifecycleHook' {roleARN} -> roleARN) (\s@LifecycleHook' {} a -> s {roleARN = a} :: LifecycleHook)

-- | Additional information that is included any time Amazon EC2 Auto Scaling
-- sends a message to the notification target.
lifecycleHook_notificationMetadata :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Text)
lifecycleHook_notificationMetadata = Lens.lens (\LifecycleHook' {notificationMetadata} -> notificationMetadata) (\s@LifecycleHook' {} a -> s {notificationMetadata = a} :: LifecycleHook)

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The possible
-- values are @CONTINUE@ and @ABANDON@.
lifecycleHook_defaultResult :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Text)
lifecycleHook_defaultResult = Lens.lens (\LifecycleHook' {defaultResult} -> defaultResult) (\s@LifecycleHook' {} a -> s {defaultResult = a} :: LifecycleHook)

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications
-- to when an instance is in the transition state for the lifecycle hook.
-- The notification target can be either an SQS queue or an SNS topic.
lifecycleHook_notificationTargetARN :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Text)
lifecycleHook_notificationTargetARN = Lens.lens (\LifecycleHook' {notificationTargetARN} -> notificationTargetARN) (\s@LifecycleHook' {} a -> s {notificationTargetARN = a} :: LifecycleHook)

-- | The maximum time, in seconds, that an instance can remain in a
-- @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800
-- seconds (48 hours) or 100 times @HeartbeatTimeout@, whichever is
-- smaller.
lifecycleHook_globalTimeout :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Int)
lifecycleHook_globalTimeout = Lens.lens (\LifecycleHook' {globalTimeout} -> globalTimeout) (\s@LifecycleHook' {} a -> s {globalTimeout = a} :: LifecycleHook)

-- | The name of the Auto Scaling group for the lifecycle hook.
lifecycleHook_autoScalingGroupName :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Text)
lifecycleHook_autoScalingGroupName = Lens.lens (\LifecycleHook' {autoScalingGroupName} -> autoScalingGroupName) (\s@LifecycleHook' {} a -> s {autoScalingGroupName = a} :: LifecycleHook)

-- | The state of the EC2 instance to which to attach the lifecycle hook. The
-- following are possible values:
--
-- -   autoscaling:EC2_INSTANCE_LAUNCHING
--
-- -   autoscaling:EC2_INSTANCE_TERMINATING
lifecycleHook_lifecycleTransition :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Text)
lifecycleHook_lifecycleTransition = Lens.lens (\LifecycleHook' {lifecycleTransition} -> lifecycleTransition) (\s@LifecycleHook' {} a -> s {lifecycleTransition = a} :: LifecycleHook)

-- | The maximum time, in seconds, that can elapse before the lifecycle hook
-- times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling
-- performs the action that you specified in the @DefaultResult@ parameter.
lifecycleHook_heartbeatTimeout :: Lens.Lens' LifecycleHook (Prelude.Maybe Prelude.Int)
lifecycleHook_heartbeatTimeout = Lens.lens (\LifecycleHook' {heartbeatTimeout} -> heartbeatTimeout) (\s@LifecycleHook' {} a -> s {heartbeatTimeout = a} :: LifecycleHook)

instance Core.FromXML LifecycleHook where
  parseXML x =
    LifecycleHook'
      Prelude.<$> (x Core..@? "LifecycleHookName")
      Prelude.<*> (x Core..@? "RoleARN")
      Prelude.<*> (x Core..@? "NotificationMetadata")
      Prelude.<*> (x Core..@? "DefaultResult")
      Prelude.<*> (x Core..@? "NotificationTargetARN")
      Prelude.<*> (x Core..@? "GlobalTimeout")
      Prelude.<*> (x Core..@? "AutoScalingGroupName")
      Prelude.<*> (x Core..@? "LifecycleTransition")
      Prelude.<*> (x Core..@? "HeartbeatTimeout")

instance Prelude.Hashable LifecycleHook where
  hashWithSalt _salt LifecycleHook' {..} =
    _salt `Prelude.hashWithSalt` lifecycleHookName
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` notificationMetadata
      `Prelude.hashWithSalt` defaultResult
      `Prelude.hashWithSalt` notificationTargetARN
      `Prelude.hashWithSalt` globalTimeout
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` lifecycleTransition
      `Prelude.hashWithSalt` heartbeatTimeout

instance Prelude.NFData LifecycleHook where
  rnf LifecycleHook' {..} =
    Prelude.rnf lifecycleHookName
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf notificationMetadata
      `Prelude.seq` Prelude.rnf defaultResult
      `Prelude.seq` Prelude.rnf notificationTargetARN
      `Prelude.seq` Prelude.rnf globalTimeout
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf lifecycleTransition
      `Prelude.seq` Prelude.rnf heartbeatTimeout

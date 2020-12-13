{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleHook
  ( LifecycleHook (..),

    -- * Smart constructor
    mkLifecycleHook,

    -- * Lenses
    lhDefaultResult,
    lhLifecycleHookName,
    lhHeartbeatTimeout,
    lhAutoScalingGroupName,
    lhNotificationMetadata,
    lhGlobalTimeout,
    lhNotificationTargetARN,
    lhLifecycleTransition,
    lhRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a lifecycle hook, which tells Amazon EC2 Auto Scaling that you want to perform an action whenever it launches instances or terminates instances.
--
-- /See:/ 'mkLifecycleHook' smart constructor.
data LifecycleHook = LifecycleHook'
  { -- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The possible values are @CONTINUE@ and @ABANDON@ .
    defaultResult :: Lude.Maybe Lude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Lude.Maybe Lude.Text,
    -- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter.
    heartbeatTimeout :: Lude.Maybe Lude.Int,
    -- | The name of the Auto Scaling group for the lifecycle hook.
    autoScalingGroupName :: Lude.Maybe Lude.Text,
    -- | Additional information that is included any time Amazon EC2 Auto Scaling sends a message to the notification target.
    notificationMetadata :: Lude.Maybe Lude.Text,
    -- | The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
    globalTimeout :: Lude.Maybe Lude.Int,
    -- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
    notificationTargetARN :: Lude.Maybe Lude.Text,
    -- | The state of the EC2 instance to which to attach the lifecycle hook. The following are possible values:
    --
    --
    --     * autoscaling:EC2_INSTANCE_LAUNCHING
    --
    --
    --     * autoscaling:EC2_INSTANCE_TERMINATING
    lifecycleTransition :: Lude.Maybe Lude.Text,
    -- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleHook' with the minimum fields required to make a request.
--
-- * 'defaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The possible values are @CONTINUE@ and @ABANDON@ .
-- * 'lifecycleHookName' - The name of the lifecycle hook.
-- * 'heartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group for the lifecycle hook.
-- * 'notificationMetadata' - Additional information that is included any time Amazon EC2 Auto Scaling sends a message to the notification target.
-- * 'globalTimeout' - The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
-- * 'notificationTargetARN' - The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
-- * 'lifecycleTransition' - The state of the EC2 instance to which to attach the lifecycle hook. The following are possible values:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
-- * 'roleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
mkLifecycleHook ::
  LifecycleHook
mkLifecycleHook =
  LifecycleHook'
    { defaultResult = Lude.Nothing,
      lifecycleHookName = Lude.Nothing,
      heartbeatTimeout = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      notificationMetadata = Lude.Nothing,
      globalTimeout = Lude.Nothing,
      notificationTargetARN = Lude.Nothing,
      lifecycleTransition = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The possible values are @CONTINUE@ and @ABANDON@ .
--
-- /Note:/ Consider using 'defaultResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhDefaultResult :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Text)
lhDefaultResult = Lens.lens (defaultResult :: LifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {defaultResult = a} :: LifecycleHook)
{-# DEPRECATED lhDefaultResult "Use generic-lens or generic-optics with 'defaultResult' instead." #-}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhLifecycleHookName :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Text)
lhLifecycleHookName = Lens.lens (lifecycleHookName :: LifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleHookName = a} :: LifecycleHook)
{-# DEPRECATED lhLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhHeartbeatTimeout :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Int)
lhHeartbeatTimeout = Lens.lens (heartbeatTimeout :: LifecycleHook -> Lude.Maybe Lude.Int) (\s a -> s {heartbeatTimeout = a} :: LifecycleHook)
{-# DEPRECATED lhHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

-- | The name of the Auto Scaling group for the lifecycle hook.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhAutoScalingGroupName :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Text)
lhAutoScalingGroupName = Lens.lens (autoScalingGroupName :: LifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: LifecycleHook)
{-# DEPRECATED lhAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | Additional information that is included any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- /Note:/ Consider using 'notificationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhNotificationMetadata :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Text)
lhNotificationMetadata = Lens.lens (notificationMetadata :: LifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {notificationMetadata = a} :: LifecycleHook)
{-# DEPRECATED lhNotificationMetadata "Use generic-lens or generic-optics with 'notificationMetadata' instead." #-}

-- | The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
--
-- /Note:/ Consider using 'globalTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhGlobalTimeout :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Int)
lhGlobalTimeout = Lens.lens (globalTimeout :: LifecycleHook -> Lude.Maybe Lude.Int) (\s a -> s {globalTimeout = a} :: LifecycleHook)
{-# DEPRECATED lhGlobalTimeout "Use generic-lens or generic-optics with 'globalTimeout' instead." #-}

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- /Note:/ Consider using 'notificationTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhNotificationTargetARN :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Text)
lhNotificationTargetARN = Lens.lens (notificationTargetARN :: LifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {notificationTargetARN = a} :: LifecycleHook)
{-# DEPRECATED lhNotificationTargetARN "Use generic-lens or generic-optics with 'notificationTargetARN' instead." #-}

-- | The state of the EC2 instance to which to attach the lifecycle hook. The following are possible values:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
--
-- /Note:/ Consider using 'lifecycleTransition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhLifecycleTransition :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Text)
lhLifecycleTransition = Lens.lens (lifecycleTransition :: LifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleTransition = a} :: LifecycleHook)
{-# DEPRECATED lhLifecycleTransition "Use generic-lens or generic-optics with 'lifecycleTransition' instead." #-}

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhRoleARN :: Lens.Lens' LifecycleHook (Lude.Maybe Lude.Text)
lhRoleARN = Lens.lens (roleARN :: LifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: LifecycleHook)
{-# DEPRECATED lhRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromXML LifecycleHook where
  parseXML x =
    LifecycleHook'
      Lude.<$> (x Lude..@? "DefaultResult")
      Lude.<*> (x Lude..@? "LifecycleHookName")
      Lude.<*> (x Lude..@? "HeartbeatTimeout")
      Lude.<*> (x Lude..@? "AutoScalingGroupName")
      Lude.<*> (x Lude..@? "NotificationMetadata")
      Lude.<*> (x Lude..@? "GlobalTimeout")
      Lude.<*> (x Lude..@? "NotificationTargetARN")
      Lude.<*> (x Lude..@? "LifecycleTransition")
      Lude.<*> (x Lude..@? "RoleARN")

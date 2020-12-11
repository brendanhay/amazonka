-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleHookSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleHookSpecification
  ( LifecycleHookSpecification (..),

    -- * Smart constructor
    mkLifecycleHookSpecification,

    -- * Lenses
    lhsDefaultResult,
    lhsHeartbeatTimeout,
    lhsNotificationMetadata,
    lhsNotificationTargetARN,
    lhsRoleARN,
    lhsLifecycleHookName,
    lhsLifecycleTransition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes information used to specify a lifecycle hook for an Auto Scaling group.
--
-- A lifecycle hook tells Amazon EC2 Auto Scaling to perform an action on an instance when the instance launches (before it is put into service) or as the instance terminates (before it is fully terminated).
-- This step is a part of the procedure for creating a lifecycle hook for an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Amazon EC2 Auto Scaling launches or terminates instances.
--
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Amazon EC2 Auto Scaling to publish lifecycle notifications to the target.
--
--
--     * __Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.__
--
--
--     * If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.
--
--
--     * If you finish before the timeout period ends, complete the lifecycle action.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkLifecycleHookSpecification' smart constructor.
data LifecycleHookSpecification = LifecycleHookSpecification'
  { defaultResult ::
      Lude.Maybe Lude.Text,
    heartbeatTimeout ::
      Lude.Maybe Lude.Int,
    notificationMetadata ::
      Lude.Maybe Lude.Text,
    notificationTargetARN ::
      Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    lifecycleHookName :: Lude.Text,
    lifecycleTransition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleHookSpecification' with the minimum fields required to make a request.
--
-- * 'defaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @ABANDON@ .
-- * 'heartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out.
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
-- * 'lifecycleHookName' - The name of the lifecycle hook.
-- * 'lifecycleTransition' - The state of the EC2 instance to which you want to attach the lifecycle hook. The valid values are:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
-- * 'notificationMetadata' - Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
-- * 'notificationTargetARN' - The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
-- * 'roleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
mkLifecycleHookSpecification ::
  -- | 'lifecycleHookName'
  Lude.Text ->
  -- | 'lifecycleTransition'
  Lude.Text ->
  LifecycleHookSpecification
mkLifecycleHookSpecification
  pLifecycleHookName_
  pLifecycleTransition_ =
    LifecycleHookSpecification'
      { defaultResult = Lude.Nothing,
        heartbeatTimeout = Lude.Nothing,
        notificationMetadata = Lude.Nothing,
        notificationTargetARN = Lude.Nothing,
        roleARN = Lude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        lifecycleTransition = pLifecycleTransition_
      }

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @ABANDON@ .
--
-- /Note:/ Consider using 'defaultResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsDefaultResult :: Lens.Lens' LifecycleHookSpecification (Lude.Maybe Lude.Text)
lhsDefaultResult = Lens.lens (defaultResult :: LifecycleHookSpecification -> Lude.Maybe Lude.Text) (\s a -> s {defaultResult = a} :: LifecycleHookSpecification)
{-# DEPRECATED lhsDefaultResult "Use generic-lens or generic-optics with 'defaultResult' instead." #-}

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out.
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsHeartbeatTimeout :: Lens.Lens' LifecycleHookSpecification (Lude.Maybe Lude.Int)
lhsHeartbeatTimeout = Lens.lens (heartbeatTimeout :: LifecycleHookSpecification -> Lude.Maybe Lude.Int) (\s a -> s {heartbeatTimeout = a} :: LifecycleHookSpecification)
{-# DEPRECATED lhsHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

-- | Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- /Note:/ Consider using 'notificationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsNotificationMetadata :: Lens.Lens' LifecycleHookSpecification (Lude.Maybe Lude.Text)
lhsNotificationMetadata = Lens.lens (notificationMetadata :: LifecycleHookSpecification -> Lude.Maybe Lude.Text) (\s a -> s {notificationMetadata = a} :: LifecycleHookSpecification)
{-# DEPRECATED lhsNotificationMetadata "Use generic-lens or generic-optics with 'notificationMetadata' instead." #-}

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- /Note:/ Consider using 'notificationTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsNotificationTargetARN :: Lens.Lens' LifecycleHookSpecification (Lude.Maybe Lude.Text)
lhsNotificationTargetARN = Lens.lens (notificationTargetARN :: LifecycleHookSpecification -> Lude.Maybe Lude.Text) (\s a -> s {notificationTargetARN = a} :: LifecycleHookSpecification)
{-# DEPRECATED lhsNotificationTargetARN "Use generic-lens or generic-optics with 'notificationTargetARN' instead." #-}

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsRoleARN :: Lens.Lens' LifecycleHookSpecification (Lude.Maybe Lude.Text)
lhsRoleARN = Lens.lens (roleARN :: LifecycleHookSpecification -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: LifecycleHookSpecification)
{-# DEPRECATED lhsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsLifecycleHookName :: Lens.Lens' LifecycleHookSpecification Lude.Text
lhsLifecycleHookName = Lens.lens (lifecycleHookName :: LifecycleHookSpecification -> Lude.Text) (\s a -> s {lifecycleHookName = a} :: LifecycleHookSpecification)
{-# DEPRECATED lhsLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The state of the EC2 instance to which you want to attach the lifecycle hook. The valid values are:
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
lhsLifecycleTransition :: Lens.Lens' LifecycleHookSpecification Lude.Text
lhsLifecycleTransition = Lens.lens (lifecycleTransition :: LifecycleHookSpecification -> Lude.Text) (\s a -> s {lifecycleTransition = a} :: LifecycleHookSpecification)
{-# DEPRECATED lhsLifecycleTransition "Use generic-lens or generic-optics with 'lifecycleTransition' instead." #-}

instance Lude.ToQuery LifecycleHookSpecification where
  toQuery LifecycleHookSpecification' {..} =
    Lude.mconcat
      [ "DefaultResult" Lude.=: defaultResult,
        "HeartbeatTimeout" Lude.=: heartbeatTimeout,
        "NotificationMetadata" Lude.=: notificationMetadata,
        "NotificationTargetARN" Lude.=: notificationTargetARN,
        "RoleARN" Lude.=: roleARN,
        "LifecycleHookName" Lude.=: lifecycleHookName,
        "LifecycleTransition" Lude.=: lifecycleTransition
      ]

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutLifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a lifecycle hook for the specified Auto Scaling group.
--
-- A lifecycle hook tells Amazon EC2 Auto Scaling to perform an action on an instance when the instance launches (before it is put into service) or as the instance terminates (before it is fully terminated).
-- This step is a part of the procedure for adding a lifecycle hook to an Auto Scaling group:
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
--     * If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state using the 'RecordLifecycleActionHeartbeat' API call.
--
--
--     * If you finish before the timeout period ends, complete the lifecycle action using the 'CompleteLifecycleAction' API call.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you exceed your maximum limit of lifecycle hooks, which by default is 50 per Auto Scaling group, the call fails.
-- You can view the lifecycle hooks for an Auto Scaling group using the 'DescribeLifecycleHooks' API call. If you are no longer using a lifecycle hook, you can delete it by calling the 'DeleteLifecycleHook' API.
module Network.AWS.AutoScaling.PutLifecycleHook
  ( -- * Creating a request
    PutLifecycleHook (..),
    mkPutLifecycleHook,

    -- ** Request lenses
    plhDefaultResult,
    plhLifecycleHookName,
    plhHeartbeatTimeout,
    plhAutoScalingGroupName,
    plhNotificationMetadata,
    plhNotificationTargetARN,
    plhLifecycleTransition,
    plhRoleARN,

    -- * Destructuring the response
    PutLifecycleHookResponse (..),
    mkPutLifecycleHookResponse,

    -- ** Response lenses
    plhrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLifecycleHook' smart constructor.
data PutLifecycleHook = PutLifecycleHook'
  { -- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. This parameter can be either @CONTINUE@ or @ABANDON@ . The default value is @ABANDON@ .
    defaultResult :: Lude.Maybe Lude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Lude.Text,
    -- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. The range is from @30@ to @7200@ seconds. The default value is @3600@ seconds (1 hour).
    --
    -- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling the 'RecordLifecycleActionHeartbeat' API.
    heartbeatTimeout :: Lude.Maybe Lude.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text,
    -- | Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
    notificationMetadata :: Lude.Maybe Lude.Text,
    -- | The ARN of the notification target that Amazon EC2 Auto Scaling uses to notify you when an instance is in the transition state for the lifecycle hook. This target can be either an SQS queue or an SNS topic.
    --
    -- If you specify an empty string, this overrides the current ARN.
    -- This operation uses the JSON format when sending notifications to an Amazon SQS queue, and an email key-value pair format when sending notifications to an Amazon SNS topic.
    -- When you specify a notification target, Amazon EC2 Auto Scaling sends it a test message. Test messages contain the following additional key-value pair: @"Event": "autoscaling:TEST_NOTIFICATION"@ .
    notificationTargetARN :: Lude.Maybe Lude.Text,
    -- | The instance state to which you want to attach the lifecycle hook. The valid values are:
    --
    --
    --     * autoscaling:EC2_INSTANCE_LAUNCHING
    --
    --
    --     * autoscaling:EC2_INSTANCE_TERMINATING
    --
    --
    -- Required for new lifecycle hooks, but optional when updating existing hooks.
    lifecycleTransition :: Lude.Maybe Lude.Text,
    -- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
    --
    -- Required for new lifecycle hooks, but optional when updating existing hooks.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLifecycleHook' with the minimum fields required to make a request.
--
-- * 'defaultResult' - Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. This parameter can be either @CONTINUE@ or @ABANDON@ . The default value is @ABANDON@ .
-- * 'lifecycleHookName' - The name of the lifecycle hook.
-- * 'heartbeatTimeout' - The maximum time, in seconds, that can elapse before the lifecycle hook times out. The range is from @30@ to @7200@ seconds. The default value is @3600@ seconds (1 hour).
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling the 'RecordLifecycleActionHeartbeat' API.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'notificationMetadata' - Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
-- * 'notificationTargetARN' - The ARN of the notification target that Amazon EC2 Auto Scaling uses to notify you when an instance is in the transition state for the lifecycle hook. This target can be either an SQS queue or an SNS topic.
--
-- If you specify an empty string, this overrides the current ARN.
-- This operation uses the JSON format when sending notifications to an Amazon SQS queue, and an email key-value pair format when sending notifications to an Amazon SNS topic.
-- When you specify a notification target, Amazon EC2 Auto Scaling sends it a test message. Test messages contain the following additional key-value pair: @"Event": "autoscaling:TEST_NOTIFICATION"@ .
-- * 'lifecycleTransition' - The instance state to which you want to attach the lifecycle hook. The valid values are:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
-- Required for new lifecycle hooks, but optional when updating existing hooks.
-- * 'roleARN' - The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
--
-- Required for new lifecycle hooks, but optional when updating existing hooks.
mkPutLifecycleHook ::
  -- | 'lifecycleHookName'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  PutLifecycleHook
mkPutLifecycleHook pLifecycleHookName_ pAutoScalingGroupName_ =
  PutLifecycleHook'
    { defaultResult = Lude.Nothing,
      lifecycleHookName = pLifecycleHookName_,
      heartbeatTimeout = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_,
      notificationMetadata = Lude.Nothing,
      notificationTargetARN = Lude.Nothing,
      lifecycleTransition = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. This parameter can be either @CONTINUE@ or @ABANDON@ . The default value is @ABANDON@ .
--
-- /Note:/ Consider using 'defaultResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhDefaultResult :: Lens.Lens' PutLifecycleHook (Lude.Maybe Lude.Text)
plhDefaultResult = Lens.lens (defaultResult :: PutLifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {defaultResult = a} :: PutLifecycleHook)
{-# DEPRECATED plhDefaultResult "Use generic-lens or generic-optics with 'defaultResult' instead." #-}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhLifecycleHookName :: Lens.Lens' PutLifecycleHook Lude.Text
plhLifecycleHookName = Lens.lens (lifecycleHookName :: PutLifecycleHook -> Lude.Text) (\s a -> s {lifecycleHookName = a} :: PutLifecycleHook)
{-# DEPRECATED plhLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. The range is from @30@ to @7200@ seconds. The default value is @3600@ seconds (1 hour).
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling the 'RecordLifecycleActionHeartbeat' API.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhHeartbeatTimeout :: Lens.Lens' PutLifecycleHook (Lude.Maybe Lude.Int)
plhHeartbeatTimeout = Lens.lens (heartbeatTimeout :: PutLifecycleHook -> Lude.Maybe Lude.Int) (\s a -> s {heartbeatTimeout = a} :: PutLifecycleHook)
{-# DEPRECATED plhHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhAutoScalingGroupName :: Lens.Lens' PutLifecycleHook Lude.Text
plhAutoScalingGroupName = Lens.lens (autoScalingGroupName :: PutLifecycleHook -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: PutLifecycleHook)
{-# DEPRECATED plhAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- /Note:/ Consider using 'notificationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhNotificationMetadata :: Lens.Lens' PutLifecycleHook (Lude.Maybe Lude.Text)
plhNotificationMetadata = Lens.lens (notificationMetadata :: PutLifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {notificationMetadata = a} :: PutLifecycleHook)
{-# DEPRECATED plhNotificationMetadata "Use generic-lens or generic-optics with 'notificationMetadata' instead." #-}

-- | The ARN of the notification target that Amazon EC2 Auto Scaling uses to notify you when an instance is in the transition state for the lifecycle hook. This target can be either an SQS queue or an SNS topic.
--
-- If you specify an empty string, this overrides the current ARN.
-- This operation uses the JSON format when sending notifications to an Amazon SQS queue, and an email key-value pair format when sending notifications to an Amazon SNS topic.
-- When you specify a notification target, Amazon EC2 Auto Scaling sends it a test message. Test messages contain the following additional key-value pair: @"Event": "autoscaling:TEST_NOTIFICATION"@ .
--
-- /Note:/ Consider using 'notificationTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhNotificationTargetARN :: Lens.Lens' PutLifecycleHook (Lude.Maybe Lude.Text)
plhNotificationTargetARN = Lens.lens (notificationTargetARN :: PutLifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {notificationTargetARN = a} :: PutLifecycleHook)
{-# DEPRECATED plhNotificationTargetARN "Use generic-lens or generic-optics with 'notificationTargetARN' instead." #-}

-- | The instance state to which you want to attach the lifecycle hook. The valid values are:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
-- Required for new lifecycle hooks, but optional when updating existing hooks.
--
-- /Note:/ Consider using 'lifecycleTransition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhLifecycleTransition :: Lens.Lens' PutLifecycleHook (Lude.Maybe Lude.Text)
plhLifecycleTransition = Lens.lens (lifecycleTransition :: PutLifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleTransition = a} :: PutLifecycleHook)
{-# DEPRECATED plhLifecycleTransition "Use generic-lens or generic-optics with 'lifecycleTransition' instead." #-}

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
--
-- Required for new lifecycle hooks, but optional when updating existing hooks.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhRoleARN :: Lens.Lens' PutLifecycleHook (Lude.Maybe Lude.Text)
plhRoleARN = Lens.lens (roleARN :: PutLifecycleHook -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: PutLifecycleHook)
{-# DEPRECATED plhRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest PutLifecycleHook where
  type Rs PutLifecycleHook = PutLifecycleHookResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "PutLifecycleHookResult"
      ( \s h x ->
          PutLifecycleHookResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutLifecycleHook where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutLifecycleHook where
  toPath = Lude.const "/"

instance Lude.ToQuery PutLifecycleHook where
  toQuery PutLifecycleHook' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutLifecycleHook" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "DefaultResult" Lude.=: defaultResult,
        "LifecycleHookName" Lude.=: lifecycleHookName,
        "HeartbeatTimeout" Lude.=: heartbeatTimeout,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "NotificationMetadata" Lude.=: notificationMetadata,
        "NotificationTargetARN" Lude.=: notificationTargetARN,
        "LifecycleTransition" Lude.=: lifecycleTransition,
        "RoleARN" Lude.=: roleARN
      ]

-- | /See:/ 'mkPutLifecycleHookResponse' smart constructor.
newtype PutLifecycleHookResponse = PutLifecycleHookResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLifecycleHookResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutLifecycleHookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutLifecycleHookResponse
mkPutLifecycleHookResponse pResponseStatus_ =
  PutLifecycleHookResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhrsResponseStatus :: Lens.Lens' PutLifecycleHookResponse Lude.Int
plhrsResponseStatus = Lens.lens (responseStatus :: PutLifecycleHookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutLifecycleHookResponse)
{-# DEPRECATED plhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

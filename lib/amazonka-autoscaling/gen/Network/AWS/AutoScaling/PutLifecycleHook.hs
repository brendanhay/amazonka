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
    plhLifecycleHookName,
    plhAutoScalingGroupName,
    plhDefaultResult,
    plhHeartbeatTimeout,
    plhLifecycleTransition,
    plhNotificationMetadata,
    plhNotificationTargetARN,
    plhRoleARN,

    -- * Destructuring the response
    PutLifecycleHookResponse (..),
    mkPutLifecycleHookResponse,

    -- ** Response lenses
    plhrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLifecycleHook' smart constructor.
data PutLifecycleHook = PutLifecycleHook'
  { -- | The name of the lifecycle hook.
    lifecycleHookName :: Types.LifecycleHookName,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.AutoScalingGroupName,
    -- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. This parameter can be either @CONTINUE@ or @ABANDON@ . The default value is @ABANDON@ .
    defaultResult :: Core.Maybe Types.DefaultResult,
    -- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. The range is from @30@ to @7200@ seconds. The default value is @3600@ seconds (1 hour).
    --
    -- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling the 'RecordLifecycleActionHeartbeat' API.
    heartbeatTimeout :: Core.Maybe Core.Int,
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
    lifecycleTransition :: Core.Maybe Types.LifecycleTransition,
    -- | Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
    notificationMetadata :: Core.Maybe Types.NotificationMetadata,
    -- | The ARN of the notification target that Amazon EC2 Auto Scaling uses to notify you when an instance is in the transition state for the lifecycle hook. This target can be either an SQS queue or an SNS topic.
    --
    -- If you specify an empty string, this overrides the current ARN.
    -- This operation uses the JSON format when sending notifications to an Amazon SQS queue, and an email key-value pair format when sending notifications to an Amazon SNS topic.
    -- When you specify a notification target, Amazon EC2 Auto Scaling sends it a test message. Test messages contain the following additional key-value pair: @"Event": "autoscaling:TEST_NOTIFICATION"@ .
    notificationTargetARN :: Core.Maybe Types.NotificationTargetARN,
    -- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
    --
    -- Required for new lifecycle hooks, but optional when updating existing hooks.
    roleARN :: Core.Maybe Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecycleHook' value with any optional fields omitted.
mkPutLifecycleHook ::
  -- | 'lifecycleHookName'
  Types.LifecycleHookName ->
  -- | 'autoScalingGroupName'
  Types.AutoScalingGroupName ->
  PutLifecycleHook
mkPutLifecycleHook lifecycleHookName autoScalingGroupName =
  PutLifecycleHook'
    { lifecycleHookName,
      autoScalingGroupName,
      defaultResult = Core.Nothing,
      heartbeatTimeout = Core.Nothing,
      lifecycleTransition = Core.Nothing,
      notificationMetadata = Core.Nothing,
      notificationTargetARN = Core.Nothing,
      roleARN = Core.Nothing
    }

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhLifecycleHookName :: Lens.Lens' PutLifecycleHook Types.LifecycleHookName
plhLifecycleHookName = Lens.field @"lifecycleHookName"
{-# DEPRECATED plhLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhAutoScalingGroupName :: Lens.Lens' PutLifecycleHook Types.AutoScalingGroupName
plhAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED plhAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. This parameter can be either @CONTINUE@ or @ABANDON@ . The default value is @ABANDON@ .
--
-- /Note:/ Consider using 'defaultResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhDefaultResult :: Lens.Lens' PutLifecycleHook (Core.Maybe Types.DefaultResult)
plhDefaultResult = Lens.field @"defaultResult"
{-# DEPRECATED plhDefaultResult "Use generic-lens or generic-optics with 'defaultResult' instead." #-}

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. The range is from @30@ to @7200@ seconds. The default value is @3600@ seconds (1 hour).
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling the 'RecordLifecycleActionHeartbeat' API.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhHeartbeatTimeout :: Lens.Lens' PutLifecycleHook (Core.Maybe Core.Int)
plhHeartbeatTimeout = Lens.field @"heartbeatTimeout"
{-# DEPRECATED plhHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

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
plhLifecycleTransition :: Lens.Lens' PutLifecycleHook (Core.Maybe Types.LifecycleTransition)
plhLifecycleTransition = Lens.field @"lifecycleTransition"
{-# DEPRECATED plhLifecycleTransition "Use generic-lens or generic-optics with 'lifecycleTransition' instead." #-}

-- | Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- /Note:/ Consider using 'notificationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhNotificationMetadata :: Lens.Lens' PutLifecycleHook (Core.Maybe Types.NotificationMetadata)
plhNotificationMetadata = Lens.field @"notificationMetadata"
{-# DEPRECATED plhNotificationMetadata "Use generic-lens or generic-optics with 'notificationMetadata' instead." #-}

-- | The ARN of the notification target that Amazon EC2 Auto Scaling uses to notify you when an instance is in the transition state for the lifecycle hook. This target can be either an SQS queue or an SNS topic.
--
-- If you specify an empty string, this overrides the current ARN.
-- This operation uses the JSON format when sending notifications to an Amazon SQS queue, and an email key-value pair format when sending notifications to an Amazon SNS topic.
-- When you specify a notification target, Amazon EC2 Auto Scaling sends it a test message. Test messages contain the following additional key-value pair: @"Event": "autoscaling:TEST_NOTIFICATION"@ .
--
-- /Note:/ Consider using 'notificationTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhNotificationTargetARN :: Lens.Lens' PutLifecycleHook (Core.Maybe Types.NotificationTargetARN)
plhNotificationTargetARN = Lens.field @"notificationTargetARN"
{-# DEPRECATED plhNotificationTargetARN "Use generic-lens or generic-optics with 'notificationTargetARN' instead." #-}

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
--
-- Required for new lifecycle hooks, but optional when updating existing hooks.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhRoleARN :: Lens.Lens' PutLifecycleHook (Core.Maybe Types.RoleARN)
plhRoleARN = Lens.field @"roleARN"
{-# DEPRECATED plhRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.AWSRequest PutLifecycleHook where
  type Rs PutLifecycleHook = PutLifecycleHookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PutLifecycleHook")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "LifecycleHookName" lifecycleHookName)
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "DefaultResult" Core.<$> defaultResult)
                Core.<> (Core.toQueryValue "HeartbeatTimeout" Core.<$> heartbeatTimeout)
                Core.<> ( Core.toQueryValue "LifecycleTransition"
                            Core.<$> lifecycleTransition
                        )
                Core.<> ( Core.toQueryValue "NotificationMetadata"
                            Core.<$> notificationMetadata
                        )
                Core.<> ( Core.toQueryValue "NotificationTargetARN"
                            Core.<$> notificationTargetARN
                        )
                Core.<> (Core.toQueryValue "RoleARN" Core.<$> roleARN)
            )
      }
  response =
    Response.receiveXMLWrapper
      "PutLifecycleHookResult"
      ( \s h x ->
          PutLifecycleHookResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutLifecycleHookResponse' smart constructor.
newtype PutLifecycleHookResponse = PutLifecycleHookResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecycleHookResponse' value with any optional fields omitted.
mkPutLifecycleHookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutLifecycleHookResponse
mkPutLifecycleHookResponse responseStatus =
  PutLifecycleHookResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plhrrsResponseStatus :: Lens.Lens' PutLifecycleHookResponse Core.Int
plhrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED plhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

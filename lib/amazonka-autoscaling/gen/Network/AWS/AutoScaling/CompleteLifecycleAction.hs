{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CompleteLifecycleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes the lifecycle action for the specified token or instance with the specified result.
--
-- This step is a part of the procedure for adding a lifecycle hook to an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Amazon EC2 Auto Scaling launches or terminates instances.
--
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Amazon EC2 Auto Scaling to publish lifecycle notifications to the target.
--
--
--     * Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.
--
--
--     * If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.
--
--
--     * __If you finish before the timeout period ends, complete the lifecycle action.__
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.CompleteLifecycleAction
  ( -- * Creating a request
    CompleteLifecycleAction (..),
    mkCompleteLifecycleAction,

    -- ** Request lenses
    claLifecycleHookName,
    claAutoScalingGroupName,
    claLifecycleActionResult,
    claInstanceId,
    claLifecycleActionToken,

    -- * Destructuring the response
    CompleteLifecycleActionResponse (..),
    mkCompleteLifecycleActionResponse,

    -- ** Response lenses
    clarrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCompleteLifecycleAction' smart constructor.
data CompleteLifecycleAction = CompleteLifecycleAction'
  { -- | The name of the lifecycle hook.
    lifecycleHookName :: Types.AsciiStringMaxLen255,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The action for the group to take. This parameter can be either @CONTINUE@ or @ABANDON@ .
    lifecycleActionResult :: Types.LifecycleActionResult,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.XmlStringMaxLen19,
    -- | A universally unique identifier (UUID) that identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
    lifecycleActionToken :: Core.Maybe Types.LifecycleActionToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteLifecycleAction' value with any optional fields omitted.
mkCompleteLifecycleAction ::
  -- | 'lifecycleHookName'
  Types.AsciiStringMaxLen255 ->
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  -- | 'lifecycleActionResult'
  Types.LifecycleActionResult ->
  CompleteLifecycleAction
mkCompleteLifecycleAction
  lifecycleHookName
  autoScalingGroupName
  lifecycleActionResult =
    CompleteLifecycleAction'
      { lifecycleHookName,
        autoScalingGroupName,
        lifecycleActionResult,
        instanceId = Core.Nothing,
        lifecycleActionToken = Core.Nothing
      }

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLifecycleHookName :: Lens.Lens' CompleteLifecycleAction Types.AsciiStringMaxLen255
claLifecycleHookName = Lens.field @"lifecycleHookName"
{-# DEPRECATED claLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claAutoScalingGroupName :: Lens.Lens' CompleteLifecycleAction Types.ResourceName
claAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED claAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The action for the group to take. This parameter can be either @CONTINUE@ or @ABANDON@ .
--
-- /Note:/ Consider using 'lifecycleActionResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLifecycleActionResult :: Lens.Lens' CompleteLifecycleAction Types.LifecycleActionResult
claLifecycleActionResult = Lens.field @"lifecycleActionResult"
{-# DEPRECATED claLifecycleActionResult "Use generic-lens or generic-optics with 'lifecycleActionResult' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claInstanceId :: Lens.Lens' CompleteLifecycleAction (Core.Maybe Types.XmlStringMaxLen19)
claInstanceId = Lens.field @"instanceId"
{-# DEPRECATED claInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A universally unique identifier (UUID) that identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleActionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLifecycleActionToken :: Lens.Lens' CompleteLifecycleAction (Core.Maybe Types.LifecycleActionToken)
claLifecycleActionToken = Lens.field @"lifecycleActionToken"
{-# DEPRECATED claLifecycleActionToken "Use generic-lens or generic-optics with 'lifecycleActionToken' instead." #-}

instance Core.AWSRequest CompleteLifecycleAction where
  type Rs CompleteLifecycleAction = CompleteLifecycleActionResponse
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
            ( Core.pure ("Action", "CompleteLifecycleAction")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "LifecycleHookName" lifecycleHookName)
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "LifecycleActionResult" lifecycleActionResult)
                Core.<> (Core.toQueryValue "InstanceId" Core.<$> instanceId)
                Core.<> ( Core.toQueryValue "LifecycleActionToken"
                            Core.<$> lifecycleActionToken
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CompleteLifecycleActionResult"
      ( \s h x ->
          CompleteLifecycleActionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCompleteLifecycleActionResponse' smart constructor.
newtype CompleteLifecycleActionResponse = CompleteLifecycleActionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteLifecycleActionResponse' value with any optional fields omitted.
mkCompleteLifecycleActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CompleteLifecycleActionResponse
mkCompleteLifecycleActionResponse responseStatus =
  CompleteLifecycleActionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clarrsResponseStatus :: Lens.Lens' CompleteLifecycleActionResponse Core.Int
clarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
    claInstanceId,
    claLifecycleHookName,
    claLifecycleActionToken,
    claLifecycleActionResult,
    claAutoScalingGroupName,

    -- * Destructuring the response
    CompleteLifecycleActionResponse (..),
    mkCompleteLifecycleActionResponse,

    -- ** Response lenses
    clarsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCompleteLifecycleAction' smart constructor.
data CompleteLifecycleAction = CompleteLifecycleAction'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Lude.Text,
    -- | A universally unique identifier (UUID) that identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
    lifecycleActionToken :: Lude.Maybe Lude.Text,
    -- | The action for the group to take. This parameter can be either @CONTINUE@ or @ABANDON@ .
    lifecycleActionResult :: Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteLifecycleAction' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'lifecycleHookName' - The name of the lifecycle hook.
-- * 'lifecycleActionToken' - A universally unique identifier (UUID) that identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
-- * 'lifecycleActionResult' - The action for the group to take. This parameter can be either @CONTINUE@ or @ABANDON@ .
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkCompleteLifecycleAction ::
  -- | 'lifecycleHookName'
  Lude.Text ->
  -- | 'lifecycleActionResult'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  CompleteLifecycleAction
mkCompleteLifecycleAction
  pLifecycleHookName_
  pLifecycleActionResult_
  pAutoScalingGroupName_ =
    CompleteLifecycleAction'
      { instanceId = Lude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        lifecycleActionToken = Lude.Nothing,
        lifecycleActionResult = pLifecycleActionResult_,
        autoScalingGroupName = pAutoScalingGroupName_
      }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claInstanceId :: Lens.Lens' CompleteLifecycleAction (Lude.Maybe Lude.Text)
claInstanceId = Lens.lens (instanceId :: CompleteLifecycleAction -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CompleteLifecycleAction)
{-# DEPRECATED claInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLifecycleHookName :: Lens.Lens' CompleteLifecycleAction Lude.Text
claLifecycleHookName = Lens.lens (lifecycleHookName :: CompleteLifecycleAction -> Lude.Text) (\s a -> s {lifecycleHookName = a} :: CompleteLifecycleAction)
{-# DEPRECATED claLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | A universally unique identifier (UUID) that identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleActionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLifecycleActionToken :: Lens.Lens' CompleteLifecycleAction (Lude.Maybe Lude.Text)
claLifecycleActionToken = Lens.lens (lifecycleActionToken :: CompleteLifecycleAction -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleActionToken = a} :: CompleteLifecycleAction)
{-# DEPRECATED claLifecycleActionToken "Use generic-lens or generic-optics with 'lifecycleActionToken' instead." #-}

-- | The action for the group to take. This parameter can be either @CONTINUE@ or @ABANDON@ .
--
-- /Note:/ Consider using 'lifecycleActionResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLifecycleActionResult :: Lens.Lens' CompleteLifecycleAction Lude.Text
claLifecycleActionResult = Lens.lens (lifecycleActionResult :: CompleteLifecycleAction -> Lude.Text) (\s a -> s {lifecycleActionResult = a} :: CompleteLifecycleAction)
{-# DEPRECATED claLifecycleActionResult "Use generic-lens or generic-optics with 'lifecycleActionResult' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claAutoScalingGroupName :: Lens.Lens' CompleteLifecycleAction Lude.Text
claAutoScalingGroupName = Lens.lens (autoScalingGroupName :: CompleteLifecycleAction -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: CompleteLifecycleAction)
{-# DEPRECATED claAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest CompleteLifecycleAction where
  type Rs CompleteLifecycleAction = CompleteLifecycleActionResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "CompleteLifecycleActionResult"
      ( \s h x ->
          CompleteLifecycleActionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CompleteLifecycleAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CompleteLifecycleAction where
  toPath = Lude.const "/"

instance Lude.ToQuery CompleteLifecycleAction where
  toQuery CompleteLifecycleAction' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CompleteLifecycleAction" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "LifecycleHookName" Lude.=: lifecycleHookName,
        "LifecycleActionToken" Lude.=: lifecycleActionToken,
        "LifecycleActionResult" Lude.=: lifecycleActionResult,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkCompleteLifecycleActionResponse' smart constructor.
newtype CompleteLifecycleActionResponse = CompleteLifecycleActionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteLifecycleActionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCompleteLifecycleActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CompleteLifecycleActionResponse
mkCompleteLifecycleActionResponse pResponseStatus_ =
  CompleteLifecycleActionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clarsResponseStatus :: Lens.Lens' CompleteLifecycleActionResponse Lude.Int
clarsResponseStatus = Lens.lens (responseStatus :: CompleteLifecycleActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CompleteLifecycleActionResponse)
{-# DEPRECATED clarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

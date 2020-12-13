{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a heartbeat for the lifecycle action associated with the specified token or instance. This extends the timeout by the length of time defined using the 'PutLifecycleHook' API call.
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
--     * __If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.__
--
--
--     * If you finish before the timeout period ends, complete the lifecycle action.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Auto Scaling lifecycle> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
  ( -- * Creating a request
    RecordLifecycleActionHeartbeat (..),
    mkRecordLifecycleActionHeartbeat,

    -- ** Request lenses
    rlahInstanceId,
    rlahLifecycleHookName,
    rlahLifecycleActionToken,
    rlahAutoScalingGroupName,

    -- * Destructuring the response
    RecordLifecycleActionHeartbeatResponse (..),
    mkRecordLifecycleActionHeartbeatResponse,

    -- ** Response lenses
    rlahrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRecordLifecycleActionHeartbeat' smart constructor.
data RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeat'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The name of the lifecycle hook.
    lifecycleHookName :: Lude.Text,
    -- | A token that uniquely identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target that you specified when you created the lifecycle hook.
    lifecycleActionToken :: Lude.Maybe Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordLifecycleActionHeartbeat' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'lifecycleHookName' - The name of the lifecycle hook.
-- * 'lifecycleActionToken' - A token that uniquely identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target that you specified when you created the lifecycle hook.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkRecordLifecycleActionHeartbeat ::
  -- | 'lifecycleHookName'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  RecordLifecycleActionHeartbeat
mkRecordLifecycleActionHeartbeat
  pLifecycleHookName_
  pAutoScalingGroupName_ =
    RecordLifecycleActionHeartbeat'
      { instanceId = Lude.Nothing,
        lifecycleHookName = pLifecycleHookName_,
        lifecycleActionToken = Lude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_
      }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahInstanceId :: Lens.Lens' RecordLifecycleActionHeartbeat (Lude.Maybe Lude.Text)
rlahInstanceId = Lens.lens (instanceId :: RecordLifecycleActionHeartbeat -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: RecordLifecycleActionHeartbeat)
{-# DEPRECATED rlahInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahLifecycleHookName :: Lens.Lens' RecordLifecycleActionHeartbeat Lude.Text
rlahLifecycleHookName = Lens.lens (lifecycleHookName :: RecordLifecycleActionHeartbeat -> Lude.Text) (\s a -> s {lifecycleHookName = a} :: RecordLifecycleActionHeartbeat)
{-# DEPRECATED rlahLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | A token that uniquely identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target that you specified when you created the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleActionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahLifecycleActionToken :: Lens.Lens' RecordLifecycleActionHeartbeat (Lude.Maybe Lude.Text)
rlahLifecycleActionToken = Lens.lens (lifecycleActionToken :: RecordLifecycleActionHeartbeat -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleActionToken = a} :: RecordLifecycleActionHeartbeat)
{-# DEPRECATED rlahLifecycleActionToken "Use generic-lens or generic-optics with 'lifecycleActionToken' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahAutoScalingGroupName :: Lens.Lens' RecordLifecycleActionHeartbeat Lude.Text
rlahAutoScalingGroupName = Lens.lens (autoScalingGroupName :: RecordLifecycleActionHeartbeat -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: RecordLifecycleActionHeartbeat)
{-# DEPRECATED rlahAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest RecordLifecycleActionHeartbeat where
  type
    Rs RecordLifecycleActionHeartbeat =
      RecordLifecycleActionHeartbeatResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "RecordLifecycleActionHeartbeatResult"
      ( \s h x ->
          RecordLifecycleActionHeartbeatResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RecordLifecycleActionHeartbeat where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RecordLifecycleActionHeartbeat where
  toPath = Lude.const "/"

instance Lude.ToQuery RecordLifecycleActionHeartbeat where
  toQuery RecordLifecycleActionHeartbeat' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RecordLifecycleActionHeartbeat" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "LifecycleHookName" Lude.=: lifecycleHookName,
        "LifecycleActionToken" Lude.=: lifecycleActionToken,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkRecordLifecycleActionHeartbeatResponse' smart constructor.
newtype RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordLifecycleActionHeartbeatResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRecordLifecycleActionHeartbeatResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RecordLifecycleActionHeartbeatResponse
mkRecordLifecycleActionHeartbeatResponse pResponseStatus_ =
  RecordLifecycleActionHeartbeatResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahrsResponseStatus :: Lens.Lens' RecordLifecycleActionHeartbeatResponse Lude.Int
rlahrsResponseStatus = Lens.lens (responseStatus :: RecordLifecycleActionHeartbeatResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RecordLifecycleActionHeartbeatResponse)
{-# DEPRECATED rlahrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

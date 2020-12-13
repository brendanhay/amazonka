{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the health status of the specified instance.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.SetInstanceHealth
  ( -- * Creating a request
    SetInstanceHealth (..),
    mkSetInstanceHealth,

    -- ** Request lenses
    sihInstanceId,
    sihShouldRespectGracePeriod,
    sihHealthStatus,

    -- * Destructuring the response
    SetInstanceHealthResponse (..),
    mkSetInstanceHealthResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetInstanceHealth' smart constructor.
data SetInstanceHealth = SetInstanceHealth'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | If the Auto Scaling group of the specified instance has a @HealthCheckGracePeriod@ specified for the group, by default, this call respects the grace period. Set this to @False@ , to have the call not respect the grace period associated with the group.
    --
    -- For more information about the health check grace period, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup> in the /Amazon EC2 Auto Scaling API Reference/ .
    shouldRespectGracePeriod :: Lude.Maybe Lude.Bool,
    -- | The health status of the instance. Set to @Healthy@ to have the instance remain in service. Set to @Unhealthy@ to have the instance be out of service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy instance.
    healthStatus :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetInstanceHealth' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'shouldRespectGracePeriod' - If the Auto Scaling group of the specified instance has a @HealthCheckGracePeriod@ specified for the group, by default, this call respects the grace period. Set this to @False@ , to have the call not respect the grace period associated with the group.
--
-- For more information about the health check grace period, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup> in the /Amazon EC2 Auto Scaling API Reference/ .
-- * 'healthStatus' - The health status of the instance. Set to @Healthy@ to have the instance remain in service. Set to @Unhealthy@ to have the instance be out of service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy instance.
mkSetInstanceHealth ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'healthStatus'
  Lude.Text ->
  SetInstanceHealth
mkSetInstanceHealth pInstanceId_ pHealthStatus_ =
  SetInstanceHealth'
    { instanceId = pInstanceId_,
      shouldRespectGracePeriod = Lude.Nothing,
      healthStatus = pHealthStatus_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceId :: Lens.Lens' SetInstanceHealth Lude.Text
sihInstanceId = Lens.lens (instanceId :: SetInstanceHealth -> Lude.Text) (\s a -> s {instanceId = a} :: SetInstanceHealth)
{-# DEPRECATED sihInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | If the Auto Scaling group of the specified instance has a @HealthCheckGracePeriod@ specified for the group, by default, this call respects the grace period. Set this to @False@ , to have the call not respect the grace period associated with the group.
--
-- For more information about the health check grace period, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup> in the /Amazon EC2 Auto Scaling API Reference/ .
--
-- /Note:/ Consider using 'shouldRespectGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihShouldRespectGracePeriod :: Lens.Lens' SetInstanceHealth (Lude.Maybe Lude.Bool)
sihShouldRespectGracePeriod = Lens.lens (shouldRespectGracePeriod :: SetInstanceHealth -> Lude.Maybe Lude.Bool) (\s a -> s {shouldRespectGracePeriod = a} :: SetInstanceHealth)
{-# DEPRECATED sihShouldRespectGracePeriod "Use generic-lens or generic-optics with 'shouldRespectGracePeriod' instead." #-}

-- | The health status of the instance. Set to @Healthy@ to have the instance remain in service. Set to @Unhealthy@ to have the instance be out of service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy instance.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihHealthStatus :: Lens.Lens' SetInstanceHealth Lude.Text
sihHealthStatus = Lens.lens (healthStatus :: SetInstanceHealth -> Lude.Text) (\s a -> s {healthStatus = a} :: SetInstanceHealth)
{-# DEPRECATED sihHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

instance Lude.AWSRequest SetInstanceHealth where
  type Rs SetInstanceHealth = SetInstanceHealthResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull SetInstanceHealthResponse'

instance Lude.ToHeaders SetInstanceHealth where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetInstanceHealth where
  toPath = Lude.const "/"

instance Lude.ToQuery SetInstanceHealth where
  toQuery SetInstanceHealth' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetInstanceHealth" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "ShouldRespectGracePeriod" Lude.=: shouldRespectGracePeriod,
        "HealthStatus" Lude.=: healthStatus
      ]

-- | /See:/ 'mkSetInstanceHealthResponse' smart constructor.
data SetInstanceHealthResponse = SetInstanceHealthResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetInstanceHealthResponse' with the minimum fields required to make a request.
mkSetInstanceHealthResponse ::
  SetInstanceHealthResponse
mkSetInstanceHealthResponse = SetInstanceHealthResponse'

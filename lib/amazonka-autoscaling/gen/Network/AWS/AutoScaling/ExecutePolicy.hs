{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ExecutePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes the specified policy. This can be useful for testing the design of your scaling policy.
module Network.AWS.AutoScaling.ExecutePolicy
  ( -- * Creating a request
    ExecutePolicy (..),
    mkExecutePolicy,

    -- ** Request lenses
    epHonorCooldown,
    epMetricValue,
    epAutoScalingGroupName,
    epBreachThreshold,
    epPolicyName,

    -- * Destructuring the response
    ExecutePolicyResponse (..),
    mkExecutePolicyResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExecutePolicy' smart constructor.
data ExecutePolicy = ExecutePolicy'
  { honorCooldown ::
      Lude.Maybe Lude.Bool,
    metricValue :: Lude.Maybe Lude.Double,
    autoScalingGroupName :: Lude.Maybe Lude.Text,
    breachThreshold :: Lude.Maybe Lude.Double,
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutePolicy' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'breachThreshold' - The breach threshold for the alarm.
--
-- Required if the policy type is @StepScaling@ and not supported otherwise.
-- * 'honorCooldown' - Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before executing the policy.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'metricValue' - The metric value to compare to @BreachThreshold@ . This enables you to execute a policy of type @StepScaling@ and determine which step adjustment to use. For example, if the breach threshold is 50 and you want to use a step adjustment with a lower bound of 0 and an upper bound of 10, you can set the metric value to 59.
--
-- If you specify a metric value that doesn't correspond to a step adjustment for the policy, the call returns an error.
-- Required if the policy type is @StepScaling@ and not supported otherwise.
-- * 'policyName' - The name or ARN of the policy.
mkExecutePolicy ::
  -- | 'policyName'
  Lude.Text ->
  ExecutePolicy
mkExecutePolicy pPolicyName_ =
  ExecutePolicy'
    { honorCooldown = Lude.Nothing,
      metricValue = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      breachThreshold = Lude.Nothing,
      policyName = pPolicyName_
    }

-- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before executing the policy.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'honorCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epHonorCooldown :: Lens.Lens' ExecutePolicy (Lude.Maybe Lude.Bool)
epHonorCooldown = Lens.lens (honorCooldown :: ExecutePolicy -> Lude.Maybe Lude.Bool) (\s a -> s {honorCooldown = a} :: ExecutePolicy)
{-# DEPRECATED epHonorCooldown "Use generic-lens or generic-optics with 'honorCooldown' instead." #-}

-- | The metric value to compare to @BreachThreshold@ . This enables you to execute a policy of type @StepScaling@ and determine which step adjustment to use. For example, if the breach threshold is 50 and you want to use a step adjustment with a lower bound of 0 and an upper bound of 10, you can set the metric value to 59.
--
-- If you specify a metric value that doesn't correspond to a step adjustment for the policy, the call returns an error.
-- Required if the policy type is @StepScaling@ and not supported otherwise.
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epMetricValue :: Lens.Lens' ExecutePolicy (Lude.Maybe Lude.Double)
epMetricValue = Lens.lens (metricValue :: ExecutePolicy -> Lude.Maybe Lude.Double) (\s a -> s {metricValue = a} :: ExecutePolicy)
{-# DEPRECATED epMetricValue "Use generic-lens or generic-optics with 'metricValue' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epAutoScalingGroupName :: Lens.Lens' ExecutePolicy (Lude.Maybe Lude.Text)
epAutoScalingGroupName = Lens.lens (autoScalingGroupName :: ExecutePolicy -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: ExecutePolicy)
{-# DEPRECATED epAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The breach threshold for the alarm.
--
-- Required if the policy type is @StepScaling@ and not supported otherwise.
--
-- /Note:/ Consider using 'breachThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epBreachThreshold :: Lens.Lens' ExecutePolicy (Lude.Maybe Lude.Double)
epBreachThreshold = Lens.lens (breachThreshold :: ExecutePolicy -> Lude.Maybe Lude.Double) (\s a -> s {breachThreshold = a} :: ExecutePolicy)
{-# DEPRECATED epBreachThreshold "Use generic-lens or generic-optics with 'breachThreshold' instead." #-}

-- | The name or ARN of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyName :: Lens.Lens' ExecutePolicy Lude.Text
epPolicyName = Lens.lens (policyName :: ExecutePolicy -> Lude.Text) (\s a -> s {policyName = a} :: ExecutePolicy)
{-# DEPRECATED epPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest ExecutePolicy where
  type Rs ExecutePolicy = ExecutePolicyResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull ExecutePolicyResponse'

instance Lude.ToHeaders ExecutePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ExecutePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery ExecutePolicy where
  toQuery ExecutePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ExecutePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "HonorCooldown" Lude.=: honorCooldown,
        "MetricValue" Lude.=: metricValue,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "BreachThreshold" Lude.=: breachThreshold,
        "PolicyName" Lude.=: policyName
      ]

-- | /See:/ 'mkExecutePolicyResponse' smart constructor.
data ExecutePolicyResponse = ExecutePolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutePolicyResponse' with the minimum fields required to make a request.
mkExecutePolicyResponse ::
  ExecutePolicyResponse
mkExecutePolicyResponse = ExecutePolicyResponse'

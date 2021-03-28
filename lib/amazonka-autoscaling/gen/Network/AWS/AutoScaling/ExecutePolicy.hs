{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ExecutePolicy (..)
    , mkExecutePolicy
    -- ** Request lenses
    , epPolicyName
    , epAutoScalingGroupName
    , epBreachThreshold
    , epHonorCooldown
    , epMetricValue

    -- * Destructuring the response
    , ExecutePolicyResponse (..)
    , mkExecutePolicyResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExecutePolicy' smart constructor.
data ExecutePolicy = ExecutePolicy'
  { policyName :: Types.PolicyName
    -- ^ The name or ARN of the policy.
  , autoScalingGroupName :: Core.Maybe Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , breachThreshold :: Core.Maybe Core.Double
    -- ^ The breach threshold for the alarm.
--
-- Required if the policy type is @StepScaling@ and not supported otherwise.
  , honorCooldown :: Core.Maybe Core.Bool
    -- ^ Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before executing the policy.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
  , metricValue :: Core.Maybe Core.Double
    -- ^ The metric value to compare to @BreachThreshold@ . This enables you to execute a policy of type @StepScaling@ and determine which step adjustment to use. For example, if the breach threshold is 50 and you want to use a step adjustment with a lower bound of 0 and an upper bound of 10, you can set the metric value to 59.
--
-- If you specify a metric value that doesn't correspond to a step adjustment for the policy, the call returns an error.
-- Required if the policy type is @StepScaling@ and not supported otherwise.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutePolicy' value with any optional fields omitted.
mkExecutePolicy
    :: Types.PolicyName -- ^ 'policyName'
    -> ExecutePolicy
mkExecutePolicy policyName
  = ExecutePolicy'{policyName, autoScalingGroupName = Core.Nothing,
                   breachThreshold = Core.Nothing, honorCooldown = Core.Nothing,
                   metricValue = Core.Nothing}

-- | The name or ARN of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyName :: Lens.Lens' ExecutePolicy Types.PolicyName
epPolicyName = Lens.field @"policyName"
{-# INLINEABLE epPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epAutoScalingGroupName :: Lens.Lens' ExecutePolicy (Core.Maybe Types.AutoScalingGroupName)
epAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE epAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The breach threshold for the alarm.
--
-- Required if the policy type is @StepScaling@ and not supported otherwise.
--
-- /Note:/ Consider using 'breachThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epBreachThreshold :: Lens.Lens' ExecutePolicy (Core.Maybe Core.Double)
epBreachThreshold = Lens.field @"breachThreshold"
{-# INLINEABLE epBreachThreshold #-}
{-# DEPRECATED breachThreshold "Use generic-lens or generic-optics with 'breachThreshold' instead"  #-}

-- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before executing the policy.
--
-- Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'honorCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epHonorCooldown :: Lens.Lens' ExecutePolicy (Core.Maybe Core.Bool)
epHonorCooldown = Lens.field @"honorCooldown"
{-# INLINEABLE epHonorCooldown #-}
{-# DEPRECATED honorCooldown "Use generic-lens or generic-optics with 'honorCooldown' instead"  #-}

-- | The metric value to compare to @BreachThreshold@ . This enables you to execute a policy of type @StepScaling@ and determine which step adjustment to use. For example, if the breach threshold is 50 and you want to use a step adjustment with a lower bound of 0 and an upper bound of 10, you can set the metric value to 59.
--
-- If you specify a metric value that doesn't correspond to a step adjustment for the policy, the call returns an error.
-- Required if the policy type is @StepScaling@ and not supported otherwise.
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epMetricValue :: Lens.Lens' ExecutePolicy (Core.Maybe Core.Double)
epMetricValue = Lens.field @"metricValue"
{-# INLINEABLE epMetricValue #-}
{-# DEPRECATED metricValue "Use generic-lens or generic-optics with 'metricValue' instead"  #-}

instance Core.ToQuery ExecutePolicy where
        toQuery ExecutePolicy{..}
          = Core.toQueryPair "Action" ("ExecutePolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoScalingGroupName")
                autoScalingGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BreachThreshold")
                breachThreshold
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HonorCooldown")
                honorCooldown
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MetricValue") metricValue

instance Core.ToHeaders ExecutePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ExecutePolicy where
        type Rs ExecutePolicy = ExecutePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ExecutePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExecutePolicyResponse' smart constructor.
data ExecutePolicyResponse = ExecutePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutePolicyResponse' value with any optional fields omitted.
mkExecutePolicyResponse
    :: ExecutePolicyResponse
mkExecutePolicyResponse = ExecutePolicyResponse'

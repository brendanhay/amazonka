{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types.MetricPolicy
  ( MetricPolicy (..)
  -- * Smart constructor
  , mkMetricPolicy
  -- * Lenses
  , mpContainerLevelMetrics
  , mpMetricPolicyRules
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types.ContainerLevelMetrics as Types
import qualified Network.AWS.MediaStore.Types.MetricPolicyRule as Types
import qualified Network.AWS.Prelude as Core

-- | The metric policy that is associated with the container. A metric policy allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include rules to define groups of objects that you want MediaStore to send object-level metrics for.
--
-- To view examples of how to construct a metric policy for your use case, see <https://docs.aws.amazon.com/mediastore/latest/ug/policies-metric-examples.html Example Metric Policies> .
--
-- /See:/ 'mkMetricPolicy' smart constructor.
data MetricPolicy = MetricPolicy'
  { containerLevelMetrics :: Types.ContainerLevelMetrics
    -- ^ A setting to enable or disable metrics at the container level.
  , metricPolicyRules :: Core.Maybe (Core.NonEmpty Types.MetricPolicyRule)
    -- ^ A parameter that holds an array of rules that enable metrics at the object level. This parameter is optional, but if you choose to include it, you must also include at least one rule. By default, you can include up to five rules. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricPolicy' value with any optional fields omitted.
mkMetricPolicy
    :: Types.ContainerLevelMetrics -- ^ 'containerLevelMetrics'
    -> MetricPolicy
mkMetricPolicy containerLevelMetrics
  = MetricPolicy'{containerLevelMetrics,
                  metricPolicyRules = Core.Nothing}

-- | A setting to enable or disable metrics at the container level.
--
-- /Note:/ Consider using 'containerLevelMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpContainerLevelMetrics :: Lens.Lens' MetricPolicy Types.ContainerLevelMetrics
mpContainerLevelMetrics = Lens.field @"containerLevelMetrics"
{-# INLINEABLE mpContainerLevelMetrics #-}
{-# DEPRECATED containerLevelMetrics "Use generic-lens or generic-optics with 'containerLevelMetrics' instead"  #-}

-- | A parameter that holds an array of rules that enable metrics at the object level. This parameter is optional, but if you choose to include it, you must also include at least one rule. By default, you can include up to five rules. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
--
-- /Note:/ Consider using 'metricPolicyRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpMetricPolicyRules :: Lens.Lens' MetricPolicy (Core.Maybe (Core.NonEmpty Types.MetricPolicyRule))
mpMetricPolicyRules = Lens.field @"metricPolicyRules"
{-# INLINEABLE mpMetricPolicyRules #-}
{-# DEPRECATED metricPolicyRules "Use generic-lens or generic-optics with 'metricPolicyRules' instead"  #-}

instance Core.FromJSON MetricPolicy where
        toJSON MetricPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerLevelMetrics" Core..= containerLevelMetrics),
                  ("MetricPolicyRules" Core..=) Core.<$> metricPolicyRules])

instance Core.FromJSON MetricPolicy where
        parseJSON
          = Core.withObject "MetricPolicy" Core.$
              \ x ->
                MetricPolicy' Core.<$>
                  (x Core..: "ContainerLevelMetrics") Core.<*>
                    x Core..:? "MetricPolicyRules"

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.MetricPolicy
  ( MetricPolicy (..),

    -- * Smart constructor
    mkMetricPolicy,

    -- * Lenses
    mpMetricPolicyRules,
    mpContainerLevelMetrics,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types.ContainerLevelMetrics
import Network.AWS.MediaStore.Types.MetricPolicyRule
import qualified Network.AWS.Prelude as Lude

-- | The metric policy that is associated with the container. A metric policy allows AWS Elemental MediaStore to send metrics to Amazon CloudWatch. In the policy, you must indicate whether you want MediaStore to send container-level metrics. You can also include rules to define groups of objects that you want MediaStore to send object-level metrics for.
--
-- To view examples of how to construct a metric policy for your use case, see <https://docs.aws.amazon.com/mediastore/latest/ug/policies-metric-examples.html Example Metric Policies> .
--
-- /See:/ 'mkMetricPolicy' smart constructor.
data MetricPolicy = MetricPolicy'
  { metricPolicyRules ::
      Lude.Maybe (Lude.NonEmpty MetricPolicyRule),
    containerLevelMetrics :: ContainerLevelMetrics
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricPolicy' with the minimum fields required to make a request.
--
-- * 'containerLevelMetrics' - A setting to enable or disable metrics at the container level.
-- * 'metricPolicyRules' - A parameter that holds an array of rules that enable metrics at the object level. This parameter is optional, but if you choose to include it, you must also include at least one rule. By default, you can include up to five rules. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
mkMetricPolicy ::
  -- | 'containerLevelMetrics'
  ContainerLevelMetrics ->
  MetricPolicy
mkMetricPolicy pContainerLevelMetrics_ =
  MetricPolicy'
    { metricPolicyRules = Lude.Nothing,
      containerLevelMetrics = pContainerLevelMetrics_
    }

-- | A parameter that holds an array of rules that enable metrics at the object level. This parameter is optional, but if you choose to include it, you must also include at least one rule. By default, you can include up to five rules. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
--
-- /Note:/ Consider using 'metricPolicyRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpMetricPolicyRules :: Lens.Lens' MetricPolicy (Lude.Maybe (Lude.NonEmpty MetricPolicyRule))
mpMetricPolicyRules = Lens.lens (metricPolicyRules :: MetricPolicy -> Lude.Maybe (Lude.NonEmpty MetricPolicyRule)) (\s a -> s {metricPolicyRules = a} :: MetricPolicy)
{-# DEPRECATED mpMetricPolicyRules "Use generic-lens or generic-optics with 'metricPolicyRules' instead." #-}

-- | A setting to enable or disable metrics at the container level.
--
-- /Note:/ Consider using 'containerLevelMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpContainerLevelMetrics :: Lens.Lens' MetricPolicy ContainerLevelMetrics
mpContainerLevelMetrics = Lens.lens (containerLevelMetrics :: MetricPolicy -> ContainerLevelMetrics) (\s a -> s {containerLevelMetrics = a} :: MetricPolicy)
{-# DEPRECATED mpContainerLevelMetrics "Use generic-lens or generic-optics with 'containerLevelMetrics' instead." #-}

instance Lude.FromJSON MetricPolicy where
  parseJSON =
    Lude.withObject
      "MetricPolicy"
      ( \x ->
          MetricPolicy'
            Lude.<$> (x Lude..:? "MetricPolicyRules")
            Lude.<*> (x Lude..: "ContainerLevelMetrics")
      )

instance Lude.ToJSON MetricPolicy where
  toJSON MetricPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MetricPolicyRules" Lude..=) Lude.<$> metricPolicyRules,
            Lude.Just ("ContainerLevelMetrics" Lude..= containerLevelMetrics)
          ]
      )

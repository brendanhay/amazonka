{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary
  ( SubscribedRuleGroupSummary (..),

    -- * Smart constructor
    mkSubscribedRuleGroupSummary,

    -- * Lenses
    srgsRuleGroupId,
    srgsName,
    srgsMetricName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.MetricName as Types
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types

-- | A summary of the rule groups you are subscribed to.
--
-- /See:/ 'mkSubscribedRuleGroupSummary' smart constructor.
data SubscribedRuleGroupSummary = SubscribedRuleGroupSummary'
  { -- | A unique identifier for a @RuleGroup@ .
    ruleGroupId :: Types.ResourceId,
    -- | A friendly name or description of the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
    name :: Types.ResourceName,
    -- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
    metricName :: Types.MetricName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscribedRuleGroupSummary' value with any optional fields omitted.
mkSubscribedRuleGroupSummary ::
  -- | 'ruleGroupId'
  Types.ResourceId ->
  -- | 'name'
  Types.ResourceName ->
  -- | 'metricName'
  Types.MetricName ->
  SubscribedRuleGroupSummary
mkSubscribedRuleGroupSummary ruleGroupId name metricName =
  SubscribedRuleGroupSummary' {ruleGroupId, name, metricName}

-- | A unique identifier for a @RuleGroup@ .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgsRuleGroupId :: Lens.Lens' SubscribedRuleGroupSummary Types.ResourceId
srgsRuleGroupId = Lens.field @"ruleGroupId"
{-# DEPRECATED srgsRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | A friendly name or description of the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgsName :: Lens.Lens' SubscribedRuleGroupSummary Types.ResourceName
srgsName = Lens.field @"name"
{-# DEPRECATED srgsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgsMetricName :: Lens.Lens' SubscribedRuleGroupSummary Types.MetricName
srgsMetricName = Lens.field @"metricName"
{-# DEPRECATED srgsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

instance Core.FromJSON SubscribedRuleGroupSummary where
  parseJSON =
    Core.withObject "SubscribedRuleGroupSummary" Core.$
      \x ->
        SubscribedRuleGroupSummary'
          Core.<$> (x Core..: "RuleGroupId")
          Core.<*> (x Core..: "Name")
          Core.<*> (x Core..: "MetricName")
